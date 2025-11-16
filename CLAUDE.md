# CLAUDE.md - AI Assistant Guide for simple-amount

This file is intended for use by AI coding assistants to understand and modify this codebase effectively.

## Development Commands

### Building
```bash
# Using Cabal (primary method)
cabal build

# Using Nix flake (reproducible builds)
nix build

# Enter development shell with all dependencies
nix develop
```

### Testing
```bash
# Run all tests
cabal test

# Run tests with specific pattern
cabal test --test-options="-p 'prop_basic'"

# Run with profiling
cabal configure --enable-profiling
cabal test --enable-profiling
```

### Development Workflow
```bash
# Regenerate .cabal file after modifying package.yaml
hpack

# Start REPL with library loaded
cabal repl

# Check which GHC/packages are in Nix environment
nix develop -c ghc --version
nix develop -c ghc-pkg list
```

### System Dependencies Required
```bash
# macOS
brew install mpfr gmp pkg-config

# Linux (Debian/Ubuntu)
apt-get install libmpfr-dev libgmp-dev pkg-config
```

## High-Level Architecture

### Core Design: Type-Level Precision Tracking

The fundamental architecture revolves around encoding decimal precision at the **type level** using phantom types. This design spans multiple components:

1. **Type Definition** (`src/Amount.hs`): The `Amount (dec :: Nat)` type uses DataKinds to lift natural numbers to types, creating `Amount 2` for dollars, `Amount 8` for bitcoin, etc. The phantom parameter never appears in the runtime representation—it exists solely for compile-time guarantees.

2. **FFI Bridge** (`src/mpfr_printf.c`): Rational-to-decimal conversion happens through MPFR (Multiple Precision Floating-Point Reliable library). The C code receives a GMP rational (numerator/denominator pair) and precision requirement, then produces an exactly-rounded decimal string. This separation allows the Haskell side to remain pure while leveraging battle-tested C libraries for decimal formatting.

3. **Memory Management Pattern**: The interaction between Haskell and C follows a specific pattern:
   - Haskell allocates temporary buffers using `alloca`
   - C code writes to these buffers via MPFR
   - MPFR allocates its own strings that must be freed with `mpfr_free_str`
   - `unsafePerformIO` wraps the entire operation to maintain a pure interface

### Critical Architectural Decisions

**Rational Foundation**: All amounts are stored as `Rational` (ratio of unbounded `Integer`s). This means:
- Operations never lose precision due to representation limits
- Denominators can grow exponentially without normalization
- Every arithmetic operation potentially triggers GCD calculations
- The representation is exact but potentially expensive

**String-Based Equality**: The `Eq` instance compares the rendered decimal strings rather than the underlying rationals. Two amounts are equal if they would display the same to the user at their respective precisions. This ties correctness to the MPFR rendering implementation.

**Optics-Based API**: Instead of traditional getters/setters, the library provides profunctor optics (`_Amount` prism, `rounded` iso). This requires understanding the Van Laarhoven lens encoding and profunctor constraints to modify effectively.

### Cross-Module Interactions

The codebase has a specific flow of data and control:

```
User Code → Amount.hs (Haskell types & operations)
            ↓
         FFI boundary (CString, CUInt parameters)
            ↓
         mpfr_printf.c (MPFR/GMP operations)
            ↓
         String rendering (with specified precision)
            ↓
         Back through FFI (with manual memory cleanup)
```

Understanding this flow is essential for:
- Debugging precision issues (trace through FFI calls)
- Performance optimization (identify where strings are created)
- Adding new rounding modes (modify both Haskell and C sides)

## Type-Level Programming Patterns

### The KnownNat Constraint

Throughout the codebase, you'll see `KnownNat n =>` constraints. This allows extracting the type-level natural number at runtime:

```haskell
-- The phantom type 'n' is only known at compile time
-- KnownNat brings it to runtime via natVal
showAmount :: forall n. KnownNat n => Amount n -> String
showAmount amt =
  let precision = natVal (Proxy :: Proxy n)  -- Extract type-level value
```

Any function that needs to know the decimal precision must have this constraint.

### Coerce for Zero-Cost Conversions

The `coerce` function appears frequently for converting between `Amount` types with different precision:

```haskell
rounded :: Amount n -> Amount m
rounded = coerce  -- Works because newtype has same runtime representation
```

This is safe for the internal `Rational` but changes the type-level precision marker.

## Critical Implementation Details

### The spreadAmounts Algorithm

The `spreadAmounts` function distributes amounts proportionally but has a specific behavior for handling rounding differences: **the first element receives any rounding discrepancy**. This is a deliberate design choice common in financial systems but must be preserved in any modifications.

### MPFR Rounding Modes

The C code exposes 7 rounding modes from MPFR:
- `mpfr_RNDN` (0): Round to nearest, ties to even
- `mpfr_RNDZ` (1): Round toward zero
- `mpfr_RNDU` (2): Round up
- `mpfr_RNDD` (3): Round down
- `mpfr_RNDA` (4): Round away from zero
- `mpfr_RNDF` (5): Faithful rounding
- `mpfr_RNDNA` (6): Round to nearest, ties away (DEFAULT)

The default `showAmount` uses `mpfr_RNDNA`. Changing this affects all decimal display.

### JSON Serialization Limitation

Current implementation:
```haskell
instance ToJSON (Amount n) where
  toJSON = Number . fromRational . toRational . getAmount
```

This converts `Rational → Scientific`, which **loses precision** because `Scientific` uses finite precision. Any modification must address this if exact JSON roundtrips are required.

## Known Issues Requiring Attention

1. **Expensive Equality**: Every `(==)` comparison triggers MPFR rendering to compare strings. In tight loops, this becomes a bottleneck.

2. **JSON Precision Loss**: As noted above, JSON serialization through `Scientific` loses exact rational representation.

3. **Limited Test Coverage**: Only one property test (`prop_basic`) is active. The commented-out `prop_math` test suggests issues with high-precision exponentiation.

4. **No Normalization Strategy**: Rational denominators can grow without bound. After many operations, you may have rationals like `123456789987654321 % 987654321123456789`.

## Build System Interactions

The project uses **dual build systems**:

1. **Hpack** generates the `.cabal` file from `package.yaml`. Always modify `package.yaml` and run `hpack` rather than editing the `.cabal` file directly.

2. **Nix Flakes** provide reproducible builds with pinned dependencies. The flake specifies GHC 9.10 and includes HLS for IDE support.

The C FFI code requires MPFR and GMP libraries at both compile and runtime. The Nix flake handles this automatically; Cabal users must install system packages manually.

## Modification Patterns

### Adding Operations That Preserve Precision

New operations should maintain the phantom type:

```haskell
-- Good: Preserves type-level precision
applyTax :: KnownNat n => Amount n -> Rational -> Amount n
applyTax (Amount r) rate = Amount (r * rate)

-- Bad: Loses type safety
unsafeApplyTax :: Amount n -> Rational -> Amount m
unsafeApplyTax = coerce . applyTax
```

### Modifying FFI Functions

When changing C code:
1. Preserve the memory allocation pattern (alloca + manual free)
2. Maintain error handling for GMP operations
3. Test with extreme values (very large numerators/denominators)
4. Verify no memory leaks with Valgrind

### Changing Core Type Representation

The `Amount` type derives many instances:
```haskell
deriving (Generic, Data, Typeable, Ord, Num, Fractional, Real, RealFrac)
```

Changing the underlying type from `Rational` breaks all these instances. Any alternative must provide equivalent operations.

## Performance Considerations

Profile before optimizing. The main bottlenecks are:

1. **String allocation** from show-based equality
2. **FFI overhead** for each decimal rendering
3. **GCD calculations** in rational arithmetic
4. **Denominator growth** without normalization

The codebase prioritizes correctness over performance. Any optimization must maintain exact arithmetic semantics.