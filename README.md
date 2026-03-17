# simple-amount

I've been working with financial amounts in Haskell for years now, and the one
thing that's always bugged me is precision loss. Most libraries reach for
`Double` or `Decimal` and call it a day, but if you've ever had a ledger that's
off by a penny after a few thousand transactions, you know that's not good
enough.

`simple-amount` takes a different approach: every amount is stored as an exact
`Rational` -- a ratio of unbounded integers -- so arithmetic never loses
precision. The decimal places you care about are tracked at the type level using
GHC's `DataKinds`, which means `Amount 2` (dollars) and `Amount 8` (bitcoin
satoshis) are different types. The compiler won't let you accidentally mix them.

When it's time to actually display a number, the library drops into C via FFI to
use MPFR for correctly-rounded decimal rendering. So you get exact internal
arithmetic with properly-rounded output -- the best of both worlds.

## Quick start

```haskell
{-# LANGUAGE DataKinds #-}

import Amount

-- Two decimal places, like USD
price :: Amount 2
price = 19.99

tax :: Amount 2
tax = price * 0.0825  -- tax rate as a rational

total :: Amount 2
total = price + tax
-- show total => "21.64"
```

## Building

```bash
# Nix (recommended -- handles MPFR/GMP dependencies automatically)
nix develop
cabal build
cabal test

# Without Nix (requires MPFR and GMP system libraries)
# macOS: brew install mpfr gmp pkg-config
# Linux: apt-get install libmpfr-dev libgmp-dev pkg-config
cabal build
```

## What's here

- **`Amount n`** -- a newtype over `Rational` with a phantom `Nat` for decimal
  precision. Derives `Num`, `Fractional`, `Real`, `RealFrac`, `Ord`, and more.

- **`showAmount`** -- renders an amount to a decimal string at its type-level
  precision, using MPFR with configurable rounding modes (defaults to
  round-to-nearest, ties away from zero).

- **`spreadAmounts`** -- distributes an amount proportionally across a list,
  with any rounding remainder going to the first element. This is the standard
  approach in financial systems.

- **`_Amount`** and **`rounded`** -- profunctor optics for parsing amounts from
  strings and converting between precisions.

## The FFI bit

The C code in `src/mpfr_printf.c` handles rational-to-decimal conversion through
GMP and MPFR. The Haskell side passes a string representation of the rational
(numerator/denominator), and the C side does the division and rounding at the
requested precision. Memory management follows the standard `alloca` +
`mpfr_free_str` pattern.

## License

BSD-3-Clause. See [LICENSE.md](LICENSE.md).
