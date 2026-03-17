{
  description = "simple-amount: Fractional amounts as type-level-precision rationals";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };

      overlays = [
        haskellNix.overlay
        (final: prev: {
          simple-amount =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";
              shell = {
                tools = {
                  cabal = {};
                  haskell-language-server = {};
                  hlint = {};
                  fourmolu = {};
                };
                buildInputs = with pkgs; [
                  pkg-config
                  lefthook
                  clang-tools
                  cppcheck
                ];
              };
              modules = [{
                packages.simple-amount.components = {
                  library.ghcOptions = [
                    "-Wall" "-Wcompat"
                    "-Wincomplete-record-updates"
                    "-Wincomplete-uni-patterns"
                    "-Wredundant-constraints"
                    "-Werror"
                  ];
                  tests.simple-amount-tests.ghcOptions = [
                    "-Wall" "-Wcompat" "-Werror"
                    "-threaded" "-rtsopts" "-with-rtsopts=-N"
                  ];
                  benchmarks.simple-amount-bench.ghcOptions = [
                    "-Wall" "-Wcompat"
                    "-threaded" "-rtsopts" "-with-rtsopts=-N"
                  ];
                };
              }];
            };
        })
      ];

      flake = pkgs.simple-amount.flake {};

      # Use nixpkgs Haskell tools to avoid IFD in nix flake check
      fourmolu = pkgs.haskellPackages.fourmolu;
      hlint = pkgs.haskellPackages.hlint;

      cSrc = pkgs.lib.sources.sourceFilesBySuffices ./src [ ".c" ".h" ];

    in flake // {
      packages = flake.packages // {
        default = flake.packages."simple-amount:lib:simple-amount";

        # Format all code
        format = pkgs.writeShellScriptBin "simple-amount-format" ''
          set -euo pipefail
          echo "Formatting Haskell files..."
          find src test bench -name '*.hs' -exec ${fourmolu}/bin/fourmolu -i {} +
          echo "Formatting C files..."
          find src -name '*.c' -o -name '*.h' | xargs ${pkgs.clang-tools}/bin/clang-format -i
          echo "Done."
        '';

        # Run linting
        lint = pkgs.writeShellScriptBin "simple-amount-lint" ''
          set -euo pipefail
          echo "Running hlint..."
          ${hlint}/bin/hlint src/ test/ bench/
          echo "Running cppcheck..."
          ${pkgs.cppcheck}/bin/cppcheck --enable=all --error-exitcode=1 \
            --suppress=missingIncludeSystem \
            --suppress=unusedFunction \
            --suppress=normalCheckLevelMaxBranches \
            --suppress=checkersReport \
            src/mpfr_printf.c
          echo "Lint passed."
        '';
      };

      checks = (flake.checks or {}) // {
        # Library builds cleanly with -Werror
        build = flake.packages."simple-amount:lib:simple-amount";

        # Haskell formatting
        format-haskell = pkgs.runCommand "check-format-haskell" {
          nativeBuildInputs = [ fourmolu ];
          src = ./.;
        } ''
          cd $src
          find src test bench -name '*.hs' -exec fourmolu --mode check {} + 2>&1
          touch $out
        '';

        # C formatting
        format-c = pkgs.runCommand "check-format-c" {
          nativeBuildInputs = [ pkgs.clang-tools ];
          src = cSrc;
        } ''
          cd $src
          find . -name '*.c' -o -name '*.h' | while read -r f; do
            clang-format --dry-run --Werror "$f"
          done
          touch $out
        '';

        # Haskell linting
        lint-haskell = pkgs.runCommand "check-lint-haskell" {
          nativeBuildInputs = [ hlint ];
          src = ./.;
        } ''
          cd $src
          hlint src/ test/ bench/ 2>&1
          touch $out
        '';

        # C linting
        lint-c = pkgs.runCommand "check-lint-c" {
          nativeBuildInputs = [ pkgs.cppcheck ];
          src = cSrc;
        } ''
          cd $src
          cppcheck --enable=all --error-exitcode=1 \
            --suppress=missingIncludeSystem \
            --suppress=unusedFunction \
            --suppress=normalCheckLevelMaxBranches \
            --suppress=checkersReport \
            . 2>&1
          touch $out
        '';
      };
    });
}
