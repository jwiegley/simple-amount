{
  description = "simpl-amount";

  inputs.nixpkgs.url = "github:jwiegley/nixpkgs";

  outputs = { self, nixpkgs }:
    let
      version = "0.2.0";

      compiler = "ghc8107";

      supportedSystems = [ "x86_64-linux" "x86_64-darwin"
                           "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          haskellPackages = pkgs.haskell.packages.${compiler};
        in {
          simple-amount = haskellPackages.callCabal2nix "simple-amount" ./. {};
        });
      defaultPackage = forAllSystems (system: self.packages.${system}.simple-amount);
      checks = self.packages;

      devShell = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          haskellPackages = pkgs.haskell.packages.${compiler};
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.simple-amount];

          withHoogle = true;
          buildInputs = with haskellPackages; [
            hasktags
            hpack
            ormolu
          ];

          shellHook = ''
            CABAL_REPL="${pkgs.cabal-install}/bin/cabal repl \
                            --extra-lib-dirs=${pkgs.mpfr.out}/lib \
                            --extra-lib-dirs=${pkgs.gmp.out}/lib"
            export CABAL_REPL
          '';
        });
    };
}
