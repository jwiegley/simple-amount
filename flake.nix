{
  description = "Simple-Amount";

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
      flake = pkgs.simple-amount.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          simple-amount =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc963";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
            };
        })
      ];
    in flake // {
      packages.default = flake.packages."simple-amount:exe:simple-amount";

      devShell = pkgs.haskellPackages.shellFor {
        packages = with pkgs.haskellPackages; p: [
          cabal-install
          haskell-language-server
        ];

        buildInputs = with pkgs.haskellPackages; [
          cabal-install
        ];

        withHoogle = true;
      };
    });
}
