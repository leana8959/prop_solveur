{
  # credit: https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
  description = ''
    prop-solveur, a simple logic solver
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};

      hPkgs = pkgs.haskell.packages.ghc947;

      devTools = [
        stack-wrapped
        hPkgs.ghc
        hPkgs.haskell-language-server
        hPkgs.stylish-haskell
      ];

      stack-wrapped = pkgs.symlinkJoin {
        name = "stack";
        paths = [pkgs.stack];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "--no-nix --system-ghc --no-install-ghc"
        '';
      };
    in {
      formatter = pkgs.alejandra;

      packages.default = (pkgs.haskellPackages.callCabal2nix "prop-solveur" ./. {}).overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.installShellFiles];
        postInstall =
          (old.postInstall or "")
          + ''
            installShellCompletion --cmd prop-solveur \
                --bash <("$out/bin/prop-solveur" --bash-completion-script "$out/bin/prop-solveur") \
                --fish <("$out/bin/prop-solveur" --fish-completion-script "$out/bin/prop-solveur") \
                --zsh  <("$out/bin/prop-solveur" --zsh-completion-script  "$out/bin/prop-solveur")
          '';
      });

      devShells.default = pkgs.mkShell {
        buildInputs = devTools;

        # Make external Nix c libraries like zlib known to GHC, like pkgs.haskell.lib.buildStackProject does
        # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
      };
    });
}
