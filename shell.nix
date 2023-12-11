{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  packages = with pkgs; ([
    stack
    haskell.compiler.ghc947
    (haskell-language-server.override { supportedGhcVersions = [ "947" ]; })
    haskellPackages.fourmolu
    haskellPackages.stylish-haskell
    haskellPackages.hoogle
    haskellPackages.cabal-fmt
    haskellPackages.cabal-install
  ]);
}
