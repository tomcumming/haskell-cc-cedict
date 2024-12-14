{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  };
  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          pkgs.haskell.compiler.ghc910
          pkgs.cabal-install
          (pkgs.haskell-language-server.override { supportedGhcVersions = [ "910" ]; })
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
