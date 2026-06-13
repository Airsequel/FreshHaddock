{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            bash
            coreutils
            fd
            gnumake
            haskell.compiler.ghc910
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.fourmolu
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [ "9103" ];
            })
            haskellPackages.hlint
            haskellPackages.stack
            pkg-config
            zlib
            # C libraries for the graphics-related packages
            # (dear-imgui, brillo, sdl2, …)
            SDL2
            glew
            glfw
          ];
          shellHook = ''
            cargo install --locked term-transcript-cli@0.4.0
          '';
        };
        formatter = pkgs.nixfmt-tree; # Format this file with `nix fmt`
      }
    );
}
