{
  description = "A multiplatform Guile 3.0 and raylib environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nixgl.url = "github:nix-community/nixGL";
    raylib-guile.url = "github:jjwatt/raylib-guile";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nixgl,
      raylib-guile,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ raylib-guile.overlays.default ]
          ++ (if system == "x86_64-linux" then [ nixgl.overlay ] else [ ]);
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        # Access the package from the external flake overlay
        raylib-guile-pkg = pkgs.raylib-guile;

        guile_3_0-wrapped =
          if system == "x86_64-linux" then
            pkgs.writeShellScriptBin "guile" ''
              if [ -e /etc/NIXOS ]; then
                  exec ${pkgs.guile_3_0}/bin/guile "$@"
              else
                  # Non-NixOS x86_64 Linux
                  exec ${pkgs.nixgl.nixGLMesa}/bin/nixGLMesa ${pkgs.guile_3_0}/bin/guile "$@"
              fi
            ''
          else
            pkgs.guile_3_0;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.gmp
            guile_3_0-wrapped
            pkgs.raylib
            raylib-guile-pkg
          ];

          shellHook = ''
            export GUILE_LOAD_PATH="$PWD:${raylib-guile-pkg}/share/guile/site/3.0:$GUILE_LOAD_PATH"
            export GUILE_EXTENSIONS_PATH="$PWD:${raylib-guile-pkg}/lib/guile/3.0/extensions:$GUILE_EXTENSIONS_PATH"
          '';
        };
      }
    );
}
