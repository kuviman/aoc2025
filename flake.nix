{
  description = "A devShell example";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    kast.url = "github:kast-lang/kast";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs = import inputs.nixpkgs { inherit system overlays; };
        kast = inputs.kast.packages.${system}.default;
        tcc2 = pkgs.wrapCCWith {
          cc = pkgs.tinycc;
          # Ensure tcc uses its own internal library path along with the wrapper's headers
          extraPackages = [ pkgs.tinycc ];
        };
        tcc-wrapped = pkgs.writeShellApplication {
          name = "tcc";
          text = ''
            # Initialize arrays for flags
            declare -a include_flags
            declare -a link_flags

            # Convert Nix environment variables (passed by mkShell) to TCC arguments
            if [ -n "$NIX_CFLAGS_COMPILE" ]; then
              for flag in $NIX_CFLAGS_COMPILE; do
                include_flags+=("$flag")
              done
            fi

            if [ -n "$NIX_LDFLAGS" ]; then
              for flag in $NIX_LDFLAGS; do
                # Convert -rpath to a format TCC's internal linker accepts
                if [[ "$flag" == -rpath* ]]; then
                  # TCC passes flags to its internal linker using -Wl,
                  link_flags+=("-Wl,$flag")
                else
                  link_flags+=("$flag")
                fi
              done
            fi

            # Execute raw tcc with all intercepted paths + any user arguments
            exec tcc "''${include_flags[@]}" "''${link_flags[@]}" "$@"
          '';
          runtimeInputs = [ pkgs.tinycc ];
        };
      in
      with pkgs; {
        packages = {
          inherit mytinycc;
        };
        devShells.default =
          mkShell {
            packages = [
              (pkgs.writeShellScriptBin "kast" ''
                systemd-run --user --scope -p MemoryMax=10G \
                  rlwrap ${kast}/bin/kast "$@"
              '')
              rlwrap
              nixfmt
              nodejs
              clang
              # tcc-wrapped
              # tinycc
              tcc2
              libbacktrace
              libunwind
            ];
            CLANGD_FLAGS = "--query-driver=${clang}/bin/clang*";
            CFLAGS = "-lbacktrace -lunwind -g -fsanitize=undefined,address,leak";
          };
      });
}
