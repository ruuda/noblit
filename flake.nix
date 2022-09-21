{
  description = "Noblit";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: 
    let
      name = "noblit";
      version = "0.0.0";
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      forAllNixpkgs = f: forAllSystems (system: f (import nixpkgs { inherit system; }));
    in
      {
        devShells = forAllNixpkgs (pkgs: {
          default = pkgs.mkShell {
            name = "noblit";
            nativeBuildInputs = [
              pkgs.binutils     # Provides "nm", for the symbol sanity check.
              pkgs.gcc          # For checking the generated C header.
              pkgs.gmp          # Required by the Haskell runtime.
              pkgs.mkdocs       # For building documentation.
              pkgs.mypy         # For typechecking the Python code.
              pkgs.perl         # The "prove" test harness is part of Perl.
              pkgs.python3      # For running the golden tests and other tools.
              pkgs.rustup       # Provides the Rust toolchain.
              pkgs.stack        # Provides the Haskell toolchain.
            ];

            # Needed for the locale-archive, to avoid LC_* errors.
            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          };
        });

        packages = forAllNixpkgs (pkgs: {
          default = pkgs.rustPlatform.buildRustPackage {
            inherit name version;
            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
          };
        });
      };
}
