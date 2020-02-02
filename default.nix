let
  pkgs = import (import ./nixpkgs-pinned.nix) {};
in
  pkgs.buildEnv {
    name = "noblit-devenv";
    paths = [
      pkgs.binutils # Provides "nm", for the symbol sanity check.
      pkgs.gcc      # For checking the generated C header.
      pkgs.gmp      # Required by the Haskell runtime.
      pkgs.mkdocs   # For building documentation.
      pkgs.mypy     # For typechecking the Python code.
      pkgs.perl     # The "prove" test harness is part of Perl.
      pkgs.python3  # For running the golden tests and other tools.
      pkgs.rustup   # Provides the Rust toolchain.
      pkgs.stack    # Provides the Haskell toolchain.
    ];
  }
