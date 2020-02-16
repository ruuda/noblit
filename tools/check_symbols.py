#!/usr/bin/env python3

# Noblit -- An immutable append-only database
# Copyright 2020 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Verify that all symbols in libnoblit.so are documented,
and verify that all documented symbols exist.

Usage:
    check_symbols.py target/debug/libnoblit.so
    check_symbols.py target/release/libnoblit.so
"""

import sys
import subprocess

from typing import Iterable, Set

from gen_header import get_decls_from_docs


def list_symbols(shared_object_fname: str) -> Iterable[str]:
    """
    List all exported symbols in libnoblit.so.
    """
    command = ['nm', '--extern-only', '--defined-only', shared_object_fname]
    result = subprocess.run(command, capture_output=True)
    for line in result.stdout.decode('utf-8').splitlines():
        _offset, _type, name = line.split()
        yield name


def main(shared_object_fname: str) -> None:
    actual_symbols = set(list_symbols(shared_object_fname))

    # Rust includes this symbol, we don't expect this to be documented.
    actual_symbols.remove('rust_eh_personality')

    # Assume that every declaration word with a paren in it defines a function,
    # and cut off that paren to get the symbol name.
    documented_symbols = {
        part.split('(')[0]
        for decl in get_decls_from_docs()
        for part in decl.split()
        if '(' in part
    }

    undefined_symbols = list(sorted(documented_symbols - actual_symbols))
    undocumented_symbols = list(sorted(actual_symbols - documented_symbols))

    for symbol in undefined_symbols:
        print(f'Error: Symbol "{symbol}" is documented but not exported.')

    for symbol in undocumented_symbols:
        print(f'Error: Symbol "{symbol}" is exported but not documented.')

    if len(undefined_symbols) > 0 or len(undocumented_symbols) > 0:
        sys.exit(1)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print(__doc__.strip())
        sys.exit(1)

    main(sys.argv[1])

