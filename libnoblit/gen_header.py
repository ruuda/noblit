#!/usr/bin/env python3

# Noblit -- An immutable append-only database
# Copyright 2020 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Extract the noblit.h C header from the API reference.
"""

from typing import Iterable, List


def extract_decls(lines: Iterable[str]) -> Iterable[str]:
    """
    Extract C declarations from code blocks in a markdown file.
    """
    declaration: List[str] = []
    in_block = False

    for line in lines:
        # Four-space indented code blocks must be preceded by a blank line.
        if line.strip() == '':
            declaration = []
            in_block = True
            continue

        is_indented = line.startswith('    ')

        # Include content from code blocks, and join multiple lines, until the
        # semicolon terminator.
        if in_block and is_indented:
            declaration.append(line[4:].rstrip())

            if any(end in line for end in (';', '#define', '{')):
                yield ' '.join(declaration)
                declaration = []

        if not is_indented:
            in_block = False


def get_decls_from_docs() -> List[str]:
    """
    Extract declarations from the C API reference.
    """
    with open('docs/reference/c.md', 'r', encoding='utf-8') as f:
        return list(extract_decls(f))


def main() -> None:
    print('#ifndef _NOBLIT_H_')
    print('#define _NOBLIT_H_')
    print(
        '\n/* This file was generated from docs/reference/c.md '
        'by libnoblit/gen_header.py. */\n'
    )
    print('#include <stddef.h>')
    print('#include <stdint.h>\n')

    for decl in get_decls_from_docs():
        print(decl)

    print('\n#endif')


if __name__ == '__main__':
    main()
