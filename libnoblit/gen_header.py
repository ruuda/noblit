#!/usr/bin/env python3

# Noblit -- An immutable append-only database
# Copyright 2020 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Extract the noblit.h C header from the API reference.
Accepts --output and a filename, prints to stdout otherwise.
"""

import sys

from typing import IO, Iterable, List


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


def main(out: IO[str]) -> None:
    out.write('#ifndef _NOBLIT_H_\n')
    out.write('#define _NOBLIT_H_\n')
    out.write('\n/* This file was generated from docs/reference/c.md ')
    out.write('by libnoblit/gen_header.py. */\n\n')
    out.write('#include <stddef.h>\n')
    out.write('#include <stdint.h>\n\n')

    for decl in get_decls_from_docs():
        out.write(decl)
        out.write('\n')

    out.write('\n#endif\n')


if __name__ == '__main__':
    if len(sys.argv) == 3 and sys.argv[1] == '--output':
        main(open(sys.argv[2], 'w', encoding='utf-8'))
    else:
        main(sys.stdout)
