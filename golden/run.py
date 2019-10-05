#!/usr/bin/env python3

"""
A test runner for running the golden tests.

The runner takes golden input files, splits them into queries and expectations,
runs the query, and prints whether the results match the expectations. This
relies on "parse.py" for parsing queries, and the "src/bin/execute.rs" binary
for executing them.

Output is TAP (Test Anything Protocol) compliant, so you can run all of the
golden tests with Prove while using this script as interpreter (with --exec).

Standalone usage:
  golden/run.py file.t

Interpreter usage:
  prove --exec golden/run.py golden
"""

import sys
import subprocess

from typing import Iterable, List, Iterator

import parse
import segment


def main(fname: str) -> None:
    with open(fname, 'r', encoding='utf-8') as f:
        blocks = list(segment.segment(f))

    # Print the number of tests we are going to run,
    # in accordance with the TAP v12 protocol.
    print(f'1..{len(blocks)}')

    for i, (query_lines, expected_lines) in enumerate(blocks):

        # Print the input query, prefixed with #.
        for line in query_lines:
            print('#', line, end='')

        query = parse.parse_query(query_lines)
        query_binary: bytes = b''.join(query.serialize())

        result = subprocess.run(
            'target/debug/execute',
            input=query_binary,
            capture_output=True,
        )

        assert result.returncode == 0
        actual_lines = result.stdout.decode('utf-8').splitlines(keepends=True)

        is_good = True
        for actual_line, expected_line in zip(actual_lines, expected_lines):
            if actual_line == expected_line:
                print('#', actual_line, end='')
            else:
                is_good = False
                # Print a diff of expected vs actual. +/- are ignored by TAP.
                print('-', expected_line, end='')
                print('+', actual_line, end='')

        if is_good:
            print('ok', i + 1)
        else:
            print('not ok', i + 1)


if __name__ == '__main__':
    fname = None

    if len(sys.argv) != 2:
        print(__doc__.strip())
        sys.exit(1)

    main(sys.argv[1])
