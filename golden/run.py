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

from enum import Enum
from typing import Iterable, List, Iterator

import parse


class State(Enum):
    """
    State for the golden parser to track what we are currently parsing.
    """
    query = 0
    expectation = 1


def split_expectations(lines: Iterable[str]) -> Iterator[List[str]]:
    """
    Split the input lines in alternating sections of query and expecations.
    Expectations can be recognized because they form a table with box-drawing
    characters.
    """
    state = State.query
    block: List[str] = []

    for line in lines:
        if line[0] == '┌' and state == State.query:
            # We found a top-left table corner, start a new block before.
            yield block
            block = [line]
            state = State.expectation

        elif line[0] == '└' and state == State.expectation:
            # We found a bottom-left table corner, start a new block after.
            block.append(line)
            yield block
            block = []
            state = State.query

        else:
            # Otherwise, continue in whatever state we were.
            block.append(line)

    # Yield the final block, if there was anything in it.
    if len(block) > 0:
        yield block


def main(fname: str) -> None:
    with open(fname, 'r', encoding='utf-8') as f:
        blocks = list(split_expectations(f))

        if len(blocks) % 2 != 0:
            print(f'Invalid test file {fname}.')
            print('Expected even number of query blocks and output blocks.')
            sys.exit(1)

    # Zip every query block with its output block as one "test".
    tests = list(zip(blocks[0::2], blocks[1::2]))

    # Print the number of tests we are going to run,
    # in accordance with the TAP v12 protocol.
    print(f'1..{len(tests)}')

    for i, (query_lines, expected_lines) in enumerate(tests):

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
            print(f'ok {i+1} - run query')
        else:
            print(f'not ok {i+1} - run query')


if __name__ == '__main__':
    fname = None

    if len(sys.argv) != 2:
        print(__doc__.strip())
        sys.exit(1)

    main(sys.argv[1])
