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
  golden/run.py [--verbose] file.t

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


def main(fname: str, verbose: bool) -> None:
    with open(fname, 'r', encoding='utf-8') as f:
        blocks = list(split_expectations(f))

        if len(blocks) != 2:
            # TODO: In the future we could accept more than one query-output pair.
            print(f'Invalid test file {fname}.')
            print('Expected query block and output block.')
            sys.exit(1)

        query_lines, expected_lines = blocks

    # Print the number of tests we are going to run,
    # in accordance with the TAP v12 protocol.
    print('1..1')

    if verbose:
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
            if verbose:
                print('#', actual_line, end='')
        else:
            is_good = False
            if verbose:
                # Print a diff of expected vs actual. +/- are ignored by TAP.
                print('-', expected_line, end='')
                print('+', actual_line, end='')

    if is_good:
        print('ok 1 - run query')
    else:
        print('not ok 1 - run query')


if __name__ == '__main__':
    is_verbose = False
    fname = None

    for arg in sys.argv[1:]:
        if arg == '--verbose':
            is_verbose = True
        else:
            assert fname is None, 'Already have a file to run.'
            fname = arg

    if fname is None:
        print(__doc__.strip())
        sys.exit(1)

    main(fname, verbose=is_verbose)
