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

import os
import subprocess
import sys

from itertools import zip_longest
from typing import Iterable, List, Iterator

import parse
import segment


def main(fname: str) -> None:
    with open(fname, 'r', encoding='utf-8') as f:
        blocks = list(segment.segment(f))

    # Print the number of tests we are going to run,
    # in accordance with the TAP v12 protocol.
    print(f'1..{len(blocks)}', flush=True)

    # Keep one executor process open for the entire test, so assertions
    # made in one transaction are still present for the next query. Run with
    # RUST_BACKTRACE=1 so we get a backtrace if the executor panics.
    os.putenv('RUST_BACKTRACE', '1')
    with subprocess.Popen(
        ['target/debug/execute'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    ) as executor:
        assert executor.stdin is not None, 'We piped stdin, it should be there'
        assert executor.stdout is not None, 'We piped stdout, it should be there'

        # Run every query (read or write), one by one.
        for i, (query_lines, expected_lines) in enumerate(blocks):

            # Print the input query, prefixed with #.
            for line in query_lines:
                print('#', line, end='')

            # Feed the query (in binary format) into the executor.
            query = parse.parse_query(query_lines)
            for query_bytes in query.serialize():
                executor.stdin.write(query_bytes)
            executor.stdin.flush()

            # Read executor output until ascii 0x04, "end of transmission", or
            # until EOF (in case the process printed something unexpected).
            # TODO: We should put a timeout on this, in case the executor prints
            # nothing at all.
            result_lines: List[str] = []
            for line_bytes in executor.stdout:
                line_str = line_bytes.decode('utf-8')

                if line_str == '\u0004\n':
                    break

                result_lines.append(line_str)

            # We should only interpret the output lines as meaningful output if
            # the executor is still behaving according to protocol. When it
            # exited early, it probably printed a panic message.
            if executor.poll() is not None:
                print('not ok', i + 1, 'Executor exited unexpectedly.', flush=True)
                break

            is_good = True
            for actual_line, expected_line in zip_longest(result_lines, expected_lines):
                if actual_line == expected_line:
                    # Echo the lines; '>' is ignored by TAP.
                    print('>', actual_line, end='')
                else:
                    # Print a diff of expected vs actual. + and - are ignored by TAP.
                    if expected_line is not None:
                        print('-', expected_line, end='')
                    if actual_line is not None:
                        print('+', actual_line, end='')
                    is_good = False

            if is_good:
                print('ok', i + 1, flush=True)
            else:
                print('not ok', i + 1, flush=True)

        # Shut down the executor, now that we have run all tests.
        timeout_seconds = 0.1
        executor.stdin.close()
        executor.wait(timeout_seconds)


if __name__ == '__main__':
    fname = None

    if len(sys.argv) != 2:
        print(__doc__.strip())
        sys.exit(1)

    main(sys.argv[1])
