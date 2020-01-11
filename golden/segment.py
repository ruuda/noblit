"""
Segment golden tests into input queries and expected outputs.
"""

import unicodedata

from typing import Iterable, Iterator, List, NamedTuple


class Block(NamedTuple):
    query: List[str]
    output: List[str]


def segment(lines: Iterable[str]) -> Iterator[Block]:
    """
    Split the input lines in alternating sections of query and expecations.
    Expectations can be recognized because they start with "> ".
    """
    is_query = True
    query: List[str] = []
    output: List[str] = []

    for line in lines:
        if line.startswith('> '):
            # A line of expected output.
            output.append(line[2:])
            continue

        elif len(output) > 0:
            # A new query line, after having finished an output segment.
            yield Block(query, output)
            query = []
            output = []

        query.append(line)

    yield Block(query, output)
