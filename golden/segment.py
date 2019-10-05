"""
Segment golden tests into input queries and expected outputs.
"""

import unicodedata

from typing import Iterable, Iterator, List, NamedTuple


class Block(NamedTuple):
    query: List[str]
    output: List[str]


def segment(lines: Iterable[str]) -> Iterator[Block]:
    is_query = True
    query: List[str] = []
    output: List[str] = []

    for line in lines:
        # Expected output starts with box drawing characters.
        is_box = len(line) > 0 and unicodedata.category(line[0]) == 'So'

        if is_box:
            # A line of expected output.
            output.append(line)
            continue

        elif len(output) > 0:
            # A new query line, after having finished an output segment.
            yield Block(query, output)
            query = []
            output = []

        query.append(line)

    yield Block(query, output)
