#!/usr/bin/env python3

"""
This is a very rudametary "parser" for the ad-hoc query language.

Its purpose is to get something going to make it easier to write queries, in
order to test the rest of the system. If we want to keep the ad-hock query
language at all, this parser should be replaced with a proper one, probably in
Rust. For now, we do some fishy string manipulation here.
"""

import ast
import sys

from enum import Enum
from typing import Iterable, List, NamedTuple, Union


class Var(NamedTuple):
    number: int

    def __repr__(self) -> str:
        # Pretty-print variables a bit less verbose.
        return f'${self.number}'


class Statement(NamedTuple):
    entity: Var
    attribute: str
    value: Union[Var, int, str]


class Query(NamedTuple):
    variable_names: List[str]
    where_statements: List[Statement]
    select: List[Var]


class VarMap:
    def __init__(self) -> None:
        self._map: Dict[str, Var] = {}

    def get(self, name: str) -> Var:
        """
        Get a variable by name, creating it if it does not exist.
        """
        if name not in self._map:
            var = Var(len(self._map))
            self._map[name] = var

        return self._map[name]

    def names(self) -> List[str]:
        """
        Return the names of all variables, in sequence.
        """
        # We rely on dicts iteration order being the insertion order here.
        return list(self._map.keys())


class State(Enum):
    """
    State for the parser to track what we are currently parsing.
    """
    init = 0
    where = 1
    select = 2


def parse_state(line: str) -> State:
    keywords = {
        'where\n': State.where,
        'select\n': State.select,
    }
    state = keywords.get(line)

    if state is None:
        print(f'Unexpected input: {line.strip()}')
        sys.exit(1)

    return state


def parse_statement(variables: VarMap, line: str) -> Statement:
    statement = line.strip().split(' ', maxsplit=2)

    if len(statement) != 3:
        print(f'Invalid statement: {line.strip()}')
        sys.exit(1)

    entity_str, attribute, value_str = statement
    value: Union[str, int, Var]

    if value_str.startswith('"'):
        # We are dealing with a string literal. We reuse Python's parser.
        parsed = ast.literal_eval(value_str)
        assert isinstance(parsed, str), 'Expected to parse string literal.'
        value = parsed
    elif value_str.isdigit():
        # We are dealing with an integer literal.
        value = int(value_str)
    else:
        # If it is not a literal, then we treat it as a variable.
        assert ' ' not in value_str, f'Variable "{value_str}" contains space.'
        value = variables.get(value_str)

    return Statement(
        entity=variables.get(entity_str),
        attribute=attribute,
        value=value,
    )


def parse_query(lines: Iterable[str]):
    """
    Parse a query that contains a where and a select clause.
    """
    state = State.init
    variables = VarMap()
    statements: List[Statement] = []
    select: List[Var] = []

    for line in lines:
        if state == state.where and line.startswith('  '):
            # We are parsing a statement in a where block.
            statements.append(parse_statement(variables, line))

        elif state == state.select and line.startswith('  '):
            # We are parsing comma-separated variables in a select block.
            vs = (v.strip() for v in line.split(','))
            vs = (v for v in vs if len(v) > 0)
            for v in vs:
                select.append(variables.get(v))

        elif line.strip() == '' or line.startswith('--'):
            # Skip blank lines and allow comments with --.
            continue

        else:
            # Interpret anything else as a state change.
            state = parse_state(line)

    return Query(
        variable_names=variables.names(),
        where_statements=statements,
        select=select,
    )


if __name__ == '__main__':
    q = parse_query(sys.stdin)
    print(q)
