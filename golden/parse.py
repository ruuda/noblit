#!/usr/bin/env python3

"""
A very rudametary "parser" for the ad-hoc query language.

Its purpose is to get something going to make it easier to write queries, in
order to test the rest of the system. If we want to keep the ad-hoc query
language at all, this parser should be replaced with a proper one, probably in
Rust. For now, we do some fishy string manipulation here.

The program accepts a textual query on stdin, and it serializes a binary
representation of it to stdout, unless stdout is a tty, in which case it
debug-prints the parsed query.
"""

import ast
import re
import sys

from enum import Enum
from typing import Dict, Iterable, Iterator, List, NamedTuple, Union


def u8(x: int) -> bytes:
    """
    Encode an 8-bit unsigned integer.
    """
    assert 0 <= x < 256
    return x.to_bytes(1, byteorder='little')


def u16_le(x: int) -> bytes:
    """
    Encode a 16-bit unsigned integer in little endian.
    """
    assert 0 <= x < 65536
    return x.to_bytes(2, byteorder='little')


def u64_le(x: int) -> bytes:
    """
    Encode a 64-bit unsigned integer in little endian.
    """
    assert 0 <= x < 2**64
    return x.to_bytes(8, byteorder='little')


class Var(NamedTuple):
    number: int

    def __repr__(self) -> str:
        # Pretty-print variables a bit less verbose.
        return f'${self.number}'


class Statement(NamedTuple):
    entity: Var
    attribute: str
    value: Union[Var, int, str]

    def __str__(self) -> str:
        return f'  {self.entity} {self.attribute} {self.value!r}'

    def serialize(self) -> Iterator[bytes]:
        """
        Serialize the statement to a binary format that Noblit can read.
        """
        # 2-byte entity variable number.
        yield u16_le(self.entity.number)

        # 2-byte attribute name length, and the attribute itself as UTF-8.
        attribute_utf8 = self.attribute.encode('utf-8')
        yield u16_le(len(attribute_utf8))
        yield attribute_utf8

        if isinstance(self.value, Var):
            # 1-byte tag 0; 2-byte variable number.
            yield u8(0)
            yield u16_le(self.value.number)
        elif isinstance(self.value, int):
            # 1-byte tag 1; 8-byte integer.
            yield u8(1)
            yield u64_le(self.value)
        elif isinstance(self.value, str):
            # 1-byte tag 2; 16-byte length, UTF-8 string.
            value_utf8 = self.value.encode('utf-8')
            yield u8(2)
            yield u16_le(len(value_utf8))
            yield value_utf8
        else:
            raise ValueError('Invalid value in statement.')


class Query(NamedTuple):
    variable_names: List[str]
    where_statements: List[Statement]
    assertions: List[Statement]
    select: List[Var]
    explain: bool

    def __str__(self) -> str:
        """
        Pretty-print the query in a format similar to what we parsed.
        """
        lines = []

        lines.extend([
          'variables',
          *(f'  ${i}: {name}' for i, name in enumerate(self.variable_names)),
        ])

        lines.extend([
          'where',
          *(str(statement) for statement in self.where_statements),
        ])

        if len(self.assertions) > 0:
            lines.extend([
              'assert',
              *(str(statement) for statement in self.assertions),
            ])

        lines.extend([
          'select',
          '  ' + ', '.join(str(var) for var in self.select),
        ])

        return '\n'.join(lines)

    def serialize(self) -> Iterator[bytes]:
        """
        Serialize the query to a binary format that Noblit can read.
        """
        # 1-byte query type: 0 for a read, 1 for a write, 2 for explain.
        if self.explain:
            yield u8(2)
        else:
            type_ = 0 if len(self.assertions) == 0 else 1
            yield u8(type_)

        # 2-byte number of variables, followed by length-prefixed names.
        yield u16_le(len(self.variable_names))
        for variable in self.variable_names:
            name_utf8 = variable.encode('utf-8')
            yield u16_le(len(name_utf8))
            yield name_utf8

        # 2-byte number of statements, followed by their encodings.
        yield u16_le(len(self.where_statements))
        for statement in self.where_statements:
            yield from statement.serialize()

        # 2-byte nubmer of selects, followed by their 2-byte variable numbers.
        yield u16_le(len(self.select))
        for var in self.select:
            yield u16_le(var.number)

        # If there are assertions, the 2-byte number of them,
        # followed by their encodings.
        if len(self.assertions) > 0:
            yield u16_le(len(self.assertions))
            for statement in self.assertions:
                yield from statement.serialize()


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
    INIT = 0
    WHERE = 1
    SELECT = 2
    ASSERT = 3


def parse_state(line: str) -> State:
    keywords = {
        'where\n': State.WHERE,
        'select\n': State.SELECT,
        'assert\n': State.ASSERT,
    }
    state = keywords.get(line)

    if state is None:
        print(f'Unexpected input: {line.strip()}')
        sys.exit(1)

    return state


def parse_statement(variables: VarMap, line: str) -> Statement:
    statement = re.split('\\s+', line.strip(), maxsplit=2)

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

    elif value_str in ('true', 'false'):
        # A boolean literal, but booleans are uint64 internally.
        value = 1 if value_str == 'true' else 0

    else:
        # If it is not a literal, then we treat it as a variable.
        assert ' ' not in value_str, f'Variable "{value_str}" contains space.'
        value = variables.get(value_str)

    return Statement(
        entity=variables.get(entity_str),
        attribute=attribute,
        value=value,
    )


def parse_query(lines: Iterable[str]) -> Query:
    """
    Parse a query that contains a where and a select clause.
    """
    state = State.INIT
    variables = VarMap()
    statements: List[Statement] = []
    assertions: List[Statement] = []
    select: List[Var] = []
    explain = False

    for line in lines:
        if line.strip() == '' or line.strip().startswith('--'):
            # Skip blank lines and allow comments with --.
            continue

        elif state == State.WHERE and line.startswith('  '):
            # We are parsing a statement in a where block.
            statements.append(parse_statement(variables, line))

        elif state == State.ASSERT and line.startswith('  '):
            # We are parsing a statement in an assert block.
            assertions.append(parse_statement(variables, line))

        elif state == State.SELECT and line.startswith('  '):
            # We are parsing comma-separated variables in a select block.
            vs = (v.strip() for v in line.split(','))
            vs = (v for v in vs if len(v) > 0)
            for v in vs:
                select.append(variables.get(v))

        elif state == State.INIT and line == 'explain\n':
            # Allow an "explain" prefix on queries.
            explain = True

        else:
            # Interpret anything else as a state change.
            state = parse_state(line)

    if explain:
        assert len(assertions) == 0, '"Explain" is not supported with assertions.'

    return Query(
        variable_names=variables.names(),
        where_statements=statements,
        assertions=assertions,
        select=select,
        explain=explain,
    )


if __name__ == '__main__':
    import segment

    for i, block in enumerate(segment.segment(sys.stdin)):
        q = parse_query(block.query)

        if sys.stdout.isatty():
            # Pretty-print the query when printing to a tty.
            sep = '\n' if i > 0 else ''
            print(f'{sep}{q}')

        else:
            # When stdout is not a tty, write the binary query there.
            for bs in q.serialize():
                sys.stdout.buffer.write(bs)
