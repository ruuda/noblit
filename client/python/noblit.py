# Noblit -- An immutable append-only database
# Copyright 2020 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.
from __future__ import annotations

from ctypes import CDLL, POINTER, Structure, byref, cdll
from ctypes import c_byte, c_char, c_char_p, c_uint32, c_size_t, c_void_p, c_int
from enum import Enum
from typing import IO, Optional, NamedTuple

_LIB: Optional[CDLL] = None

def load_lib() -> CDLL:
    """
    Load libnoblit.so.
    """
    last_exc = None

    # TODO: How to clean this up to make it usable in practice?
    for prefix in ('', 'target/debug/', 'target/release/'):
        try:
            return cdll.LoadLibrary(prefix + 'libnoblit.so')

        except OSError as exc:
            last_exc = exc
            pass

    # If we get here, we should have either returned or stored an exception,
    # but Mypy does not know that.
    assert last_exc is not None
    raise last_exc


def ensure_lib() -> CDLL:
    """
    Memoized version of 'load_lib()'.
    """
    global _LIB

    if _LIB is None:
        _LIB = load_lib()

        _LIB.noblit_get_last_error.argtypes = [c_void_p]
        _LIB.noblit_get_last_error.restype = _Slice
        _LIB.noblit_open_packed_in_memory.argtypes = [c_int]
        _LIB.noblit_open_packed_in_memory.restype = c_void_p
        _LIB.noblit_close.argtypes = [c_void_p]
        _LIB.noblit_close.restype = None
        _LIB.noblit_query_open.argtypes = [c_void_p, c_char_p, c_size_t, c_void_p]
        _LIB.noblit_query_open.retype = c_uint32

    return _LIB


class _Slice(Structure):
    _fields_ = [('data', POINTER(c_byte)), ('len', c_size_t)]

    def into_bytes(self) -> bytes:
        return POINTER(c_char).from_buffer(self.data)[:self.len]


class NoblitResult(Enum):
    """
    Result codes that can be returned from the Noblit API.
    """
    OK = 0
    IO_ERROR = 1


class NoblitError(Exception):
    """
    Raised when a Noblit FFI call returned a non-ok result.

    Contains the result code, which is the error category, and the message with
    more details, including details of this particular instance of the error.
    """
    def __init__(self, result: int, message: str) -> None:
        self.result = NoblitResult(result)
        self.message = message

    def __str__(self) -> str:
        return self.message


class Database:
    def __init__(self, db: c_void_p) -> None:
        self._db = db
        # Cache a reference to the library.
        self._lib = ensure_lib()

    def _get_last_error(self) -> str:
        message_utf8 = self._lib.noblit_get_last_error(self._db).into_bytes()
        return message_utf8.decode('utf-8')

    def _check_result(self, result: c_uint32) -> None:
        if result == 0:
            return

        message = self._get_last_error()
        raise NoblitError(result, message)


    @staticmethod
    def open_packed_in_memory(dbfile: IO[bytes]) -> Database:
        lib = ensure_lib()
        db = lib.noblit_open_packed_in_memory(dbfile.fileno())
        return Database(c_void_p(db))

    def __del__(self) -> None:
        self._lib.noblit_close(self._db)

    def __repr__(self) -> str:
        return f'Database(0x{self._db.value:x})'

    def query(self, q: bytes) -> Evaluator:
        evaluator = c_void_p()
        result = self._lib.noblit_query_open(self._db, q, len(q), byref(evaluator))
        self._check_result(result)
        assert evaluator.value is not None
        return Evaluator(self._lib, self, evaluator)


class Evaluator:
    def __init__(self, lib: CDLL, db: Database, evaluator: c_void_p) -> None:
        self._evaluator = evaluator
        # We store the database, to ensure that it outlives the evaluator.
        self._db = db
        # Cache a reference to the library.
        self._lib = lib

    def __del__(self) -> None:
        self._lib.noblit_query_close(self._evaluator)

    def __repr__(self) -> str:
        return f'Evaluator(0x{self._evaluator.value:x})'


with open('../mindec/mindec.ndb', 'rb') as f:
    db = Database.open_packed_in_memory(f)
    print(db)
    query_builtin_types = (
        b'\x02\x00\x04\x00name\x01\x00t\x01\x00\x01\x00\x0c\x00'
        b'db.type.name\x00\x00\x00\x02\x00\x01\x00\x00\x00'
    )
    evaluator = db.query(query_builtin_types)
    print(evaluator)
