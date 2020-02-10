# Noblit -- An immutable append-only database
# Copyright 2020 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.
from __future__ import annotations

from ctypes import CDLL, cdll, c_char_p, c_size_t, c_void_p, c_int
from typing import IO, Optional, NamedTuple

_LIB: Optional[CDLL] = None

def load_lib() -> CDLL:
    """
    Load libnoblit.so.
    """
    last_exc = None

    # TODO: How to clean this up to make it usable in practice?
    for prefix in ('', 'target/debug/', 'target/release'):
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

        _LIB.noblit_open_packed_in_memory.argtypes = [c_int]
        _LIB.noblit_open_packed_in_memory.restype = c_void_p
        _LIB.noblit_close.argtypes = [c_void_p]
        _LIB.noblit_close.restype = None

    return _LIB


class Database:
    def __init__(self, db: c_void_p) -> None:
        self._db = db

    @staticmethod
    def open_packed_in_memory(dbfile: IO[bytes]) -> Database:
        lib = ensure_lib()
        db = lib.noblit_open_packed_in_memory(dbfile.fileno())
        return Database(db)

    def __del__(self) -> None:
        lib = ensure_lib()
        lib.noblit_close(self._db)


with open('../mindec/mindec.ndb', 'rb') as f:
    db = Database.open_packed_in_memory(f)
    print(db)