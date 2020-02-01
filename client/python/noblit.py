# Noblit -- An immutable append-only database
# Copyright 2020 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.
from __future__ import annotations

from ctypes import CDLL, cdll, c_char_p, c_size_t, c_void_p
from typing import Optional, NamedTuple

_LIB: Optional[CDLL] = None

def load_lib() -> CDLL:
    """
    Load libnoblit.so.
    """
    # TODO: How to clean this up to make it usable in practice?
    for prefix in ('', 'target/debug/', 'target/release'):
        try:
            return cdll.LoadLibrary(prefix + 'libnoblit.so')
        except OSError as exc:
            pass

    raise exc


def ensure_lib() -> CDLL:
    """
    Memoized version of 'load_lib()'.
    """
    global _LIB

    if _LIB is None:
        _LIB = load_lib()

        _LIB.noblit_db_read_packed.argtypes = [c_char_p, c_size_t]
        _LIB.noblit_db_read_packed.restype = c_void_p
        _LIB.noblit_db_free.argtypes = [c_void_p]
        _LIB.noblit_db_free.restype = None

    return _LIB


class Database:
    def __init__(self, db: c_void_p) -> None:
        self._db = db

    @staticmethod
    def read_packed(fname: bytes) -> Database:
        lib = ensure_lib()
        db = lib.noblit_db_read_packed(fname, len(fname))
        return Database(db)

    def __del__(self) -> None:
        lib = ensure_lib()
        lib.noblit_db_free(self._db)


db = Database.read_packed(b'../mindec/mindec.ndb')
print(db)
