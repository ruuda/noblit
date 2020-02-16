#!/usr/bin/env python3

# Noblit -- An immutable append-only database
# Copyright 2020 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Pack up a fuzz corpus into a single file that can be compressed well.

Similar to tar, but specialized to fuzz inputs to facilitate compression:

  * File names or permissions are not stored; the files are named after their
    hash anyway, so they can be recomputed later.
  * Store files sorted and back to back, such that similar inputs are close,
    which makes backreferences more useful.

Usage:
    pack_corups.py pack fuzz/corpus/htree_insert | brotli -q9 --output htree_insert.br
    brotli --decompress --stdout htree_insert.br | pack_corpus.py unpack fuzz/corpus/htree_insert
"""

import sys
import subprocess
import os
import os.path

from typing import IO, Set


def load_corpus(path: str) -> Set[bytes]:
    fnames: List[str] = []

    for fname in os.listdir(path):
        fname = os.path.join(path, fname)
        if os.path.isfile(fname):
            fnames.append(fname)

    corpus: Set[bytes] = set()

    for i, fname in enumerate(fnames):
        print(f'\r[{i + 1} / {len(fnames)}] {fname}', end='', file=sys.stderr)
        with open(fname, 'rb') as f:
            corpus.add(f.read())

    return corpus


def u32_le(x: int) -> bytes:
    return x.to_bytes(length=4, byteorder='little', signed=False)


def i32_le(x: int) -> bytes:
    return x.to_bytes(length=4, byteorder='little', signed=True)


def write_corpus(out: IO[bytes], corpus: Set[bytes]) -> None:
    assert len(corpus) > 0
    sorted_corpus = sorted(corpus)
    
    # First, the size of the corpus.
    out.write(u32_le(len(sorted_corpus)))

    # Then the size of each entry, encoded as a delta from the previous entry,
    # apart from the first one, which is stored as-is. This leads to small
    # numbers, which compress well.
    prev_len = len(sorted_corpus[0])
    out.write(u32_le(prev_len))
    for entry in sorted_corpus[1:]:
        new_len = len(entry)
        out.write(i32_le(new_len - prev_len))
        prev_len = new_len

    # After that, write the entries themselves. They are sorted, so even with a
    # small window, dictionaries work well.
    for entry in sorted_corpus[1:]:
        out.write(entry)


def main() -> None:
    if len(sys.argv) != 3:
        print(__doc__)
        sys.exit(1)

    action = sys.argv[1]
    path = sys.argv[2]

    if action == 'pack':
        corpus = load_corpus(path)
        if sys.stdout.buffer.isatty():
            total_size = sum(len(entry) for entry in corpus)
            print(
                f'Would pack {len(corpus)} files '
                f'totalling {total_size // 1000} kB, if stdout was not a tty.'
            )
        else:
            write_corpus(sys.stdout.buffer, corpus)


if __name__ == '__main__':
    main()
