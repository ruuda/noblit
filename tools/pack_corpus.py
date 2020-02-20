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

Comparison on a corpus of 30k files, ~75 MB, using Brotli -11 for compression:

    tar     100,044,800 bytes
    tar.br    2,223,921 bytes
    pack     75,526,720 bytes
    pack.br     579,591 bytes

So the packed-compressed corpus is about 25% the size of the tarred-compressed
corpus, and about 0.77% the original size.

Usage:
    pack_corups.py pack fuzz/corpus/htree_insert | brotli -q9 --output htree_insert.br
    brotli --decompress --stdout htree_insert.br | pack_corpus.py unpack fuzz/corpus/htree_insert
"""

import hashlib
import os
import os.path
import subprocess
import sys

from typing import IO, Iterable, List, Set


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

    print('\n', end='', file=sys.stderr, flush=True)
    return corpus


def u32_le(x: int) -> bytes:
    return x.to_bytes(length=4, byteorder='little', signed=False)


def u16_le(x: int) -> bytes:
    return x.to_bytes(length=2, byteorder='little', signed=False)


def from_unsigned_le(data: bytes) -> int:
    return int.from_bytes(data, byteorder='little', signed=False)


def unsign(x: int) -> int:
    """
    Encode a signed integer as an unsigned integer, in such a way that values
    close to zero end up close to zero. Credit for this way of encoding integers
    goes to the FLAC format, which encodes deltas in a similar manner.

    For a 30k file, 75 MB corpus, these are the sizes after compressing with
    Brotli -9:

    * Raw u32le:                   623871 bytes
    * Delta, i3le:                 620851 bytes
    * Delta, u32le through unsign: 619479 bytes
    * Delta, u16le through unsign: 615054 bytes

    So we can save a few bytes by encoding the deltas like this.
    """
    if x >= 0:
        return x * 2
    else:
        return (x * -2) - 1


def resign(x: int) -> int:
    """
    Inverse of "unsign".
    """
    if x % 2 == 0:
        return x // 2
    else:
        return (x + 1) // -2


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
        out.write(u16_le(unsign(new_len - prev_len)))
        prev_len = new_len

    # After that, write the entries themselves. They are sorted, so even with a
    # small window, dictionaries work well.
    for entry in sorted_corpus:
        out.write(entry)


def read_corpus(inp: IO[bytes]) -> Iterable[bytes]:
    num_entries = from_unsigned_le(inp.read(4))
    prev_len = from_unsigned_le(inp.read(4))

    entry_lengths = [prev_len]

    for _ in range(1, num_entries):
        delta = resign(from_unsigned_le(inp.read(2)))
        entry_len = prev_len + delta
        entry_lengths.append(entry_len)
        prev_len = entry_len

    for entry_len in entry_lengths:
        yield inp.read(entry_len)


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
                f'totalling {total_size / 1e6:.3f} MB, if stdout was not a tty.'
            )
        else:
            write_corpus(sys.stdout.buffer, corpus)

    if action == 'unpack':
        for entry in read_corpus(sys.stdin.buffer):
            # Name entries after their SHA1 hash, as that is what libfuzzer
            # names them.
            fname = os.path.join(path, hashlib.sha1(entry).hexdigest())
            with open(fname, 'wb') as f:
                f.write(entry)


if __name__ == '__main__':
    main()
