// Noblit -- An immutable append-only database
// Copyright 2022 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

/// Trait to allow compile-time parametrization over the page size.
///
/// This is mainly useful in tests, where constructing large trees is
/// undesirable, and for fuzzing, where small examples are faster to explore.
/// In that case we can use smaller pages, while in release mode we use a page
/// size that is a multiple of the OS page size.
pub trait PageSize {
    /// The number of bytes in a page. A page stores exactly one tree node.
    const SIZE: usize;

    /// The number of datoms that fit in a page.
    const CAPACITY: usize;
}

/// A queue models a device or service that supports asynchronous IO.
trait Queue {
    type Input;
    type Output;
    type Error;

    pub fn send(request: Input) -> Result<(), Error>;
    pub fn recv() -> Result<Output, Error>;
}

pub struct BlockId(u64);
pub struct SuperBlockId(u64);

enum BlockStoreIn {
    GetBlock(BlockId),
    Allocate,
    EraseSuperBlock(SuperBlockId);
}

enum BlockStoreOut {
    GotBlock(BlockId, Vec<u8>),
    Allocated(BlockId),
    ErasedSuperBlock(SuperBlockId),
}

trait BlockStore {
    /// The size of one block, in bytes.
    const BLOCK_SIZE: usize;

    /// The size of a superblock, in bytes. Must be a multiple of the block size.
    const SUPERBLOCK_SIZE: usize;
}
