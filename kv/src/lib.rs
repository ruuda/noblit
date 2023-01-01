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

trait Error {}

pub struct Block(Box<[u8]>);
pub struct BlockId(u64);

trait BlockStoreRead {
    fn get_blocks(&mut self, ids: &[BlockId]) -> &[Block];
}

trait BlockStoreWrite {
    fn put_blocks(&mut self, blocks: Vec<Block>) -> Vec<BlockId>;
}

trait BlockStore {
    /// The size of one block, in bytes.
    const BLOCK_SIZE: usize;

    /// The size of a superblock, in bytes. Must be a multiple of the block size.
    const SUPERBLOCK_SIZE: usize;
}
