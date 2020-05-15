// Noblit -- An immutable append-only database
// Copyright 2020 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

extern crate noblit;

#[macro_use]
mod mono;

use std::fs;
use std::io;
use std::os::raw::{c_int, c_void};
use std::slice;
use std::string::ToString;

use noblit::binary::Cursor;
use noblit::database::View;
use noblit::database;
use noblit::disk;
use noblit::error::Error;
use noblit::error::Result;
use noblit::eval;
use noblit::heap;
use noblit::parse;
use noblit::plan::Plan;
use noblit::planner::Planner;
use noblit::store;
use noblit::temp_heap::Temporaries;

use mono::Contextual;

/// Wraps a `noblit::Database` for use through the C API.
///
/// This struct is referred to as `noblit_t` in the C API reference.
pub struct Context {
    /// The database to manage.
    db: mono::Database,

    /// If the last call returned an error, this contains the formatted message.
    last_error: Option<String>,
}

impl Context {
    /// Clear `self.last_error`.
    fn clear_error(&mut self) {
        self.last_error = None;
    }

    /// Set `self.last_error` to the error message, return the associated error code.
    fn observe_error(&mut self, err: Error) -> u32 {
        self.last_error = Some(err.to_string());
        match err {
            Error::IoError(..) => 1,
        }
    }
}

pub struct Evaluator<'a, Store: 'a + store::Store, Heap: 'a + heap::Heap> {
    eval: eval::Evaluator<'a, Store, Heap>,
    view: Box<View<'a, Store, Heap>>,
    plan: Box<Plan>,
}

/// No-op function on `Database` and `Evaluator`.
///
/// The only point of this function is to trigger type inference to infer the
/// types of the store and heap on the evaluator, given a database reference.
/// This ensures that after passing an evaluator through the FFI, which erases
/// its type, we can get back the type later, if we have a typed database
/// reference.
fn infer_evaluator_type<'a, 'call, Store: store::Store, Heap: heap::Heap>(
    _: &'call database::Database<Store, Heap>,
    _: &'call Evaluator<'a, Store, Heap>,
) {}

pub fn noblit_query_open_impl<'a, 'b, Store: 'a + store::Store, Heap: 'a + heap::Heap>(
    db: &'a database::Database<Store, Heap>,
    query_bytes: &'b [u8]
) -> Result<Evaluator<'a, Store, Heap>> {
    let mut temporaries = Temporaries::new();
    let mut cursor = Cursor::from_bytes(query_bytes);
    // TODO: Report a proper error.
    let mut query = parse::parse_query(&mut cursor, &mut temporaries)?;
    let view = Box::new(db.view(temporaries));

    // Resolve named attributes to id-based attributes, and plan the query.
    query.fix_attributes(&view);

    // TODO: Add getter to get the slice of selected types.
    let types = query.infer_types(&view).expect("Type error.");
    let _select_types: Vec<_> = query.select.iter().map(|s| types[s.0 as usize]).collect();

    let plan = Box::new(Planner::plan(&query));

    // Get references to the plan and view inside the boxes. Usually in Rust
    // code, these would live on the stack, along with the evaluator itself,
    // and the borrow checker ensures that the borrows of the plan and the
    // view remain valid as long as the evaluator exists. But here in FFI,
    // we can't put these things on the stack if we want the evaluator to
    // outlive this call (which is needed so the foreign caller can call
    // next on it). Ideally, they would live in the frame of the foreign
    // caller, but doing so is not easy from non-C languages, and we clutter
    // the API with pointers to implementation details. So to make the API
    // harder to abuse, we instead put everything on the heap, where we have
    // control over the lifetimes. The borrow checker can no longer help us
    // here, we need to manually make sure to drop the evaluator before
    // dropping the plan and view.
    let (plan_ptr, view_ptr) = unsafe {
        (&*(&*plan as *const Plan), &*(&*view as *const View<_, _>))
    };

    let eval = Evaluator {
        view: view,
        plan: plan,
        eval: eval::Evaluator::new(plan_ptr, view_ptr),
    };

    Ok(eval)
}

#[repr(C)]
pub struct noblit_slice_t {
    data: *const u8,
    len: usize,
}

/// Unsafely cast a byte slice to a struct exposable through the FFI.
unsafe fn as_slice(data: &[u8]) -> noblit_slice_t {
    noblit_slice_t {
        data: data.as_ptr(),
        len: data.len(),
    }
}

#[no_mangle]
pub unsafe extern fn noblit_get_last_error(db: *const Context) -> noblit_slice_t {
    match (*db).last_error {
        None => as_slice(b""),
        Some(ref message) => as_slice(message.as_bytes()),
    }
}

fn noblit_open_packed_in_memory_impl(file: &mut fs::File) -> Box<Context> {
    let db = disk::read_packed(&mut io::BufReader::new(file)).expect("Failed to read database.");
    let context = Context {
        db: mono::Database::Memory(db),
        last_error: None,
    };
    Box::new(context)
}

#[no_mangle]
pub unsafe extern fn noblit_close(db: *mut Context) {
    // Take back ownership of the box, and drop that box when it goes out of scope.
    let _ = Box::from_raw(db);
}

#[no_mangle]
pub unsafe extern fn noblit_open_packed_in_memory(fd: c_int) -> *mut Context {
    use std::os::unix::io::{FromRawFd, IntoRawFd};
    // Turn the foreign file handle into a Rust File, read the database from it.
    let mut file = fs::File::from_raw_fd(fd);
    let context = noblit_open_packed_in_memory_impl(&mut file);
    // We only borrow the foreign file, so the File::drop should not close it.
    // Turning the file back into the foreign file descriptor prevents the drop.
    let _ = file.into_raw_fd();
    Box::into_raw(context)
}

#[no_mangle]
pub unsafe extern fn noblit_query_open(
    ctx: *mut Context,
    query: *const u8,
    query_len: usize,
    out_eval: *mut *mut c_void
) -> u32 {
    let query_bytes = slice::from_raw_parts(query, query_len);
    with_database!(ctx, |db| {
        let evaluator = noblit_query_open_impl(db, query_bytes)?;
        *out_eval = Contextual::<Evaluator<_, _>>::new(ctx, evaluator);
        Ok(())
    })
}

#[no_mangle]
pub unsafe extern fn noblit_query_close(query: *mut c_void) {
    into_context!(query, |db, evaluator| {
        infer_evaluator_type(db, evaluator);
        Ok(())
    });
}
