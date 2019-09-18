// Noblit -- An immutable append-only database
// Copyright 2018 Ruud van Asseldonk

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// A copy of the License has been included in the root of the repository.

//! Defines the types of values.

use std::io;
use std::fmt;

use datom::Value;
use pool;

/// The supported value types for entity values.
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Type {
    Bool,
    Ref,
    Uint64,
    Bytes,
    String,
}

pub fn write_value<W, P: pool::Pool>(
    writer: &mut W,
    pool: &P,
    value: Value,
    value_type: Type,
    ) -> Result<(), fmt::Error>
where
    W: fmt::Write
{
    match value_type {
        Type::Bool if value.as_bool() => write!(writer, "true"),
        Type::Bool => write!(writer, "false"),
        Type::Ref => write!(writer, "# {}", value.as_u64(pool)),
        Type::Uint64 => write!(writer, "{}", value.as_u64(pool)),
        Type::Bytes => unimplemented!("TODO: Bytes formatting."),
        Type::String => write!(writer, "{:?}", value.as_str(pool)),
    }
}

// TODO: The formatting functions below are bloody ugly. Part of this is because
// formatting with proper alignment is a lot harder than it seems at first
// sight. But propably some things can be simplified by using the std::fmt
// formatter and args to build up a format string and pass in the right values.

fn format_row<'a, W, ElemIter, WidthIter>(
    writer: &mut W,
    begin: &'a str,
    elems: ElemIter,
    widths: WidthIter,
    sep: &'a str,
    end: &'a str
    ) -> io::Result<()>
where
  W: io::Write,
  ElemIter: IntoIterator<Item = &'a str>,
  WidthIter: IntoIterator<Item = usize>,
{
    write!(writer, "{}", begin)?;
    let mut needs_sep = false;
    for (elem, w) in elems.into_iter().zip(widths) {
        if needs_sep {
            write!(writer, "{}", sep)?;
        }
        write!(writer, "{}", elem)?;
        let len = elem.chars().count();
        assert!(len <= w);
        write!(writer, "{}", " ".repeat(w - len))?;
        needs_sep = true;
    }
    write!(writer, "{}", end)
}

/// Format values into a table using box drawing characters.
pub fn draw_table<'a, W, HeaderIter, RowIter, Pool>(
    writer: &'a mut W,
    pool: &Pool,
    headers: HeaderIter,
    rows: RowIter,
    value_types: &'a [Type],
    ) -> io::Result<()>
where
    W: io::Write,
    Pool: pool::Pool,
    HeaderIter: Clone + IntoIterator<Item = &'a str>,
    RowIter: IntoIterator<Item = &'a [Value]>,
{
    let mut fmt_rows: Vec<Vec<String>> = Vec::new();
    let mut widths: Vec<usize> = headers.clone().into_iter().map(|h| h.chars().count()).collect();

    for row in rows {
        let mut fmt_row = Vec::with_capacity(value_types.len());
        for ((&value, &value_type), w) in row.iter().zip(value_types).zip(widths.iter_mut()) {
            let mut fmt_value = String::new();
            write_value(&mut fmt_value, pool, value, value_type).unwrap();
            *w = fmt_value.chars().count().max(*w);
            fmt_row.push(fmt_value);
        }
        fmt_rows.push(fmt_row);
    }

    let dashes: Vec<String> = widths.iter().map(|&w| "─".repeat(w)).collect();

    format_row(writer, "┌─", dashes.iter().map(|ref s| &s[..]), widths.iter().cloned(), "─┬─", "─┐\n")?;
    format_row(writer, "│ ", headers.into_iter(), widths.iter().cloned(), " │ ", " │\n")?;
    format_row(writer, "├─", dashes.iter().map(|ref s| &s[..]), widths.iter().cloned(), "─┼─", "─┤\n")?;
    for row in fmt_rows {
        format_row(writer, "│ ", row.iter().map(|ref s| &s[..]), widths.iter().cloned(), " │ ", " │\n")?;
    }
    format_row(writer, "└─", dashes.iter().map(|ref s| &s[..]), widths.iter().cloned(), "─┴─", "─┘\n")?;

    Ok(())
}
