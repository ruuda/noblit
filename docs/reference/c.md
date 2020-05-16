# C API reference

Noblit exposes a C interface for use from non-Rust code. The C interface is an
unsafe layer on top of the Rust crate that implements Noblit. The C interface is
also written in Rust, but exposes unmangled symbols with C-compatible types.

The C interface is intended to be used by client libraries and therefore minimal.
There are no functions to construct or manipulate queries. Instead, Noblit
accepts queries in a binary format, and expects client libraries to build and
serialize queries. This ensures that only simple data types have to be passed
across <abbr>FFI</abbr> boundaries, and it minimizes ownership transfer across
<abbr>FFI</abbr> boundaries. This eliminates room for error and makes it easier
to fuzz the remaining <abbr>API</abbr>.

The Python and Haskell client libraries are built upon the C interface.

## noblit_t

    typedef struct noblit noblit_t;

An opaque database handle. Internally, this structure contains a
`noblit::Database`, with type parameters fixed to one of three cases:

 * **In-memory**. In-memory databases are always mutable.
 * **Immutable on-disk**. The files can be opened in read-only mode.
   (Not yet implemented.)
 * **Mutable on-disk**. The files need to be opened writeable.
   (Not yet implemented.)

Furthermore, the `noblit_t` structure stores the most recent error that
occurred, if any. In Rust, errors are tracked through the [`Result`][rust-result]
type, and these are returned to the C interface as a status code, with separate
getters for the error details in case of an error. TODO: Implement this.

[rust-result]: https://doc.rust-lang.org/std/result/enum.Result.html

## noblit_result_t

    typedef uint32_t noblit_result_t;

Noblit functions return status codes. A zero return value means success, other
values indicate failure. The status codes map to `noblit::error::Error` in Rust.
If a function returns a non-zero status code,
[`noblit_get_last_error`](#noblit_get_last_error) will return a string that
contains the full error message.

    #define NOBLIT_OK 0
    #define NOBLIT_IO_ERROR 1

## noblit_slice_t

    typedef struct noblit_slice {
      uint8_t const* data;
      size_t len;
    } noblit_slice_t;

A view into an immutable byte array owned by Noblit.

## noblit_get_last_error

    noblit_slice_t
    noblit_get_last_error(noblit_t const* db);

If the last call to a function that returns [`noblit_result_t`](#noblit_result_t)
returned a nonzero status code, this function returns the full error message
string as <abbr>UTF-8</abbr> bytes, excluding null terminator.

The slice pointed to is valid until the next call to a Noblit function. If the
message is needed for after that, the caller needs to `memcpy` it elsewhere. If
a C-style null terminated string is desired, the caller needs to copy it into a
buffer one longer than the slice, to accomodate the null terminator.

## noblit_open_packed_in_memory

    noblit_t*
    noblit_open_packed_in_memory(int fd);

Load a database from a file in packed format into memory. The resulting database
is mutable, but mutations are applied to the in-memory database. The file that
it was loaded from is left untouched when the database is mutated.

<dl>
  <dt>fd</dt>
  <dd>
    File descriptor of the file to read from. The file descriptor is borrowed
    mutably for the duration of the call.
  </dd>
</dl>
<dl>
  <dt>return value</dt>
  <dd>
    A pointer to a database struct.
    The pointer should be treated as an opaque pointer.
  </dd>
</dl>

## noblit_open_packed_mmap

    noblit_t*
    noblit_open_packed_mmap(int fd);

Map a database from a file in packed format into memory. The resulting database
is immutable.

<dl>
  <dt>fd</dt>
  <dd>
    File descriptor of the file to read from. The file descriptor is borrowed
    mutably for the duration of the call.
  </dd>
</dl>
<dl>
  <dt>return value</dt>
  <dd>
    A pointer to a database struct.
    The pointer should be treated as an opaque pointer.
  </dd>
</dl>

Memory-mapping a database has a **severe caveat**: there is no way to handle
<abbr>IO</abbr> errors gracefully. The process will receive <abbr>SIGBUS</abbr>
on <abbr>IO</abbr> errors.

## noblit_close

    void
    noblit_close(noblit_t* db);

Close the database, release associated resources, and deallocate the `noblit_t`
struct.

## noblit_evaluator_t

    typedef struct noblit_query noblit_evaluator_t;

A query evaluator.

## noblit_query_open

    noblit_result_t
    noblit_query_open(
        noblit_t *const db,
        uint8_t *const query,
        size_t query_len,
        noblit_evaluator_t** evaluator
    );

Parse and plan a query, produce an evaluator to iterate the results.

<dl>
  <dt>db</dt>
  <dd>
    Database to query. The database remains borrowed for as long as the
    evaluator exists; it should not be referenced until the matching
    <a href="#noblit_query_close"><code>noblit_query_close</code></a>.
  </dd>
  <dt>query</dt>
  <dd>
    Buffer that contains the query in binary format. The query buffer is
    borrowed for the duration of the call, Noblit does not reference it after
    the call returns. See also <code>noblit::binary</code> module for the binary
    format reference. TODO: Document it properly.
  </dd>
  <dt>query_len</dt>
  <dd>Length of the query buffer in bytes.</dd>
  <dt>evaluator</dt>
  <dd>
    Out parameter for the resulting evaluator. When the function returns 0,
    <code>*evaluator</code> will contain a pointer to the evaluator.
  <dd>
</dl>


## noblit_query_close

    void
    noblit_query_close(noblit_evaluator_t* evaluator);

Take ownership of the evaluator and deallocate it.
