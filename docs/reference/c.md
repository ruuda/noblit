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

    typedef struct Noblit noblit_t;

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

## noblit_db_read_packed

    noblit_t*
    noblit_db_read_packed(uint8_t const* fname, size_t fname_len);

<dl>
  <dt>fname</dt>
  <dd>
    Character array with the file name to open.
    Should not include a null terminator.
    Borrowed immutably for the duration of the call.
  </dd>
  <dt>fname_len</dt>
  <dd>
    Length of the file name array in bytes.
  </dd>
</dl>
<dl>
  <dt>return value</dt>
  <dd>
    A pointer to a database struct.
    The pointer should be treated as an opaque pointer,
    Should be treated as an opaque
  </dd>
</dl>

## noblit_db_free

    void
    noblit_db_free(noblit_t* db);

Close the database, release associated resources, and deallocate the `noblit_t`
struct.
