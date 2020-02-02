# Fuzz tests

Noblits internals are tested thoroughly through fuzz testing. Noblit uses
structure-aware fuzzing to generate sequences of calls to internal
<abbr>API</abbr>s, after which all invariants are checked. See the code in the
`fuzz` directory.

TODO: Expand these docs.

TODO: Add more traditional fuzzer, for the parser of the binary query format.

TODO: Add a structure-aware fuzzer for the higher-level <abbr>API</abbr>, in
addition to those that focus on internals. Maybe just against the C interface.

TODO: Add an abstraction for <abbr>IO</abbr>, and an implementation that that
can inject errors based on the fuzz input.
