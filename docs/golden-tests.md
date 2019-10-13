# Golden tests

Noblit comes with a suite of golden test: queries with known-good reference
results. The golden tests are stored in .t files in the _golden_ directory.
Each file contains a query and its expected outcome. Goldens can be verified
with the test runner, `golden/run.py`.

## Example

Below is a formatted example golden test that selects the entity id and name of
every built-in type. It consists of the query, followed by the expected output:

    where
      t db.type.name name
    select
      t, name

  | t    | name           |
  |------|----------------|
  | # 7  | db.type.bool   |
  | # 10 | db.type.bytes  |
  | # 8  | db.type.ref    |
  | # 11 | db.type.string |
  | # 9  | db.type.uint64 |

The above example is formatted for inclusion in the docs. In the .t files,
expected output is included as a table drawn with box-drawing characters, as
this is also what the `execute` binary prints. The encoding of the .t file is
<abbr>UTF-8</abbr>.

## Checking all goldens

The test runner `golden/run.py` is desiged to be used with a [<abbr>TAP</abbr>][tap]
harness such as [Prove][prove]. The runner prints <abbr>TAP</abbr>-compliant
output to stdout. The runner can be used with Prove to verify all goldens:

    $ prove --exec golden/run.py golden

This will run `golden/run.py` for every .t file in the _golden_ directory, and
print a summary of the results. Prove looks for .t files by default, which is
also why Noblit uses that extension for goldens. You can get verbose output of
the query and output by passing `--verbose`:

    $ prove --exec 'golden/run.py --verbose' --verbose golden

[tap]:   https://testanything.org/
[prove]: https://perldoc.perl.org/prove.html

## Stages

Checking a golden consists of several stages:

* The runner reads .t file and splits it into a query and expected outcome.
* The query is parsed by `golden/parse.py`, and serialized into a binary format.
* The `execute` binary parses the binary query and executes it.
* The runner pipes the binary query into the `execute` binary, and compares its
  output with the reference output.

The parser and executor can also be used standalone. For example, to run the
builtin types query:

    $ cat golden/builtin_types.t | golden/parse.py | target/debug/execute

In this mode, the parser discards the reference output in the file.
