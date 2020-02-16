# Noblit

**Vaporware warning: much of the content below is hypothetical.**

[![Build Status][ci-img]][ci]

Noblit is an embeddable append-only database. The database records a history
of immutable (entity, attribute, value) tuples. Tuples can be asserted and
retracted. A retraction is recorded as a new fact; it is not a delete. Any
historical state of the database can be reproduced, and the history is
first-class and queryable.

## Learn more

Select documentation links:

 * [Overview](https://ruuda.github.io/noblit/)
 * [Building](https://ruuda.github.io/noblit/building/)
 * [Architecture](https://ruuda.github.io/noblit/architecture/)

Some example queries, in the form of golden tests:

 * [Film database example](golden/film.t)
 * [Bug tracker example](golden/bug_tracker.t)

## Status

Noblit is a toy project. It is pre-alpha, and not supported in any way.

## License

Noblit is licensed under the [Apache 2.0][apache2] license. It may be used in
free software as well as closed-source applications, both for commercial and
non-commercial use under the conditions given in the license. If you want to
use Noblit in your GPLv2-licensed software, you can add an [exception][except]
to your copyright notice. Please do not open an issue if you disagree with the
choice of license.

[ci-img]:  https://travis-ci.org/ruuda/noblit.svg?branch=master
[ci]:      https://travis-ci.org/ruuda/noblit
[apache2]: https://www.apache.org/licenses/LICENSE-2.0
[except]:  https://www.gnu.org/licenses/gpl-faq.html#GPLIncompatibleLibs
