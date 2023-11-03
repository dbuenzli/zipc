Zipc – ZIP archive and deflate codec for OCaml
==============================================

Zipc is an in-memory [ZIP archive] and [deflate] compression
codec. Other compression formats in ZIP archives can be supported by
using third-party libraries.

Zipc has no dependencies and no C code. It is distributed under the
ISC license.

[ZIP archive]: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
[deflate]: https://www.rfc-editor.org/rfc/rfc1951

Homepage: <https://erratique.ch/software/zipc>

## Installation

Zipc can be installed with `opam`:

    opam install zipc
    opam install zipc cmdliner  # For the zipc tool.

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc zipc`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: https://erratique.ch/software/zipc/doc
[OCaml forum]: https://discuss.ocaml.org

## Examples

The [`zipc`] tool operates on ZIP archives with the library.

See also the examples from the [quick start] and `js_of_ocaml`
examples in [`test`].

[`zipc`]: test/zipc_tool.ml
[quick start]: test/examples.ml
[`test`]: test/

## Acknowledgments 

A grant from the [OCaml Software Foundation] helped to bring the first
public release of `zipc`.

The deflate compressor and general information about ZIP files
benefited from Hans Wennborg's thorough [article] about Zip files. The
deflate decompressor implementation started as a port of Joergen
Ibsen's [`tinf`] library.

[`tinf`]: https://github.com/jibsen/tinf
[article]: https://www.hanshq.net/zip.html
[OCaml Software Foundation]: http://ocaml-sf.org/
