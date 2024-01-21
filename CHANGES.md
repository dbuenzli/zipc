
- `Zipc.File.make`: fix default value of `version_needed_to_extract`. It
  was the same as `version_made_by` which is wrong. Now defaults
  to `20`(PKZip 2.0). Thanks to Valentin Gatien-Baron for the report (#3).

- Write entries in the central directory in the same order as files
  are written in the archive. In particular this gives less
  surprising result on `unzip -l`. Thanks to Valentin Gatien-Baron
  for the patch (#2).

v0.1.0 2023-11-08 Zagreb
------------------------

First release.

Supported by a grant from the OCaml Software Foundation.
