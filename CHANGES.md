v0.2.0 2024-01-22 La Forclaz (VS)
---------------------------------

- Decompression limits in `Zipc_deflate` (breaking change). When the
  `decompression_size` argument of decompression functions is
  specified, decompression errors as soon as the decompressed size
  exceeds that value. Previously it was only treated as a hint.

- `Zipc.File.to_binary_string`, error if the decompression size starts
  exceeding `File.decompressed_size` rather than ignoring this value.
  This allows clients to enforce limits on the decompression of
  untrusted zip file. Thanks to Valentin Gatien-Baron for suggesting (#1).

- `Zipc.File.make`: fix default value of `version_needed_to_extract`. It
  was the same as `version_made_by` which is wrong. Now defaults
  to `20`(PKZip 2.0). Thanks to Valentin Gatien-Baron for the report (#3).

- Fix encoding of `Zipc.File.gp_flags`. Bit 3 indicates presence of a
  data descriptor. Since we never write one, we clear the bit on
  encoding. Not doing this would result in interoperability issues
  when rewriting archives that originally had data descriptors. Thanks
  to Valentin Gatien-Baron for tracking this down (#4).

- Fix swapped date and time in the encoding of local file headers.
  Thanks to Valentin Gatien-Baron for the fix (#5).

- Write entries in the central directory in the same order as files
  are written in the archive. In particular this gives less
  surprising result on `unzip -l`. Thanks to Valentin Gatien-Baron
  for the patch (#2).

v0.1.0 2023-11-08 Zagreb
------------------------

First release.

Supported by a grant from the OCaml Software Foundation.
