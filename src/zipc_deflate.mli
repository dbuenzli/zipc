(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Deflate and zlib compressed data formats.

    {b Note.} This not needed to use {!Zipc}.

    This module provides (non-streaming) support to compress and decompress
    the deflate ({{:https://www.rfc-editor.org/rfc/rfc1951}RFC 1951})
    and zlib ({{:https://www.rfc-editor.org/rfc/rfc1950}RFC 1950})
    data formats. *)

(** {1:checksums Checksums} *)

type uint16 = int
(** The type for unsigned 16-bit integers. *)

type uint32 = int32
(** The type for unsigned 32-bit integers. *)

(** ZIP CRC-32 checksums. *)
module Crc_32 : sig

  (** {1:checksums Checksums} *)

  type t = uint32
  (** The type for ZIP CRC-32 checksums (polynomial [0xedb88320]). *)

  val equal : t -> t -> bool
  (** [equal c0 c1] is [true] iff [c0] and [c1] are equal. *)

  val check : expect:t -> found:t -> (unit, string) result
  (** [check ~expect ~found] is [Ok ()] iff [equal exp found] is
      [true] otherwise it errors with a human error message that
      mentions the checksums. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a checksum. *)

  (** {1:computing Computing} *)

  val string : ?start:int -> ?len:int -> string -> t
  (** [string s] is the CRC-32 checksum of [s] in the range
      \[[start];[start+len-1]\]. [start] defaults
      to [0] and [len] defaults to [String.length s - start]. *)
end

(** Adler-32 checksums. *)
module Adler_32 : sig

  (** {1:checksums Checksums} *)

  type t = uint32
  (** The type for Adler-32 checksums. *)

  val equal : t -> t -> bool
  (** [equal c0 c1] is [true] iff [c0] and [c1] are equal. *)

  val check : expect:t -> found:t -> (unit, string) result
  (** [check ~expect ~found] is [Ok ()] iff [equal exp found] is
      [true] otherwise it errors with a human error message that
      mentions the checksums. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a checksum. *)

  (** {1:computing Computing} *)

  val string : ?start:int -> ?len:int -> string -> t
  (** [string s] is the Adler-32 checksum of [s] in the range
      \[[start];[start+len-1]\]. [start] defaults
      to [0] and [length] defaults to [String.length b - start]. *)
end

(** {1:decompressing Decompressing} *)

val inflate :
  ?decompressed_size:int -> ?start:int -> ?len:int -> string ->
  (string, string) result
(** [inflate s] are the decompressed bytes of the deflate compressed
    data in the range \[[start];[start+len-1]\] of [s]. [start]
    defaults to [0] and [len] defaults to [String.length s -
    start]. [decompressed_size] is the expected size of the
    decompressed data, errors if exceeded.

    Returns [Error _] with an english error message if the data is
    corrupted, if the decompressed data exceeds [decompressed_size]
    or {!Sys.max_string_length}. *)

val inflate_and_crc_32 :
  ?decompressed_size:int -> ?start:int -> ?len:int -> string ->
  (string * Crc_32.t, string) result
(** [inflate_and_crc_32] is like {!inflate} but also returns the
    CRC-32 checksum of the output bytes. *)

val inflate_and_adler_32 :
  ?decompressed_size:int -> ?start:int -> ?len:int -> string ->
  (string * Adler_32.t, string) result
(** [inflate_and_crc_32] is like {!inflate} but also returns the
    Adler-32 checksum of the output bytes. *)

val zlib_decompress :
  ?decompressed_size:int -> ?start:int -> ?len:int -> string ->
  (string * Adler_32.t, (Adler_32.t * Adler_32.t) option * string) result
(** [zlib_decompress s] are the decompressed bytes of the zlib
    compressed data in the range \[[start];[start+len-1]\] of
    [s]. [start] defaults to [0] and [len] defaults to [String.length
    s - start]. [decompressed_size] is the expected size of
    decompressed data, errors if exceeded. The (successfully checked)
    Adler-32 checksum of the decompressed data is also returned.

    Returns [Error _] with an english error message if the data is
    corrupted, if the checksum mismatches (in which case you get the
    expected and found CRCs in the option, the error message mentions
    them), if the stream declares a preset dictionary, if the decompressed
    data exceeds [decompressed_size] or {!Sys.max_string_length}. *)


(** {1:compression Compression} *)

type level =
  [ `None (** Only non-compressed blocks. *)
  | `Fast | `Default | `Best ]
(** The type for compression levels. *)

val deflate :
  ?level:level -> ?start:int -> ?len:int -> string -> (string, string) result
(** [deflate s] is the compressed data in
    {{:https://www.rfc-editor.org/rfc/rfc1951}deflate format} of the
    uncompressed bytes stored in the range \[[start];[start+len-1]\]
    of [s].  [start] defaults to [0] and [len] defaults to
    [String.length s - start]. level defaults to [`Default].

    Returns [Error _] if a string cannot hold the compressed result on
    32-bit platforms. *)

val crc_32_and_deflate :
  ?level:level -> ?start:int -> ?len:int -> string ->
  (Crc_32.t * string, string) result
(** [crc_32_and_deflate] is like {!deflate} but is also returns the
    CRC-32 checksum of the input bytes. *)

val adler_32_and_deflate :
  ?level:level -> ?start:int -> ?len:int -> string ->
  (Adler_32.t * string, string) result
(** [adler_32_and_deflate] is like {!deflate} but is also returns the
    Adler-32 checksum of the input bytes. *)

val zlib_compress :
  ?level:level -> ?start:int -> ?len:int -> string ->
  (Adler_32.t * string, string) result
(** [zlib_compress s] is the compressed data in
    {{:https://www.rfc-editor.org/rfc/rfc1950}zlib format} of the
    uncompressed bytes stored in the range \[[start];[start+len-1]\]
    of [s].  [start] defaults to [0] and [len] defaults to
    [String.length s - start]. [level] defaults to [`Default]. For
    information the Adler-32 checksum of [s] is also returned.

    Returns [Error _] if a string cannot hold the compressed result on
    32-bit platforms. *)
