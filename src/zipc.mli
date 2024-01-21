(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** ZIP archives.

    Consult the {{!page-index.quick}quick start} and
    {{!Zipc.limitations}limitations}.

    {b References.}
    {ul
    {- PKWARE. {{:https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT}
       {e ZIP File Format Specification}}. 2022}
    {- Hans Wennborg. {{:https://www.hanshq.net/zip.html}{e Zip Files:
       History, Explanation and Implementation}}. 2020}} *)

(** {1:archive_members Archive members} *)

type compression =
| Bzip2 | Deflate (** Via {!Zipc_deflate}. *)
| Lzma | Stored (** No compression. *)
| Xz | Zstd | Other of int (** *)
(** The type for compression formats.

    [Zipc] only {{!File.to_binary_string}handles} [Stored] and
    [Deflate] but third party libraries can be used to support others
    formats or to plug an alternate implementation of [Deflate]. *)

val pp_compression : Format.formatter -> compression -> unit
(** [pp_compression] formats compression formats. *)

(** File paths and modes. *)
module Fpath : sig

  (** {1:file_paths File paths} *)

  type t = string
  (** The type for file paths. Note that this may be the empty string. *)

  val ensure_unix : t -> t
  (** [ensure_unix p] substitutes any ['\\'] by ['/'] in [p]. *)

  val ensure_directoryness : t -> t
  (** [ensure_directoryness p] makes sure [p] ends with a ['/'].
      Returns ["./"] on the empty string. *)

  val sanitize : t -> t
  (** [sanitize_path p] is a sanitized for path [p]. This is either
      a relative path rooted in the current directory or the
      empty string. Directoryness (trailing ['/']) is suppressed and
      the function is not injective.

      No effort is made to interpret relative segments correctly; a
      zip file with such paths is likely malice or clumsiness. More
      precisely this splits [p] on ['/'] and ['\\'], removes any
      ["."], [".."]  or [""] segment (in particular this removes
      absolute paths) and {{!String.concat}concatenates} the resulting
      list with ["/"]. *)

  (** {1:modes File modes} *)

  type mode = int
  (** The type for UNIX file modes. *)

  val pp_mode : Format.formatter -> int -> unit
  (** [pp_mode] formats file modes like [ls -l] does. *)
end

(** POSIX time. *)
module Ptime : sig

  (** {1:posix_time POSIX time} *)

  type t = int
  (** The type for POSIX times in seconds since the Unix epoch. *)

  val dos_epoch : t
  (** [dos_epoch] is 1980-01-01 00:00:00 UTC. This is the earliest
      modification time representable in ZIP archives. *)

  val to_date_time : t -> (int * int * int) * (int * int * int)
  (** [to_date_time t] is the [(y, m, d), (hh, mm, ss)] representation
      of [t] in UTC. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats POSIX times according to
      {{:https://www.rfc-editor.org/rfc/rfc3339}RFC 3339} but without the [T]
      date and time separator. *)
end

(** Archive file data. *)
module File : sig

  (** {1:file File data} *)

  type t
  (** The type for file data. *)

  val make :
    ?version_made_by:Zipc_deflate.uint16 ->
    ?version_needed_to_extract:Zipc_deflate.uint16 ->
    ?gp_flags:Zipc_deflate.uint16 -> ?start:int -> ?compressed_size:int ->
    compression:compression -> string -> decompressed_size:int ->
    decompressed_crc_32:Zipc_deflate.Crc_32.t -> (t, string) result
  (** [make] creates file data with given properties, see their
      corresponding accessors for details. The defaults are:

      {ul
      {- [start] defaults to [0].}
      {- [compressed_size] defaults to the string's length minus [start].}
      {- [version_made_by] defaults to [0x314] indicating UNIX
         (for encoding file permissions) and PKZIP 2.0.}
      {- [version_needed_to_extract] defaults to [20] indicating
         PKZIP 2.0. This may need tweaking depending on [compression]
         but decoders likely do not care (see 4.4.3.2 in the specification).}
      {- [gp_flags] defaults to [0x800], indicating UTF-8 encoded filenames.}}

      [Error _] is returned with a suitable error message if any of
      the given length exceeds [4294967295], see
      {!max_size}. Negative lengths raise [Invalid_argument]. *)

  val stored_of_binary_string :
    ?start:int -> ?len:int -> string -> (t, string) result
  (** [stored_of_binary_string s] is [s] as [Stored] (no compression) file
      data. This errors if [s] exceeds {!max_size}. [start] defaults
      to [0] and [len] to [String.length s - start]. *)

  val deflate_of_binary_string :
    ?level:Zipc_deflate.level -> ?start:int -> ?len:int -> string ->
    (t, string) result
  (** [deflate_of_binary_string s] deflates [s] and returns it as
      [Deflate] file data. [level] defaults to [`Default]. This errors
      if [s] or its compressed size exceeds {!max_size}.  [start]
      defaults to [0] and [len] to [String.length s - start]. *)

  (** {1:properties Properties} *)

  val compression : t -> compression
  (** [compression file] is the compression format of [file]. *)

  val start : t -> int
  (** [start file] is the start index in {!compressed_bytes}. *)

  val compressed_size : t -> int
  (** [compressed_size file] is the byte size in {!compressed_bytes}. *)

  val compressed_bytes : t -> string
  (** [compressed_bytes file] are the bytes of [file] in {!val-compression}
      format, in the range defined by {!start} and {!compressed_size}. *)

  val compressed_bytes_to_binary_string : t -> string
  (** [compressed_bytes_to_binary_string file] is the range of
      {!compressed_bytes} as a tight string. *)

  val decompressed_size : t -> int
  (** [decompressed_size file] is {e metadata} indicating the
      size in bytes of the decompressed data of [file]. *)

  val decompressed_crc_32 : t -> Zipc_deflate.Crc_32.t
  (** [decompressed_crc_32 file] is {e metadata} indicating the
      CRC-32 checksum of the decompressed data of [file]. *)

  val version_made_by : t -> Zipc_deflate.uint16
  (** [version_made_by file] is the [version made by] field of
      [file]. Not really interested but we keep it to be able to update
      archives without loosing too much information.  *)

  val version_needed_to_extract : t -> Zipc_deflate.uint16
  (** [version_needed_to_extract file] is the [version needed to
      extract] field. See {!version_made_by}. *)

  val gp_flags : t -> Zipc_deflate.uint16
  (** [gp_flags file] is the [general purpose bit flag] field of [file]. In
      particular it tells us whether the file {!is_encrypted}. *)

  (** {1:preds Predicates} *)

  val is_encrypted : t -> bool
  (** [is_encrypted file] is [true] iff [file] is encrypted.
      [Zipc] has no support for file encryption. *)

  val can_extract : t -> bool
  (** [can_extract file] is [true] iff [Zipc] knows how to extract the
      bytes of [file]. In other words if [file] is not encrypted and
      its {!val-compression} is either [Stored] or [Deflate]. *)

  (** {1:decompressing Decompressing} *)

  val to_binary_string : t -> (string, string) result
  (** [to_binary_string file] is the decompressed data of [file]. This only
      supports [Stored] or [Deflate] formats and errors if:

      {ul
      {- The compression format is unsupported (does not happen if
         {!can_extract} is [true]).}
      {- The [Deflate] data is malformed.}
      {- The decompressed size exceeds {!decompressed_size} or
         {!Sys.max_string_length}}
      {- The CRC-32 of the data doesn't match the value of
         {!decompressed_crc_32}.}} *)

  val to_binary_string_no_crc_check :
    t -> (string * Zipc_deflate.Crc_32.t, string) result
  (** [to_binary_string_no_crc_check] is like {!to_binary_string} except it
      does not check the CRC-32 of the decompressed data against
      {!decompressed_crc_32}, it returns it along with the data. *)

  (** {1:limits Limits} *)

  val max_size : int
  (** [max_size] is the maximal file size representable on this
      platform in the file metadata. This is the minimum between
      {!Int.max_int} and [4294967295] (4Go), the maximum file size in
      non-ZIP64 ZIP archives. Archives that have members whose size
      exceeds this value error on {!Zipc.of_binary_string}.  *)
end

(** Archive members. *)
module Member : sig

  (** {1:member Members} *)

  type kind =
  | Dir (** Member is a directory. *)
  | File of File.t (** Member is a file. *)
  (** The type for archive member kinds. *)

  type t
  (** The type for archive members. *)

  val make :
    ?mtime:Ptime.t -> ?mode:int -> path:Fpath.t -> kind -> (t, string) result
  (** [make ~path kind] creates a member for [path] of kind [kind].

      {ul
      {- [path] goes through {!Fpath.ensure_unix} and,
         if the member is a directory, {!Fpath.ensure_directoryness}.
         Note that when you add the resulting member to an archive you may not
         be able to find it with the given [path], you need to use the
         {!path} of the result.}
      {- [mtime] defaults to {!Ptime.dos_epoch}. ZIP archives use
          MS-DOS date time whose epoch is on {!Ptime.dos_epoch}
          anything before is truncated to it}
      {- [mode] defaults [0o755] for directory entries and [0o644]
         for files.}}

      [Error _] is returned with a suitable error message if the length
      of [path] exceeds {!max_path_length}. *)

  val path : t -> Fpath.t
  (** [path m] is the file path of [m].

      {b WARNING} do not use this path on a file system without
      {{!Fpath.sanitize}sanitizing it}. Also, it may be the empty
      string. *)

  val mode : t -> Fpath.mode
  (** [mode m] is the UNIX file mode of [m]. *)

  val mtime : t -> Ptime.t
  (** [mtime m] is the last modification time of [m]. *)

  val kind : t -> kind
  (** [kind m] is the kind of member of [m]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp_member] formats members a bit like [ls -l] does. The reported
      byte size is the {!File.decompressed_size}. *)

  val pp_long : Format.formatter -> t -> unit
  (** [pp_long] is like {!pp} but adds more information. *)

  (** {1:limits Limits} *)

  val max : int
  (** [max] is [65535], the maximum number of members that can be encoded in
      a (non-ZIP64) ZIP archive. *)

  val max_path_length : int
  (** [max_path_length] is [65535], the maximal size for member paths in ZIP
      archives. *)
end

(** {1:archives Archives} *)

type t
(** The type for ZIP archives. *)

val empty : t
(** [empty] is an empty archive. *)

val is_empty : t -> bool
(** [is_empty z] is [true] iff [z] is empty. *)

val mem : Fpath.t -> t -> bool
(** [mem p z] is [true] iff [z] has a member with path [p]. *)

val find : Fpath.t -> t -> Member.t option
(** [find p z] is the member with path [p] of [z] (if any). *)

val fold : (Member.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f z acc] folds [f] over the members of [z] starting with [acc] in
    increasing lexicographic member path order. In particular this means
    that directory members, if they exist, are folded over before any of their
    content (assuming paths without relative segments). *)

val add : Member.t -> t -> t
(** [add member z] is [z] with [member] added. Overrides a previous
    member with the same path in [z] (if any). *)

val remove : Fpath.t -> t -> t
(** [remove p] is [z] with member with path [p] removed (if any). *)

val member_count : t -> int
(** [member_count z] is the number of members in [z]. *)

val to_string_map : t -> Member.t Map.Make(String).t
(** [to_string_map z] is [z] as a map from {!Member.path} to their values. *)

val of_string_map : Member.t Map.Make(String).t -> t
(** [of_string_map map] is [map] as a ZIP archive.

    {b Warning.} It is assumed that in [map] each key [k] maps to a member
    [m] with [Member.path m = k]. This is not checked by the function. *)

(** {2:decode Decode} *)

val string_has_magic : string -> bool
(** [string_has_magic s] is [true] iff [s] has at least 4 bytes and starts
    with [PK\x03\04] or [PK\x05\06] (empty archive). *)

val of_binary_string : string -> (t, string) result
(** [of_binary_string s] decodes a ZIP archive from [s].

    {b Note.} ZIP archives's integrity constraints are unclear. For now
    based on sanity and certain archives found in the wild that are supported
    by the [unzip] tool the following is done:
    {ul
    {- As a rule of thumb, all member metadata is determined only
    from the archive's
    {{:https://en.wikipedia.org/wiki/ZIP_(file_format)#Central_directory_file_header}
    central directory file header}; local file headers and data descriptors
    are ignored.}
    {- If a directory member pretends to have file data this data is ignored.}
    {- If a path is defined more than once, the second definition takes
    over.}
    {- If the central directory CRC-32 of a file member is [0] we lookup and use
      the value found in its local file header.}} *)

(** {2:encode Encode} *)

val encoding_size : t -> int
(** [encoding_size z] is the number of bytes needed to encode [z]. *)

val to_binary_string : ?first:Fpath.t -> t -> (string, string) result
(** [to_binary_string z] is the encoding of archive [z]. [Error _] is returned
    with a suitable error message in case [z] has more members than
    {!Member.max}.

    If a member with path [first] exists in [z] then this member's
    data is written first in the ZIP archive. It defaults to
    ["mimetype"] to support the EPUB
    {{:https://www.w3.org/TR/epub-33/#sec-zip-container-mime}OCF ZIP
    container} constraint (you are however in charge of making sure
    this member is not compressed in this case).

    {b Note.}
    {ul
    {- {!Member.mtime} that are before the {!Ptime.dos_epoch}
       are silently truncated to that date.}
    {- Except for [first], member data is encoded in the (deterministic)
       increasing lexical order of their path.}
    {- The encoding does not use data descriptors, so bit 3 of
       {!File.gp_flags} is always set to [0] on encoding.}} *)

val write_bytes :
  ?first:Fpath.t -> t -> ?start:int -> bytes -> (unit, string) result
(** [write_bytes t ~start b] writes {!to_binary_string} to [bytes] starting
    at [start] (defaults to [0]).

    Raises [Invalid_argument] if [b] is {{!encoding_size}too small}. *)

(** {1:limitations Limitations}

    Up to the limitations listed below [Zipc] is suitable for the following:
    {ul
    {- Reading and writing the subset of ZIP archives defined
       by ISO/IEC 21320-1 which is used as a documentation container for the
       {{:https://en.wikipedia.org/wiki/Office_Open_XML}Office Open XML}
       or {{:https://en.wikipedia.org/wiki/OpenDocument}OpenDocument}
       file formats. This subset mandates only stored or deflate compression
       formats.}
    {- Reading and writing the
       {{:https://www.w3.org/TR/epub-33/#sec-zip-container-zipreqs}EPUB}
       file format which loosely refers to the previous standard
       in its definition. These may however be ZIP64 if needed (see below).}
    {- Reading and writing dozen of others formats that are based on ZIP
       like
       {{:https://docs.oracle.com/en/java/javase/20/docs/specs/jar/jar.html}
       [.jar]},
       {{:https://en.wikipedia.org/wiki/Universal_Scene_Description}[.usdz]}
       (mandates no compression),
       {{:https://en.wikipedia.org/wiki/Keyhole_Markup_Language}[.kmz]},
       etc. Note that these formats do not always formally restrict the
       compression formats but deflate seems to be widely used.}}

    It is not the aim of [Zipc] to be able to read every ZIP archive
    out there. The format is quite loose, highly denormalized, has
    plenty of ways to encode metadata and allows many modern and
    legacy compression algorithms to be used. Hence take into account
    the following points:

    {ul
    {- The current implementation is simple, it needs the whole archive
       in-memory for encoding or decoding.}
    {- The current implementation does not preserve the information about
       the order of files in the ZIP archive and generally writes members
       in the lexicographic order of their path save for the first one
       which can be specified with the optional argument [first] in
       {!Zipc.to_binary_string} and defaults to ["mimetype"]. This
       supports the {{:https://www.w3.org/TR/epub-33/#sec-zip-container-mime}
       EPUB OCF ZIP container} constraint which is the only format we are
       aware of that mandates an ordering in ZIP archives.
       A more general scheme (e.g. a  [Zipc.Member.order] property)
       could be devised would that be needed.}
    {- It handles only deflate and stored (no compression)
       compression formats. It has decent performance but if you find yourself
       limited by it or need other formats, third-party compression libraries
       can be easily integrated.}
    {- It is possible to rewrite an archive without touching or decompressing
       some of its members, however some metadata like comment fields may be
       lost in the process. See also {!of_binary_string}.}
    {- For now it does not handle ZIP64. ZIP64 is
       needed if your ZIP archive or decompressed file sizes exceed
       4Go (2{^32}-1 bytes) or if you need more than 65535 archive
       members.}
    {- It does not handle encrypted ZIP archives. Most standards
       avoid this anyways.}
    {- It does not handle multipart archives. Most standards avoid
       this anyways.}
    {- On 32-bit platforms one is severly limited by {!Sys.max_string_size}.}
    {- Compressed and decompressed sizes are [uint32] values in Zip archives
       but are represented by an OCaml [int] in [Zipc]. This is not
       a problem on [64-bit] platforms but can be in on 32-bit platforms
       and [js_of_ocaml] where {!Int.max_int} is respectively
       2{^30}-1 and 2{^31}-1. See {!Zipc.File.max_size} for more
       information.}} *)
