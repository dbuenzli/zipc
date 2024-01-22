(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The write up at https://www.hanshq.net/zip.html helps understanding
   the bits here. *)

(* Preliminaries *)

let ( let* ) = Result.bind
let strf fmt = Format.asprintf fmt
let failwithf fmt = Printf.ksprintf failwith fmt
let invalid_argf fmt = Printf.ksprintf invalid_arg fmt
let default_length ?len s start = match len with
| None -> String.length s - start | Some size -> size

let uint32_max_or_max_int = match Int32.unsigned_to_int 0xFFFFFFFFl with
| None -> Int.max_int | Some max -> max

(* Archive members *)

type compression = Bzip2 | Deflate | Lzma | Stored | Xz | Zstd | Other of int

let compression_of_int = function
| 0 -> Stored | 8 -> Deflate | 12 -> Bzip2 | 14 -> Lzma | 93 -> Zstd
| 95 -> Xz | c -> Other c

let compression_to_int = function
| Stored -> 0 | Deflate -> 8 | Bzip2 -> 12 | Lzma -> 14 | Zstd -> 93
| Xz -> 95 | Other c -> c

let compression_to_string = function
| Bzip2 -> "bz2" | Deflate -> "defl" | Lzma -> "lzma" | Stored -> "none"
| Xz -> "xz" | Zstd -> "zst" | Other i -> Format.sprintf "%04d" i

let pp_compression ppf c = Format.pp_print_string ppf (compression_to_string c)

module Fpath = struct
  type t = string
  let ensure_unix p = String.map (function '\\' -> '/' | c -> c) p
  let ensure_directoryness = function
  | "" -> "./"
  | p when p.[String.length p - 1] = '/' -> p
  | p -> p ^ "/"

  let sanitize p =
    let keep_seg = function "" | ".." | "." -> false | _ -> true in
    let segs = String.split_on_char '/' p in
    let segs = List.concat_map (String.split_on_char '\\') segs in
    String.concat "/" (List.filter keep_seg segs)

  type mode = int
  let pp_mode ppf m =
    let pp_entity ppf m =
      let r = if (m land 0o4 <> 0) then 'r' else '-' in
      let w = if (m land 0o2 <> 0) then 'w' else '-' in
      let x = if (m land 0o1 <> 0) then 'x' else '-' in
      Format.fprintf ppf "%c%c%c" r w x
    in
    pp_entity ppf (m lsr 6); pp_entity ppf (m lsr 3); pp_entity ppf m
end

module Ptime = struct
  type t = int
  let jd_posix_epoch = 2_440_588 (* the Julian day of the POSIX epoch *)
  let to_date_time ptime_s =
    let jd = (ptime_s / 86400) + jd_posix_epoch in
    let jd_rem = ptime_s mod 86400 in
    let hh = jd_rem / 3600 in
    let hh_rem = jd_rem mod 3600 in
    let mm = hh_rem / 60 in
    let ss = hh_rem mod 60 in
    let date = (* Date of julian day cf. ptime *)
      let a = jd + 32044 in
      let b = (4 * a + 3) / 146097 in
      let c = a - ((146097 * b) / 4) in
      let d = (4 * c + 3) / 1461 in
      let e = c - ((1461 * d) / 4) in
      let m = (5 * e + 2) / 153 in
      let day = e - ((153 * m + 2) / 5) + 1 in
      let month = m + 3 - (12 * (m / 10)) in
      let year = 100 * b + d - 4800 + (m / 10) in
      (year, month, day)
    in
    date, (hh, mm, ss)

  let pp ppf ptime = (* Like RFC3339 without the T separator *)
    let (year, month, day), (hh, mm, ss) = to_date_time ptime in
    Format.fprintf ppf "%04d-%02d-%02d %02d:%02d:%02dZ" year month day hh mm ss

  (* MS-DOS date time https://www.ctyme.com/intr/rb-2992.htm#table1665 *)

  let dos_epoch = 315532800 (* in POSIX time. *)

  let of_dos_date_time ~dos_date ~dos_time  =
    if dos_date < 0x21 (* 1980-01-01 *) then dos_epoch else
    let hh = (dos_time lsr 11) in
    let mm = (dos_time lsr 5) land 0x3F in
    let ss = (dos_time land 0x1F) * 2 in
    let jd = (* Julian day of date *)
      let year = ((dos_date lsr 9) land 0x7F) + 1980 in
      let month = ((dos_date lsr 5) land 0xF) in
      let day = (dos_date land 0x1F) in
      (* Cf. ptime *)
      let a = (14 - month) / 12 in
      let y = year + 4800 - a in
      let m = month + 12 * a - 3 in
      day + ((153 * m) + 2) / 5 + 365 * y +
      (y / 4) - (y / 100) + (y / 400) - 32045
    in
    let d = jd - jd_posix_epoch in
    d * 86400 + hh * 3600 + mm * 60 + ss

  let to_dos_date_time ptime_s =
    let ((y, _, _), _ as date_time) = to_date_time ptime_s in
    let (year, month, day), (hh, mm, ss) =
      if y < 1980 then (1980, 01, 01), (00, 00, 00) else
      if y > 2107 then (2107, 12, 31), (23, 59, 59) else
      date_time
    in
    let dos_date = day lor (month lsl 5) lor ((year - 1980) lsl 9) in
    let dos_time = (ss / 2) lor (mm lsl 5) lor (hh lsl 11) in
    dos_date, dos_time
end

module File = struct
  let err_format c = strf "Compression %a not supported" pp_compression c
  let err_encrypted = "Encrypted file not supported"
  let err_size compressed_size decompressed_size =
    strf "Maximum ZIP byte size 4294967295 exceeded by compressed \
          (%d) or decompressed (%d) file size"
      compressed_size decompressed_size

  let check_non_negative name len =
    if len < 0 then invalid_argf "%s is negative (%d)" name len

  let max_size = uint32_max_or_max_int
  let gp_is_encrypted = 0x1
  let gp_utf_8 = 0x800
  let gp_default = gp_utf_8
  let version_made_by_default = (3 (* UNIX *) lsl 8) lor 20 (* PKZIP 2.0 *)
  let version_needed_to_extract_default = 20 (* PKZIP 2.0 *)

  type t =
    { version_made_by : Zipc_deflate.uint16;
      version_needed_to_extract : Zipc_deflate.uint16;
      gp_flags : Zipc_deflate.uint16;
      compression : compression; (* in [compressed_bytes] buffer. *)
      start : int; (* in [compressed_bytes] buffer. *)
      compressed_size : int; (* in [compressed_bytes] in buffer. *)
      compressed_bytes : string;
      decompressed_size : int;
      decompressed_crc_32 : Zipc_deflate.Crc_32.t }

  let make
      ?(version_made_by = version_made_by_default)
      ?(version_needed_to_extract = version_needed_to_extract_default)
      ?(gp_flags = gp_default) ?(start = 0) ?compressed_size:len
      ~compression compressed_bytes ~decompressed_size ~decompressed_crc_32
    =
    let compressed_size = default_length ?len compressed_bytes start in
    check_non_negative "compressed_size" compressed_size;
    check_non_negative "decompressed_size" decompressed_size;
    if compressed_size > max_size || decompressed_size > max_size
    then Error (err_size compressed_size decompressed_size) else
    Ok { version_made_by; version_needed_to_extract; gp_flags;
         compression; start; compressed_size; compressed_bytes;
         decompressed_size; decompressed_crc_32; }

  let stored_of_binary_string ?(start = 0) ?len s =
    let compression = Stored in
    let decompressed_size = default_length ?len s start in
    let decompressed_crc_32 = Zipc_deflate.Crc_32.string ~start ?len s in
    let compressed_size = decompressed_size in
    make ~start ~compressed_size ~compression s ~decompressed_size
      ~decompressed_crc_32

  let deflate_of_binary_string ?level ?(start = 0) ?len s =
    let compression = Deflate in
    let decompressed_size = default_length ?len s start in
    let* decompressed_crc_32, cs =
      Zipc_deflate.crc_32_and_deflate ?level ~start ?len s
    in
    make ~compression cs ~decompressed_size ~decompressed_crc_32

  let version_made_by file = file.version_made_by
  let version_needed_to_extract file = file.version_needed_to_extract
  let gp_flags file = file.gp_flags
  let compression file = file.compression
  let start file = file.start
  let compressed_size file = file.compressed_size
  let compressed_bytes file = file.compressed_bytes
  let decompressed_size file = file.decompressed_size
  let decompressed_crc_32 file = file.decompressed_crc_32

  let compressed_bytes_to_binary_string file =
    String.sub file.compressed_bytes file.start file.compressed_size

  let is_encrypted file = file.gp_flags land gp_is_encrypted <> 0
  let can_extract file =
    not (is_encrypted file) && match file.compression with
    | Stored | Deflate -> true | _ -> false

  let to_binary_string_no_crc_check file =
    if is_encrypted file then Error err_encrypted else
    match file.compression with
    | Stored ->
        let s = compressed_bytes_to_binary_string file in
        Ok (s, Zipc_deflate.Crc_32.string s)
    | Deflate ->
        let cs = file.compressed_bytes in
        let start = file.start and len = file.compressed_size in
        let decompressed_size = file.decompressed_size in
        Result.map_error (strf "deflate: %s") @@
        Zipc_deflate.inflate_and_crc_32 cs ~start ~len ~decompressed_size
    | c -> Error (err_format c)

  let to_binary_string file = match to_binary_string_no_crc_check file with
  | Error _ as e -> e
  | Ok (s, found) ->
      let expect = file.decompressed_crc_32 in
      match Zipc_deflate.Crc_32.check ~expect ~found with
      | Error _ as e -> e
      | Ok () -> Ok s
end

module Member = struct
  let max = 0xFFFF
  let max_path_length = 0xFFFF
  let err_count count =
    strf "Maximum ZIP member count %d exceeded (%d)" max count

  let err_path_len len =
    strf "Maximum ZIP path length %d exceeded (%d)" max_path_length len

  type kind = Dir | File of File.t
  type t =
    { path : Fpath.t;
      kind : kind;
      mode : Fpath.mode;
      mtime : Ptime.t }

  let make ?(mtime = Ptime.dos_epoch) ?mode ~path kind =
    let path = Fpath.ensure_unix path in
    let path = match kind with
    | Dir -> Fpath.ensure_directoryness path | File _ -> path
    in
    let path_len = String.length path in
    if path_len > max_path_length then Error (err_path_len path_len) else
    let mode = match mode with
    | Some m -> m | None -> match kind with Dir -> 0o755 | File _ -> 0o644
    in
    let mtime = if mtime < Ptime.dos_epoch then Ptime.dos_epoch else mtime in
    Ok { path; kind; mode; mtime }

  let path m = m.path
  let kind m = m.kind
  let mode m = m.mode
  let mtime m = m.mtime
  let _pp ~crc ppf m =
    let is_dir = match m.kind with Dir -> 'd' | File _ -> '-' in
    let comp = match m.kind with
    | Dir -> "none"
    | File f -> strf "%4s" (compression_to_string f.compression)
    in
    let encrypted = match m.kind with
    | File f when File.is_encrypted f -> 'X' | _ -> ' '
    in
    let size = match m.kind with Dir -> 0 | File f -> f.decompressed_size in
    let pct = match m.kind with
    | Dir -> "    "
    | File f ->
        let r = (float f.compressed_size) /. (float f.decompressed_size) in
        strf "%3d%%" (Float.to_int (r *. 100.))
    in
    let crc =
      if not crc then "" else match m.kind with
      | Dir -> "        "
      | File f -> strf "%08lx" f.decompressed_crc_32
    in
    Format.fprintf ppf "%c%a %s%c%s %8d %s %a %s"
      is_dir Fpath.pp_mode m.mode comp encrypted crc size pct
      Ptime.pp m.mtime m.path

  let pp = _pp ~crc:false
  let pp_long = _pp ~crc:true
end

let error_zip64 () = failwith "ZIP64 archives are not supported"
let error_multipart () = failwith "Multipart archives are not supported"
let error_eocd () = failwith "Corrupted end of central directory record"
let error_no_eocd () =
  failwith "Likely not a ZIP archive: no end of central directory record found"

let error_short () = failwith "File too short to be a ZIP archive"
let error_truncated_cd () = failwith "Truncated central directory"
let error_corrupted_cdfh () = failwith "Corrupted central directory file header"
let error_corrupted_lfh () = failwith "Corrupted local file header"

module String_map = Map.Make (String)
type t = Member.t String_map.t
let empty = String_map.empty
let is_empty = String_map.is_empty
let mem p z = String_map.mem p z
let find p z = String_map.find_opt p z
let fold f z acc = String_map.fold (fun _ m acc -> f m acc) z acc
let add m z = String_map.add m.Member.path m z
let remove p z = String_map.remove p z
let member_count z = String_map.cardinal z
let to_string_map = Fun.id
let of_string_map = Fun.id

(* Decoding

   The spec is https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
   but https://en.wikipedia.org/wiki/ZIP_(file_format) has tables with
   offsets which makes it easier to understand the indexes below.  *)

let get_uint32_le_as_int s i =
  let u = String.get_int32_le s i in
  match Int32.unsigned_to_int u with
  | Some u -> u
  | None -> failwithf "Cannot convert 32-bit %lu to OCaml int" u

let lfh_sig = 0x04034b50l
let lfh_min_size = 30
let decode_data_start_of_lfh s i ~compressed_size =
  let len = String.length s in
  if i + lfh_min_size > len || String.get_int32_le s i <> lfh_sig
  then error_corrupted_lfh () else
  let path_len = String.get_uint16_le s (i + 26) in
  let extra_len = String.get_uint16_le s (i + 28) in
  let data_start = i + lfh_min_size + path_len + extra_len in
  if data_start + compressed_size > len then error_corrupted_lfh () else
  data_start

let get_lfh_crc_32 s ~start_local =
  (* assert decode_data_start_of_lfh has been called *)
  String.get_int32_le s (start_local + 14)

let cdfh_sig = 0x02014b50l
let cdfh_min_size = 46
let decode_member_of_cd s cd_max i =
  if i + cdfh_min_size - 1 > cd_max || String.get_int32_le s i <> cdfh_sig
  then error_corrupted_cdfh () else
  let path_len = String.get_uint16_le s (i + 28) in
  let next_member_idx =
    let extra_len = String.get_uint16_le s (i + 30) in
    let comment_len = String.get_uint16_le s (i + 32) in
    let n = i + cdfh_min_size + path_len + extra_len + comment_len in
    if n - 1 > cd_max then error_corrupted_cdfh () else n
  in
  let path = String.sub s (i + 46) path_len in
  let mtime =
    let dos_time = String.get_uint16_le s (i + 12) in
    let dos_date = String.get_uint16_le s (i + 14) in
    Ptime.of_dos_date_time ~dos_date ~dos_time
  in
  let is_dir, mode =
    let ext_atts_16_hi = String.get_uint16_le s (i + 40) in
    if ext_atts_16_hi <> 0 then (* Unix permissions *)
      (ext_atts_16_hi land 0o70000) = 0o40000,
      (ext_atts_16_hi land 0o07777)
    else (* check for MS-DOS directory bit *)
    let ext_atts_8_lo = String.get_uint8 s (i + 38) in
    if ext_atts_8_lo land 0x10 <> 0
    then (true, 0o755) else (false, 0o644)
  in
  let kind =
    if is_dir then Member.Dir else
    let compression = compression_of_int (String.get_uint16_le s (i + 10)) in
    let version_made_by           = String.get_uint16_le s (i +  4) in
    let version_needed_to_extract = String.get_uint16_le s (i +  6) in
    let gp_flags                  = String.get_uint16_le s (i +  8) in
    let compressed_size           = get_uint32_le_as_int s (i + 20) in
    let decompressed_size         = get_uint32_le_as_int s (i + 24) in
    let decompressed_crc_32       = String.get_int32_le  s (i + 16) in
    let start_local               = get_uint32_le_as_int s (i + 42) in
    if start_local >= String.length s then error_corrupted_cdfh () else
    let start = decode_data_start_of_lfh s start_local ~compressed_size in
    let decompressed_crc_32 =
      if decompressed_crc_32 = 0l
      then get_lfh_crc_32 s ~start_local else decompressed_crc_32
    in
    File { version_made_by; version_needed_to_extract; gp_flags;
           compression; compressed_bytes = s; start; compressed_size;
           decompressed_size; decompressed_crc_32 }
  in
  next_member_idx, { Member.path; mtime; mode; kind }

let rec decode_cd_members z count s cd_max i =
  if count = 0 then z else
  if i > cd_max then error_truncated_cd () else
  let i, member = decode_member_of_cd s cd_max i in
  decode_cd_members (add member z) (count - 1) s cd_max i

let eocd_sig = 0x06054b50l
let eocd_min_size = 22
let find_cd_info_in_eocd s =
  let info_of_eocd s i =
    let disk_num             = String.get_uint16_le s (i + 4) in
    let disk_num_of_cd_start = String.get_uint16_le s (i + 6) in
    if disk_num = 0xFFFF then error_zip64 () else
    if disk_num <> 0 then error_multipart () else
    if disk_num_of_cd_start <> 0 then error_multipart () else
    let cd_member_count      = String.get_uint16_le s (i + 10) in
    let cd_size              = get_uint32_le_as_int s (i + 12) in
    let cd_start             = get_uint32_le_as_int s (i + 16) in
    if cd_start + cd_size > String.length s then error_eocd () else
    cd_start, cd_size, cd_member_count
  in
  (* eocd ends with a variable length comment, we need to search to find it.
     Good luck if the comment contains the sig… *)
  let rec loop min_start start =
    if start < min_start || start < 0 then error_no_eocd () else
    if String.get_int32_le s start = eocd_sig
    then info_of_eocd s start
    else loop min_start (start - 1)
  in
  let len = String.length s in
  let max_comment_size = 65535 in
  let min_start = len - max_comment_size - eocd_min_size in
  let start = len - eocd_min_size in
  if start < 0 then error_short () else loop min_start start

let string_has_magic s =
  if String.length s < 4 then false else
  let m = String.get_int32_le s 0 in
  Int32.equal m lfh_sig || Int32.equal m eocd_sig

let of_binary_string s =
  try
    let cd_start, cd_size, count = find_cd_info_in_eocd s in
    let cd_max = cd_start + cd_size - 1 in
    Ok (decode_cd_members empty count s cd_max cd_start)
  with
  | Failure e -> Error e

(* Encoding *)

let cleaned_gp_flags file =
  (* Bit 3 indicates presence of data descriptors which we never encode.
     So we make sure the bit is cleared. *)
  File.gp_flags file land (lnot (1 lsl 3))

let encoding_size z =
  let add_member m acc =
    let path_len = String.length (Member.path m) in
    let data_size = match Member.kind m with
    | Dir -> 0 | File f -> File.compressed_size f
    in
    acc + lfh_min_size + path_len + data_size + cdfh_min_size + path_len
  in
  fold add_member z eocd_min_size

let encode_member b m (start, acc) =
  let encode_dir_lfh b start =
    Bytes.set_uint16_le b (start +  4) File.version_needed_to_extract_default;
    Bytes.set_uint16_le b (start +  6) File.gp_default;
    Bytes.set_uint16_le b (start +  8) (compression_to_int Stored);
    Bytes.set_int32_le  b (start + 14) 0l (* CRC *);
    Bytes.set_int32_le  b (start + 18) 0l (* Compressed size *);
    Bytes.set_int32_le  b (start + 22) 0l (* Uncompressed size *)
  in
  let encode_file_lfh b file start =
    Bytes.set_uint16_le b (start +  4) (File.version_needed_to_extract file);
    Bytes.set_uint16_le b (start +  6) (cleaned_gp_flags file);
    Bytes.set_uint16_le b (start +  8)
      (compression_to_int (File.compression file));
    Bytes.set_int32_le  b (start + 14) (File.decompressed_crc_32 file);
    Bytes.set_int32_le  b (start + 18)
      (Int32.of_int (File.compressed_size file));
    Bytes.set_int32_le  b (start + 22)
      (Int32.of_int (File.decompressed_size file))
  in
  let path = Member.path m in
  let path_length = String.length path in
  let date, time = Ptime.to_dos_date_time (Member.mtime m) in
  Bytes.set_int32_le b  (start     ) lfh_sig;
  Bytes.set_uint16_le b (start + 10) time;
  Bytes.set_uint16_le b (start + 12) date;
  Bytes.set_uint16_le b (start + 26) path_length;
  Bytes.set_uint16_le b (start + 28) 0; (* Extra field length *)
  Bytes.blit_string path 0 b (start + 30) path_length;
  let next = match Member.kind m with
  | Dir -> encode_dir_lfh b start; start + 30 + path_length
  | File file ->
      encode_file_lfh b file start;
      let start = start + 30 + path_length in
      let src = File.compressed_bytes file and src_start = File.start file in
      let size = File.compressed_size file in
      Bytes.blit_string src src_start b start size;
      start + size
  in
  (next, (start, m) :: acc)

let encode_cd_member b start (lfh_offset, m) =
  let encode_dir_cd_member b start =
    Bytes.set_uint16_le b (start +  4) File.version_made_by_default;
    Bytes.set_uint16_le b (start +  6) File.version_needed_to_extract_default;
    Bytes.set_uint16_le b (start +  8) File.gp_default;
    Bytes.set_uint16_le b (start + 10) (compression_to_int Stored);
    Bytes.set_int32_le  b (start + 16) 0l (* CRC *);
    Bytes.set_int32_le  b (start + 20) 0l (* Compressed size *);
    Bytes.set_int32_le  b (start + 24) 0l (* Uncompressed size *)
  in
  let encode_file_cd_member b file start =
    Bytes.set_uint16_le b (start +  4) (File.version_made_by file);
    Bytes.set_uint16_le b (start +  6) (File.version_needed_to_extract file);
    Bytes.set_uint16_le b (start +  8) (cleaned_gp_flags file);
    Bytes.set_uint16_le b (start + 10)
      (compression_to_int (File.compression file));
    Bytes.set_int32_le  b (start + 16) (File.decompressed_crc_32 file);
    Bytes.set_int32_le  b (start + 20)
      (Int32.of_int (File.compressed_size file));
    Bytes.set_int32_le  b (start + 24)
      (Int32.of_int (File.decompressed_size file))
  in
  let path = Member.path m in
  let path_length = String.length path in
  let date, time = Ptime.to_dos_date_time (Member.mtime m) in
  let extatts_hi, extatts_lo =
    let kind_hi, kind_lo = match Member.kind m with
    | Dir -> 0o040000, 0x10 (* MS-DOS dir bit *) | File _ -> 0o100000, 0
    in
    kind_hi lor (Member.mode m land 0o7777), kind_lo
  in
  Bytes.set_int32_le b  (start     ) cdfh_sig;
  Bytes.set_uint16_le b (start + 12) time;
  Bytes.set_uint16_le b (start + 14) date;
  Bytes.set_uint16_le b (start + 28) path_length;
  Bytes.set_uint16_le b (start + 30) 0; (* Extra field length *)
  Bytes.set_uint16_le b (start + 32) 0; (* File comment length *)
  Bytes.set_uint16_le b (start + 34) 0; (* Disk number start *)
  Bytes.set_uint16_le b (start + 36) 0; (* Internal file attributes *)
  Bytes.set_uint16_le b (start + 38) extatts_lo;
  Bytes.set_uint16_le b (start + 40) extatts_hi;
  Bytes.set_int32_le b  (start + 42) (Int32.of_int lfh_offset);
  Bytes.blit_string path 0 b (start + 46) path_length;
  begin match Member.kind m with
  | Dir -> encode_dir_cd_member b start
  | File file -> encode_file_cd_member b file start
  end;
  start + 46 + path_length

let err_uint32 kind n =
  Error (strf "Maximum ZIP %s 4294967295 exceeded (%d)" kind n)

let err_cd_start n = err_uint32 "central directory offset" n
let err_cd_size n = err_uint32 "central directory size" n

let encode_eocd b start ~member_count ~cd_start ~cd_size =
  if cd_start > uint32_max_or_max_int then err_cd_start cd_start else
  if cd_size > uint32_max_or_max_int then err_cd_size cd_size else
  begin
    Bytes.set_int32_le  b (start     ) eocd_sig;
    Bytes.set_uint16_le b (start +  4) 0; (* Number of this disk *)
    Bytes.set_uint16_le b (start +  6) 0; (* Disk where cd starts *)
    Bytes.set_uint16_le b (start +  8) member_count;
    Bytes.set_uint16_le b (start + 10) member_count;
    Bytes.set_int32_le  b (start + 12) (Int32.of_int cd_size);
    Bytes.set_int32_le  b (start + 16) (Int32.of_int cd_start);
    Bytes.set_uint16_le b (start + 20) 0; (* Comment length *)
    Ok ()
  end

let default_first = "mimetype"

let write_bytes ?(first = default_first) z ?(start = 0) b =
  if is_empty z
  then encode_eocd b start ~member_count:0 ~cd_start:0 ~cd_size:0 else
  let count = member_count z in
  if count > Member.max then Error (Member.err_count count) else
  let cd_start, ms = match String_map.find_opt first z with
  | None ->  fold (encode_member b) z (start, [])
  | Some m ->
      let acc = encode_member b m (start, []) in
      fold (encode_member b) (remove first z) acc
  in
  let eocd_start = List.fold_left (encode_cd_member b) cd_start (List.rev ms) in
  let cd_size = eocd_start - cd_start in
  encode_eocd b eocd_start ~member_count:count ~cd_start ~cd_size

let to_binary_string ?first z =
  let b = Bytes.create (encoding_size z) in
  let* () = write_bytes ?first z b in
  Ok (Bytes.unsafe_to_string b)
