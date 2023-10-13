(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Note, when OCaml 5 is required, archive member encoding/decoding is
   trivially parallelizable. Hopefully at that point the stdlib will have
   some kind of usable ambient parallel work queue. *)

let ( let* ) = Result.bind
let result_error_to_failure = function Ok v -> v | Error e -> failwith e
let strf = Format.asprintf
let failwithf fmt = Format.kasprintf failwith fmt

module String_set = Set.Make (String)
module String_map = Map.Make (String)

(* Cut the B0_std.Os dep, from which most of this ported. *)

module Os = struct
  let uerror = Unix.error_message

  let rec set_mtime p t = try Ok (Unix.utimes p (float t) (float t)) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> set_mtime p t
  | Unix.Unix_error (e, _, _) -> Error (strf "%s: %s" p (uerror e))

  let rec chmod p mode = try Ok (Unix.chmod p mode) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> chmod p mode
  | Unix.Unix_error (e, _, _) -> Error (strf "%s: %s" p (uerror e))

  let rec path_exists p = try (ignore (Unix.stat p); Ok true) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok false
  | Unix.Unix_error (Unix.ENOTDIR, _, _) -> Ok false
  | Unix.Unix_error (Unix.EINTR, _, _) -> path_exists p
  | Unix.Unix_error (e, _, _) -> Error (strf "%s: %s" p (uerror e))

  let rec stat p = try Ok (Unix.stat p) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> stat p
  | Unix.Unix_error (e, _, _) -> Error (strf "%s: %s" p (uerror e))

  let rec is_dir p = try Ok ((Unix.stat p).Unix.st_kind = Unix.S_DIR) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> is_dir p
  | Unix.Unix_error (e, _, _) -> Error (strf "%s: %s" p (uerror e))

  let strf = Printf.sprintf
  let ferr file e = Error (strf "%s: %s" file e)
  let err_doing doing e = strf "%s: %s" doing e

  let rec _is_dir p = try (Unix.stat p).Unix.st_kind = Unix.S_DIR with
  | Unix.Unix_error (Unix.EINTR, _, _) -> _is_dir p

  let mkdir ?(mode = 0o755) ~make_path dir =
    (* returns [false] if [dir] existed. *)
    let create_op = "Creating" in
    let mkdir dir mode = Unix.mkdir dir mode in
    try
      let pmode = 0o755 in
      try Ok (mkdir dir mode; true) with
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          if _is_dir dir then Ok false else
          ferr dir (err_doing create_op "Path exists but not a directory")
      | Unix.Unix_error (Unix.ENOENT, _, _) when make_path ->
          let rec down = function
          | [] -> assert false
          | [dir] ->
              (try Ok (mkdir dir mode; true) with
              | Unix.Unix_error (Unix.EEXIST, _, _) -> Ok false)
          | dir :: dirs ->
              match mkdir dir pmode with
              | () -> down dirs
              | exception Unix.Unix_error (Unix.EEXIST, _, _) -> down dirs
          in
          let rec up todo p = match Unix.mkdir p pmode with
          | () -> down todo
          | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
              up (p :: todo) (Filename.dirname p)
          in
          up [dir] (Filename.dirname dir)
    with
    | Unix.Unix_error (e, _, p) ->
        match String.equal dir p with
        | true -> ferr dir (err_doing create_op (uerror e))
        | false ->
            let perr = strf "%s: %s" p (uerror e) in
            ferr dir (err_doing create_op perr)

  let rec _stat p = try (Unix.stat p) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> _stat p

  let rec _lstat p = try (Unix.lstat p) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> _lstat p

  let rec readdir ~dotfiles dir =
    let is_dot_file s = String.length s <> 0 && s.[0] = '.' in
    let rec loop ~dotfiles dir dh acc = match Unix.readdir dh with
    | exception End_of_file -> acc
    | ".." | "." -> loop ~dotfiles dir dh acc
    | n when is_dot_file n && not dotfiles -> loop ~dotfiles dir dh acc
    | n -> loop ~dotfiles dir dh (n :: acc)
    in
    let dh = Unix.opendir dir in
    match loop ~dotfiles dir dh [] with
    | fs -> Unix.closedir dh; fs
    | exception e ->
        (try Unix.closedir dh with Unix.Unix_error (_, _, _) -> ());
        raise e

  let fold_no_rec ~filter ~rel ~dotfiles ~follow_symlinks dir f acc =
    let rec loop stat f acc adir = function
    | [] -> Ok acc
    | n :: ns ->
        let full = Filename.concat adir n in
        match stat full with
        | st ->
            begin match st.Unix.st_kind with
            | Unix.S_DIR ->
                if filter = `Non_dir then loop stat f acc adir ns else
                let p = if rel then n else full in
                loop stat f (f st n p acc) adir ns
            | _ when filter <> `Dir ->
                let p = if rel then n else full in
                loop stat f (f st n p acc) adir ns
            | _ ->
                loop stat f acc adir ns
            end
        | exception Unix.Unix_error ((ENOENT|EBADF|ENOTDIR|EPERM), _, _) ->
            loop stat f acc adir ns
    in
    let _stat = if follow_symlinks then _stat else _lstat in
    loop _stat f acc dir (readdir ~dotfiles dir)

  let fold_rec ~filter ~rel ~dotfiles ~follow_symlinks ~prune dir f acc =
    let rec loop stat todo adir rdir f acc = function
    | [] ->
        begin match todo with
        | (dir, rdir, ns) :: todo -> loop stat todo dir rdir f acc ns
        | [] -> Ok acc
        end
    | n :: ns ->
        let full = Filename.concat adir n in
        begin match stat full with
        | st ->
            begin match st.Unix.st_kind with
            | Unix.S_DIR ->
                let rp = match rdir with
                | None -> n | Some rdir -> Filename.concat rdir n
                in
                let p = if not rel then full else rp in
                if prune st n p acc
                then loop stat todo adir rdir f acc ns else
                let acc = if filter = `Non_dir then acc else f st n p acc in
                let todo = (adir, rdir, ns) :: todo in
                loop stat todo full (Some rp) f acc (readdir ~dotfiles full)
            | _ when filter <> `Dir ->
                let p = if not rel then full else match rdir with
                  | None -> n | Some rdir -> Filename.concat rdir n
                in
                loop stat todo adir rdir f (f st n p acc) ns
            | _ ->
                loop stat todo adir rdir f acc ns
            end
        | exception Unix.Unix_error ((ENOENT|ENOTDIR|EBADF|EPERM), _, _) ->
            loop stat todo adir rdir f acc ns
        end
    in
    let stat = if follow_symlinks then _stat else _lstat in
    loop stat [] dir None f acc (readdir ~dotfiles dir)

  let _fold
      ~(filter : [`Any | `Non_dir | `Dir]) ?(rel = false) ?(dotfiles = false)
      ?(follow_symlinks = true) ?(prune = fun _ _ _ _ -> false) ~recurse
      f dir acc
    =
    let listing_op = "Listing" in
    try
      if recurse
      then fold_rec ~filter ~rel ~dotfiles ~follow_symlinks ~prune dir f acc
      else fold_no_rec ~filter ~rel ~dotfiles ~follow_symlinks dir f acc
    with
    | Failure e -> ferr dir (err_doing listing_op e)
    | Unix.Unix_error (e, _, ep) ->
        if String.equal dir ep
        then ferr dir (err_doing listing_op @@ uerror e)
        else ferr dir (err_doing listing_op @@ strf "%s: %s" ep (uerror e))

  let dir_fold ?rel ?dotfiles ?follow_symlinks ?prune ~recurse f dir acc =
    _fold ~filter:`Any ?rel ?dotfiles ?follow_symlinks ?prune ~recurse
      f dir acc

  let dir_prune_denied _ _ p _ =
    try (Unix.access p Unix.[R_OK; X_OK]; false) with
    | Unix.Unix_error ((EACCES|EPERM), _, _) -> true
end

let file_error file msg = Printf.sprintf "%s: %s" file msg

let read_file file =
  let read file ic = try Ok (In_channel.input_all ic) with
  | Sys_error e -> Error (file_error file e)
  in
  let binary_stdin () = In_channel.set_binary_mode In_channel.stdin true in
  try match file with
  | "-" -> binary_stdin (); read file In_channel.stdin
  | file -> In_channel.with_open_bin file (read file)
  with Sys_error e -> Error e

let write_file file s =
  let write file s oc = try Ok (Out_channel.output_string oc s) with
  | Sys_error e -> Error (file_error file e)
  in
  let binary_stdout () = Out_channel.set_binary_mode Out_channel.stdout true in
  try match file with
  | "-" -> binary_stdout (); write file s Out_channel.stdout
  | file -> Out_channel.with_open_bin file (write file s)
  with Sys_error e -> Error e

let read_magic file =
  let read4 file ic =
    try Ok (Option.value ~default:"" (In_channel.really_input_string ic 4)) with
    | Sys_error e -> Error (file_error file e)
  in
  if file = "-" then read4 file In_channel.stdin else
  try In_channel.with_open_bin file (read4 file) with
  | Sys_error e -> Error e

let write_to_tmp s =
  try
    let tmp = Filename.temp_file "zipc" ".zip" in
    let* () = write_file tmp s in
    Ok tmp
  with Sys_error e -> Error e

(* Basic logging *)

module Log = struct
  let exec = Filename.basename Sys.executable_name
  let isatty = Unix.isatty Unix.stderr
  type verbosity = [ `Quiet | `Normal | `Verbose ]
  let level : verbosity ref = ref `Normal
  let only_stderr = ref false
  let init ~verbosity:v ~only_stderr:e = level := v; only_stderr := e

  let pp_tty ~m ppf s =
    let reset = if isatty then "\027[m" else "" in
    let m = if isatty then "\027[" ^ m else "" in
    Format.fprintf ppf "@<0>%s%s@<0>%s" m s reset

  let pp_error ppf () = pp_tty ~m:"31;1m" ppf "Error"
  let pp_warning ppf () = pp_tty ~m:"33;1m" ppf "Warning"
  let pp_ok ppf () = pp_tty ~m:"32;1m" ppf " OK "
  let pp_fail ppf () = pp_tty ~m:"31;1m" ppf "FAIL"
  let pp_unknown ppf () = pp_tty ~m:"33;1m" ppf " ?? "
  let pp_dashes ppf () =        pp_tty ~m:"33;1m" ppf " -- "
  let pp_code ppf s = pp_tty ~m:"1m" ppf s

  let app fmt =
    if !level = `Quiet then Format.ifprintf Format.std_formatter fmt else
    if !only_stderr then Format.fprintf Format.err_formatter fmt else
    Format.fprintf Format.std_formatter fmt

  let verb fmt =
    if !level <> `Verbose then Format.ifprintf Format.std_formatter fmt else
    if !only_stderr then Format.fprintf Format.err_formatter fmt else
    Format.fprintf Format.err_formatter fmt

  let err fmt =
    if !level = `Quiet then Format.ifprintf Format.err_formatter fmt else
    Format.fprintf Format.err_formatter ("%s: %a: @[" ^^ fmt ^^ "@]@.")
      exec pp_error ()

  let warn fmt =
    if !level = `Quiet then Format.ifprintf Format.err_formatter fmt else
    Format.fprintf Format.err_formatter ("%s: %a: @[" ^^ fmt ^^ "@]@.")
      exec pp_warning ()

  let if_error ~use r = match r with Ok v -> v | Error e -> err "%s" e; use
  let if_error' ~use r = match r with Error e -> err "%s" e; Ok use | v -> v
end

(* Program exits *)

module Exit = struct
  open Cmdliner
  let ok = 0
  let err_path = 1
  let err_corrupted = 2
  let err_unsupported = 3
  let err_no_magic = 4
  let err_some = Cmd.Exit.some_error

  let exits_base = Cmd.Exit.defaults

  let exits_path =
    Cmd.Exit.info err_path
      ~doc:"if a requested path does not exist." ::
    exits_base

  let exits_sniff =
    Cmd.Exit.info err_no_magic
      ~doc:"if a file does not start with a ZIP magic number." ::
    exits_base

  let exits_unzip =
    Cmd.Exit.info err_corrupted
      ~doc:"if an unzipped file is corrupted (checksum failure)." ::
    Cmd.Exit.info err_unsupported
      ~doc:"if a compression format is unsupported." ::
    exits_base

  let exits_all = List.hd exits_path :: List.hd exits_sniff :: exits_unzip
end

(* Selecting archive members *)

let error_no_path archive p = Log.err "%s: %s: No such path" archive p
let select_members ~paths ~includes ~include_suffixes ~excludes z =
  let missing = List.filter (fun p -> not (Zipc.mem p z)) paths in
  match missing with
  | _ :: _ -> Error missing
  | [] ->
    let excluded p by = String.starts_with ~prefix:by p in
    let included p by = String.starts_with ~prefix:by p in
    let included_suff p by = String.ends_with ~suffix:by p in
    let add ~paths:ps ~includes:is ~include_suffixes:iss ~excludes:xs m acc =
      let p = Zipc.Member.path m in
      if (List.mem p ps) ||
         (not (List.exists (excluded p) xs) &&
          (List.exists (included p) is || List.exists (included_suff p) iss))
      then m :: acc
      else acc
    in
    let paths = match paths, includes, include_suffixes, excludes with
    | [], [], [], [] -> List.rev (Zipc.fold List.cons z [])
    | [], [], [], excludes ->
        let includes = [""] (* Select all *) in
        let add = add ~paths:[] ~includes ~include_suffixes:[] ~excludes in
        List.rev (Zipc.fold add z [])
    | paths, includes, include_suffixes, excludes ->
        let add = add ~paths ~includes ~include_suffixes ~excludes in
        List.rev (Zipc.fold add z [])
    in
    Ok paths

(* Compression info *)

let pct ~num ~den ~pad =
  let pct = Float.to_int ((float num /. float den) *. 100.) in
  if pad then strf "%3d%%" pct else strf "%d%%" pct

let member_ratio m ~pad = match Zipc.Member.kind m with
| Zipc.Member.Dir -> ""
| Zipc.Member.File file ->
    let num = Zipc.File.compressed_size file in
    let den = Zipc.File.decompressed_size file in
    pct ~num ~den ~pad

let log_compression_info ~cs ~s =
  let r = pct ~num:(String.length cs) ~den:(String.length s) ~pad:false in
  Log.verb "Compressed size: %a@." Log.pp_code r

(* Compress command *)

let compress format infile outfile level verbosity =
  Log.init ~verbosity ~only_stderr:(outfile = "-");
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* s = read_file infile in
  let* cs =
    Result.map_error (file_error infile) @@
    match format with
    | `Deflate -> Zipc_deflate.deflate ~level s
    | `Zlib -> Result.map snd (Zipc_deflate.zlib_compress ~level s)
  in
  let* () = write_file outfile cs in
  log_compression_info ~cs ~s;
  Ok Exit.ok

(* Crc command *)

let crc kind infile verbosity =
  Log.init ~verbosity ~only_stderr:false;
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* s = read_file infile in
  begin match kind with
  | `Crc_32 -> Zipc_deflate.(Log.app "%a@." Crc_32.pp (Crc_32.string s))
  | `Adler_32 -> Zipc_deflate.(Log.app "%a@." Adler_32.pp (Adler_32.string s))
  end;
  Ok Exit.ok

(* Decompress command *)

let decompress format infile outfile verbosity =
  Log.init ~verbosity ~only_stderr:(outfile = "-");
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* cs = read_file infile in
  let* s =
    Result.map_error (file_error infile) @@
    match format with
    | `Deflate -> Zipc_deflate.inflate cs
    | `Zlib ->
        Result.map_error snd @@
        Result.map fst (Zipc_deflate.zlib_decompress cs)
  in
  let* () = write_file outfile s in
  log_compression_info ~cs ~s;
  Ok Exit.ok

(* List command *)

let list archive select_members output_format verbosity =
  Log.init ~verbosity ~only_stderr:false;
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* s = read_file archive in
  Result.map_error (file_error archive) @@
  let* z = Zipc.of_binary_string s in
  match select_members z with
  | Error miss -> List.iter (error_no_path archive) miss; Ok Exit.err_path
  | Ok mems ->
      let pp_short ppf m = Format.fprintf ppf "%s" (Zipc.Member.path m) in
      let pp_member = match output_format with
      | `Short -> pp_short
      | `Normal -> Zipc.Member.pp
      | `Long -> Zipc.Member.pp_long
      in
      let pp_members = Format.(pp_print_list pp_member) in
      (if mems = [] then () else Log.app "@[<v>%a@]@." pp_members mems);
      Ok Exit.ok

(* Recode command *)

let log_op op m = Log.verb "%a %s@." Log.pp_code op (Zipc.Member.path m)
let log_recode_info op ~old ~new' =
  let o = member_ratio old ~pad:true and n = member_ratio new' ~pad:true in
  let info = if o = "" then "               " else strf "%s (was %s)" n o in
  Log.verb "%a %s %s@." Log.pp_code op info (Zipc.Member.path new')

let err_checksum old new' =
  strf "Recoding changed the checksum from %a to %a (zipc bug)"
    Zipc_deflate.Crc_32.pp old Zipc_deflate.Crc_32.pp new'

let recode_member level c m = match Zipc.Member.kind m with
| Zipc.Member.Dir -> m
| Zipc.Member.File file when not (Zipc.File.can_extract file) -> m
| Zipc.Member.File file ->
    result_error_to_failure @@
    Result.map_error (file_error (Zipc.Member.path m)) @@
    let* data = Zipc.File.to_binary_string file in
    let* file' = match c with
    | Zipc.Stored -> Zipc.File.stored_of_binary_string data
    | Zipc.Deflate -> Zipc.File.deflate_of_binary_string ~level data
    | _ -> assert false
    in
    let old_crc = Zipc.File.decompressed_crc_32 file in
    let new_crc = Zipc.File.decompressed_crc_32 file' in
    if not (Zipc_deflate.Crc_32.equal old_crc new_crc)
    then Error (err_checksum old_crc new_crc) else
    let path = Zipc.Member.path m and mode = Zipc.Member.mode m in
    let mtime = Zipc.Member.mtime m and kind = Zipc.Member.File file' in
    Zipc.Member.make ~mtime ~mode ~path kind

let recode_members level c mems z =
  let recode z old =
    let new' = recode_member level c old in
    log_recode_info "Recode" ~old ~new'; Zipc.add new' z
  in
  List.fold_left recode z mems

let delete_members mems z =
  let delete z m = log_op "Delete" m; Zipc.remove (Zipc.Member.path m) z in
  List.fold_left delete z mems

let keep_only_members recode level mems z =
  let keep z m = match recode with
  | None -> log_op "Keep" m; Zipc.add m z
  | Some c ->
      let new' = recode_member level c m in
      log_recode_info "Keep" ~old:m ~new'; Zipc.add new' z
  in
  List.fold_left keep Zipc.empty mems

let log_recode_success archive ~oldlen recoded =
  let pct = pct ~num:(String.length recoded) ~den:oldlen ~pad:false in
  Log.verb "No errors in %a recode (%s of old size)@."
    Log.pp_code archive pct

let recode_check_with_cmd archive ~oldlen recoded cmd =
  let cmd_err exit = Log.err "%s: check command returned %d" archive exit in
  let is_unzip = String.starts_with ~prefix:"unzip" cmd in
  let* tmpfile = write_to_tmp recoded in
  let* exit = match Sys.command (String.concat " " [cmd; tmpfile]) with
  | exception Sys_error e -> Error e | exit -> Ok exit
  in
  let () = try Unix.unlink tmpfile with Unix.Unix_error _ -> () in
  if exit = 0 ||
     (is_unzip && (exit = 1 (* empty *)|| exit = 82 (* encrypt *)))
  then (log_recode_success archive ~oldlen recoded; Ok Exit.ok)
  else (cmd_err exit; Ok Exit.err_some)

let recode_check_in_memory archive ~oldlen recoded =
  let* z = Zipc.of_binary_string recoded in
  let check m exit = match Zipc.Member.kind m with
  | Zipc.Member.Dir -> exit
  | Zipc.Member.File file when Zipc.File.can_extract file ->
      begin match Zipc.File.to_binary_string file with
      | Ok _ -> exit
      | Error e ->
          Log.verb "%s: %s: %s" archive (Zipc.Member.path m) e;
          Exit.err_corrupted
      end
  | Zipc.Member.File file -> exit
  in
  let exit = Zipc.fold check z Exit.ok in
  if exit <> Exit.ok
  then Log.err "%s: Some recoded archive members had errors" archive
  else (log_recode_success archive ~oldlen recoded);
  Ok exit

let recode_check archive check check_cmd ~oldlen recoded =
  Result.map_error (fun e -> "recode check: " ^ e) @@
  match check_cmd with
  | Some cmd -> recode_check_with_cmd archive ~oldlen recoded cmd
  | None -> recode_check_in_memory archive ~oldlen recoded

let recode_archive z mems recode level = function
| `Recode when recode = None -> z
| `Recode -> recode_members level (Option.get recode) mems z
| `Delete -> delete_members mems z
| `Keep_only -> keep_only_members recode level mems z

let recode
    archive output select_members recode mode level verbosity check check_cmd
  =
  Log.init ~verbosity ~only_stderr:(output = "-" && not check);
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  try
    let* s = read_file archive in
    Result.map_error (file_error archive) @@
    let* z = Zipc.of_binary_string s in
    match select_members z with
    | Error miss -> List.iter (error_no_path archive) miss; Ok Exit.err_path
    | Ok mems ->
        let z = recode_archive z mems recode level mode in
        let oldlen = String.length s in
        let* s = Zipc.to_binary_string z in
        if check
        then recode_check archive check check_cmd ~oldlen s
        else let* () = write_file output s in Ok Exit.ok
  with
  | Failure e -> Error e
  | exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printf.eprintf "%s: Internal error on %s\n%!" Log.exec archive;
      Printexc.raise_with_backtrace exn bt

(* Sniff command *)

let sniff files nul_sep recurse no_follow_symlinks verbosity =
  let follow_symlinks = not no_follow_symlinks in
  let log_path p = if nul_sep then Log.app "%s\x00" p else Log.app "%s@." p in
  let sniff_file exit file =
    let* s = read_magic file in
    if Zipc.string_has_magic s then (log_path file; Ok exit) else
    let exit = if exit = Exit.ok then Exit.err_no_magic else exit in
    Log.err "%s: Not a ZIP archive" file; Ok exit
  in
  let sniff_dir exit dir =
    let sniff stat _ p () = match stat.Unix.st_kind with
    | Unix.S_REG ->
        Log.if_error ~use:() @@
        let* s = read_magic p in
        Ok (if Zipc.string_has_magic s then log_path p else ())
    | _ -> ()
    in
    let* () =
      let dotfiles = true and prune = Some (Os.dir_prune_denied) in
      Os.dir_fold ~dotfiles ~follow_symlinks ?prune ~recurse sniff dir ()
    in
    Ok exit
  in
  let sniff_path exit p =
    Log.if_error ~use:Exit.err_some @@
    let* is_dir = Os.is_dir p in
    if is_dir then sniff_dir exit p else sniff_file exit p
  in
  Log.init ~verbosity ~only_stderr:false;
  List.fold_left sniff_path Exit.ok files

(* Unzip command *)

let log_path m = Log.verb "%s" (Zipc.Member.path m)
let log_err_archive ar = Log.err "%s: Some archive members had errors" ar
let log_err_member m e = Log.err "%s: %s" (Zipc.Member.path m) e
let log_err_dest_exists p = Log.err "%s: Directory exists" p
let log_err_unsupported ar m file =
  let log_err_encrypted ar m =
    Log.err "%s: %s: Cannot decompress encrypted file" ar (Zipc.Member.path m)
  in
  let log_err_format ar m file =
    Log.err "%s: %s: Cannot decompress format %a"
      ar (Zipc.Member.path m) Zipc.pp_compression (Zipc.File.compression file)
  in
  if Zipc.File.is_encrypted file
  then log_err_encrypted ar m
  else log_err_format ar m file

let log_err_absurd_path ar m =
  Log.err "%s: path %a sanitizes to the empty string, not extracted"
    ar Log.pp_code (Zipc.Member.path m)

let log_err_dupe_path ar p =
  Log.err "%s: Two paths sanitize to %a, only one was extracted"
    ar Log.pp_code p

let check_archive archive ~skip mems =
  let log op m msg = Log.verb "[%a] %s%s@." op () (Zipc.Member.path m) msg in
  let check exit m = match Zipc.Member.kind m with
  | Zipc.Member.Dir -> log Log.pp_dashes m ""; exit
  | Zipc.Member.File file when Zipc.File.can_extract file ->
      begin match Zipc.File.to_binary_string file with
      | Ok _ -> log Log.pp_ok m ""; exit
      | Error e -> log Log.pp_fail m (" " ^ e); Exit.err_corrupted
      end
  | Zipc.Member.File file ->
      let comp = strf " %a" Zipc.pp_compression (Zipc.File.compression file) in
      let enc = if Zipc.File.is_encrypted file then " encrypted" else "" in
      log Log.pp_unknown m (enc ^ comp);
      if skip then exit else
      (log_err_unsupported archive m file; Exit.err_unsupported)
  in
  let exit = List.fold_left check Exit.ok mems in
  if exit <> Exit.ok
  then log_err_archive archive
  else (Log.verb "@.No errors in %a@." Log.pp_code archive);
  Ok exit

let unzip_to_stdout archive ~skip ~raw ~dump_errors mems =
  let write_raw exit m file =
    log_path m;
    output_string stdout (Zipc.File.compressed_bytes_to_binary_string file);
    exit
  in
  let unzip exit m = match Zipc.Member.kind m with
  | Zipc.Member.Dir -> exit
  | Zipc.Member.File file when raw -> write_raw exit m file
  | Zipc.Member.File file when Zipc.File.can_extract file && dump_errors ->
      begin match Zipc.File.to_binary_string_no_crc_check file with
      | Error e -> log_err_member m e; Exit.err_corrupted
      | Ok (data, found) ->
          let expect = Zipc.File.decompressed_crc_32 file in
          let exit = match Zipc_deflate.Crc_32.check ~expect ~found with
          | Error e -> log_err_member m e; Exit.err_corrupted
          | Ok () -> exit
          in
          Out_channel.output_string stdout data; exit
      end
  | Zipc.Member.File file when Zipc.File.can_extract file ->
      begin match Zipc.File.to_binary_string file with
      | Ok data -> log_path m; Out_channel.output_string stdout data; exit
      | Error e -> log_err_member m e; Exit.err_corrupted
      end
  | Zipc.Member.File file when skip -> exit
  | Zipc.Member.File file ->
      log_err_unsupported archive m file; write_raw Exit.err_unsupported m file
  in
  Out_channel.set_binary_mode Out_channel.stdout true;
  let exit = List.fold_left unzip Exit.ok mems in
  if exit <> Exit.ok then log_err_archive archive;
  Ok exit

let unzip_members archive ~root ~skip ~raw ~dump_errors mems =
  let set_path_meta exit path m =
    let* () = Os.set_mtime path (Zipc.Member.mtime m) in
    let* () = Os.chmod path (Zipc.Member.mode m) in
    Log.verb "%s@." path; Ok exit
  in
  let mkdir exit dir m =
    Log.if_error ~use:Exit.err_some @@
    let* _created = Os.mkdir ~make_path:true dir in
    set_path_meta exit dir m
  in
  let write_path exit path s m = (* Parent directory of [path] may not exist. *)
    Log.if_error ~use:Exit.err_some @@
    let* _created = Os.mkdir ~make_path:true (Filename.dirname path) in
    let* () = write_file path s in
    set_path_meta exit path m
  in
  let unzip exit ~o:path m = match Zipc.Member.kind m with
  | Zipc.Member.Dir -> mkdir exit path m
  | Zipc.Member.File file when raw ->
      write_path exit path (Zipc.File.compressed_bytes_to_binary_string file) m
  | Zipc.Member.File file when Zipc.File.can_extract file && dump_errors ->
      begin match Zipc.File.to_binary_string_no_crc_check file with
      | Error e -> log_err_member m e; Exit.err_corrupted
      | Ok (data, found) ->
          let expect = Zipc.File.decompressed_crc_32 file in
          let exit = match Zipc_deflate.Crc_32.check ~expect ~found with
          | Error e -> log_err_member m e; Exit.err_corrupted
          | Ok () -> exit
          in
          write_path exit path data m
      end
  | Zipc.Member.File file when Zipc.File.can_extract file ->
      begin match Zipc.File.to_binary_string file with
      | Error e -> log_err_member m e; Exit.err_corrupted
      | Ok data -> write_path exit path data m
      end
  | Zipc.Member.File _ when skip -> exit
  | Zipc.Member.File file ->
      let raw = Zipc.File.compressed_bytes_to_binary_string file in
      log_err_unsupported archive m file;
      write_path Exit.err_unsupported path raw m
  in
  let rec loop exit seen = function
  | [] -> exit
  | m :: ms ->
      match Zipc.Fpath.sanitize (Zipc.Member.path m) with
      | "" -> log_err_absurd_path archive m; loop Exit.err_some seen ms
      | p when String_set.mem p seen ->
          log_err_dupe_path archive p; loop Exit.err_some seen ms
      | p ->
          let exit = unzip exit ~o:(Filename.concat root p) m in
          loop exit (String_set.add p seen) ms
  in
  let exit = loop Exit.ok String_set.empty mems in
  if exit <> Exit.ok then log_err_archive archive;
  Ok exit

let unzip
    archive dst select_members verbosity skip force raw check dump_errors
  =
  Log.init ~verbosity ~only_stderr:(dst = Some "-" && not check);
  Log.if_error ~use:Exit.err_some @@
  let* s = read_file archive in
  Result.map_error (file_error archive) @@
  let* z = Zipc.of_binary_string s in
  match select_members z with
  | Error miss -> List.iter (error_no_path archive) miss; Ok Exit.err_path
  | Ok mems ->
      if check then check_archive archive ~skip mems else
      match dst with
      | Some "-" -> unzip_to_stdout archive ~skip ~raw ~dump_errors mems
      | dst ->
          let root = match dst with
          | None -> Filename.remove_extension archive | Some dst -> dst
          in
          Log.if_error' ~use:Exit.err_some @@
          let* created = Os.mkdir ~make_path:false root in
          if not created && not force
          then (log_err_dest_exists root; Ok Exit.err_some)
          else unzip_members archive ~root ~skip ~raw ~dump_errors mems

(* Zip command *)

let err_no_outfile () =
  strf "Multiple paths to add, need to specify output file with %a"
    Log.pp_code "-o"


let make_file_data ~stored level stat path =
  let stored = List.exists (fun s -> String.ends_with ~suffix:s path) stored in
  result_error_to_failure @@
  let* s = read_file path in
  Result.map_error (file_error path) @@
  let* file = match stored with
  | true -> Zipc.File.stored_of_binary_string s
  | false -> Zipc.File.deflate_of_binary_string ~level s
  in
  Ok (Zipc.Member.File file)

let make_member ~strip ~strip_prefix path stat kind =
  let strip_path ~strip path =
    let slen = String.length strip in
    String.sub path slen (String.length path - slen)
  in
  let mtime = Int.of_float stat.Unix.st_mtime in
  let mode = stat.Unix.st_perm in
  let path =
    let path = strip_path ~strip path in
    match String.starts_with ~prefix:strip_prefix path with
    | true -> strip_path ~strip:strip_prefix path | false -> path
  in
  let path, ratio = match kind with
  | Zipc.Member.Dir ->
      Zipc.Fpath.ensure_directoryness path (* for logging *), "    "
  | Zipc.Member.File f ->
      let num = Zipc.File.compressed_size f in
      let den = Zipc.File.decompressed_size f in
      path, pct ~num ~den ~pad:true
  in
  Log.verb "%a %s %s@." Log.pp_code "Add" ratio path;
  match Zipc.Member.make ~mtime ~mode ~path kind with
  | Ok m -> m | Error e -> failwith (file_error path e)

let make_archive
    ~strip ~strip_prefix ~no_recurse ~no_dirs ~stored level init paths
  =
  let recurse = not no_recurse in
  let add m z =
    let p = Zipc.Member.path m in
    if Zipc.mem p z
    then failwithf "Trying to add two members with path %s" p
    else Zipc.add m z
  in
  let zip_add_dir z stat path =
    let add_path stat _filename path z = match stat.Unix.st_kind with
    | S_DIR when no_dirs -> z
    | S_DIR ->
        let dir = Zipc.Member.Dir in
        add (make_member ~strip ~strip_prefix path stat dir) z
    | _ ->
        let file = make_file_data ~stored level stat path in
        add (make_member ~strip ~strip_prefix path stat file) z
    in
    let z =
      if no_dirs then z else
      add (make_member ~strip ~strip_prefix path stat Dir) z
    in
    Os.dir_fold ~dotfiles:true ~recurse add_path path z
    |> result_error_to_failure
  in
  let add_path z path =
    let stat = Os.stat path |> result_error_to_failure in
    match stat.Unix.st_kind with
    | S_DIR -> zip_add_dir z stat path
    | _ ->
        let file = make_file_data ~stored level stat path in
        add (make_member ~strip ~strip_prefix path stat file) z
  in
  try Ok (List.fold_left add_path init paths) with
  | Failure e -> Error e

let zip
    init paths strip_prefix dst no_recurse no_dirs stored level verbosity
  =
  Log.init ~verbosity ~only_stderr:(dst = Some "-");
  Log.if_error ~use:Exit.err_some @@
  let* init = match init with
  | None -> Ok Zipc.empty
  | Some init ->
      let* s = read_file init in
      Result.map_error (file_error init) (Zipc.of_binary_string s)
  in
  let* dst, strip, paths = match dst, paths with
  | dst, [path] ->
      let prefix = Filename.dirname path in
      let prefix = match prefix with
      | "." | "" -> "" | p -> Zipc.Fpath.ensure_directoryness p
      in
      let dst = match dst with
      | None -> Filename.concat prefix (Filename.basename path ^ ".zip")
      | Some dst -> dst
      in
      Ok (dst, prefix, [path])
  | None, _ -> Error (err_no_outfile ())
  | Some archive, paths -> Ok (archive, "", paths)
  in
  let strip_prefix = match strip_prefix with
  | None | Some "" -> "" | Some p -> Zipc.Fpath.ensure_directoryness p
  in
  let* z =
    make_archive
      ~strip ~strip_prefix ~no_recurse ~no_dirs ~stored level init paths
  in
  let* s = Zipc.to_binary_string z in
  let* () = write_file dst s in
  Ok Exit.ok

(* Comand line interface *)

open Cmdliner

let s_path_sel = "OPTIONS FOR PATH SELECTION"
let docs = s_path_sel

let paths =
  let doc =
    "Select archive member $(docv). An explicit $(docv) missing from the \
     archive reports an error and leads to a non-zero exit code.\n\n\
     Paths from the archive matching $(b,--include) or $(b,--include-suffix) \
     are added to the set of paths to select. If no $(i,PATH) is specified \
     all of them is implied or just the ones matching $(b,--include) and \
     $(b,--include-suffix) options. \n\n\
     The set of all paths or the one defined by $(b,--include) and \
     $(b,--include-suffix) matches is pruned by $(b,--exclude) matches."
  in
  Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"PATH")

let includes =
  let doc = "Include paths that have $(docv) as a prefix. Repeatable." in
  let docv = "PREFIX" in
  Arg.(value & opt_all string [] & info ["i"; "include"] ~doc ~docs ~docv)

let include_suffixes =
  let doc = "Include paths that have $(docv) as a suffix. Repeatable." in
  let docv = "SUFFIX" and opts = ["S"; "include-suffix"] in
  Arg.(value & opt_all string [] & info opts ~doc ~docs ~docv)

let excludes =
  let doc =
    "Exclude path that have $(docv) as a prefix of the set of all paths \
     or of the one defined by $(b,--include) and $(b,--include-suffix). \
     Repeatable."
  in
  let docv = "PREFIX" in
  Arg.(value & opt_all string [] & info ["x"; "exclude"] ~doc ~docs ~docv)

let select_members =
  let select paths includes include_suffixes excludes =
    select_members ~paths ~includes ~include_suffixes ~excludes
  in
  Term.(const select $ paths $ includes $ include_suffixes $ excludes)

let archive =
  let doc = "Operate on ZIP archive $(docv). Use $(b,-) for $(b,stdin)." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"ARCHIVE")

let compression_format =
  let deflate =
    let doc = "Use the deflate (RFC 1951) compression format. Default." in
    Arg.info ["deflate"] ~doc
  in
  let zlib =
    let doc = "Use the zlib (RFC 1950) compression format." in
    Arg.info ["z"; "zlib"] ~doc
  in
  let opts = [ `Deflate, deflate; `Zlib, zlib ] in
  Arg.(value & vflag `Deflate opts)

let s_deflate_levels = "OPTIONS FOR COMPRESSION LEVELS"
let deflate_level =
  let docs = s_deflate_levels in
  let none =
    let doc = "Deflate with no compression (only non-compressed blocks)." in
    Arg.info ["deflate-none"] ~doc ~docs
  in
  let fast =
    let doc = "Fast deflate compression." in
    Arg.info ["f"; "deflate-fast"] ~doc ~docs
  in
  let default =
    let doc = "Default deflate compression (default)." in
    Arg.info ["deflate-default"] ~doc ~docs
  in
  let best =
    let doc = "Best deflate compression." in
    Arg.info ["b"; "deflate-best"] ~doc ~docs
  in
  let opts = [`None, none; `Fast, fast; `Default, default; `Best, best] in
  Arg.(value & vflag `Default opts)

let infile =
  let doc = "Input bytes from file $(docv). Use $(b,-) for $(b,stdin)." in
  Arg.(value & pos 0 string "-" & info [] ~doc ~docv:"INPUT")

let outfile =
  let doc = "Output bytes to file $(docv). Use $(b,-) for $(b,stdout)." in
  Arg.(value & opt string "-" & info ["o"] ~doc ~docv:"OUTPUT")

let outarchive =
  let doc = "Output archive to $(docv). Use $(b,-) for $(b,stdout)." in
  Arg.(value & opt string "-" & info ["o"] ~doc ~docv:"OUTPUT")

let verbosity =
  let quiet = Arg.info ["q"; "quiet"] ~doc:"Be quiet." in
  let verbose =
    let doc = "Be more verbose about what is happening." in
    Arg.(info ["v"; "verbose"] ~doc)
  in
  let opts = [`Quiet, quiet; `Verbose, verbose ] in
  Arg.(value & vflag `Normal opts)

(* Commands *)

let compress =
  let doc = "Compress bytes" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) compresses bytes of $(i,INPUT) to a byte sequence written \
        on $(b,stdout). The output format defaults to deflate (RFC 1951), use \
        option $(b,--zlib) for zlib (RFC 1950).";
    `P "Note that it reads all of $(i,INPUT) in memory before compressing.";
    `S Manpage.s_options;
    `S s_deflate_levels]
  in
  Cmd.v (Cmd.info "compress" ~doc ~man ~exits:Exit.exits_base) @@
  Term.(const compress $ compression_format $ infile $ outfile $
        deflate_level $ verbosity)

let crc =
  let doc = "Compute a CRC" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) computes the ZIP CRC-32 of the bytes of $(i,INPUT) and
        writes it on $(b,stdout) in hexadecimal. Other CRCs can be \
        computed, see options.";
    `P "Note that it reads all of $(i,INPUT) in memory before computing \
        the CRC." ]
  in
  let crc_kind =
    let crc_32 =
      let doc = "Compute ZIP CRC-32 (polynomial 0xedb88320). Default." in
      Arg.info ["z"; "zip-crc-32"] ~doc
    in
    let adler_32 = Arg.info ["a"; "adler-32"] ~doc:"Compute Adler-32." in
    let opts = [ `Crc_32, crc_32; `Adler_32, adler_32 ] in
    Arg.(value & vflag `Crc_32 opts)
  in
  Cmd.v (Cmd.info "crc" ~doc ~man ~exits:Exit.exits_base) @@
  Term.(const crc $ crc_kind $ infile $ verbosity)

let decompress =
  let doc = "Decompress bytes" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) decompresses to $(b,stdout) a byte sequence read from \
        $(i,INPUT). The input format defaults to deflate (RFC 1951), use \
        option $(b,--zlib) for zlib (RFC 1950).";
    `P "Note that it reads all of $(i,INPUT) in memory before decompressing." ]
  in
  Cmd.v (Cmd.info "decompress" ~doc ~man ~exits:Exit.exits_base) @@
  Term.(const decompress $ compression_format $ infile $ outfile $ verbosity)

let list =
  let doc = "List archive members" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) lists archive members.";
    `Pre "$(iname) $(b,archive.zip)          # Show archive members";
    `Noblank;
    `Pre "$(iname) $(b,-s archive.zip)       # Show only paths (short output)";
    `Noblank;
    `Pre "$(iname) $(b,-S .png archive.zip)  \
          # Show files ending with $(b,.png)";
    `P "Reported sizes are in decompressed bytes.";
    `S Manpage.s_options;
    `S s_path_sel;
  ]
  in
  let output_format =
    let short =
      let doc = "Short output. Outputs only paths." in
      Arg.info ["s"; "short"] ~doc
    in
    let long =
      let doc = "Long output. Notably adds CRC values to the output." in
      Arg.info ["l"; "long"] ~doc
    in
    Arg.(value & vflag `Normal [`Short, short; `Long, long])
  in
  Cmd.v (Cmd.info "list" ~doc ~man ~exits:Exit.exits_path) @@
  Term.(const list $ archive $ select_members $ output_format $ verbosity)

let recode =
  let doc = "Recode or delete archive members" in
  let s_recoding = "OPTIONS FOR RECODING" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) recodes selected members of an archive to another archive \
        in various ways. See $(i,PATH) to see how members are selected. It \
        can be used to recode selected members while keeping the other ones \
        untouched, delete the selected members, or only keep the selected \
        members:";
    `Pre "$(iname) $(b,archive.zip -u -S .png -o a.zip) \
          # Uncompress $(b,.png) files"; `Noblank;
    `Pre "$(iname) $(b,archive.zip -d -S .png -o a.zip) \
          # Delete $(b,.png) files"; `Noblank;
    `Pre "$(iname) $(b,archive.zip -k -S .png -o a.zip) \
          # Keep only $(b,.png) files";
    `P "Note that some metadata may be lost during recoding. For \
        example $(b,zipc) drops archive member comments. Members are written \
        in the lexicographic order of their path, except for the member \
        whose path is $(b,mimetype) which, if it exists, is written first. \
        If your are operating on an EPUB file you should make sure this \
        file is not compressed in the result. For example to recompress an \
        EPUB file use:";
     `P "$(iname) $(b,book.epub --deflate -x mimetype -o b.zip)";
    `S Manpage.s_options;
    `S s_recoding;
    `S s_deflate_levels;
    `S s_path_sel; ]
  in
  let mode =
    let delete = Arg.info ["d"; "delete"] ~doc:"Delete selected members." in
    let recode = Arg.info ["keep-all"] ~doc:
        "Recode selected members and keep others (default)."
    in
    let keep_only = Arg.info ["k"; "keep-only"] ~doc:
        "Recode and keep only selected members."
    in
    let opts = [`Recode, recode; `Delete, delete; `Keep_only, keep_only] in
    Arg.(value & vflag `Recode opts)
  in
  let recode' =
    let docs = s_recoding in
    let raw =
      Arg.info ["as-is"] ~docs ~doc:
        "Write selected members as found in the source archive (default)."
    in
    let deflate = Arg.info ["deflate"] ~docs ~doc:
        "Write selected members using deflate compression. Members whose \
         compression format cannot be decompressed by $(b,zipc) are kept as is."
    in
    let stored = Arg.info ["u"; "stored"] ~docs ~doc:
        "Write selected members using no compression. Members whose \
         compression format cannot be decompressed by $(b,zipc) are kept as is."
    in
    let opts =
      [ None, raw; Some Zipc.Deflate, deflate; Some Zipc.Stored, stored ]
    in
    Arg.(value & vflag None opts)
  in
  let check =
    let doc =
      "Recode and redecode without writing to the file system. Unless \
       $(b,--check-cmd) is specified."
    in
    Arg.(value & flag & info ["t"; "check"] ~doc)
  in
  let check_cmd =
    let doc =
      "Use command invocation $(docv) to test the result of $(b,--check). \
       The recoded archive is written to a temporary file and $(docv) is \
       invoked on it. $(mname) returns with a non-zero exit code if the \
       invocation returns a non-zero exit code. A special case is made
       if $(docv) starts with $(b,unzip), exit code 1 or 82 are also treated \
       a success: with $(b,unzip -t -P '') exits with 1 on empty \
       archives and 82 on decryption password errors."
    in
    let docv = "CMD" in
    Arg.(value & opt (some string) None & info ["check-cmd"] ~doc ~docv)
  in
  Cmd.v (Cmd.info "recode" ~doc ~man ~exits:Exit.exits_path) @@
  Term.(const recode $ archive $ outarchive $ select_members $ recode' $ mode $
        deflate_level $ verbosity $ check $ check_cmd)

let sniff =
  let doc = "Sniff for ZIP archives" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) checks given files or files in directories start with \
        a (non-multipart) ZIP magic number. That is either \
        $(b,PK\\\\x03\\\\x04) or $(b,PK\\\\x05\\\\x06) (empty archive).";
    `P "The path of files that identify as such are printed on \
        $(b,stdout). For $(i,PATH) that are files but do not identify \
        as a ZIP archive an error is explicitely reported on $(b,stderr); \
        in this case the tool exits with a specific non-zero exit code.";
    `P "$(iname) $(b,-P -r /)"; `Noblank;
    `P "$(iname) $(b,archive.zip"; ]
  in
  let paths =
    let doc = "$(docv) is the file or directory to sniff." in
    Arg.(non_empty & pos_all string [] & info [] ~doc ~docv:"PATH")
  in
  let recurse =
    let doc = "Recurse into directories." in
    Arg.(value & flag & info ["r"; "recurse"] ~doc)
  in
  let no_follow_symlinks =
    let doc = "Do not follow symlinks when recursing." in
    Arg.(value & flag & info ["P"; "no-dereference"] ~doc)
  in
  let nul_sep =
    let doc = "Separate output paths by $(b,x00) bytes rather than newlines." in
    Arg.(value & flag & info ["0"] ~doc)
  in
  Cmd.v (Cmd.info "sniff" ~doc ~man ~exits:Exit.exits_sniff) @@
  Term.(const sniff $ paths $ nul_sep $ recurse $ no_follow_symlinks $
        verbosity)

let unzip =
  let doc = "Extract archive members" in
  let man = [
    `S Manpage.s_description;
    `P "$(iname) extracts given $(i,PATH) archive members.";
    `Pre "$(iname) $(b,archive.zip)  # Have no fear, see $(b,-o)."; `Noblank;
    `Pre "$(iname) $(b,-o /tmp/archive archive.zip)"; `Noblank;
    `Pre "$(iname) $(b,-o - archive.zip file.txt > file.txt)";
    `S Manpage.s_options;
    `S s_path_sel; ]
  in
  let outpath =
    let doc =
      "Use $(docv) as the root destination directory for writing archive \
       members. All written files are rooted in $(docv); dot segments and \
       absolute paths in archives are sanitized. The tool errors if $(docv) \
       already exists unless $(b,--force) is used in which case any path \
       therein may be rewritten. If $(docv) is $(b,-) the extracted files \
       are written to $(b,stdout)."
    in
    let absent =
      "A directory named after the archive basename is created in the \
       directory of the archive"
    in
    let docv = "DIR" in
    Arg.(value & opt (some string) None & info ["o"] ~doc ~docv ~absent)
  in
  let force =
    let doc =
      "Proceed even if destination directory already exists. Use with care, \
       it may overwrite your files."
    in
    Arg.(value & flag & info ["f"; "force"] ~doc)
  in
  let raw =
    let doc = "Never decompress, output raw archive member data." in
    Arg.(value & flag & info ["raw"] ~doc)
  in
  let skip =
    let doc =
      "Skip members with unsupported compression formats instead of \
       reporting errors and outputing them raw."
    in
    Arg.(value & flag & info ["skip"] ~doc)
  in
  let check =
    let doc = "Decompress and check data integrity but do not write files." in
    Arg.(value & flag & info ["t"; "check"] ~doc)
  in
  let dump_errors =
    let doc =
      "Extract members data regardless of member errors. In case of CRC \
       mismatches this is the data as decompressed. In case of decoding \
       errors this the raw archive member data."
    in
    Arg.(value & flag & info ["dump-errors"] ~doc)
  in
  Cmd.v (Cmd.info "unzip" ~doc ~man ~exits:Exit.exits_unzip) @@
  Term.(const unzip $ archive $ outpath $ select_members $ verbosity $ skip $
        force $ raw $ check $ dump_errors)

let zip =
  let doc = "Archive files and directories" in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) archives given $(i,PATH) files and directories in \
        a ZIP archive.";
    `P "If a single $(i,PATH) is given, the paths to add are \
        made relative to the directory of $(i,PATH) and if no $(b,-o) option \
        is specified the archive $(i,PATH)$(b,.zip) is created.";
    `Pre "$(iname) $(b,path/to/dir)      # Creates path/to/dir.zip";
    `Noblank;
    `Pre "$(iname) $(b,path/to/file.ml)  # Creates path/to/file.ml.zip";
    `P "If multiple paths are specified, the paths to add are as written \
        and the output archive file needs to be specified with the
        $(b,-o) option.";
    `Pre "$(iname) $(b,-o archive.zip doc src/file.ml)";
    `P "Finally before a path gets added in the archive a prefix can be \
        removed with the $(b,-p) option:";
    `Pre "$(iname) $(b,-o archive.zip -p dir dir)"; `Noblank;
    `Pre "$(iname) $(b,-p dir path/to/dir)";` Noblank;
    `Pre "$(iname) $(b,-o archive.zip -p path/to/dir path/to/dir/*)";
    `P "$(i,PATH) that are directories are added recursively (unless \
        $(b,--no-recurse) is specified) and create directory members \
        in the archive (unless $(b,--no-dirs) is specified). \
        $(i,PATH) that are files \
        do not create directory members for the components that lead to them.";
    `P "Archive members are written in the lexicographic order of their \
        $(i,PATH), except for the member whose path is $(b,mimetype) which, \
        if it exists, is written first. If you are trying to make an \
        EPUB, use option $(b,-u) (see next paragraph) to make sure it does \
        not get compressed.";
    `Pre "$(iname) $(b,-o book.epub -u mimetype -p book book)";
    `P "Use the repeatable $(b,-u) option to specify path suffixes that \
        should not be compressed. Using with the empty string makes sure \
        no archive member gets compressed.";
    `Pre "$(iname) $(b,-u .png dir)  # No .png recompression";
    `Noblank;
    `Pre "$(iname) $(b,-u '' dir)    # No compressed members";
    `P "The archive can be initialized with the contents of another one \
        by using the $(b,--init) option. This can be used to add files \
        to existing archive.";
    `Pre "$(iname) $(b,-i old.zip dir -o archive.zip)  # old.zip + dir ";
    `S Manpage.s_options;
    `S s_deflate_levels;
  ]
  in
  let archive =
    let doc =
      "Write the archive to $(docv). Can be omitted if a single $(i,PATH)
       to add is specified. In this case the archive $(i,PATH)$(b,.zip) is \
       created and the paths in the archive are made relative to the \
       directory of $(i,PATH)."
    in
    let docv = "ARCHIVE" and absent = "See below" in
    Arg.(value & opt (some string) None & info ["o"] ~doc ~docv ~absent)
  in
  let paths =
    let doc =
      "Add $(docv) to the archive. If $(docv) is a directory its contents \
       is recursively added, unless $(b,--no-recursion) is specified. \
       Path are stripped by the value specified in option $(b,--strip-prefix) \
       but otherwise added as is to the archive, except for the special case \
       of a single $(docv): in this case they are first made relative to the \
       directory of $(docv) before stripping them."
    in
    let docv = "PATH" in
    Arg.(value & pos_all string [] & info [] ~doc ~docv)
  in
  let strip_prefix =
    let doc =
      "Strip directory prefix $(docv) from $(i,PATH). If $(docv) has no \
       trailing slash one is added."
    in
    let docv = "PREFIX" in
    Arg.(value & opt (some string) None & info ["p"; "strip-prefix"] ~doc ~docv)
  in
  let no_recurse =
    let doc = "Do not recursively add the content of directories." in
    Arg.(value & flag & info ["n"; "no-recursion"] ~doc)
  in
  let no_dirs =
    let doc = "Do not add members for directories." in
    Arg.(value & flag & info ["no-dirs"] ~doc)
  in
  let stored =
    let doc =
      "Write members whose paths ends with suffix $(docv) with no compression. \
       Use with the empty string to make sure no member gets compressed. \
       Repeatable."
    in
    Arg.(value & opt_all string [] & info ["u"; "stored"] ~doc ~docv:"SUFFIX")
  in
  let init =
    let doc =
      "Initialize archive with contents of file $(docv). Case arising \
       $(i,PATH) arguments override those matching in $(docv). Use $(b,-) for \
       $(b,stdin)."
    in
    let docv = "ARCHIVE" and absent = "An empty archive is used" in
    Arg.(value & opt (some string) None & info ["i"; "init"] ~doc ~docv ~absent)
  in
  Cmd.v (Cmd.info "zip" ~doc ~man ~exits:Exit.exits_base) @@
  Term.(const zip $ init $ paths $ strip_prefix $ archive $ no_recurse $
        no_dirs $ stored $ deflate_level $ verbosity)

let zipc =
  let doc = "Process ZIP archives" in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) processes ZIP archives in various ways.";
    `Pre "$(mname) $(b,list archive.zip)   # List archive members";`Noblank;
    `Pre "$(mname) $(b,unzip archive.zip)  # Have no fear, see $(b,--help)";
    `Noblank;
    `Pre "$(mname) $(b,zip dir)            # Archive $(b,dir) to $(b,dir.zip)";
    `Noblank;
    `Pre "$(mname) $(b,sniff couldbezip)   # Is it a ZIP archive ?";
    `P "Invoke subcommands with $(b,--help) for more information.";
    `P "Note that $(mname) always works out of memory, it may use more \
        memory than you would expect.";
    `S Manpage.s_bugs;
    `P "This program is distributed with the $(b,zipc) OCaml library. \
        See $(i,https://erratique.ch/software/zipc) for contact \
        information." ]
  in
  let exits = Exit.exits_all in
  Cmd.group (Cmd.info "zipc" ~version:"%%VERSION%%" ~doc ~man ~exits) @@
  [ compress; crc; decompress; list; recode; sniff; unzip; zip;]

let main () = Cmd.eval' zipc
let () = if !Sys.interactive then () else exit (main ())
