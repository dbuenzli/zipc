(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

let read_file file =
  let read file ic = try Ok (In_channel.input_all ic) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdin () = In_channel.set_binary_mode In_channel.stdin true in
  try match file with
  | "-" -> binary_stdin (); read file In_channel.stdin
  | file -> In_channel.with_open_bin file (read file)
  with Sys_error e -> Error e

let write_file file s =
  let write file s oc = try Ok (Out_channel.output_string oc s) with
  | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
  in
  let binary_stdout () = Out_channel.(set_binary_mode stdout true) in
  try match file with
  | "-" -> binary_stdout (); write file s Out_channel.stdout
  | file -> Out_channel.with_open_bin file (write file s)
  with Sys_error e -> Error e

let unzip_path ~path ~archive =
  let ( let* ) = Result.bind in
  let file_error file msg = Printf.sprintf "%s: %s" file msg in
  let* s = read_file archive in
  Result.map_error (file_error archive) @@
  let* z = Zipc.of_binary_string s in
  Result.map_error (file_error path) @@
  match Option.map Zipc.Member.kind (Zipc.find path z) with
  | None -> Error "No such path"
  | Some Zipc.Member.Dir -> Error "Is a directory"
  | Some Zipc.Member.File file ->
      let* data = Zipc.File.to_binary_string file in
      Ok (Zipc.Fpath.sanitize path, data)

let zip_file ?(compress = true) ~file ~archive () =
  let ( let* ) = Result.bind in
  let file_error file msg = Printf.sprintf "%s: %s" file msg in
  let* s = read_file file in
  let* m =
    Result.map_error (file_error file) @@
    let path = Filename.basename file in
    let* file = match compress with
    | true -> Zipc.File.deflate_of_binary_string s
    | false -> Zipc.File.stored_of_binary_string s
    in
    Zipc.Member.make ~path (Zipc.Member.File file)
  in
  let* s = Zipc.to_binary_string (Zipc.add m Zipc.empty) in
  write_file archive s
