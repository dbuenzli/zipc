(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* UI to select a file and unzip it *)

open Brr

let ( let* ) = Result.bind
let strf fmt = Format.asprintf fmt
let error_string e = Jstr.to_string (Jv.Error.message e)
let relax f = ignore (G.set_timeout ~ms:0 f)

let data_url s =
  Result.map_error error_string @@
  let data = Base64.data_of_binary_jstr (Jstr.binary_of_octets s) in
  let* data = Base64.encode data in
  Ok (Jstr.(v "data:;base64," + data))

let download_link path data_url =
  El.a ~at:At.[href data_url; download (Jstr.v path)] [El.txt' path]

let unzip_members z ~tty =
  let show_txt s = El.append_children tty [El.txt' s] in
  let show m = match Zipc.Member.kind m with
  | Zipc.Member.Dir -> show_txt (strf "[ -- ] %a@." Zipc.Member.pp_long m)
  | Zipc.Member.File file when Zipc.File.can_extract file ->
      begin match Zipc.File.to_binary_string file with
      | Ok data ->
          let s = strf "[ OK ] %a" Zipc.Member.pp_long m in
          begin match data_url data with
          | Error e -> show_txt (s ^ " " ^ e ^ "\n")
          | Ok data_url ->
              let path = Zipc.Member.path m in
              let s = String.sub s 0 (String.length s - String.length path) in
              let path = download_link path data_url in
              El.append_children tty [El.txt' s; path; El.txt' "\n"]
          end
      | Error e ->
          show_txt (strf "[FAIL] @[<v>%a@,%s@]@." Zipc.Member.pp_long m e)
      end
  | Zipc.Member.File _ -> show_txt (strf "[ ?? ] %a@." Zipc.Member.pp_long m)
  in
  let start = Performance.now_ms G.performance in
  let finish stop = show_txt (strf "[DONE] %.3fs" ((stop -. start) /. 1000.)) in
  let rec loop = function
  | [] -> finish (Performance.now_ms G.performance)
  | m :: ms -> relax @@ fun () -> show m; loop ms
  in
  loop (List.rev (Zipc.fold List.cons z []))

let unzip_file ~tty file =
  let file_error path e = strf "%s: %s" path e in
  let contents = Blob.array_buffer (File.as_blob file) in
  let path = Jstr.to_string (File.name file) in
  Fut.await contents @@ fun contents ->
  let z =
    Result.map_error (file_error path) @@
    let* contents = Result.map_error error_string contents in
    let s = Tarray.to_string (Tarray.of_buffer Tarray.Uint8 contents) in
    Zipc.of_binary_string s
  in
  match z with
  | Error e ->
      let err = [El.h1 [El.txt' "Error"]; El.p [El.txt' e]] in
      El.set_children tty err
  | Ok z ->
      let pre = El.pre [] in
      El.set_children tty [El.h1 [El.txt' path]; pre];
      unzip_members z ~tty:pre

let set_monospace el =
  El.set_inline_style (Jstr.v "font-family") (Jstr.v "monospace") el

let main () =
  let () = set_monospace (Document.body G.document) in
  let h1 = El.h1 [El.txt' "Unzip file"] in
  let tty = El.div [] in
  let files = El.input ~at:At.[type' (Jstr.v "file")] () in
  let on_change e =
    unzip_file ~tty (List.hd (El.Input.files files));
    (* If we don't reset, selecting the same file doesn't retrigger *)
    El.set_prop El.Prop.value Jstr.empty files;
  in
  let () = ignore (Ev.listen Ev.change on_change (El.as_target files)) in
  El.set_children (Document.body G.document) [h1; El.p [files]; tty]

let () = if !Sys.interactive then () else main ()
