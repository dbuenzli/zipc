(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* UI to select a directory and zip them *)

open Brr

let ( let* ) = Result.bind
let strf fmt = Format.asprintf fmt
let error_string e = Jstr.to_string (Jv.Error.message e)

let data_url s =
  Result.map_error error_string @@
  let data = Base64.data_of_binary_jstr (Jstr.binary_of_octets s) in
  let* data = Base64.encode data in
  Ok (Jstr.(v "data:;base64," + data))

let download_link path data_url =
  El.a ~at:At.[href data_url; download (Jstr.v path)] [El.txt' path]

let zip_file file ~compress =
  let path = Jstr.to_string (File.relative_path file) in
  let contents = Blob.array_buffer (File.as_blob file) in
  Fut.bind contents @@ fun contents ->
  Fut.return @@ Result.map_error (fun e -> strf "%s: %s" path e) @@
  let* contents = Result.map_error error_string contents in
  let data = Tarray.to_string (Tarray.of_buffer Uint8 contents) in
  let* file = match compress with
  | true -> Zipc.File.deflate_of_binary_string data
  | false -> Zipc.File.stored_of_binary_string data
  in
  Zipc.Member.make ~path (File file)

let make_archive ~tty files ~compress =
  let pre = El.pre [] in
  let () = El.append_children tty [El.h1 [El.txt' "…"]; pre] in
  let show_txt s = El.append_children pre [El.txt' s] in
  let error e = [El.h1 [El.txt' "Error"]; El.p [El.txt' e]; pre] in
  let start = Performance.now_ms G.performance in
  let finish stop =
    show_txt (strf "\nDone in %.3fs" ((stop -. start) /. 1000.))
  in
  let rec loop z = function
  | [] ->
      let r =
        Result.fold ~ok:Fun.id ~error @@
        let* zip = Zipc.to_binary_string z in
        let* url = data_url zip in
        Ok [El.h1 [download_link "archive.zip" url]; pre]
      in
      El.set_children tty r;
      finish (Performance.now_ms G.performance);
  | file :: files ->
      show_txt (strf "Add %s@." (Jstr.to_string (File.relative_path file)));
      Fut.await (zip_file file ~compress) @@ function
      | Error e -> show_txt e
      | Ok m -> loop (Zipc.add m z) files
  in
  loop Zipc.empty files

let set_monospace el =
  El.set_inline_style (Jstr.v "font-family") (Jstr.v "monospace") el

let main () =
  let () = set_monospace (Document.body G.document) in
  let h1 = El.h1 [El.txt' "Zip a directory"] in
  let tty = El.div [] in
  let compress = El.input ~at:At.[type' (Jstr.v "checkbox"); checked] () in
  let files =
    (* let multiple = At.true' (Jstr.v "multiple") in *)
    let directory = At.true' (Jstr.v "webkitdirectory") in
    El.input ~at:At.[type' (Jstr.v "file"); directory] ()
  in
  let on_change e =
    let compress = El.prop El.Prop.checked compress in
    make_archive ~tty (El.Input.files files) ~compress;
    (* If we don't reset, selecting the same file doesn't retrigger *)
    El.set_prop El.Prop.value Jstr.empty files;
  in
  let () = ignore (Ev.listen Ev.change on_change (El.as_target files)) in
  let inputs =
    El.div [El.p [files]; El.label [El.txt' "Compress files "; compress; ]]
  in
  El.set_children (Document.body G.document) [h1; inputs; tty]

let () = if !Sys.interactive then () else main ()
