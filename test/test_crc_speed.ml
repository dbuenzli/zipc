(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let random ~length =
  let ic = In_channel.open_bin "/dev/random" in
  let finally () = In_channel.close_noerr ic in
  Fun.protect ~finally @@ fun () ->
  In_channel.really_input_string ic length |> Option.get

let test_crc_speed kind crc_of_string s =
  let start = Sys.time () in
  let crc = crc_of_string s in
  let stop = Sys.time () in
  let dur = (stop -. start) *. 1000. in
  Format.printf "%s: %dms %lx\n" kind (truncate dur) crc

let main () =
  let length = (20 * 1024 * 1024) in
  let s = random ~length in
  test_crc_speed "CRC-32" Zipc_deflate.Crc_32.string s;
  test_crc_speed "CRC-32" Zipc_deflate.Crc_32.string s;
  test_crc_speed "Adler-32" Zipc_deflate.Adler_32.string s;
  test_crc_speed "Adler-32" Zipc_deflate.Adler_32.string s

let () = if !Sys.interactive then () else main ()
