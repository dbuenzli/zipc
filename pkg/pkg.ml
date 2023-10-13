#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"

let ( let* ) = Result.bind

let adapt src =
  Log.app (fun m -> m "Adapting %s for 32-bit" src);
  let* s = OS.File.read src in
  let* s = (* Topkg.String.cut works only with a char :-( *)
    let needle = "(*32*)" and cut = ref 0 in
    let needle_max = String.length needle - 1 and max = String.length s - 1 in
    try
      while !cut <= max do
        if s.[!cut] = '(' && !cut + needle_max <= max &&
           s.[!cut + needle_max] = ')' &&
           String.sub s !cut (needle_max + 1) = needle
        then raise Exit else incr cut;
      done;
      Log.app (fun m -> m "Already adapted");
      Ok s
    with Exit ->
      let rstart = !cut + needle_max + 1 in
      Ok (String.concat "32" [String.sub s 0 !cut;
                              String.sub s rstart (max - rstart + 1)])
  in
  OS.File.write src s

let adapt_to_platform_word_size c =
  let ws = Conf.OCaml.word_size (Conf.OCaml.v c `Host_os) in
  if ws < 32 then adapt "src/zipc_deflate.ml" else Ok ()

let () =
  let build = Pkg.build ~pre:adapt_to_platform_word_size () in
  Pkg.describe "zipc" ~build @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/zipc.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.bin ~cond:cmdliner "test/zipc_tool" ~dst:"zipc"; ]
