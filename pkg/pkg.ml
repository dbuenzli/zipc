#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "zipc" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/zipc.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.bin ~cond:cmdliner "test/zipc_tool" ~dst:"zipc"; ]
