open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let zipc = B0_ocaml.libname "zipc"

let brr = B0_ocaml.libname "brr" (* Only for test *)
let cmdliner = B0_ocaml.libname "cmdliner"
let unix = B0_ocaml.libname "unix"

let zipc_lib =
  let srcs = [ `Dir ~/"src" ] in
  let requires = [] in
  B0_ocaml.lib zipc ~name:"zipc-lib" ~srcs ~requires

(* Tools *)

let zipc_tool =
  let srcs = [`File ~/"test/zipc_tool.ml"] in
  let requires = [cmdliner; unix; zipc] in
  B0_ocaml.exe "zipc" ~public:true ~srcs ~requires

(* Tests *)

let test ~src ~doc =
  let src = Fpath.(~/"test"/ src) in
  let srcs = [ `File src ] in
  let requires = [zipc] in
  let meta = B0_meta.empty |> B0_meta.(tag test) in
  B0_ocaml.exe (Fpath.basename ~strip_exts:true src) ~doc ~meta ~srcs ~requires

let test' = test ~src:"test.ml" ~doc:"Basic tests"
let speed_crc_tests = test ~src:"test_crc_speed.ml" ~doc:"CRCs speed test"
let examples = test ~src:"examples.ml" ~doc:"Documentation examples"
let jsoo =
  let doc = "Test unzip in the browser" in
  let srcs = [ `File ~/"test/jsoo_unzip.ml" ] in
  let requires = [brr; zipc] in
  let meta = B0_meta.empty |> B0_meta.(tag test) in
  B0_jsoo.html_page "jsoo_unzip" ~doc ~meta ~srcs ~requires

let jsoo =
  let doc = "Test zip creation in the browser" in
  let srcs = [ `File ~/"test/jsoo_zip.ml" ] in
  let requires = [brr; zipc] in
  let meta = B0_meta.empty |> B0_meta.(tag test) in
  B0_jsoo.html_page "jsoo_zip" ~doc ~meta ~srcs ~requires

(* Actions *)

let time_inflate =
  let doc = "Time unzip and zipc inflation check (-t) on the same archive" in
  B0_unit.of_action "time-inflate" ~units:[zipc_tool] ~doc @@
  fun env _ ~args ->
  let* args =
    if not (Cmd.is_empty args) then Ok args else
    let test = B0_env.in_scope_dir env ~/"tmp/silesia.zip" in
    let* exists = Os.File.exists test in
    if exists then Ok (Cmd.(path test)) else
    Fmt.error "Need to specify a ZIP archive"
  in
  let* time = B0_env.get_cmd env Cmd.(arg "time" % "-h") in
  let* zipc = Result.map Cmd.path (B0_env.unit_exe_file env zipc_tool) in
  let* unzip = B0_env.get_cmd env (Cmd.arg "unzip") in
  let stdout = Os.Cmd.out_null in
  Log.stdout (fun m -> m "%a vs %a" Fmt.code "unzip" Fmt.code "zipc");
  let* () = Os.Cmd.run ~stdout Cmd.(time %% unzip % "-t" %% args) in
  let* () = Os.Cmd.run ~stdout Cmd.(time %% zipc % "unzip" % "-t" %% args) in
  Ok ()

let zipc_for_each =
  let doc = "Invoke zipc on parallel on files in a given null separated list" in
  B0_unit.of_action' "zipc-for-each" ~units:[zipc_tool] ~doc @@
  B0_unit.Action.of_cmdliner_term @@ fun env u ->
  let run list args =
    Log.if_error ~use:Os.Exit.some_error @@
    let* time = B0_env.get_cmd env Cmd.(arg "time" % "-h") in
    let* xargs = B0_env.get_cmd env Cmd.(arg "xargs" % "-0" % "-P0" % "-L1") in
    let* zipc = Result.map Cmd.path (B0_env.unit_exe_file env zipc_tool) in
    let stdin = match list with
    | "-" -> Os.Cmd.in_stdin | file -> Os.Cmd.in_file (Fpath.v list)
    in
    let* () = Os.Cmd.run ~stdin Cmd.(time %% xargs %% zipc %% list args) in
    Ok Os.Exit.ok
  in
  (* TODO b0: streamline the arg parsing, do a mini getopts. *)
  let open Cmdliner in
  let list =
    let doc =
      "$(docv) is the file with files to invoke $(b,zipc). File paths must \
       be null separated. Use $(b,-) for stdin."
    in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")
  in
  let args' =
    let doc =
      "$(docv) are $(b,zipc)'s arguments. Specify them after $(b,--)."
    in
    Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"ARGS")
  in
  Term.(const run $ list $ args')

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The zipc programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/zipc"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/zipc/doc"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/zipc.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/zipc/issues"
    |> B0_meta.(add description_tags)
      ["codec"; "zip"; "deflate"; "zlib"; "org:erratique"; ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
                  "--with-cmdliner" "%{cmdliner:installed}%"]]|}
    |> B0_meta.add B0_opam.depopts ["cmdliner", ""]
    |> B0_meta.add B0_opam.conflicts [ "cmdliner", {|< "1.1.0"|}]
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.1.0"|};
        "b0", {|dev & with-test|};
      ]
  in
  B0_pack.make "default" ~doc:"The zipc package" ~meta ~locked:true @@
  B0_unit.list ()
