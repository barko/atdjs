(* $Id: ag_main.ml 52381 2010-11-24 22:40:21Z martin $ *)

open Printf

let set_once varname var x =
  match !var with
      Some y ->
        if x <> y then
          failwith (sprintf "\
Command-line parameter %S is set multiple times
to incompatible values."
                      varname)
        
    | None ->
        var := Some x
          

let main () =
  let files = ref [] in
  let opens = ref [] in
  let out_prefix = ref None in
  (* let type_aliases = ref None in *)

  let set_opens s =
    let l = Str.split (Str.regexp " *, *\\| +") s in
    opens := List.rev_append l !opens
  in

  let options = [
    (*
    "-extend", Arg.String (fun s -> type_aliases := Some s),
    "MODULE
          Assume that all type definitions are provided by the specified
          module unless otherwise annotated.  Type aliases are created
          for each type, e.g.
            type t = Module.t";
    *)

    "-o", Arg.String (fun s ->
                        let out =
                          match s with
                              "-" -> `Stdout
                            | s -> `Files s
                        in
                        set_once "output prefix" out_prefix out),
    "[ PREFIX | - ]
          Use this prefix for the generated files, e.g. 'foo/bar' for
          foo/bar.ml and foo/bar.mli.
          `-' designates stdout and produces code of the form
            struct ... end : sig ... end";

    "-open", Arg.String set_opens,
    "MODULE1,MODULE2,...
          List of modules to open (comma-separated or space-separated)";

  ]
  in
  let msg = sprintf "\
Generate OCaml serializers and deserializers.
Default serialization format is biniou.
Usage: %s FILE.atd" Sys.argv.(0) in
  Arg.parse options (fun file -> files := file :: !files) msg;

  let atd_file =
    match !files with
      |	[s] -> s
      | _ ->
	  Arg.usage options msg;
	  exit 1
  in
  let ocaml_prefix =
    match !out_prefix with
      | Some x -> x
      | None ->
	`Files (
          if Filename.check_suffix atd_file ".atd" then
	    Filename.chop_extension atd_file
	  else
	    atd_file
        )
  in
  let opens = List.rev !opens in
  Atdjs_emit.make_ocaml_files ~opens atd_file ocaml_prefix
    
let () =
  try 
    main ()
  with
    | Atd_ast.Atd_error s

    | Failure s ->
      flush stdout;
      eprintf "%s\n%!" s;
      exit 1

    | e -> raise e
