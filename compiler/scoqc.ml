(* Simple Coq compiler *)

let print_st_stats { Vernacstate.parsing; system; lemmas; _ } =
  Format.eprintf "State stats:@\n%!";
  Format.eprintf " [parsing] mem reach: %d@\n%!" (Obj.reachable_words (Obj.magic parsing));
  Format.eprintf " [system ] mem reach: %d@\n%!" (Obj.reachable_words (Obj.magic system));
  Format.eprintf " [lemmas ] mem reach: %d@\n%!" (Obj.reachable_words (Obj.magic lemmas));
  Vernacstate.System.print_stats system;
  ()

let mk_vo_path ?(has_ml=false) unix_path coq_path implicit =
  let coq_path = Libnames.dirpath_of_string coq_path in
  { Loadpath.unix_path; coq_path; has_ml; implicit; recursive = true }

let default_vo_load_path ~coqlib =
  let coqlib = Option.default Coq_config.coqlib coqlib in
  [ mk_vo_path (Filename.concat coqlib "theories") "Coq" true
  ; mk_vo_path (Filename.concat coqlib "plugins")  "Coq" true ~has_ml:true
  ]

let dirpath_of_file f =
  let ldir0 =
    try
      let lp = Loadpath.find_load_path (Filename.dirname f) in
      Loadpath.logical lp
    with Not_found -> Libnames.default_root_prefix
  in
  let f = try Filename.chop_extension (Filename.basename f) with Invalid_argument _ -> f in
  let id = Names.Id.of_string f in
  let ldir = Libnames.add_dirpath_suffix ldir0 id in
  ldir

let fb_handler = let open Feedback in function
  | Feedback.{ contents; _ } ->
    match contents with
    | Feedback.Message(Debug,_,_)
    | Feedback.Message(Info,_,_)
    | Feedback.Message(Notice,_,_) ->
      ()
    | Feedback.Message(_lvl,_loc,msg)->
      Format.printf "%s@\n%!" Pp.(string_of_ppcmds msg)
    | _ -> ()

let load_objs libs =
  let rq_file (dir, from, exp) =
    let mp = Libnames.qualid_of_string dir in
    let mfrom = Option.map Libnames.qualid_of_string from in
    Flags.silently (Vernacentries.vernac_require mfrom exp) [mp]
  in
  List.(iter rq_file (rev libs))

let dft_require_libs = ["Coq.Init.Prelude", None, Some false]

let init_coq ~vo_path ~ml_path ~require_libs in_file =
  Lib.init ();
  (* Global.set_engagement Declarations.PredicativeSet; *)
  (* Global.set_engagement Declarations.PredicativeSet; *)
  Global.set_indices_matter false;
  Flags.set_native_compiler false;
  Global.set_native_compiler false;
  Safe_typing.allow_delayed_constants := false;

  ignore (Feedback.add_feeder fb_handler);

  List.iter Mltop.add_ml_dir ml_path;
  List.iter Loadpath.add_vo_path vo_path;

  (* Get logical name *)
  let libname = dirpath_of_file in_file in
  Declaremods.start_library libname;
  load_objs require_libs;
  Vernacstate.freeze_interp_state ~marshallable:false, libname

let parse ~st pa =
  let mode = Option.map (fun _ -> Vernacinterp.get_default_proof_mode ()) st.Vernacstate.lemmas in
  Vernacstate.(Parser.parse st.parsing Pvernac.(main_entry mode)) pa

let execute = Vernacinterp.interp

let rec cloop ~st pa =
  match parse ~st pa with
  | None ->
    st
  | Some stm ->
    let st = execute ~st stm in
    (cloop [@ocaml.tailcall]) ~st pa

let touch_file f =
  let out = open_out f in
  output_char out ' ';
  close_out out

let save_library ldir in_file ~out_file =
  let out_vo = Option.default (Filename.(remove_extension in_file) ^ ".vo") out_file in
  let todo_proofs = Library.ProofsTodoNone in
  Library.save_library_to todo_proofs ~output_native_objects:false ldir out_vo;

  let out_aux =
    let d, f = Filename.dirname in_file, Filename.basename in_file in
    let f = Filename.(remove_extension f) in
    d ^ Filename.dir_sep ^ "." ^ f ^ ".aux"
  in
  touch_file out_aux

let start_glob ~in_file =
  let out_vo = Filename.(remove_extension in_file) ^ ".vo" in
  Dumpglob.push_output Dumpglob.MultFiles;
  Dumpglob.start_dump_glob ~vfile:in_file ~vofile:out_vo;
  (* Dumpglob.dump_string ("F" ^ Names.DirPath.to_string ldir ^ "\n"); *)
  ()

let version () =
  Printf.printf "The Coq Proof Assistant, version %s \n"
    Coq_config.version;
  Printf.printf "with OCaml %s\n" Coq_config.caml_version

let compile ~vo_path ~ml_path ~require_libs ~in_file ~out_file =
  let f_in = open_in in_file in
  let () = start_glob ~in_file in
  let st, ldir = init_coq ~vo_path ~ml_path ~require_libs in_file in
  let pa = Pcoq.Parsable.make (Stream.of_channel f_in) in
  let _st = cloop ~st pa in
  (* Compact the heap, just in case. *)
  Gc.compact ();
  (* print_st_stats st; *)
  let () = save_library ldir in_file ~out_file in
  Dumpglob.end_dump_glob ();
  ()

module Args = struct

  type t =
    { vo_path : Loadpath.vo_path list
    ; ml_path : string list
    ; init : bool
    ; boot : bool
    ; coqlib : string option
    ; in_file : string option
    ; out_file : string option
    }

  let init =
    { vo_path = []
    ; ml_path = []
    ; init = true
    ; boot = false
    ; coqlib = None
    ; in_file = None
    ; out_file = None
    }

  module Error = struct

    let duplicate_argument in_file file =
      CErrors.user_err
        Pp.(str "parse args error, too many files: " ++ str in_file ++ str " and " ++ str file)
  end

  let set_out_file cur file =
    match cur.out_file with
    | None ->
      { cur with out_file = Some file }
    | Some out_file ->
      Error.duplicate_argument out_file file

  let set_in_file cur file =
    match cur.in_file with
    | None ->
      { cur with in_file = Some file }
    | Some in_file ->
      Error.duplicate_argument in_file file

  let return cur = { cur with vo_path = List.rev cur.vo_path; ml_path = List.rev cur.ml_path }

end

let rec parse_args (args : string list) cur : Args.t =
  match args with
  | "-noinit" :: rem ->
    parse_args rem { cur with Args.init = false }
  | "-boot" :: rem ->
    parse_args rem { cur with Args.boot = true }
  | "-q" :: rem ->
    parse_args rem cur
  | (("-Q" | "-R") as impl_str) :: d :: p :: rem ->
    let implicit = String.equal impl_str "-R" in
    let vo_path = mk_vo_path d p implicit :: cur.Args.vo_path in
    parse_args rem { cur with Args.vo_path }
  | "-coqlib" :: lib :: rem ->
    parse_args rem { cur with Args.coqlib = Some lib }
  | "-I" :: d :: rem ->
    let ml_path = d :: cur.Args.ml_path in
    parse_args rem { cur with Args.ml_path }
  | "--print-version" :: _rem ->
    version (); exit 0
  (* Ignored, for test suite / compat / TODO *)
  | "-test-mode" :: rem ->
    Flags.quiet := false;
    parse_args rem cur
  | "-async-proofs-cache" :: _ :: rem
  | "-no-glob" :: rem
  | "-top" :: _ :: rem
  | "-w" :: _ :: rem
  | "-native-compiler" :: _ :: rem ->
    parse_args rem cur
  (* | "--print-version" :: _rem ->
   *   version (); exit 0 *)
  | "-o" :: out_file :: rem ->
    parse_args rem (Args.set_out_file cur out_file)
  | in_file :: rem ->
    parse_args rem (Args.set_in_file cur in_file)
  | [] ->
    match cur.Args.in_file with
    | None ->
      CErrors.user_err Pp.(str "parse args error, missing input file.")
    | Some file ->
      Args.return cur

let () =
  Flags.quiet := true;
  System.trust_file_cache := true;
  try
    let { Args.vo_path; ml_path; init; boot; coqlib; in_file; out_file } =
      parse_args (List.tl @@ Array.to_list Sys.argv) Args.init in
    let in_file = Option.get in_file in
    let require_libs = if init then dft_require_libs else [] in
    let vo_path = if boot then vo_path else default_vo_load_path ~coqlib @ vo_path in
    compile ~vo_path ~ml_path ~require_libs ~in_file ~out_file
  with exn ->
    Format.eprintf "Error: @[%a@]@\n%!" Pp.pp_with (CErrors.print exn)
