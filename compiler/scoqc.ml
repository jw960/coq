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

let save_library ldir in_file =
  let out_vo = Filename.(remove_extension in_file) ^ ".vo" in
  let todo_proofs = Library.ProofsTodoNone in
  Library.save_library_to todo_proofs ~output_native_objects:false ldir out_vo;

  let out_aux =
    let d, f = Filename.dirname in_file, Filename.basename in_file in
    let f = Filename.(remove_extension f) in
    d ^ Filename.dir_sep ^ "." ^ f ^ ".aux"
  in
  let out_glob = Filename.(remove_extension in_file) ^ ".glob" in
  touch_file out_aux;
  touch_file out_glob

let compile ~vo_path ~ml_path ~require_libs ~in_file =
  let f_in = open_in in_file in
  let st, ldir = init_coq ~vo_path ~ml_path ~require_libs in_file in
  let pa = Pcoq.Parsable.make (Stream.of_channel f_in) in
  let _st = cloop ~st pa in
  (* Compact the heap, just in case. *)
  Gc.compact ();
  (* print_st_stats st; *)
  let () = save_library ldir in_file in
  ()

let rec parse_args (args : string list) vo_acc ml_acc init boot coqlib
  : _ * _ * _ * _ * _ * string =
  match args with
  | [] -> CErrors.user_err (Pp.str "parse args error: missing argument")
  | "-noinit" :: rem ->
    parse_args rem vo_acc ml_acc false boot coqlib
  | "-boot" :: rem ->
    parse_args rem vo_acc ml_acc init true coqlib
  | "-q" :: rem ->
    parse_args rem vo_acc ml_acc init boot coqlib
  | (("-Q" | "-R") as impl_str) :: d :: p :: rem ->
    let implicit = String.equal impl_str "-R" in
    let vo_acc = mk_vo_path d p implicit :: vo_acc in
    parse_args rem vo_acc ml_acc init boot coqlib
  | "-coqlib" :: lib :: rem ->
    parse_args rem vo_acc ml_acc init boot (Some lib)
  | "-w" :: _ :: rem ->
    parse_args rem vo_acc ml_acc init boot coqlib
  | "-native-compiler" :: _ :: rem ->
    parse_args rem vo_acc ml_acc init boot coqlib
  | "-I" :: d :: rem ->
    let ml_acc = d :: ml_acc in
    parse_args rem vo_acc ml_acc init boot coqlib
  | [file] ->
    List.rev vo_acc, List.rev ml_acc, init, boot, coqlib, file
  | args ->
    let args_msg = String.concat " " args in
    CErrors.user_err Pp.(str "parse args error, too many arguments: " ++ str args_msg)

let () =
  Flags.quiet := true;
  System.trust_file_cache := true;
  try
    let vo_path, ml_path, init, boot, coqlib, in_file =
      parse_args (List.tl @@ Array.to_list Sys.argv) [] [] true false None in
    let require_libs = if init then dft_require_libs else [] in
    let vo_path = if boot then vo_path else default_vo_load_path ~coqlib @ vo_path in
    compile ~vo_path ~ml_path ~require_libs ~in_file
  with exn ->
    Format.eprintf "Error: @[%a@]@\n%!" Pp.pp_with (CErrors.print exn)
