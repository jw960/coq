(* Simple Coq compiler *)

let vo_load_path =
  [ { Loadpath.unix_path =
        Filename.concat Coq_config.coqlib "theories"
    ; coq_path = Names.DirPath.make [Names.Id.of_string "Coq"]
    ; implicit = true
    ; has_ml = false
    ; recursive = true
    }
  ; { Loadpath.unix_path =
        Filename.concat Coq_config.coqlib "plugins"
    ; coq_path = Names.DirPath.make [Names.Id.of_string "Coq"]
    ; implicit = true
    ; has_ml = true
    ; recursive = true
    }
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

let fb_handler = function
  | Feedback.{ contents; _ } ->
    match contents with
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

let require_libs = ["Coq.Init.Prelude", None, Some false]

let init_coq ~vo_path ~ml_path in_file =
  Lib.init ();
  Global.set_engagement Declarations.PredicativeSet;
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
  | None -> ()
  | Some stm ->
    let st = execute ~st stm in
    cloop ~st pa

let save_library ldir in_file =
  let out_vo = Filename.(remove_extension in_file) ^ ".vo" in
  let todo_proofs = Library.ProofsTodoNone in
  Library.save_library_to todo_proofs ~output_native_objects:false ldir out_vo (Global.opaque_tables ())

let compile ~vo_path ~ml_path ~in_file =
  let f_in = open_in in_file in
  let st, ldir = init_coq ~vo_path ~ml_path in_file in
  let pa = Pcoq.Parsable.make (Stream.of_channel f_in) in
  let () = cloop ~st pa in
  let () = save_library ldir in_file in
  ()

let add_vo_include unix_path coq_path implicit =
  let open Loadpath in
  let coq_path = Libnames.dirpath_of_string coq_path in
  { unix_path; coq_path; has_ml = false; implicit; recursive = true }

let rec parse_args (args : string list) vo_acc ml_acc : _ * _ * string =
  match args with
  | [] -> CErrors.user_err (Pp.str "parse args error")
  | "-Q" :: rem ->
    begin match rem with
      | d :: p :: rem ->
        let vo_acc = add_vo_include d p false :: vo_acc in
        parse_args rem vo_acc ml_acc
      | _ ->
        CErrors.user_err (Pp.str "parse args error")
    end
  | "-R" :: rem ->
    begin match rem with
      | d :: p :: rem ->
        let vo_acc = add_vo_include d p true :: vo_acc in
        parse_args rem vo_acc ml_acc
      | _ ->
        CErrors.user_err (Pp.str "parse args error")
    end
  | "-I" :: rem ->
    begin match rem with
      | d :: rem ->
        let ml_acc = d :: ml_acc in
        parse_args rem vo_acc ml_acc
      | _ ->
        CErrors.user_err (Pp.str "parse args error")
    end
  | [file] ->
    List.rev vo_acc, List.rev ml_acc, file
  | _ ->
    CErrors.user_err (Pp.str "parse args error")

let () =
  try
    let vo_path, ml_path, in_file = parse_args (List.tl @@ Array.to_list Sys.argv) vo_load_path [] in
    compile ~vo_path ~ml_path ~in_file
  with exn ->
    Format.eprintf "Error: @[%a@]@\n%!" Pp.pp_with (CErrors.print exn)
