(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Pp
open Coqargs
open Coqcargs

let fatal_error msg =
  Topfmt.std_logger Feedback.Error msg;
  flush_all ();
  exit 1

(******************************************************************************)
(* Interactive Load File Simulation                                           *)
(******************************************************************************)
let load_vernacular opts ~state =
  List.fold_left
    (fun state (f_in, echo) ->
      let s = Loadpath.locate_file f_in in
      (* Should make the beautify logic clearer *)
      let load_vernac f = Vernac.load_vernac ~echo ~interactive:false ~check:true ~state f in
      if !Flags.beautify
      then Flags.with_option Flags.beautify_file load_vernac f_in
      else load_vernac s
    ) state (List.rev opts.load_vernacular_list)

let load_init_vernaculars opts ~state =
  let state =
    if opts.load_rcfile then
      Topfmt.(in_phase ~phase:LoadingRcFile) (fun () ->
          Coqinit.load_rcfile ~rcfile:opts.rcfile ~state) ()
    else begin
      Flags.if_verbose Feedback.msg_info (str"Skipping rcfile loading.");
      state
    end in

  load_vernacular opts ~state

(******************************************************************************)
(* File Compilation                                                           *)
(******************************************************************************)
let warn_file_no_extension =
  CWarnings.create ~name:"file-no-extension" ~category:"filesystem"
         (fun (f,ext) ->
          str "File \"" ++ str f ++
            strbrk "\" has been implicitly expanded to \"" ++
            str f ++ str ext ++ str "\"")

let ensure_ext ext f =
  if Filename.check_suffix f ext then f
  else begin
    warn_file_no_extension (f,ext);
    f ^ ext
  end

let chop_extension f =
  try Filename.chop_extension f with _ -> f

let ensure_bname src tgt =
  let src, tgt = Filename.basename src, Filename.basename tgt in
  let src, tgt = chop_extension src, chop_extension tgt in
  if src <> tgt then
    fatal_error (str "Source and target file names must coincide, directories can differ" ++ fnl () ++
                   str "Source: " ++ str src                                                ++ fnl () ++
                   str "Target: " ++ str tgt)

let ensure ext src tgt = ensure_bname src tgt; ensure_ext ext tgt

let ensure_exists f =
  if not (Sys.file_exists f) then
    fatal_error (hov 0 (str "Can't find file" ++ spc () ++ str f))

let ensure_exists_with_prefix f_in f_out src_suffix tgt_suffix =
  let long_f_dot_src = ensure src_suffix f_in f_in in
  ensure_exists long_f_dot_src;
  let long_f_dot_tgt = match f_out with
    | None -> chop_extension long_f_dot_src ^ tgt_suffix
    | Some f -> ensure tgt_suffix long_f_dot_src f in
  long_f_dot_src, long_f_dot_tgt

let stm_new_doc _ _ _ = (), ()

(* Compile a vernac file *)
let compile opts copts ~echo ~f_in ~f_out =
  let open Vernac.State in
  let iload_path = build_load_path opts in
  let require_libs = require_libs opts in

  let output_native_objects = match opts.native_compiler with
    | NativeOff -> false | NativeOn {ondemand} -> not ondemand
  in
  let long_f_dot_v, long_f_dot_vo =
    ensure_exists_with_prefix f_in f_out ".v" ".vo" in

  let _doc, _sid = Topfmt.(in_phase ~phase:LoadingPrelude)
      stm_new_doc long_f_dot_vo iload_path require_libs in
  let state = { doc = 0; sid = 0 ; proof = None; time = opts.time } in
  let state = load_init_vernaculars opts ~state in
  let check = true in
  let _state = Vernac.load_vernac ~echo ~check ~interactive:false ~state long_f_dot_v in
  let ldir = Names.DirPath.make [] in
  Library.save_library_to ~output_native_objects ldir long_f_dot_vo (Global.opaque_tables ())

let compile opts copts ~echo ~f_in ~f_out =
  compile opts copts ~echo ~f_in ~f_out

let compile_file opts copts (f_in, echo) =
  let f_out = copts.compilation_output_name in
  if !Flags.beautify then
    Flags.with_option Flags.beautify_file
      (fun f_in -> compile opts copts ~echo ~f_in ~f_out) f_in
  else
    compile opts copts ~echo ~f_in ~f_out

let compile_files opts copts =
  let compile_list = List.rev copts.compile_list in
  List.iter (compile_file opts copts) compile_list
