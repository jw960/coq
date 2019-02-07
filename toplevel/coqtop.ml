(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Coqargs

let () = at_exit flush_all

(******************************************************************************)
(* Fatal Errors                                                               *)
(******************************************************************************)

(** Prints info which is either an error or an anomaly and then exits
    with the appropriate error code *)
let fatal_error_exn exn =
  Topfmt.(in_phase ~phase:Initialization print_err_exn exn);
  flush_all ();
  let exit_code =
    if CErrors.(is_anomaly exn || not (handled exn)) then 129 else 1
  in
  exit exit_code

(** Support for custom coqtop loops *)
type custom_toplevel =
  { init : Coqinit.init_fn
  ; run : opts:Coqargs.t -> state:Vernac.State.t -> unit
  ; opts : Coqargs.t
  }

let init_toploop opts =
  let iload_path = build_load_path opts in
  let require_libs = require_libs opts in
  let stm_options = opts.stm_flags in
  let open Vernac.State in
  let doc, sid =
    Stm.(new_doc
           { doc_type = Interactive opts.toplevel_name;
             iload_path; require_libs; stm_options;
           }) in
  let state = { doc; sid; proof = None; time = opts.time } in
  Coqinit.load_init_vernaculars ~opts ~state, opts

(* To remove in 8.11 *)
let call_coqc args =
  let remove str arr = Array.(of_list List.(filter (fun l -> not String.(equal l str)) (to_list arr))) in
  let coqc_name = Filename.remove_extension (System.get_toplevel_path "coqc") in
  let args = remove "-compile" args in
  Unix.execv coqc_name args

let deprecated_coqc_warning = CWarnings.(create
    ~name:"deprecate-compile-arg"
    ~category:"toplevel"
    ~default:Enabled
    (fun opt_name -> Pp.(seq [str "The option "; str opt_name; str" is deprecated, please use coqc."])))

let rec coqc_deprecated_check args acc extras =
  match extras with
  | [] -> acc
  | "-o" :: _ :: rem ->
    deprecated_coqc_warning "-o";
    coqc_deprecated_check args acc rem
  | ("-compile"|"-compile-verbose") :: file :: rem ->
    deprecated_coqc_warning "-compile";
    call_coqc args
  | x :: rem ->
    coqc_deprecated_check args (x::acc) rem

let coqtop_toplevel =
  { init = (fun ~opts extra -> opts, extra)
  ; run = Coqloop.loop
  ; opts = Coqargs.default
  }

let start_coq custom =
  let init_feeder = Feedback.add_feeder Coqloop.coqloop_feed in
  (* Init phase *)
  let state, opts =
    try
      let opts, extras =
        Coqinit.init_toplevel
          ~help:Usage.print_usage_coqtop ~init:default custom.init
          (List.tl (Array.to_list Sys.argv)) in
      let extras = coqc_deprecated_check Sys.argv [] extras in
      if not (CList.is_empty extras) then begin
        prerr_endline ("Don't know what to do with "^String.concat " " extras);
        prerr_endline "See -help for the list of supported options";
        exit 1
      end;
      init_toploop opts
    with any ->
      flush_all();
      fatal_error_exn any in
  Feedback.del_feeder init_feeder;
  if not opts.batch then custom.run ~opts ~state;
  exit 0
