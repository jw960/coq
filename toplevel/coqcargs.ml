(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

type t =
  { compile_list: (string * bool) list  (* bool is verbosity  *)
  ; compilation_output_name : string option

  ; echo : bool

  ; outputstate : string option;
  }

let default =
  { compile_list = []
  ; compilation_output_name = None

  ; echo = false

  ; outputstate = None
  }

let depr opt =
  Feedback.msg_warning Pp.(seq[str "Option "; str opt; str " is a noop and deprecated"])

(* XXX Remove this duplication with Coqargs *)
let fatal_error exn =
  Topfmt.(in_phase ~phase:ParsingCommandLine print_err_exn exn);
  let exit_code = if CErrors.(is_anomaly exn || not (handled exn)) then 129 else 1 in
  exit exit_code

let error_missing_arg s =
  prerr_endline ("Error: extra argument expected after option "^s);
  prerr_endline "See -help for the syntax of supported options";
  exit 1

let check_compilation_output_name_consistency args =
  match args.compilation_output_name, args.compile_list with
  | Some _, _::_::_ ->
    prerr_endline ("Error: option -o is not valid when more than one");
    prerr_endline ("file have to be compiled")
  | _ -> ()

let is_dash_argument s = String.length s > 0 && s.[0] = '-'

let add_compile ?echo copts s =
  if is_dash_argument s then (prerr_endline ("Unknown option " ^ s); exit 1);
  (* make the file name explicit; needed not to break up Coq loadpath stuff. *)
  let echo = Option.default copts.echo echo in
  let s =
    let open Filename in
    if is_implicit s
    then concat current_dir_name s
    else s
  in
  { copts with compile_list = (s,echo) :: copts.compile_list }

let warn_deprecated_outputstate =
  CWarnings.create ~name:"deprecated-outputstate" ~category:"deprecated"
         (fun () ->
          Pp.strbrk "The outputstate option is deprecated and discouraged.")

let set_outputstate opts s =
  warn_deprecated_outputstate ();
  { opts with outputstate = Some s }

let parse arglist : t =
  let echo = ref false in
  let args = ref arglist in
  let extras = ref [] in
  let rec parse (oval : t) = match !args with
    | [] ->
      (oval, List.rev !extras)
    | opt :: rem ->
      args := rem;
      let next () = match !args with
        | x::rem -> args := rem; x
        | [] -> error_missing_arg opt
      in
      let noval : t = begin match opt with
        (* Deprecated options *)
        | "-opt"
        | "-byte" as opt ->
          depr opt;
          oval
        | "-image" as opt ->
          depr opt;
          let _ = next () in
          oval
        (* Verbose == echo mode *)
        | "-verbose" ->
          echo := true;
          oval
        (* Output filename *)
        | "-o" ->
          { oval with compilation_output_name = Some (next ()) }

        | "-outputstate" ->
          set_outputstate oval (next ())

        | s ->
          extras := s :: !extras;
          oval
      end in
      parse noval
  in
  try
    let opts, extra = parse default in
    let args = List.fold_left add_compile opts extra in
    check_compilation_output_name_consistency args;
    args
  with any -> fatal_error any
