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

let print_emacs = ref false

let top_stderr x =
  Format.fprintf !Topfmt.err_ft "@[%a@]%!" pp_with x

(* A buffer for the character read from a channel. We store the command
 * entered to be able to report errors without pretty-printing. *)

type input_buffer = {
  mutable prompt : Stm.doc -> string;
  mutable str : Bytes.t; (* buffer of already read characters *)
  mutable len : int;    (* number of chars in the buffer *)
  mutable bols : int list; (* offsets in str of beginning of lines *)
  mutable tokens : Pcoq.Parsable.t; (* stream of tokens *)
  mutable start : int } (* stream count of the first char of the buffer *)

(* Double the size of the buffer. *)

let resize_buffer ibuf = let open Bytes in
  let nstr = create (2 * length ibuf.str + 1) in
  blit ibuf.str 0 nstr 0 (length ibuf.str);
  ibuf.str <- nstr

(* Delete all irrelevant lines of the input buffer. Keep the last line
   in the buffer (useful when there are several commands on the same line). *)

let resynch_buffer ibuf =
  match ibuf.bols with
    | ll::_ ->
        let new_len = ibuf.len - ll in
        Bytes.blit ibuf.str ll ibuf.str 0 new_len;
        ibuf.len <- new_len;
        ibuf.bols <- [];
        ibuf.start <- ibuf.start + ll
    | _ -> ()


(* emacs special prompt tag for easy detection. No special character,
   to avoid interfering with utf8. Compatibility code removed. *)
let emacs_prompt_startstring () = if !print_emacs then "<prompt>"  else ""
let emacs_prompt_endstring   () = if !print_emacs then "</prompt>" else ""

(* Read a char in an input channel, displaying a prompt at every
   beginning of line. *)
let prompt_char doc ic ibuf count =
  let bol = match ibuf.bols with
    | ll::_ -> Int.equal ibuf.len ll
    | [] -> Int.equal ibuf.len 0
  in
  if bol && not !print_emacs then top_stderr (str (ibuf.prompt doc));
  try
    let c = input_char ic in
    if c == '\n' then ibuf.bols <- (ibuf.len+1) :: ibuf.bols;
    if ibuf.len == Bytes.length ibuf.str then resize_buffer ibuf;
    Bytes.set ibuf.str ibuf.len c;
    ibuf.len <- ibuf.len + 1;
    Some c
  with End_of_file ->
    None

(* Reinitialize the char stream (after a Drop) *)

let reset_input_buffer doc ic ibuf =
  ibuf.str <- Bytes.empty;
  ibuf.len <- 0;
  ibuf.bols <- [];
  ibuf.tokens <- Pcoq.Parsable.make (Stream.from (prompt_char doc ic ibuf));
  ibuf.start <- 0


(*s The Coq prompt is the name of the focused proof, if any, and "Coq"
    otherwise. We trap all exceptions to prevent the error message printing
    from cycling. *)
let make_prompt () =
  try
    (Names.Id.to_string (Vernacstate.Proof_global.get_current_proof_name ())) ^ " < "
  with Vernacstate.Proof_global.NoCurrentProof ->
    "Coq < "

(* the coq prompt added to the default one when in emacs mode
   The prompt contains the current state label [n] (for global
   backtracking) and the current proof state [p] (for proof
   backtracking) plus the list of open (nested) proofs (for proof
   aborting when backtracking). It looks like:

   "n |lem1|lem2|lem3| p < "
*)
let make_emacs_prompt doc =
  let statnum = Stateid.to_string (Stm.get_current_state ~doc) in
  let dpth = Stm.current_proof_depth ~doc in
  let pending = Stm.get_all_proof_names ~doc in
  let pendingprompt =
    List.fold_left
      (fun acc x -> acc ^ (if CString.is_empty acc then "" else "|") ^ Names.Id.to_string x)
      "" pending in
  let proof_info = if dpth >= 0 then string_of_int dpth else "0" in
  if !print_emacs then statnum ^ " |" ^ pendingprompt ^ "| " ^ proof_info ^ " < "
  else ""

(* A buffer to store the current command read on stdin. It is
 * initialized when a vernac command is immediately followed by "\n",
 * or after a Drop. *)
let top_buffer =
  let pr doc =
    emacs_prompt_startstring()
    ^ make_prompt()
    ^ make_emacs_prompt doc
    ^ emacs_prompt_endstring()
  in
  { prompt = pr;
    str = Bytes.empty;
    len = 0;
    bols = [];
    tokens = Pcoq.Parsable.make (Stream.of_list []);
    start = 0 }

let set_prompt prompt =
  top_buffer.prompt
  <- (fun doc ->
    emacs_prompt_startstring()
    ^ prompt ()
    ^ emacs_prompt_endstring())

(* Read the input stream until a dot is encountered *)
let parse_to_dot =
  let rec dot st = match Stream.next st with
    | Tok.KEYWORD ("."|"...") -> ()
    | Tok.EOI -> ()
    | _ -> dot st
  in
  Pcoq.Entry.of_parser "Coqtoplevel.dot" dot

(* If an error occurred while parsing, we try to read the input until a dot
   token is encountered.
   We assume that when a lexer error occurs, at least one char was eaten *)

let rec discard_to_dot () =
  try
    Pcoq.Entry.parse parse_to_dot top_buffer.tokens
  with
    | CLexer.Error.E _ -> discard_to_dot ()
    | e when CErrors.noncritical e -> ()

let read_sentence ~state input =
  (* XXX: careful with ignoring the state Eugene!*)
  let open Vernac.State in
  try Stm.parse_sentence ~doc:state.doc state.sid ~entry:G_toplevel.vernac_toplevel input
  with reraise ->
    let reraise = CErrors.push reraise in
    discard_to_dot ();
    (* The caller of read_sentence does the error printing now, this
       should be re-enabled once we rely on the feedback error
       printer again *)
    (* TopErr.print_toplevel_parse_error reraise top_buffer; *)
    Exninfo.iraise reraise

(** Main coq loop : read vernacular expressions until Drop is entered.
    Ctrl-C is handled internally as Sys.Break instead of aborting Coq.
    Normally, the only exceptions that can come out of [do_vernac] and
    exit the loop are Drop and Quit. Any other exception there indicates
    an issue with [print_toplevel_error] above. *)

(* Flush in a compatible order with 8.5 *)
(* This mimics the semantics of the old Pp.flush_all *)
let loop_flush_all () =
  Pervasives.flush stderr;
  Pervasives.flush stdout;
  Format.pp_print_flush !Topfmt.std_ft ();
  Format.pp_print_flush !Topfmt.err_ft ()

(* Goal equality heuristic. *)
let pequal cmp1 cmp2 (a1,a2) (b1,b2) = cmp1 a1 b1 && cmp2 a2 b2
let evleq e1 e2 = CList.equal Evar.equal e1 e2
let cproof p1 p2 =
  let Proof.{goals=a1;stack=a2;shelf=a3;given_up=a4} = Proof.data p1 in
  let Proof.{goals=b1;stack=b2;shelf=b3;given_up=b4} = Proof.data p2 in
  evleq a1 b1 &&
  CList.equal (pequal evleq evleq) a2 b2 &&
  CList.equal Evar.equal a3 b3 &&
  CList.equal Evar.equal a4 b4

let drop_last_doc = ref None

(* todo: could add other Set/Unset commands, such as "Printing Universes" *)
let print_anyway_opts = [
  [ "Diffs" ];
  ]

let print_anyway c =
  let open Vernacexpr in
  match c with
  | VernacExpr (_, VernacSetOption (_, opt, _))
  | VernacExpr (_, VernacUnsetOption (_, opt)) ->
    List.mem opt print_anyway_opts
  | _ -> false

(* We try to behave better when goal printing raises an exception
   [usually Ctrl-C]

   This is mostly a hack as we should protect printing in a more
   generic way, but that'll do for now *)
let top_goal_print ~doc c oldp newp =
  try
    let proof_changed = not (Option.equal cproof oldp newp) in
    let print_goals = proof_changed && Vernacstate.Proof_global.there_are_pending_proofs () ||
                      print_anyway c in
    if not !Flags.quiet && print_goals then begin
      let dproof = Stm.get_prev_proof ~doc (Stm.get_current_state ~doc) in
      Printer.print_and_diff dproof newp
    end
  with
  | exn ->
    let (e, info) = CErrors.push exn in
    let loc = Loc.get_loc info in
    let msg = CErrors.iprint (e, info) in
    TopErr.print_error_for_buffer ?loc Feedback.Error msg top_buffer

let exit_on_error =
  let open Goptions in
  declare_bool_option_and_ref ~depr:false ~name:"coqtop-exit-on-error" ~key:["Coqtop";"Exit";"On";"Error"]
    ~value:false

let rec vernac_loop ~state =
  let open CAst in
  let open Vernac.State in
  let open G_toplevel in
  loop_flush_all ();
  top_stderr (fnl());
  if !print_emacs then top_stderr (str (top_buffer.prompt state.doc));
  resynch_buffer top_buffer;
  (* execute one command *)
  try
    let input = top_buffer.tokens in
    match read_sentence ~state input with
    | Some { v = VernacBacktrack(bid,_,_) } ->
      let bid = Stateid.of_int bid in
      let doc, res = Stm.edit_at ~doc:state.doc bid in
      assert (res = `NewTip);
      let state = { state with doc; sid = bid } in
      vernac_loop ~state

    | Some { v = VernacQuit } ->
      exit 0

    | Some { v = VernacDrop } ->
      if Mltop.is_ocaml_top()
      then (drop_last_doc := Some state; state)
      else (Feedback.msg_warning (str "There is no ML toplevel."); vernac_loop ~state)

    | Some { v = VernacControl c; loc } ->
      let nstate = Vernac.process_expr ~state (make ?loc c) in
      top_goal_print ~doc:state.doc c state.proof nstate.proof;
      vernac_loop ~state:nstate

    | None ->
      top_stderr (fnl ()); exit 0

  with
  (* Exception printing should be done by the feedback listener,
     however this is not yet ready so we rely on the exception for
     now. *)
  | any ->
    let (e, info) = CErrors.push any in
    let loc = Loc.get_loc info in
    let msg = CErrors.iprint (e, info) in
    TopErr.print_error_for_buffer ?loc Feedback.Error msg top_buffer;
    if exit_on_error () then exit 1;
    vernac_loop ~state

let rec loop ~state =
  let open Vernac.State in
  Sys.catch_break true;
  try
    reset_input_buffer state.doc stdin top_buffer;
    vernac_loop ~state
  with
    | any ->
      top_stderr
        (hov 0 (str "Anomaly: main loop exited with exception:" ++ spc () ++
                str (Printexc.to_string any)) ++ spc () ++
         hov 0 (str "Please report at " ++ str Coq_config.wwwbugtracker ++ str "."));
      loop ~state

(* Default toplevel loop *)
let warning s = Flags.(with_option warn Feedback.msg_warning (strbrk s))

let drop_args = ref None
let loop ~opts ~state =
  drop_args := Some opts;
  let open Coqargs in
  print_emacs := opts.print_emacs;
  (* We initialize the console only if we run the toploop_run *)
  let tl_feed = Feedback.add_feeder Topfmt.console_feed in
  if Dumpglob.dump () then begin
    Flags.if_verbose warning "Dumpglob cannot be used in interactive mode.";
    Dumpglob.noglob ()
  end;
  let _ = loop ~state in
  (* Initialise and launch the Ocaml toplevel *)
  Coqinit.init_ocaml_path();
  Mltop.ocaml_toploop();
  (* We delete the feeder after the OCaml toploop has ended so users
     of Drop can see the feedback. *)
  Feedback.del_feeder tl_feed
