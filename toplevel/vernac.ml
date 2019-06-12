(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* Parsing of vernacular. *)

open Pp
open CErrors
open Util
open Vernacexpr
open Vernacprop

(* The functions in this module may raise (unexplainable!) exceptions.
   Use the module Coqtoplevel, which catches these exceptions
   (the exceptions are explained only at the toplevel). *)

let checknav_simple ({ CAst.loc; _ } as cmd) =
  if is_navigation_vernac cmd && not (is_reset cmd) then
    CErrors.user_err ?loc (str "Navigation commands forbidden in files.")

let checknav_deep ({ CAst.loc; _ } as cmd) =
  if is_deep_navigation_vernac cmd then
    CErrors.user_err ?loc (str "Navigation commands forbidden in nested commands.")

(* Echo from a buffer based on position.
   XXX: Should move to utility file. *)
let vernac_echo ?loc in_chan = let open Loc in
  Option.iter (fun loc ->
      let len = loc.ep - loc.bp in
      seek_in in_chan loc.bp;
      Feedback.msg_notice @@ str @@ really_input_string in_chan len
    ) loc

(* Re-enable when we get back to feedback printing *)
(* let is_end_of_input any = match any with *)
(*     Stm.End_of_input -> true *)
(*   | _ -> false *)

module State = struct

  type t = {
    doc : int;
    sid : int;
    proof : Proof.t option;
    time : bool;
  }

end

let stm_add ~doc:_ ~ontop:_ _ _ = 0, 0, `NewTip
let stm_observe ~doc _ = doc, None
let stm_edit_at ~doc:_ _ = ()
let stm_parse_sentence ~doc:_ ~entry:_ _ _ = None

let interp_vernac ~check ~interactive ~state ({CAst.loc;_} as com) =
  let open State in
    try
      (* The -time option is only supported from console-based clients
         due to the way it prints. *)
      let com = if state.time
        then begin
          CAst.make ?loc @@ VernacTime(state.time,com)
        end else com in
      let doc, nsid, ntip = stm_add ~doc:state.doc ~ontop:state.sid (not !Flags.quiet) com in

      (* Main STM interaction *)
      if ntip <> `NewTip then
        anomaly (str "vernac.ml: We got an unfocus operation on the toplevel!");

      (* Force the command  *)
      let ndoc, new_proof = if check then stm_observe ~doc nsid else doc, None in
      { state with doc = ndoc; sid = nsid; proof = new_proof; }
    with reraise ->
      (* XXX: In non-interactive mode edit_at seems to do very weird
         things, so we better avoid it while we investigate *)
      if interactive then ignore(stm_edit_at ~doc:state.doc state.sid);
      let (reraise, info) = CErrors.push reraise in
      let info = begin
        match Loc.get_loc info with
        | None   -> Option.cata (Loc.add_loc info) info loc
        | Some _ -> info
      end in iraise (reraise, info)

(* Load a vernac file. CErrors are annotated with file and location *)
let load_vernac_core ~echo ~check ~interactive ~state file =
  (* Keep in sync *)
  let in_chan = open_utf8_file_in file in
  let in_echo = if echo then Some (open_utf8_file_in file) else None in
  let input_cleanup () = close_in in_chan; Option.iter close_in in_echo in

  let in_pa =
    Pcoq.Parsable.make ~loc:(Loc.initial (Loc.InFile file))
      (Stream.of_channel in_chan) in
  let open State in

  (* ids = For beautify, list of parsed sids *)
  let rec loop state ids =
    match
      stm_parse_sentence
        ~doc:state.doc ~entry:Pvernac.main_entry state.sid in_pa
    with
    | None ->
      input_cleanup ();
      state, ids, Pcoq.Parsable.comment_state in_pa
    | Some ast ->
      (* Printing of AST for -compile-verbose *)
      Option.iter (vernac_echo ?loc:ast.CAst.loc) in_echo;

      checknav_simple ast;

      let state =
        Flags.silently (interp_vernac ~check ~interactive ~state) ast in

      loop state (state.sid :: ids)
  in
  try loop state []
  with any ->   (* whatever the exception *)
    let (e, info) = CErrors.push any in
    input_cleanup ();
    iraise (e, info)

let process_expr ~state loc_ast =
  checknav_deep loc_ast;
  interp_vernac ~interactive:true ~check:true ~state loc_ast

(******************************************************************************)
(* Main driver for file loading. For now, we only do one beautify
   pass. *)
let load_vernac ~echo ~check ~interactive ~state filename =
  let ostate, ids, comments = load_vernac_core ~echo ~check ~interactive ~state filename in
  (* End pass *)
  ostate
