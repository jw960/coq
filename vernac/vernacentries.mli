(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Default proof mode set by `start_proof` *)
val get_default_proof_mode : unit -> Pvernac.proof_mode

val proof_mode_opt_name : string list

(** Vernacular entries *)
val vernac_require :
  Libnames.qualid option -> bool option -> Libnames.qualid list -> unit

(** The main interpretation function of vernacular expressions *)
val interp :
  ?verbosely:bool ->
  st:Vernacstate.t -> Vernacexpr.vernac_control -> Vernacstate.t

(** Prepare a "match" template for a given inductive type.
    For each branch of the match, we list the constructor name
    followed by enough pattern variables.
    [Not_found] is raised if the given string isn't the qualid of
    a known inductive type. *)

val make_cases : string -> string list list

(** [with_fail ~st f] runs [f ()] and expects it to fail, otherwise it fails. *)
val with_fail : st:Vernacstate.t -> (unit -> 'a) -> unit

val command_focus : unit Proof.focus_kind

val interp_redexp_hook : (Environ.env -> Evd.evar_map -> Genredexpr.raw_red_expr ->
  Evd.evar_map * Redexpr.red_expr) Hook.t

(* Flag set when the test-suite is called. Its only effect to display
   verbose information for `Fail` *)
val test_mode : bool ref
