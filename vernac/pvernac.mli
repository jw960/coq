(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Pcoq
open Genredexpr
open Vernacexpr

val uvernac : gram_universe

module Vernac_ :
  sig
    val gallina : vernac_expr Entry.t
    val gallina_ext : vernac_expr Entry.t
    val command : vernac_expr Entry.t
    val syntax : vernac_expr Entry.t
    val vernac_control : vernac_control Entry.t
    val rec_definition : (fixpoint_expr * decl_notation list) Entry.t
    val noedit_mode : vernac_expr Entry.t
    val command_entry : vernac_expr Entry.t
    val hint_info : Hints.hint_info_expr Entry.t

    val int_or_var : int Locus.or_var Entry.t
    val occs : Locus.occurrences_expr Entry.t
    val occs_nums : Locus.occurrences_expr Entry.t
    val pattern_occ : Constrexpr.constr_expr Locus.with_occurrences Entry.t
    val unfold_occ : r_cst Locus.with_occurrences Entry.t
    val ref_or_pattern_occ : (r_cst, r_trm) Util.union Locus.with_occurrences Entry.t
    val delta_flag : r_cst red_atom Entry.t
    val strategy_flag : r_cst glob_red_flag Entry.t
    val red_expr : raw_red_expr Entry.t
  end

(** The main entry: reads an optional vernac command *)
val main_entry : (Loc.t * vernac_control) option Entry.t

(** Handling of the proof mode entry *)
val get_command_entry : unit -> vernac_expr Entry.t
val set_command_entry : vernac_expr Entry.t -> unit
