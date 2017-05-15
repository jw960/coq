(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Names

type raw_cases_pattern_expr =
  | RCPatAlias of Loc.t * raw_cases_pattern_expr * Id.t
  | RCPatCstr of Loc.t * Globnames.global_reference
    * raw_cases_pattern_expr list * raw_cases_pattern_expr list
  (** [CPatCstr (_, c, l1, l2)] represents ((@c l1) l2) *)
  | RCPatAtom of Loc.t * Id.t option
  | RCPatOr of Loc.t * raw_cases_pattern_expr list

val raw_cases_pattern_expr_loc : raw_cases_pattern_expr -> Loc.t

(* The first argument denotes whether we are in an inductive pattern *)
val drop_notations_pattern :
  inductive_pattern:bool ->
  Notation.local_scopes ->
  Constrexpr.cases_pattern_expr ->
  raw_cases_pattern_expr
