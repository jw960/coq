(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Vernacexpr

(** Special Fixpoint handling when command is activated. *)
val do_fixpoint :
     pm:DeclareObl.State.t (* When [false], assume guarded. *)
  -> scope:DeclareDef.locality
  -> poly:bool
  -> fixpoint_expr list
  -> DeclareObl.State.t

val do_cofixpoint :
     pm:DeclareObl.State.t (* When [false], assume guarded. *)
  -> scope:DeclareDef.locality
  -> poly:bool
  -> cofixpoint_expr list
  -> DeclareObl.State.t
