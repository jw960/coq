(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Declaration of local constructions (Variable/Hypothesis/Local) *)

type variable_declaration =
  | SectionLocalDef of Evd.side_effects Declare.proof_entry
  | SectionLocalAssum of { typ:Constr.types; impl:Glob_term.binding_kind; }

val declare_variable
  :  name:Names.variable
  -> kind:Decls.logical_kind
  -> variable_declaration
  -> unit

val assumption_message : Names.Id.t -> unit
