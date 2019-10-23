(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Names
open Constr
open Entries

(** This module provides the official functions to declare new variables,
   parameters, constants and inductive types. Using the following functions
   will add the entries in the global environment (module [Global]), will
   register the declarations in the library (module [Lib]) --- so that the
   reset works properly --- and will fill some global tables such as
   [Nametab] and [Impargs]. *)

(** Proof entries *)
type 'a proof_entry = private {
  proof_entry_body   : 'a Entries.const_entry_body;
  (* List of section variables *)
  proof_entry_secctx : Id.Set.t option;
  (* State id on which the completion of type checking is reported *)
  proof_entry_feedback : Stateid.t option;
  proof_entry_type        : Constr.types option;
  proof_entry_universes   : Entries.universes_entry;
  proof_entry_opaque      : bool;
  proof_entry_inline_code : bool;
}

(** Declaration of local constructions (Variable/Hypothesis/Local) *)

type variable_declaration =
  | SectionLocalDef of Evd.side_effects proof_entry
  | SectionLocalAssum of { typ:types; impl:Glob_term.binding_kind; }

type 'a constant_entry =
  | DefinitionEntry of 'a proof_entry
  | ParameterEntry of parameter_entry
  | PrimitiveEntry of primitive_entry

val declare_variable
  :  name:variable
  -> kind:Decls.logical_kind
  -> variable_declaration
  -> unit

(** Declaration of global constructions
   i.e. Definition/Theorem/Axiom/Parameter/... *)

(* Default definition entries, transparent with no secctx or proj information *)
val definition_entry
  : ?fix_exn:Future.fix_exn
  -> ?opaque:bool
  -> ?inline:bool
  -> ?types:types
  -> ?univs:Entries.universes_entry
  -> ?eff:Evd.side_effects
  -> constr
  -> Evd.side_effects proof_entry

val pure_definition_entry
  : ?fix_exn:Future.fix_exn
  -> ?opaque:bool
  -> ?inline:bool
  -> ?types:types
  -> ?univs:Entries.universes_entry
  -> constr
  -> unit proof_entry

(* Delayed definition entries *)
val delayed_definition_entry
  :  ?opaque:bool
  -> ?inline:bool
  -> ?feedback_id:Stateid.t
  -> ?section_vars:Id.Set.t
  -> ?univs:Entries.universes_entry
  -> ?types:types
  -> 'a Entries.const_entry_body
  -> 'a proof_entry

type import_status = ImportDefaultBehavior | ImportNeedQualified

(** [declare_constant id cd] declares a global declaration
   (constant/parameter) with name [id] in the current section; it returns
   the full path of the declaration

  internal specify if the constant has been created by the kernel or by the
  user, and in the former case, if its errors should be silent *)
val declare_constant
  :  ?local:import_status
  -> name:Id.t
  -> kind:Decls.logical_kind
  -> Evd.side_effects constant_entry
  -> Constant.t

val declare_private_constant
  :  ?role:Evd.side_effect_role
  -> ?local:import_status
  -> name:Id.t
  -> kind:Decls.logical_kind
  -> unit proof_entry
  -> Constant.t * Evd.side_effects

(** Since transparent constants' side effects are globally declared, we
 *  need that *)
val set_declare_scheme :
  (string -> (inductive * Constant.t) array -> unit) -> unit

(** [declare_mind me] declares a block of inductive types with
   their constructors in the current section; it returns the path of
   the whole block and a boolean indicating if it is a primitive record. *)
val declare_mind : mutual_inductive_entry -> Libobject.object_name * bool

(** Declaration messages *)

val definition_message : Id.t -> unit
val assumption_message : Id.t -> unit
val fixpoint_message : int array option -> Id.t list -> unit
val cofixpoint_message : Id.t list -> unit
val recursive_message : bool (** true = fixpoint *) ->
  int array option -> Id.t list -> unit

val exists_name : Id.t -> bool

(** Global universe contexts, names and constraints *)
val declare_univ_binders : GlobRef.t -> UnivNames.universe_binders -> unit

val declare_universe_context : poly:bool -> Univ.ContextSet.t -> unit

val do_universe : poly:bool -> lident list -> unit
val do_constraint : poly:bool -> Glob_term.glob_constraint list -> unit

(* Used outside this module only in indschemes *)
exception AlreadyDeclared of (string option * Id.t)

(* For legacy support, do not use *)
module Internal : sig

  val map_entry_body : f:('a Entries.proof_output -> 'b Entries.proof_output) -> 'a proof_entry -> 'b proof_entry
  val map_entry_type : f:(Constr.t option -> Constr.t option) -> 'a proof_entry -> 'a proof_entry
  (* Overriding opacity is indeed really hacky *)
  val set_opacity : opaque:bool -> 'a proof_entry -> 'a proof_entry

  val shrink_entry : EConstr.named_context -> 'a proof_entry -> 'a proof_entry * Constr.constr list

end
