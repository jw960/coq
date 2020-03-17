(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Names
open Constr

(** This module defines the funcionality for saving obligations and
   program-defined constants.

    Saving an obligation will register it with the kernel if opaque
   and modify its body; there are 4 entry points:

   - declare_obligation: called from the top-level search when an
     obligation is automatically solved by a tactic.
   - obligation_terminator: called at `Qed/Defined` time for an obligation.
   - obligation_admitted_terminator: called at `Admitted` time for an obligation.
   - admit_obligations: will admit all pending obligations

    Once all obligations have been saved, declare_definition will send the original
   constant to the kernel.

 *)

type 'a obligation_body

module Obligation : sig
  type t = private
    { obl_name : Id.t
    ; obl_type : types
    ; obl_location : Evar_kinds.t Loc.located
    ; obl_body : pconstant obligation_body option
    ; obl_status : bool * Evar_kinds.obligation_definition_status
    ; obl_deps : Int.Set.t
    ; obl_tac : unit Proofview.tactic option }

  val set_type : typ:Constr.types -> t -> t
  val defined : t -> bool
  val deps_remaining : t array -> t -> int list
end

type obligations = {obls : Obligation.t array; remaining : int}
type fixpoint_kind = IsFixpoint of lident option list | IsCoFixpoint

(* Information about a single [Program {Definition,Lemma,..}] declaration *)
module ProgramDecl : sig
  type t = private
    { prg_name : Id.t
    ; prg_body : constr
    ; prg_type : constr
    ; prg_ctx : UState.t
    ; prg_univdecl : UState.universe_decl
    ; prg_obligations : obligations
    ; prg_deps : Id.t list
    ; prg_fixkind : fixpoint_kind option
    ; prg_implicits : Impargs.manual_implicits
    ; prg_notations : Vernacexpr.decl_notation list
    ; prg_poly : bool
    ; prg_scope : DeclareDef.locality
    ; prg_kind : Decls.definition_object_kind
    ; prg_reduce : constr -> constr
    ; prg_hook : DeclareDef.Hook.t option
    ; prg_opaque : bool }

  val make :
       ?opaque:bool
    -> ?hook:DeclareDef.Hook.t
    -> Names.Id.t
    -> udecl:UState.universe_decl
    -> uctx:UState.t
    -> impargs:Impargs.manual_implicits
    -> poly:bool
    -> scope:DeclareDef.locality
    -> kind:Decls.definition_object_kind
    -> Constr.constr option
    -> Constr.types
    -> Names.Id.t list
    -> fixpoint_kind option
    -> Vernacexpr.decl_notation list
    -> RetrieveObl.obligation_info
    -> (Constr.constr -> Constr.constr)
    -> t

  val set_uctx : uctx:UState.t -> t -> t
end

(** [declare_obligation] Save an obligation *)
val declare_obligation :
     ProgramDecl.t
  -> Obligation.t
  -> Constr.types
  -> Constr.types option
  -> Entries.universes_entry
  -> bool * Obligation.t

module State : sig
  type t

  val empty : t
  val first_pending : t -> ProgramDecl.t option

  (** Returns [Error duplicate_list] if not a single program is open *)
  val get_unique_open_prog :
    t -> Id.t option -> (ProgramDecl.t, Id.t list) result

  (** Add a new obligation *)
  val add : t -> Id.t -> ProgramDecl.t -> t

  val fold : t -> f:(Id.t -> ProgramDecl.t -> 'a -> 'a) -> init:'a -> 'a
  val all : t -> ProgramDecl.t list
  val find : t -> Id.t -> ProgramDecl.t option

  (** checks no obligations remain, called at the end of sections *)
  val check_solved_obligations : pm:t -> msg:string -> unit

end

(** Called when all the obligations have been satisfied, closes the primary constant *)
val declare_definition : State.t -> ProgramDecl.t -> State.t * Names.GlobRef.t

(** Resolution status of a program *)
type progress =
  | Remain of int  (** n obligations remaining *)
  | Dependent  (** Dependent on other definitions *)
  | Defined of GlobRef.t  (** Defined as id *)

(** Admits all obligations *)
val admit_obligations : pm:State.t -> ProgramDecl.t -> State.t * progress

type obligation_resolver =
     State.t
  -> Id.t option
  -> Int.Set.t
  -> unit Proofview.tactic option
  -> State.t * progress

type obligation_qed_info = {name : Id.t; num : int; auto : obligation_resolver}

(** [obligation_terminator] part 2 of saving an obligation, proof mode *)
val obligation_terminator :
     State.t
  -> Evd.side_effects Declare.proof_entry list
  -> UState.t
  -> obligation_qed_info
  -> State.t

(** [obligation_hook] part 2 of saving an obligation, non-interactive mode *)
val obligation_admitted_terminator :
  State.t -> obligation_qed_info -> UState.t -> GlobRef.t -> State.t

(** [update_obls prg obls n progress] What does this do? *)
val update_obls :
  State.t -> ProgramDecl.t -> Obligation.t array -> int -> State.t * progress

(** { 2 Util }  *)

val subst_deps_obl : Obligation.t array -> Obligation.t -> Obligation.t
val dependencies : Obligation.t array -> int -> Int.Set.t

(* This is a hack to make it possible for Obligations to craft a Qed
 * behind the scenes.  The fix_exn the Stm attaches to the Future proof
 * is not available here, so we provide a side channel to get it *)
val stm_get_fix_exn : (unit -> Exninfo.iexn -> Exninfo.iexn) Hook.t
