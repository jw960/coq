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

type 'a obligation_body = DefinedObl of 'a | TermObl of constr

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
  val set_body : body:pconstant obligation_body -> t -> t
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
    -> ( Names.Id.t
       * Constr.types
       * Evar_kinds.t Loc.located
       * (bool * Evar_kinds.obligation_definition_status)
       * Int.Set.t
       * unit Proofview.tactic option )
       array
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
  val num_pending : t -> int
  val first_pending : t -> ProgramDecl.t option

  (** Returns [Error duplicate_list] if not a single program is open *)
  val get_unique_open_prog :
    t -> Id.t option -> (ProgramDecl.t, Id.t list) result

  (** Add a new obligation *)
  val add : t -> Id.t -> ProgramDecl.t -> t

  val fold : t -> f:(Id.t -> ProgramDecl.t -> 'a -> 'a) -> init:'a -> 'a

  val all : t -> ProgramDecl.t list

  val find : t -> Id.t -> ProgramDecl.t option
end

val declare_definition : State.t -> ProgramDecl.t -> State.t * Names.GlobRef.t

(** Resolution status of a program *)
type progress =
  | Remain of int  (** n obligations remaining *)
  | Dependent  (** Dependent on other definitions *)
  | Defined of GlobRef.t  (** Defined as id *)

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

(** Check obligations are properly solved before closing a section / module *)
val check_solved_obligations : pm:State.t -> msg:string -> unit

(** { 2 Util }  *)

val obl_substitution :
     bool
  -> Obligation.t array
  -> Int.Set.t
  -> (Id.t * (Constr.types * Constr.types)) list

val dependencies : Obligation.t array -> int -> Int.Set.t
val err_not_transp : unit -> unit
val progmap_add : Id.t -> ProgramDecl.t CEphemeron.key -> State.t -> State.t

(* This is a hack to make it possible for Obligations to craft a Qed
 * behind the scenes.  The fix_exn the Stm attaches to the Future proof
 * is not available here, so we provide a side channel to get it *)
val stm_get_fix_exn : (unit -> Exninfo.iexn -> Exninfo.iexn) Hook.t
