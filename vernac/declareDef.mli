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

type locality = Discharge | Global of Declare.import_status

(** Declaration hooks *)
module Hook : sig
  type t

  (** Hooks allow users of the API to perform arbitrary actions at
     proof/definition saving time. For example, to register a constant
     as a Coercion, perform some cleanup, update the search database,
     etc...  *)
  module S : sig
    type t =
      { uctx : UState.t
      (** [ustate]: universe constraints obtained when the term was closed *)
      ; obls : (Id.t * Constr.t) list
      (** [(n1,t1),...(nm,tm)]: association list between obligation
          name and the corresponding defined term (might be a constant,
          but also an arbitrary term in the Expand case of obligations) *)
      ; scope : locality
      (** [scope]: Locality of the original declaration *)
      ; dref : GlobRef.t
      (** [dref]: identifier of the original declaration *)
      }
  end

  val make : (S.t -> unit) -> t
  val call : ?hook:t -> S.t -> unit
end

(** Declare an interactively-defined constant *)
val declare_entry
  :  name:Id.t
  -> scope:locality
  -> kind:Decls.logical_kind
  -> ?hook:Hook.t
  -> ?obls:(Id.t * Constr.t) list
  -> impargs:Impargs.manual_implicits
  -> uctx:UState.t
  -> Evd.side_effects Declare.proof_entry
  -> GlobRef.t

module Info : sig

  type t

  val make :
    ?poly:bool
    -> ?opaque : bool
    -> ?inline : bool
    -> ?kind : Decls.logical_kind
    -> ?udecl : UState.universe_decl
    -> ?scope : locality
    -> ?impargs : Impargs.manual_implicits
    -> ?hook : Hook.t
    -> ?obls : (Id.t * Constr.t) list
    -> ?fix_exn : (Exninfo.iexn -> Exninfo.iexn)
    -> unit -> t

end

(** Declares a non-interactive constant; [body] and [types] will be
   normalized w.r.t. the passed [evar_map] [sigma]. Universes should
   be handled properly, including minimization and restriction. Note
   that [sigma] is checked for unresolved evars, thus you should be
   careful not to submit open terms or evar maps with stale,
   unresolved existentials *)
val declare_definition
  : name:Id.t
  -> info:Info.t
  -> types:EConstr.t option
  -> body:EConstr.t
  -> Evd.evar_map
  -> GlobRef.t

val declare_assumption
  :  ?fix_exn:(Exninfo.iexn -> Exninfo.iexn)
  -> name:Id.t
  -> scope:locality
  -> hook:Hook.t option
  -> impargs:Impargs.manual_implicits
  -> uctx:UState.t
  -> Entries.parameter_entry
  -> GlobRef.t

module Recthm : sig
  type t =
    { name : Id.t
    (** Name of theorem *)
    ; typ : Constr.t
    (** Type of theorem  *)
    ; args : Name.t list
    (** Names to pre-introduce  *)
    ; impargs : Impargs.manual_implicits
    (** Explicitily declared implicit arguments  *)
    }
end

val declare_mutually_recursive
  : info:Info.t
  -> ntns:Vernacexpr.decl_notation list
  -> uctx:UState.t
  -> possible_indexes:int list list option
  -> rec_declaration:Constr.rec_declaration
  -> ?restrict_ucontext:bool
  (** XXX: restrict_ucontext should be always true, this seems like a
     bug in obligations, so this parameter should go away *)
  -> Recthm.t list
  -> Names.GlobRef.t list

val prepare_obligation
  :  ?opaque:bool
  -> ?inline:bool
  -> name:Id.t
  -> poly:bool
  -> udecl:UState.universe_decl
  -> types:EConstr.t option
  -> body:EConstr.t
  -> Evd.evar_map
  -> Constr.constr * Constr.types * UState.t * RetrieveObl.obligation_info

val prepare_parameter
  : poly:bool
  -> udecl:UState.universe_decl
  -> types:EConstr.types
  -> Evd.evar_map
  -> Evd.evar_map * Entries.parameter_entry
