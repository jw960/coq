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

(** Creating high-level proofs with an associated constant *)
module Proof_ending : sig

  type t =
    | Regular
    | End_obligation of DeclareObl.obligation_qed_info
    | End_derive of { f : Id.t; name : Id.t }
    | End_equations of { hook : Constant.t list -> Evd.evar_map -> unit
                       ; i : Id.t
                       ; types : (Environ.env * Evar.t * Evd.evar_info * EConstr.named_context * Evd.econstr) list
                       ; wits : EConstr.t list ref
                       ; sigma : Evd.evar_map
                       }

end

module Recthm : sig
  type t =
    { name : Id.t
    (** Name of theorem *)
    ; typ : EConstr.t
    (** Type of theorem  *)
    ; args : Name.t list
    (** Names to pre-introduce  *)
    ; impargs : Impargs.manual_implicits
    (** Explicitily declared implicit arguments  *)
    }
end

type lemma_possible_guards = int list list

module Info : sig

  type t =
    { hook : DeclareDef.Hook.t option
    ; compute_guard : lemma_possible_guards
    ; impargs : Impargs.manual_implicits
    ; proof_ending : Proof_ending.t CEphemeron.key
    (* This could be improved and the CEphemeron removed *)
    ; other_thms : Recthm.t list
    ; scope : DeclareDef.locality
    ; kind : Decls.logical_kind
    }

  val make
    :  ?hook: DeclareDef.Hook.t
    (** Callback to be executed at the end of the proof *)
    -> ?proof_ending : Proof_ending.t
    (** Info for special constants *)
    -> ?scope : DeclareDef.locality
    (** locality  *)
    -> ?kind:Decls.logical_kind
    (** Theorem, etc... *)
    -> unit
    -> t

end

type t =
  { proof : Proof_global.t
  ; info : Info.t
  }
(** [Lemmas.t] represents a constant that is being proved, usually
    interactively *)

(** {4 Proofs attached to a constant} *)

val set_endline_tactic : Genarg.glob_generic_argument -> t -> t
(** [set_endline_tactic tac lemma] set ending tactic for [lemma] *)

val pf_map : (Proof_global.t -> Proof_global.t) -> t -> t
(** [pf_map f l] map the underlying proof object *)

val pf_fold : (Proof_global.t -> 'a) -> t -> 'a
(** [pf_fold f l] fold over the underlying proof object *)

val by : unit Proofview.tactic -> t -> t * bool
(** [by tac l] apply a tactic to [l] *)

module Stack : sig

  type lemma = t
  type t

  val pop : t -> lemma * t option
  val push : t option -> lemma -> t

  val map_top : f:(lemma -> lemma) -> t -> t
  val map_top_pstate : f:(Proof_global.t -> Proof_global.t) -> t -> t

  val with_top : t -> f:(lemma -> 'a ) -> 'a
  val with_top_pstate : t -> f:(Proof_global.t -> 'a ) -> 'a

  val get_all_proof_names : t -> Names.Id.t list

  val copy_info : src:t -> tgt:t -> t
  (** Gets the current info without checking that the proof has been
     completed. Useful for the likes of [Admitted]. *)

end

(** Starts the proof of a constant *)
val start_lemma
  :  name:Id.t
  -> poly:bool
  -> ?udecl:UState.universe_decl
  -> ?info:Info.t
  -> Evd.evar_map
  -> EConstr.types
  -> t

val start_dependent_lemma
  :  name:Id.t
  -> poly:bool
  -> ?udecl:UState.universe_decl
  -> ?info:Info.t
  -> Proofview.telescope
  -> t

(** Pretty much internal, only used in ComFixpoint *)
val start_lemma_with_initialization
  :  ?hook:DeclareDef.Hook.t
  -> poly:bool
  -> scope:DeclareDef.locality
  -> kind:Decls.logical_kind
  -> udecl:UState.universe_decl
  -> Evd.evar_map
  -> (bool * lemma_possible_guards * unit Proofview.tactic list option) option
  -> Recthm.t list
  -> int list option
  -> t

val default_thm_id : Names.Id.t

(** Main [Lemma foo args : type.] command *)
val start_lemma_com
  :  program_mode:bool
  -> poly:bool
  -> scope:DeclareDef.locality
  -> kind:Decls.logical_kind
  -> ?inference_hook:Pretyping.inference_hook
  -> ?hook:DeclareDef.Hook.t
  -> Vernacexpr.proof_expr list
  -> t

(** {4 Saving proofs} *)

val save_lemma_admitted : lemma:t -> unit
val save_lemma_proved
  :  lemma:t
  -> opaque:Proof_global.opacity_flag
  -> idopt:Names.lident option
  -> unit

(** To be removed, don't use! *)
module Internal : sig
  val get_info : t -> Info.t
  (** Only needed due to the Proof_global compatibility layer. *)
end

(** Special cases for delayed proofs, in this case we must provide the
   proof information so the proof won't be forced. *)
val save_lemma_admitted_delayed : proof:Proof_global.proof_object -> info:Info.t -> unit
val save_lemma_proved_delayed
  :  proof:Proof_global.proof_object
  -> info:Info.t
  -> idopt:Names.lident option
  -> unit
