(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Vernacular Extension data *)

(** Interpretation of extended vernac phrases. *)

type typed_vernac =
  | VtDefault of (unit -> unit)
  | VtNoProof of (unit -> unit)
  | VtCloseProof of (lemma:Lemmas.t -> unit)
  | VtOpenProof of (unit -> Lemmas.t)
  | VtModifyProof of (pstate:Proof_global.t -> Proof_global.t)
  | VtReadProofOpt of (pstate:Proof_global.t option -> unit)
  | VtReadProof of (pstate:Proof_global.t -> unit)

type vernac_command = atts:Attributes.vernac_flags -> typed_vernac

type plugin_args = Genarg.raw_generic_argument list

val type_vernac : Vernacexpr.extend_name -> plugin_args -> vernac_command

(** {5 VERNAC EXTEND} *)

type _ ty_sig =
| TyNil : vernac_command ty_sig
| TyTerminal : string * 'r ty_sig -> 'r ty_sig
| TyNonTerminal : ('a, 'b, 'c) Extend.ty_user_symbol * 'r ty_sig -> ('a -> 'r) ty_sig

type ty_ml = TyML : bool (* deprecated *) * 'r ty_sig * 'r -> ty_ml

(** Wrapper to dynamically extend vernacular commands. *)
val vernac_extend :
  command:string ->
  ?entry:Vernacexpr.vernac_expr Pcoq.Entry.t ->
  ty_ml list -> unit

(** {5 VERNAC ARGUMENT EXTEND} *)

type 'a argument_rule =
| Arg_alias of 'a Pcoq.Entry.t
  (** This is used because CAMLP5 parser can be dumb about rule factorization,
      which sometimes requires two entries to be the same. *)
| Arg_rules of 'a Extend.production_rule list
  (** There is a discrepancy here as we use directly extension rules and thus
    entries instead of ty_user_symbol and thus arguments as roots. *)

type 'a vernac_argument = {
  arg_printer : Environ.env -> Evd.evar_map -> 'a -> Pp.t;
  arg_parsing : 'a argument_rule;
}

val vernac_argument_extend : name:string -> 'a vernac_argument ->
  ('a, unit, unit) Genarg.genarg_type * 'a Pcoq.Entry.t
