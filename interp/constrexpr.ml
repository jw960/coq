(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Names
open Libnames
open Misctypes
open Decl_kinds

(** {6 Concrete syntax for terms } *)

(** [constr_expr] is the abstract syntax tree produced by the parser *)

type notation = string

type explicitation =
  | ExplByPos of int * Id.t option
  | ExplByName of Id.t

type binder_kind =
  | Default of binding_kind
  | Generalized of binding_kind * binding_kind * bool
      (** Inner binding, outer bindings, typeclass-specific flag
	 for implicit generalization of superclasses *)

type abstraction_kind = AbsLambda | AbsPi

type proj_flag = int option (** [Some n] = proj of the n-th visible argument *)

type prim_token =
  | Numeral of Bigint.bigint (** representation of integer literals that appear in Coq scripts. *)
  | String of string

type raw_cases_pattern_expr_r =
  | RCPatAlias of raw_cases_pattern_expr * Id.t
  | RCPatCstr  of Globnames.global_reference
    * raw_cases_pattern_expr list * raw_cases_pattern_expr list
  (** [CPatCstr (_, c, l1, l2)] represents ((@c l1) l2) *)
  | RCPatAtom of Id.t option
  | RCPatOr of raw_cases_pattern_expr list
and raw_cases_pattern_expr = raw_cases_pattern_expr_r CAst.ast

type instance_expr = Misctypes.glob_level list

type cases_pattern_expr_r =
  | CPatAlias of cases_pattern_expr * Id.t
  | CPatCstr  of reference
    * cases_pattern_expr list option * cases_pattern_expr list
  (** [CPatCstr (_, c, Some l1, l2)] represents (@c l1) l2 *)
  | CPatAtom of reference option
  | CPatOr   of cases_pattern_expr list
  | CPatNotation of notation * cases_pattern_notation_substitution
    * cases_pattern_expr list (** CPatNotation (_, n, l1 ,l2) represents
				  (notation n applied with substitution l1)
				  applied to arguments l2 *)
  | CPatPrim   of prim_token
  | CPatRecord of (reference * cases_pattern_expr) list
  | CPatDelimiters of string * cases_pattern_expr
  | CPatCast   of cases_pattern_expr * constr_expr
and cases_pattern_expr = cases_pattern_expr_r CAst.ast

and cases_pattern_notation_substitution =
    cases_pattern_expr list *     (** for constr subterms *)
    cases_pattern_expr list list  (** for recursive notations *)

and ('var, 't) constr_expr_r =
  (* | CPrim of prim_token *)
  | CRef    of var
  | CEvar   of existential_name * (Id.t * 't) list
  | CPatVar of patvar
  | CApp    of (proj_flag * 't) *
                ('t * explicitation Loc.located option) list

  | CLambdaN of (Name.t Loc.located list * binder_kind) list * t * 't

  | CProdN   of (Name.t Loc.located list * binder_kind) list * 't

  | CLetIn   of Name.t Loc.located * 't * 't option * 't

  | CLetTuple of Name.t Loc.located list * (Name.t Loc.located option * 't option) * 't * 't

  | CIf of 't * (Name.t Loc.located option * 't option) * 't * 't

  | CSort   of glob_sort

  | CHole   of Evar_kinds.t option * intro_pattern_naming_expr * Genarg.raw_generic_argument option

  | CCast   of 't * 't cast_type

  (* This is modified by globalization *)
  | CFix     of Id.t Loc.located * fix_expr list
  | CCoFix   of Id.t Loc.located * cofix_expr list

  (* This is modified by globalization *)
  | CCases of case_style
    * 't option
    * ('t * Name.t option * cases_pattern_expr option) list
    * (cases_pattern_expr list list * 't) list

  (* Proper to glob_constr *)
  | CAppExpl of (proj_flag * reference * instance_expr option) * constr_expr list
  | CRecord  of (reference * 't) list
  | CNotation of notation * constr_notation_substitution
  | CDelimiters of string * 't
  | CGeneralization of binding_kind * abstraction_kind option * 't


and constr_expr = constr_expr_r CAst.ast

and case_expr = constr_expr                 (* expression that is being matched *)
	      * Name.t Loc.located option   (* as-clause *)
	      * cases_pattern_expr option   (* in-clause *)

and branch_expr =
  (cases_pattern_expr list Loc.located list * constr_expr) Loc.located


and fix_expr =
    Id.t Loc.located * (Id.t Loc.located option * recursion_order_expr) *
      local_binder_expr list * constr_expr * constr_expr

and cofix_expr =
    Id.t Loc.located * local_binder_expr list * constr_expr * constr_expr

and recursion_order_expr =
  | CStructRec
  | CWfRec of constr_expr
  | CMeasureRec of constr_expr * constr_expr option (** measure, relation *)

(** Anonymous defs allowed ?? *)
and local_binder_expr =
  | CLocalAssum   of Name.t Loc.located list * binder_kind * constr_expr
  | CLocalDef     of Name.t Loc.located * constr_expr * constr_expr option
  | CLocalPattern of (cases_pattern_expr * constr_expr option) Loc.located

and constr_notation_substitution =
    constr_expr list *      (** for constr subterms *)
    constr_expr list list * (** for recursive notations *)
    local_binder_expr list list (** for binders subexpressions *)

type typeclass_constraint = (Name.t Loc.located * Id.t Loc.located list option) * binding_kind * constr_expr

and typeclass_context = typeclass_constraint list

type constr_pattern_expr = constr_expr

(** Concrete syntax for modules and module types *)

type with_declaration_ast =
  | CWith_Module of Id.t list Loc.located * qualid Loc.located
  | CWith_Definition of Id.t list Loc.located * constr_expr

type module_ast_r =
  | CMident of qualid
  | CMapply of module_ast * module_ast
  | CMwith  of module_ast * with_declaration_ast
and module_ast = module_ast_r CAst.ast
