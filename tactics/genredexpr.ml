(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Reduction expressions *)

(** The parsing produces initially a list of [red_atom] *)

type 'a red_atom =
  | FBeta
  | FMatch
  | FFix
  | FCofix
  | FZeta
  | FConst of 'a list
  | FDeltaBut of 'a list

(** This list of atoms is immediately converted to a [glob_red_flag] *)

type 'a glob_red_flag = {
  rBeta : bool;
  rMatch : bool;
  rFix : bool;
  rCofix : bool;
  rZeta : bool;
  rDelta : bool; (** true = delta all but rConst; false = delta only on rConst*)
  rConst : 'a list
}

(** Generic kinds of reductions *)

type ('a,'b,'c) red_expr_gen =
  | Red of bool
  | Hnf
  | Simpl of 'b glob_red_flag*('b,'c) Util.union Locus.with_occurrences option
  | Cbv of 'b glob_red_flag
  | Cbn of 'b glob_red_flag
  | Lazy of 'b glob_red_flag
  | Unfold of 'b Locus.with_occurrences list
  | Fold of 'a list
  | Pattern of 'a Locus.with_occurrences list
  | ExtraRedExpr of string
  | CbvVm of ('b,'c) Util.union Locus.with_occurrences option
  | CbvNative of ('b,'c) Util.union Locus.with_occurrences option

type ('a,'b,'c) may_eval =
  | ConstrTerm of 'a
  | ConstrEval of ('a,'b,'c) red_expr_gen * 'a
  | ConstrContext of Names.lident * 'a
  | ConstrTypeOf of 'a

open Libnames
open Constrexpr

type r_trm = constr_expr
type r_pat = constr_pattern_expr
type r_cst = qualid or_by_notation

type raw_red_expr = (r_trm, r_cst, r_pat) red_expr_gen

let make0 ?dyn name =
  let wit = Genarg.make0 name in
  let () = Geninterp.register_val0 wit dyn in
  wit

type 'a and_short_name = 'a * Names.lident option

let wit_red_expr :
  ((constr_expr,qualid or_by_notation,constr_expr) red_expr_gen,
   (Genintern.glob_constr_and_expr,Names.evaluable_global_reference and_short_name Locus.or_var,Genintern.glob_constr_pattern_and_expr) red_expr_gen,
   (EConstr.t,Names.evaluable_global_reference,Pattern.constr_pattern) red_expr_gen)
    Genarg.genarg_type =
  make0 "redexpr"

open CAst
open Names
open Smartlocate
open Locus
open Tacred
open Genintern

let short_name ~strict = function
  | {v=AN qid} when qualid_is_ident qid && not strict ->
    Some (make ?loc:qid.CAst.loc @@ qualid_basename qid)
  | _ -> None

(* Globalize a reduction expression *)
let intern_evaluable_global_reference ~strict ist qid =
  try evaluable_of_global_reference ist.genv (locate_global_with_alias ~head:true qid)
  with Not_found ->
  if qualid_is_ident qid && not strict then EvalVarRef (qualid_basename qid)
  else Nametab.error_global_not_found qid

let intern_evaluable_reference_or_by_notation ~strict ist = let open CAst in function
  | {v=AN r} -> intern_evaluable_global_reference ~strict ist r
  | {v=ByNotation (ntn,sc);loc} ->
    let open GlobRef in
    evaluable_of_global_reference ist.genv
      (Notation.interp_notation_as_global_reference ?loc
         (function ConstRef _ | VarRef _ -> true | _ -> false) ntn sc)

let intern_evaluable ~strict ist r =
  let f ist r =
    let e = intern_evaluable_reference_or_by_notation ~strict ist r in
    let na = short_name ~strict r in
    ArgArg (e,na)
  in
  match r with
  | {v=AN qid} when qualid_is_ident qid && find_var (qualid_basename qid) ist ->
    ArgVar (make ?loc:qid.CAst.loc @@ qualid_basename qid)
  | {v=AN qid} when qualid_is_ident qid && not strict && find_hyp (qualid_basename qid) ist ->
    let id = qualid_basename qid in
    ArgArg (EvalVarRef id, Some (make ?loc:qid.CAst.loc id))
  | _ -> f ist r

let intern_flag ~strict ist red =
  { red with rConst = List.map (intern_evaluable ~strict ist) red.rConst }

let intern_unfold ~strict ist (l,qid) = (l,intern_evaluable ~strict ist qid)

let dummy_pat = Pattern.PRel 0
let intern_constr = Constrintern.intern_constr_gen false false
let intern_type = Constrintern.intern_constr_gen false true
let intern_constr_with_occurrences ~strict ist (l,c) = (l,intern_constr ~strict ist c)

let intern_typed_pattern ~strict ist ~as_type ~ltacvars p =
  (* we cannot ensure in non strict mode that the pattern is closed *)
  (* keeping a constr_expr copy is too complicated and we want anyway to *)
  (* type it, so we remember the pattern as a glob_constr only *)
  let metas,pat =
    if strict then
      let ltacvars = {
          Constrintern.ltac_vars = ltacvars;
          ltac_bound = Id.Set.empty;
          ltac_extra = ist.extra;
        } in
      Constrintern.intern_constr_pattern ist.genv Evd.(from_env ist.genv) ~as_type ~ltacvars p
    else
      [], dummy_pat in
  let (glob,_ as c) = Constrintern.intern_constr_gen ~strict true false ist p in
  let bound_names = Glob_ops.bound_glob_vars glob in
  metas,(bound_names,c,pat)

let intern_typed_pattern_or_ref_with_occurrences ~strict ist (l,p) =
  let open Util in
  let interp_ref r =
    try Inl (intern_evaluable ~strict ist r)
    with e when Proofview.V82.catchable_exception e ->
      (* Compatibility. In practice, this means that the code above
         is useless. Still the idea of having either an evaluable
         ref or a pattern seems interesting, with "head" reduction
         in case of an evaluable ref, and "strong" reduction in the
         subterm matched when a pattern *)
      let r = match r with
      | {v=AN r} -> r
      | {loc} -> (qualid_of_path ?loc (Nametab.path_of_global (smart_global r))) in
      let sign = {
        Constrintern.ltac_vars = ist.ltacvars;
        ltac_bound = Id.Set.empty;
        ltac_extra = ist.extra;
      } in
      let c = Constrintern.interp_reference sign r in
      let open GlobRef in
      let open Glob_term in
      match DAst.get c with
      | GRef (r,None) ->
          Inl (ArgArg (evaluable_of_global_reference ist.genv r,None))
      | GVar id ->
          let r = evaluable_of_global_reference ist.genv (VarRef id) in
          Inl (ArgArg (r,None))
      | _ ->
          let bound_names = Glob_ops.bound_glob_vars c in
          Inr (bound_names,(c,None),dummy_pat) in
  (l, match p with
  | Inl r -> interp_ref r
  | Inr { v = CAppExpl((None,r,None),[]) } ->
      (* We interpret similarly @ref and ref *)
      interp_ref (make @@ AN r)
  | Inr c ->
      Inr (snd (intern_typed_pattern ~strict ist ~as_type:false ~ltacvars:ist.ltacvars c)))

let intern_red_expr ~strict ist = function
  | Unfold l -> Unfold (List.map (intern_unfold ~strict ist) l)
  | Fold l -> Fold (List.map (intern_constr ~strict ist) l)
  | Cbv f -> Cbv (intern_flag ~strict ist f)
  | Cbn f -> Cbn (intern_flag ~strict ist f)
  | Lazy f -> Lazy (intern_flag ~strict ist f)
  | Pattern l -> Pattern (List.map (intern_constr_with_occurrences ~strict ist) l)
  | Simpl (f,o) ->
    Simpl (intern_flag ~strict ist f,
           Option.map (intern_typed_pattern_or_ref_with_occurrences ~strict ist) o)
  | CbvVm o -> CbvVm (Option.map (intern_typed_pattern_or_ref_with_occurrences ~strict ist) o)
  | CbvNative o -> CbvNative (Option.map (intern_typed_pattern_or_ref_with_occurrences ~strict ist) o)
  | (Red _ | Hnf | ExtraRedExpr _ as r ) -> r

(* val interp_int : Gneinterp_sign -> lident -> int
 *
 * val interp_int_or_var : interp_sign -> int Locus.or_var -> int *)


(* Raise Not_found if not in interpretation sign *)
let try_interp_ltac_var coerce ist env {loc;v=id} =
  let open Geninterp in
  let v = Id.Map.find id ist.lfun in
  try coerce v with CannotCoerceTo s ->
    error_ltac_variable ?loc id env v s

(* Gen Coerce *)

(* XXX: Should be moved to other place *)
let interp_int ist ({loc;v=id} as locid) =
  try try_interp_ltac_var coerce_to_int ist None locid
  with Not_found ->
    user_err ?loc ~hdr:"interp_int"
     (str "Unbound variable "  ++ Id.print id ++ str".")

let interp_int_or_var ist = function
  | ArgVar locid -> interp_int ist locid
  | ArgArg n -> n

let interp_int_or_var_as_list ist = function
  | ArgVar ({v=id} as locid) ->
      (try coerce_to_int_or_var_list (Id.Map.find id ist.lfun)
       with Not_found | CannotCoerceTo _ -> [ArgArg (interp_int ist locid)])
  | ArgArg n as x -> [x]

let interp_int_or_var_list ist l =
  List.flatten (List.map (interp_int_or_var_as_list ist) l)

(* Interprets an hypothesis name *)
let interp_occurrences ist occs =
  Locusops.occurrences_map (interp_int_or_var_list ist) occs

let interp_hyp_location ist env sigma ((occs,id),hl) =
  ((interp_occurrences ist occs,interp_hyp ist env sigma id),hl)

let interp_hyp_location_list_as_list ist env sigma ((occs,id),hl as x) =
  match occs,hl with
  | AllOccurrences,InHyp ->
      List.map (fun id -> ((AllOccurrences,id),InHyp))
        (interp_hyp_list_as_list ist env sigma id)
  | _,_ -> [interp_hyp_location ist env sigma x]

let interp_hyp_location_list ist env sigma l =
  List.flatten (List.map (interp_hyp_location_list_as_list ist env sigma) l)

let interp_clause ist env sigma { onhyps=ol; concl_occs=occs } : clause =
  { onhyps=Option.map (interp_hyp_location_list ist env sigma) ol;
    concl_occs=interp_occurrences ist occs }


(* Interprets a reduction expression *)
let interp_unfold ist env sigma (occs,qid) =
  (interp_occurrences ist occs,interp_evaluable ist env sigma qid)

let interp_flag ist env sigma red =
  { red with rConst = List.map (interp_evaluable ist env sigma) red.rConst }

let interp_constr_with_occurrences ist env sigma (occs,c) =
  let (sigma,c_interp) = interp_constr ist env sigma c in
  sigma , (interp_occurrences ist occs, c_interp)

let interp_closed_typed_pattern_with_occurrences ist env sigma (occs, a) =
  let p = match a with
  | Inl (ArgVar {loc;v=id}) ->
      (* This is the encoding of an ltac var supposed to be bound
         prioritary to an evaluable reference and otherwise to a constr
         (it is an encoding to satisfy the "union" type given to Simpl) *)
    let coerce_eval_ref_or_constr x =
      try Inl (coerce_to_evaluable_ref env sigma x)
      with CannotCoerceTo _ ->
        let c = coerce_to_closed_constr env x in
        Inr (pattern_of_constr env sigma (EConstr.to_constr sigma c)) in
    (try try_interp_ltac_var coerce_eval_ref_or_constr ist (Some (env,sigma)) (make ?loc id)
     with Not_found ->
       Nametab.error_global_not_found (qualid_of_ident ?loc id))
  | Inl (ArgArg _ as b) -> Inl (interp_evaluable ist env sigma b)
  | Inr c -> Inr (interp_typed_pattern ist env sigma c) in
  interp_occurrences ist occs, p

let interp_constr_with_occurrences_and_name_as_list =
  interp_constr_in_compound_list
    (fun c -> ((AllOccurrences,c),Anonymous))
    (function ((occs,c),Anonymous) when occs == AllOccurrences -> c
      | _ -> raise Not_found)
    (fun ist env sigma (occ_c,na) ->
      let (sigma,c_interp) = interp_constr_with_occurrences ist env sigma occ_c in
      sigma, (c_interp,
       interp_name ist env sigma na))

let interp_red_expr ist env sigma = function
  | Unfold l -> sigma , Unfold (List.map (interp_unfold ist env sigma) l)
  | Fold l ->
    let (sigma,l_interp) = interp_constr_list ist env sigma l in
    sigma , Fold l_interp
  | Cbv f -> sigma , Cbv (interp_flag ist env sigma f)
  | Cbn f -> sigma , Cbn (interp_flag ist env sigma f)
  | Lazy f -> sigma , Lazy (interp_flag ist env sigma f)
  | Pattern l ->
      let (sigma,l_interp) =
        Evd.MonadR.List.map_right
          (fun c sigma -> interp_constr_with_occurrences ist env sigma c) l sigma
      in
      sigma , Pattern l_interp
  | Simpl (f,o) ->
     sigma , Simpl (interp_flag ist env sigma f,
                    Option.map (interp_closed_typed_pattern_with_occurrences ist env sigma) o)
  | CbvVm o ->
    sigma , CbvVm (Option.map (interp_closed_typed_pattern_with_occurrences ist env sigma) o)
  | CbvNative o ->
    sigma , CbvNative (Option.map (interp_closed_typed_pattern_with_occurrences ist env sigma) o)
  | (Red _ |  Hnf | ExtraRedExpr _ as r) -> sigma , r
