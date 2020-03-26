(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Util
open Names
open Context

module NamedDecl = Context.Named.Declaration

(*** Proof Global Environment ***)

type proof_object =
  { name : Names.Id.t
  ; entries : Evd.side_effects Declare.proof_entry list
  ; uctx: UState.t
  ; udecl : UState.universe_decl
  }

type opacity_flag = Opaque | Transparent

type t =
  { endline_tactic : Genarg.glob_generic_argument option
  ; section_vars : Id.Set.t option
  ; proof : Proof.t
  ; udecl: UState.universe_decl
  (** Initial universe declarations *)
  ; initial_euctx : UState.t
  (** The initial universe context (for the statement) *)
  }

(*** Proof Global manipulation ***)

let get_proof ps = ps.proof
let get_proof_name ps = (Proof.data ps.proof).Proof.name

let get_initial_euctx ps = ps.initial_euctx

let map_proof f p = { p with proof = f p.proof }
let map_fold_proof f p = let proof, res = f p.proof in { p with proof }, res

let map_fold_proof_endline f ps =
  let et =
    match ps.endline_tactic with
    | None -> Proofview.tclUNIT ()
    | Some tac ->
      let open Geninterp in
      let {Proof.poly} = Proof.data ps.proof in
      let ist = { lfun = Id.Map.empty; poly; extra = TacStore.empty } in
      let Genarg.GenArg (Genarg.Glbwit tag, tac) = tac in
      let tac = Geninterp.interp tag ist tac in
      Ftactic.run tac (fun _ -> Proofview.tclUNIT ())
  in
  let (newpr,ret) = f et ps.proof in
  let ps = { ps with proof = newpr } in
  ps, ret

let compact_the_proof pf = map_proof Proof.compact pf

(* Sets the tactic to be used when a tactic line is closed with [...] *)
let set_endline_tactic tac ps =
  { ps with endline_tactic = Some tac }

(** [start_proof ~name ~udecl ~poly sigma goals] starts a proof of
   name [name] with goals [goals] (a list of pairs of environment and
   conclusion). The proof is started in the evar map [sigma] (which
   can typically contain universe constraints), and with universe
   bindings [udecl]. *)
let start_proof ~name ~udecl ~poly sigma goals =
  let proof = Proof.start ~name ~poly sigma goals in
  let initial_euctx = Evd.evar_universe_context Proof.((data proof).sigma) in
  { proof
  ; endline_tactic = None
  ; section_vars = None
  ; udecl
  ; initial_euctx
  }

let start_dependent_proof ~name ~udecl ~poly goals =
  let proof = Proof.dependent_start ~name ~poly goals in
  let initial_euctx = Evd.evar_universe_context Proof.((data proof).sigma) in
  { proof
  ; endline_tactic = None
  ; section_vars = None
  ; udecl
  ; initial_euctx
  }

let get_used_variables pf = pf.section_vars
let get_universe_decl pf = pf.udecl

let set_used_variables ps l =
  let open Context.Named.Declaration in
  let env = Global.env () in
  let ids = List.fold_right Id.Set.add l Id.Set.empty in
  let ctx = Environ.keep_hyps env ids in
  let ctx_set =
    List.fold_right Id.Set.add (List.map NamedDecl.get_id ctx) Id.Set.empty in
  let vars_of = Environ.global_vars_set in
  let aux env entry (ctx, all_safe as orig) =
    match entry with
    | LocalAssum ({binder_name=x},_) ->
       if Id.Set.mem x all_safe then orig
       else (ctx, all_safe)
    | LocalDef ({binder_name=x},bo, ty) as decl ->
       if Id.Set.mem x all_safe then orig else
       let vars = Id.Set.union (vars_of env bo) (vars_of env ty) in
       if Id.Set.subset vars all_safe
       then (decl :: ctx, Id.Set.add x all_safe)
       else (ctx, all_safe) in
  let ctx, _ =
    Environ.fold_named_context aux env ~init:(ctx,ctx_set) in
  if not (Option.is_empty ps.section_vars) then
    CErrors.user_err Pp.(str "Used section variables can be declared only once");
  (* EJGA: This is always empty thus we should modify the type *)
  (ctx, []), { ps with section_vars = Some (Context.Named.to_vars ctx) }

let get_open_goals ps =
  let Proof.{ goals; stack; shelf } = Proof.data ps.proof in
  List.length goals +
  List.fold_left (+) 0
    (List.map (fun (l1,l2) -> List.length l1 + List.length l2) stack) +
  List.length shelf

type closed_proof_output = (Constr.t * Evd.side_effects) list * UState.t

let private_poly_univs =
  let b = ref true in
  let _ = Goptions.(declare_bool_option {
      optdepr = false;
      optkey = ["Private";"Polymorphic";"Universes"];
      optread = (fun () -> !b);
      optwrite = ((:=) b);
    })
  in
  fun () -> !b

(* XXX: This is still separate from close_proof below due to drop_pt in the STM *)
let return_proof { proof } =
  let Proof.{name=pid;entry} = Proof.data proof in
  let initial_goals = Proofview.initial_goals entry in
  let evd = Proof.return ~pid proof in
  let eff = Evd.eval_side_effects evd in
  let evd = Evd.minimize_universes evd in
  let proof_opt c =
    match EConstr.to_constr_opt evd c with
    | Some p -> p
    | None -> CErrors.user_err Pp.(str "Some unresolved existential variables remain")
  in
  (* ppedrot: FIXME, this is surely wrong. There is no reason to duplicate
     side-effects... This may explain why one need to uniquize side-effects
     thereafter... *)
  (* EJGA: actually side-effects de-duplication and this codepath is
     unrelated. Duplicated side-effects arise from incorrect scheme
     generation code, the main bulk of it was mostly fixed by #9836
     but duplication can still happen because of rewriting schemes I
     think; however the code below is mostly untested, the only
     code-paths that generate several proof entries are derive and
     equations and so far there is no code in the CI that will
     actually call those and do a side-effect, TTBOMK *)
  (* EJGA: likely the right solution is to attach side effects to the first constant only? *)
  let proofs = List.map (fun (c, _) -> (proof_opt c, eff)) initial_goals in
  proofs, Evd.evar_universe_context evd

let close_proof ~opaque ~keep_body_ucst_separate ps =
  let elist, uctx = return_proof ps in
  let { section_vars; proof; udecl; initial_euctx } = ps in
  let Proof.{ name; poly; entry; sigma } = Proof.data proof in
  let opaque = match opaque with Opaque -> true | Transparent -> false in
  let constrain_variables ctx =
    UState.constrain_variables (fst (UState.context_set initial_euctx)) ctx
  in
  (* Because of dependent subgoals at the beginning of proofs, we could
     have existential variables in the initial types of goals, we need to
     normalise them for the kernel. *)
  let subst_evar k = Evd.existential_opt_value0 sigma k in
  let nf = UnivSubst.nf_evars_and_universes_opt_subst subst_evar (UState.subst uctx) in

  let make_body typ (c, eff) :
    Constr.types Entries.in_universes_entry * Evd.side_effects Entries.proof_output =
    let body = c in
    let allow_deferred =
      not poly && (keep_body_ucst_separate ||
                   not (Safe_typing.empty_private_constants = eff.Evd.seff_private))
    in
    let typ = if allow_deferred then typ else nf typ in
    let used_univs_body = Vars.universes_of_constr body in
    let used_univs_typ = Vars.universes_of_constr typ in
    let used_univs = Univ.LSet.union used_univs_body used_univs_typ in
    let utyp, ubody =
      if allow_deferred then
        let utyp = UState.univ_entry ~poly initial_euctx in
        let ctx = constrain_variables uctx in
        (* For vi2vo compilation proofs are computed now but we need to
           complement the univ constraints of the typ with the ones of
           the body.  So we keep the two sets distinct. *)
        let ctx_body = UState.restrict ctx used_univs in
        let univs = UState.check_mono_univ_decl ctx_body udecl in
        utyp, univs
      else if poly && opaque && private_poly_univs () then
        let universes = UState.restrict uctx used_univs in
        let typus = UState.restrict universes used_univs_typ in
        let utyp = UState.check_univ_decl ~poly typus udecl in
        let ubody = Univ.ContextSet.diff
            (UState.context_set universes)
            (UState.context_set typus)
        in
        utyp, ubody
      else
        (* Since the proof is computed now, we can simply have 1 set of
           constraints in which we merge the ones for the body and the ones
           for the typ. We recheck the declaration after restricting with
           the actually used universes.
           TODO: check if restrict is really necessary now. *)
        let ctx = UState.restrict uctx used_univs in
        let utyp = UState.check_univ_decl ~poly ctx udecl in
        utyp, Univ.ContextSet.empty
    in
    (typ, utyp), ((body, ubody), eff)
  in
  let entry_fn p (_, t) =
    let t = EConstr.Unsafe.to_constr t in
    let (typ, univs), ((body,univc),eff) = make_body t p in
    Declare.definition_entry ~opaque ?section_vars ~univs ~univc ~types:typ ~eff body
  in
  let entries = CList.map2 entry_fn elist (Proofview.initial_goals entry) in
  { name; entries; uctx; udecl }

let close_proof_delayed ~opaque ~keep_body_ucst_separate ?feedback_id
                (fpl : closed_proof_output Future.computation) ps =
  let { section_vars; proof; udecl; initial_euctx } = ps in
  let Proof.{ name; poly; entry; sigma } = Proof.data proof in
  (* We don't allow poly = true in this path *)
  assert(poly=false);

  let opaque = match opaque with Opaque -> true | Transparent -> false in
  let constrain_variables ctx =
    UState.constrain_variables (fst (UState.context_set initial_euctx)) ctx
  in
  let fpl, univs = Future.split2 fpl in
  let uctx = initial_euctx in
  (* Because of dependent subgoals at the beginning of proofs, we could
     have existential variables in the initial types of goals, we need to
     normalise them for the kernel. *)
  let subst_evar k = Evd.existential_opt_value0 sigma k in
  let nf = UnivSubst.nf_evars_and_universes_opt_subst subst_evar (UState.subst uctx) in

  let make_body t p =
    (* Already checked the univ_decl for the type universes when starting the proof. *)
    let univctx = UState.univ_entry ~poly:false uctx in
    let t = nf t in
    (t, univctx),
    Future.chain p (fun (pt,eff) ->
        (* Deferred proof, we already checked the universe declaration with
           the initial universes, ensure that the final universes respect
           the declaration as well. If the declaration is non-extensible,
           this will prevent the body from adding universes and constraints. *)
        let univs = Future.force univs in
        let univs = constrain_variables univs in
        let used_univs = Univ.LSet.union
            (Vars.universes_of_constr t)
            (Vars.universes_of_constr pt)
        in
        let univs = UState.restrict univs used_univs in
        let univs = UState.check_mono_univ_decl univs udecl in
        (pt,univs),eff)
  in
  let entry_fn p (_, t) =
    let t = EConstr.Unsafe.to_constr t in
    let (typ, utyp), body = make_body t p in
    Declare.delayed_definition_entry ~opaque ?feedback_id ?section_vars ~univs:utyp ~types:typ body
  in
  let entries = Future.map2 entry_fn fpl (Proofview.initial_goals entry) in
  { name; entries; uctx; udecl }

let return_partial_proof { proof } =
 let proofs = Proof.partial_proof proof in
 let Proof.{sigma=evd} = Proof.data proof in
 let eff = Evd.eval_side_effects evd in
 (* ppedrot: FIXME, this is surely wrong. There is no reason to duplicate
     side-effects... This may explain why one need to uniquize side-effects
     thereafter... *)
 let proofs = List.map (fun c -> EConstr.Unsafe.to_constr c, eff) proofs in
 proofs, Evd.evar_universe_context evd

let close_future_proof ~opaque ~feedback_id ps proof =
  close_proof_delayed ~opaque ~keep_body_ucst_separate:true ~feedback_id proof ps


let update_global_env =
  map_proof (fun p ->
      let { Proof.sigma } = Proof.data p in
      let tac = Proofview.Unsafe.tclEVARS (Evd.update_sigma_env sigma (Global.env ())) in
      let p, (status,info), _ = Proof.run_tactic (Global.env ()) tac p in
      p)
