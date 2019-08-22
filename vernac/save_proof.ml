open CErrors
open Util
open Names
open Entries
open Nameops
open Globnames
open Declare

[@@@ocaml.warning "-8"]

let default_thm_id = Id.of_string ""
let check_anonymity id save_ident =
  if not (String.equal (atompart_of_id id) (Id.to_string (default_thm_id))) then
    user_err Pp.(str "This command can only be used for unnamed theorem.")

(* Very simplified version of the constant save path; we have removed:
   - user messages;
   - multiple entries; [not that they were supported]
   - hooks;
   - error handling (STM)
   - useless futures
  *)
let save_proof_proved_non_poly
    Lemmas.{ proof ;
             info = { Lemmas.Info.scope ; kind; compute_guard; _ } } ~opaque ~idopt feedback_id =

  let { Proof_global.section_vars; proof; udecl; initial_euctx; _ } = proof in
  let Proof.{ name; poly; entry; } = Proof.data proof in
  let [terms, types] = Proofview.initial_goals entry in
  let proof_entry_opaque =
    match opaque with
    | Proof_global.Opaque -> true
    | Proof_global.Transparent -> false
  in
  let evd = Proof.return ~pid:name proof in
  let evd = Evd.minimize_universes evd in
  let uctx = Evd.evar_universe_context evd in

  let proof_opt c =
    match EConstr.to_constr_opt evd c with
    | Some p -> p
    | None -> CErrors.user_err Pp.(str "Some unresolved existential variables remain")
  in

  let univctx = Entries.Monomorphic_entry (UState.context_set initial_euctx) in

  let eff = Evd.eval_side_effects evd in
  let pt, eff = proof_opt terms, eff in

  (* Because of dependent subgoals at the beginning of proofs, we could
     have existential variables in the initial types of goals, we need to
     normalise them for the kernel. *)
  let subst_evar k = Proof.in_proof proof (fun m -> Evd.existential_opt_value0 m k) in
  let nf = UnivSubst.nf_evars_and_universes_opt_subst subst_evar UState.(subst initial_euctx) in

  (* Create the constant entry *)
  let types = EConstr.Unsafe.to_constr types in
  let types = nf types in

  (* Already checked the univ_decl for the type universes when starting the proof. *)
  let univs = UState.constrain_variables (fst (UState.context_set initial_euctx)) uctx in
  let used_univs = Univ.LSet.union (Vars.universes_of_constr types) (Vars.universes_of_constr pt) in
  let univs = UState.restrict univs used_univs in
  let univs = UState.check_mono_univ_decl univs udecl in

  let const =
    Proof_global.{
      proof_entry_body = Future.from_val ((pt, univs), eff);
      proof_entry_secctx = section_vars;
      proof_entry_feedback = feedback_id;
      proof_entry_type = Some types;
      proof_entry_inline_code = false;
      proof_entry_opaque;
      proof_entry_universes = univctx; }
  in

  let is_opaque, export_seff = match opaque with
    | Proof_global.Transparent -> false, true
    | Proof_global.Opaque      -> true, false
  in

  assert (is_opaque == const.Proof_global.proof_entry_opaque);

  let id = match idopt with
    | None -> name
    | Some { CAst.v = save_id } -> check_anonymity name save_id; save_id in

  let const = Lemmas.adjust_guardness_conditions const compute_guard in
  let kind = Kindops.logical_kind_of_goal_kind kind in

  match scope with
  | DeclareDef.Discharge ->
    let c = SectionLocalDef const in
    let _ = declare_variable ~kind ~name:id (Lib.cwd(), c, k) in
    ()
  | DeclareDef.Global local ->
    let kn = declare_constant id ~local (DefinitionEntry const, k) in
    let gr = ConstRef kn in
    Declare.declare_univ_binders gr (UState.universe_binders initial_euctx)
