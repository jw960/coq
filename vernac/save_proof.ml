open CErrors
open Util
open Names
open Entries
open Nameops
open Globnames
open Decl_kinds
open Declare
open Proof_global

[@@@ocaml.warning "-8"]

let default_thm_id = Id.of_string ""
let check_anonymity id save_ident =
  if not (String.equal (atompart_of_id id) (Id.to_string (default_thm_id))) then
    user_err Pp.(str "This command can only be used for unnamed theorem.")

let save_proof_proved_non_poly Lemmas.{ proof; info = { Lemmas.Info.scope; kind; _} } ~opaque ~idopt ?hook compute_guard feedback_id =
  let fix_exn = None in

  let { name; entries=[const]; universes } =
    (* Proof_global.close_proof *)
    let rproof =
      (* return_proof *)
      let proof = Proof_global.get_proof proof in
      let Proof.{name=pid;entry} = Proof.data proof in
      let initial_goals = Proofview.initial_goals entry in
      let evd = Proof.return ~pid proof in
      let eff = Evd.eval_side_effects evd in
      let evd = Evd.minimize_universes evd in
      (* ppedrot: FIXME, this is surely wrong. There is no reason to duplicate
         side-effects... This may explain why one need to uniquize side-effects
         thereafter... *)
      let proof_opt c =
        match EConstr.to_constr_opt evd c with
        | Some p -> p
        | None -> CErrors.user_err Pp.(str "Some unresolved existential variables remain")
      in
      let proofs =
        List.map (fun (c, _) -> (proof_opt c, eff)) initial_goals in
      proofs, Evd.evar_universe_context evd
    in

    let fpl = Future.from_val ?fix_exn rproof in

    (* inner close_proof *)
    let { section_vars; proof; udecl; initial_euctx } = proof in
    let { Proof.name; poly; entry } = Proof.data proof in
    let opaque = match opaque with Opaque -> true | Transparent -> false in
    let constrain_variables ctx =
      UState.constrain_variables (fst (UState.context_set initial_euctx)) ctx
    in
    let fpl, univs = Future.split2 fpl in
    let universes = if poly then Future.force univs else initial_euctx in
    (* Because of dependent subgoals at the beginning of proofs, we could
       have existential variables in the initial types of goals, we need to
       normalise them for the kernel. *)
    let subst_evar k =
      Proof.in_proof proof (fun m -> Evd.existential_opt_value0 m k) in
    let nf = UnivSubst.nf_evars_and_universes_opt_subst subst_evar
        (UState.subst universes) in

    let make_body t p =
      (* Already checked the univ_decl for the type universes when starting the proof. *)
      let univctx = Entries.Monomorphic_entry (UState.context_set universes) in
      let t = nf t in
      Future.from_val (univctx, t),
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
      let univstyp, body = make_body t p in
      let univs, typ = Future.force univstyp in
      {Proof_global.
        proof_entry_body = body;
        proof_entry_secctx = section_vars;
        proof_entry_feedback = feedback_id;
        proof_entry_type  = Some typ;
        proof_entry_inline_code = false;
        proof_entry_opaque = opaque;
        proof_entry_universes = univs; }
    in
    let entries = Future.map2 entry_fn fpl Proofview.(initial_goals entry) in
    { name; entries; poly; udecl; universes }
  in
  (* universe_proof_terminator *)
  let is_opaque, export_seff = match opaque with
    | Transparent -> false, true
    | Opaque      -> true, false
  in
  assert (is_opaque == const.proof_entry_opaque);
  let id = match idopt with
    | None -> name
    | Some { CAst.v = save_id } -> check_anonymity name save_id; save_id in
  let hook = Option.map (fun univ_hook -> univ_hook (Some universes)) hook in
  let fix_exn = Future.fix_exn_of const.proof_entry_body in

  (* save *)
  let export_seff = None in
  try
    let const = Lemmas.adjust_guardness_conditions const compute_guard in
    let k = Kindops.logical_kind_of_goal_kind kind in
    let should_suggest = const.const_entry_opaque && Option.is_empty const.const_entry_secctx in
    let r = match scope with
      | Discharge when Lib.sections_are_opened () ->
          let c = SectionLocalDef const in
          let _ = declare_variable id (Lib.cwd(), c, k) in
          let () = if should_suggest
            then Proof_using.suggest_variable (Global.env ()) id
          in
          VarRef id
      | Local | Global | Discharge ->
          let local = match locality with
          | Local | Discharge -> true
          | Global -> false
          in
          let kn =
           declare_constant ?export_seff id ~local (DefinitionEntry const, k) in
          let () = if should_suggest
            then Proof_using.suggest_constant (Global.env ()) kn
          in
          let gr = ConstRef kn in
          Declare.declare_univ_binders gr (UState.universe_binders universes);
          gr
    in
    definition_message id;
    Lemmas.call_hook ?hook locality r
  with e when CErrors.noncritical e ->
    let e = CErrors.push e in
    iraise (fix_exn e)

let save_proof_proved_poly Lemmas.{ proof; info = { Info.scope; kind; _ } } ~opaque ~idopt ?hook compute_guard feedback_id =
  let fix_exn = None in

  let { name; entries=[const]; udecl; universes } =
    (* Proof_global.close_proof *)
    let rproof =
      (* return_proof *)
      let proof = Proof_global.get_proof proof in
      let Proof.{name=pid;entry} = Proof.data proof in
      let initial_goals = Proofview.initial_goals entry in
      let evd = Proof.return ~pid proof in
      let eff = Evd.eval_side_effects evd in
      let evd = Evd.minimize_universes evd in
      (* ppedrot: FIXME, this is surely wrong. There is no reason to duplicate
         side-effects... This may explain why one need to uniquize side-effects
         thereafter... *)
      let proof_opt c =
        match EConstr.to_constr_opt evd c with
        | Some p -> p
        | None -> CErrors.user_err Pp.(str "Some unresolved existential variables remain")
      in
      let proofs =
        List.map (fun (c, _) -> (proof_opt c, eff)) initial_goals in
      proofs, Evd.evar_universe_context evd
    in

    let fpl = Future.from_val ?fix_exn rproof in

    (* inner close_proof *)
    let keep_body_ucst_separate = true in
    let { section_vars; proof; udecl } = proof in
    let Proof.{ name; poly; entry; initial_euctx } = Proof.data proof in
    let opaque = match opaque with Opaque -> true | Transparent -> false in
    let constrain_variables ctx =
      UState.constrain_variables (fst (UState.context_set initial_euctx)) ctx
    in
    let fpl, univs = Future.split2 fpl in
    let universes = if poly then Future.force univs else initial_euctx in
    (* Because of dependent subgoals at the beginning of proofs, we could
       have existential variables in the initial types of goals, we need to
       normalise them for the kernel. *)
    let subst_evar k =
      Proof.in_proof proof (fun m -> Evd.existential_opt_value0 m k) in
    let nf = UnivSubst.nf_evars_and_universes_opt_subst subst_evar
        (UState.subst universes) in

    let make_body =
      let make_body t (c, eff) =
        let body = c in
        let allow_deferred =
          not poly && (keep_body_ucst_separate ||
                       not (Safe_typing.empty_private_constants = eff))
        in
        let typ = if allow_deferred then t else nf t in
        let used_univs_body = Vars.universes_of_constr body in
        let used_univs_typ = Vars.universes_of_constr typ in
        if allow_deferred then
          let initunivs = UState.const_univ_entry ~poly initial_euctx in
          let ctx = constrain_variables universes in
          (* For vi2vo compilation proofs are computed now but we need to
             complement the univ constraints of the typ with the ones of
             the body.  So we keep the two sets distinct. *)
          let used_univs = Univ.LSet.union used_univs_body used_univs_typ in
          let ctx_body = UState.restrict ctx used_univs in
          let univs = UState.check_mono_univ_decl ctx_body udecl in
          (initunivs, typ), ((body, univs), eff)
        else if poly && opaque && private_poly_univs () then
          let used_univs = Univ.LSet.union used_univs_body used_univs_typ in
          let universes = UState.restrict universes used_univs in
          let typus = UState.restrict universes used_univs_typ in
          let udecl = UState.check_univ_decl ~poly typus udecl in
          let ubody = Univ.ContextSet.diff
              (UState.context_set universes)
              (UState.context_set typus)
          in
          (udecl, typ), ((body, ubody), eff)
        else
          (* Since the proof is computed now, we can simply have 1 set of
             constraints in which we merge the ones for the body and the ones
             for the typ. We recheck the declaration after restricting with
             the actually used universes.
             TODO: check if restrict is really necessary now. *)
          let used_univs = Univ.LSet.union used_univs_body used_univs_typ in
          let ctx = UState.restrict universes used_univs in
          let univs = UState.check_univ_decl ~poly ctx udecl in
          (univs, typ), ((body, Univ.ContextSet.empty), eff)
      in
      fun t p -> Future.split2 (Future.chain p (make_body t))
    in
    let entry_fn p (_, t) =
      let t = EConstr.Unsafe.to_constr t in
      let univstyp, body = make_body t p in
      let univs, typ = Future.force univstyp in
      {Entries.
        const_entry_body = body;
        const_entry_secctx = section_vars;
        const_entry_feedback = feedback_id;
        const_entry_type  = Some typ;
        const_entry_inline_code = false;
        const_entry_opaque = opaque;
        const_entry_universes = univs; }
    in
    let entries = Future.map2 entry_fn fpl Proofview.(initial_goals entry) in
    { name; entries; poly; udecl; universes }
  in
  (* universe_proof_terminator *)
  let is_opaque, export_seff = match opaque with
    | Transparent -> false, true
    | Opaque      -> true, false
  in
  assert (is_opaque == const.const_entry_opaque);
  let id = match idopt with
    | None -> name
    | Some { CAst.v = save_id } -> check_anonymity name save_id; save_id in
  let hook = Option.map (fun univ_hook -> univ_hook (Some universes)) hook in
  let fix_exn = Future.fix_exn_of const.Entries.const_entry_body in

  (* save *)
  let export_seff = None in
  try
    let const = Lemmas.adjust_guardness_conditions const compute_guard in
    let k = Kindops.logical_kind_of_goal_kind kind in
    let should_suggest = const.const_entry_opaque && Option.is_empty const.const_entry_secctx in
    let r = match scope with
      | Discharge when Lib.sections_are_opened () ->
          let c = SectionLocalDef const in
          let _ = declare_variable id (Lib.cwd(), c, k) in
          let () = if should_suggest
            then Proof_using.suggest_variable (Global.env ()) id
          in
          VarRef id
      | Local | Global | Discharge ->
          let local = match locality with
          | Local | Discharge -> true
          | Global -> false
          in
          let kn =
           declare_constant ?export_seff id ~local (DefinitionEntry const, k) in
          let () = if should_suggest
            then Proof_using.suggest_constant (Global.env ()) kn
          in
          let gr = ConstRef kn in
          Declare.declare_univ_binders gr (UState.universe_binders universes);
          gr
    in
    definition_message id;
    DeclareDef.Hook.call ?hook  scope r
  with e when CErrors.noncritical e ->
    let e = CErrors.push e in
    iraise (fix_exn e)
