(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* Created by Hugo Herbelin from contents related to lemma proofs in
   file command.ml, Aug 2009 *)

open Util
open Names

module NamedDecl = Context.Named.Declaration

(* Support for terminators and proofs with an associated constant
   [that can be saved] *)

type lemma_possible_guards = int list list

module Proof_ending = struct

  type t =
    | Regular
    | End_obligation of DeclareObl.obligation_qed_info
    | End_derive of { f : Id.t; name : Id.t }
    | End_equations of { hook : Constant.t list -> Evd.evar_map -> unit
                       ; i : Id.t
                       ; types : (Environ.env * Evar.t * Evd.evar_info * EConstr.named_context * Evd.econstr) list
                       ; wits : EConstr.t list ref
                       (* wits are actually computed by the proof
                          engine by side-effect after creating the
                          proof! This is due to the start_dependent_proof API *)
                       ; sigma : Evd.evar_map
                       }

end

module Recthm = struct
  type t =
    { name : Id.t
    ; typ : Constr.t
    ; args : Name.t list
    ; impargs : Impargs.manual_implicits
    }
end

module Info = struct

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

  let make ?hook ?(proof_ending=Proof_ending.Regular) ?(scope=DeclareDef.Global Declare.ImportDefaultBehavior)
      ?(kind=Decls.(IsProof Lemma)) () =
    { hook
    ; compute_guard = []
    ; impargs = []
    ; proof_ending = CEphemeron.create proof_ending
    ; other_thms = []
    ; scope
    ; kind
    }
end

(* Proofs with a save constant function *)
type t =
  { proof : Proof_global.t
  ; info : Info.t
  }

let pf_map f pf = { pf with proof = f pf.proof }
let pf_fold f pf = f pf.proof

let set_endline_tactic t = pf_map (Proof_global.set_endline_tactic t)

(* To be removed *)
module Internal = struct

  (** Gets the current terminator without checking that the proof has
      been completed. Useful for the likes of [Admitted]. *)
  let get_info ps = ps.info

end

let by tac pf =
  let proof, res = Pfedit.by tac pf.proof in
  { pf with proof }, res

(************************************************************************)
(* Creating a lemma-like constant                                       *)
(************************************************************************)

let initialize_named_context_for_proof () =
  let sign = Global.named_context () in
  List.fold_right
    (fun d signv ->
      let id = NamedDecl.get_id d in
      let d = if Decls.variable_opacity id then NamedDecl.drop_body d else d in
      Environ.push_named_context_val d signv) sign Environ.empty_named_context_val

(* Starting a goal *)
let start_lemma ~name ~poly
    ?(udecl=UState.default_univ_decl)
    ?(info=Info.make ())
    sigma c =
  (* We remove the bodies of variables in the named context marked
     "opaque", this is a hack tho, see #10446 *)
  let sign = initialize_named_context_for_proof () in
  let goals = [ Global.env_of_context sign , c ] in
  let proof = Proof_global.start_proof sigma ~name ~udecl ~poly goals in
  { proof ; info }

let start_dependent_lemma ~name ~poly
    ?(udecl=UState.default_univ_decl)
    ?(info=Info.make ()) telescope =
  let proof = Proof_global.start_dependent_proof ~name ~udecl ~poly telescope in
  { proof; info }

let rec_tac_initializer finite guard thms snl =
  if finite then
    match List.map (fun { Recthm.name; typ } -> name, (EConstr.of_constr typ)) thms with
    | (id,_)::l -> Tactics.mutual_cofix id l 0
    | _ -> assert false
  else
    (* nl is dummy: it will be recomputed at Qed-time *)
    let nl = match snl with
     | None -> List.map succ (List.map List.last guard)
     | Some nl -> nl
    in match List.map2 (fun { Recthm.name; typ } n -> (name, n, (EConstr.of_constr typ))) thms nl with
       | (id,n,_)::l -> Tactics.mutual_fix id n l 0
       | _ -> assert false

let start_lemma_with_initialization ?hook ~poly ~scope ~kind ~udecl sigma recguard thms snl =
  let intro_tac { Recthm.args; _ } = Tactics.auto_intros_tac args in
  let init_tac, compute_guard = match recguard with
  | Some (finite,guard,init_tac) ->
    let rec_tac = rec_tac_initializer finite guard thms snl in
    Some (match init_tac with
        | None ->
          Tacticals.New.tclTHENS rec_tac (List.map intro_tac thms)
        | Some tacl ->
          Tacticals.New.tclTHENS rec_tac
            List.(map2 (fun tac thm -> Tacticals.New.tclTHEN tac (intro_tac thm)) tacl thms)
      ),guard
  | None ->
    let () = match thms with [_] -> () | _ -> assert false in
    Some (intro_tac (List.hd thms)), [] in
  match thms with
  | [] -> CErrors.anomaly (Pp.str "No proof to start.")
  | { Recthm.name; typ; impargs; _}::other_thms ->
    let info =
      Info.{ hook
           ; impargs
           ; compute_guard
           ; other_thms
           ; proof_ending = CEphemeron.create Proof_ending.Regular
           ; scope
           ; kind
           } in
    let lemma = start_lemma ~name ~poly ~udecl ~info sigma (EConstr.of_constr typ) in
    pf_map (Proof_global.map_proof (fun p ->
        match init_tac with
        | None -> p
        | Some tac -> pi1 @@ Proof.run_tactic Global.(env ()) tac p)) lemma

(************************************************************************)
(* Commom constant saving path, for both Qed and Admitted               *)
(************************************************************************)

(* Support for mutually proved theorems *)

(* XXX: Most of this does belong to Declare, due to proof_entry manip *)
module MutualEntry : sig

  (* We keep this type abstract and to avoid uncontrolled hacks *)
  type t

  val variable : other_thms:Recthm.t list -> Entries.parameter_entry -> t

  val adjust_guardness_conditions
    : other_thms:Recthm.t list
    -> possible_indexes:int list list
    -> Evd.side_effects Declare.proof_entry
    -> t

  val declare_mutind
    (* Common to all recthms *)
    : ?fix_exn:(Exninfo.iexn -> Exninfo.iexn)
    -> scope:DeclareDef.locality
    -> poly:bool
    -> kind:Decls.logical_kind
    -> hook:DeclareDef.Hook.t option
    -> uctx:UState.t
    -> ?hook_data:DeclareDef.Hook.t * UState.t * (Names.Id.t * Constr.t) list
    -> udecl:UState.universe_decl
    (* Only for the first constant, introduced by compat *)
    -> ubind:UnivNames.universe_binders
    -> name:Id.t
    -> impargs:Impargs.manual_implicits
    -> t
    -> Names.GlobRef.t list

end = struct

  (* Body with the fix *)
  type et =
    | NoBody of Entries.parameter_entry
    | Single of Evd.side_effects Declare.proof_entry
    | Mutual of Evd.side_effects Declare.proof_entry

  type t =
    { other_thms : Recthm.t list
    ; entry : et
    }

  let variable ~other_thms t = { other_thms; entry = NoBody t }

  (* XXX: Refactor this with the code in
     [ComFixpoint.declare_fixpoint_generic] *)
  let guess_decreasing env possible_indexes ((body, ctx), eff) =
    let open Constr in
    match Constr.kind body with
    | Fix ((nv,0),(_,_,fixdefs as fixdecls)) ->
      let env = Safe_typing.push_private_constants env eff.Evd.seff_private in
      let indexes = Pretyping.search_guard env possible_indexes fixdecls in
      (mkFix ((indexes,0),fixdecls), ctx), eff
    | _ -> (body, ctx), eff

  let adjust_guardness_conditions ~other_thms ~possible_indexes const =
    let entry = match possible_indexes with
    | [] ->
      (* Not a recursive statement *)
      Single const
    | possible_indexes ->
      (* Try all combinations... not optimal *)
      let env = Global.env() in
      let pe = Declare.Internal.map_entry_body const
          ~f:(guess_decreasing env possible_indexes)
      in
      Mutual pe
    in { other_thms; entry }

  let rec select_body i t =
    let open Constr in
    match Constr.kind t with
    | Fix ((nv,0),decls) -> mkFix ((nv,i),decls)
    | CoFix (0,decls) -> mkCoFix (i,decls)
    | LetIn(na,t1,ty,t2) -> mkLetIn (na,t1,ty, select_body i t2)
    | Lambda(na,ty,t) -> mkLambda(na,ty, select_body i t)
    | App (t, args) -> mkApp (select_body i t, args)
    | _ ->
      CErrors.anomaly
        Pp.(str "Not a proof by induction: " ++
            Termops.Internal.debug_print_constr (EConstr.of_constr t) ++ str ".")

  let declare_mutind ?fix_exn ~scope ~poly ~kind ~hook ~uctx ?hook_data ~udecl ~ubind ?typ ~name ~impargs mutpe i =
    match mutpe with
    | NoBody pe ->
      DeclareDef.declare_assumption ?fix_exn ~name ~scope ~hook ~impargs ~uctx pe
    | Single pe ->
      DeclareDef.declare_definition ~name ~scope ~kind ?hook_data ubind pe impargs
    | Mutual pe ->
      (* if typ = None , we don't touch the type; used in the base case *)
      let pe =
        match typ with
        | None -> pe
        | Some typ ->
          Declare.Internal.map_entry_type pe ~f:(fun _ -> Some typ)
      in
      let pe = Declare.Internal.map_entry_body pe
          ~f:(fun ((body, ctx), eff) -> (select_body i body, ctx), eff) in
      DeclareDef.declare_definition ~name ~scope ~kind ?hook_data ubind pe impargs

  let declare_mutind ?fix_exn ~scope ~poly ~kind ~hook ~uctx ?hook_data ~udecl ~ubind ~name ~impargs { other_thms ; entry } =
    (* At some point make this a single iteration *)
    let r = declare_mutind ?fix_exn ~poly ~scope ~udecl ~kind ~ubind ~hook ?hook_data ~uctx ~name ~impargs entry 0 in
    (* Before we used to do this, check if that's right *)
    let ubind = UnivNames.empty_binders in
    let rs =
      List.map_i (
        fun i { Recthm.name; typ; impargs } ->
          declare_mutind ?fix_exn ~name ~poly ~scope ~udecl ~kind ~ubind ~hook ?hook_data ~uctx ~impargs ~typ entry i) 1 other_thms
    in r :: rs
end

(************************************************************************)
(* Admitting a lemma-like constant                                      *)
(************************************************************************)

(* Admitted *)
let get_keep_admitted_vars =
  Goptions.declare_bool_option_and_ref
    ~depr:false
    ~key:["Keep"; "Admitted"; "Variables"]
    ~value:true

let compute_proof_using_for_admitted proof typ pproofs =
  if not (get_keep_admitted_vars ()) then None
  else match Proof_global.get_used_variables proof, pproofs with
    | Some _ as x, _ -> x
    | None, pproof :: _ ->
      let env = Global.env () in
      let ids_typ = Environ.global_vars_set env typ in
      (* [pproof] is evar-normalized by [partial_proof]. We don't
         count variables appearing only in the type of evars. *)
      let ids_def = Environ.global_vars_set env (EConstr.Unsafe.to_constr pproof) in
      Some (Environ.really_needed env (Id.Set.union ids_typ ids_def))
    | _ -> None

let finish_admitted ~name ~poly ~scope ~kind ~uctx ~hook ~udecl ~impargs ~other_thms pe =
  let mutpe = MutualEntry.variable ~other_thms pe in
  let ubind = UnivNames.empty_binders in
  let _r : Names.GlobRef.t list =
    MutualEntry.declare_mutind ~scope ~kind ~uctx ~poly ~udecl ~hook ~ubind ~name ~impargs mutpe in
  ()

let save_lemma_admitted ~(lemma : t) : unit =
  let { Info.hook; scope; impargs; other_thms; kind } = lemma.info in
  let udecl = Proof_global.get_universe_decl lemma.proof in
  let Proof.{ name; poly; entry } = Proof.data (Proof_global.get_proof lemma.proof) in
  let typ = match Proofview.initial_goals entry with
    | [typ] -> snd typ
    | _ -> CErrors.anomaly ~label:"Lemmas.save_lemma_admitted" (Pp.str "more than one statement.")
  in
  let typ = EConstr.Unsafe.to_constr typ in
  let proof = Proof_global.get_proof lemma.proof in
  let pproofs = Proof.partial_proof proof in
  let sec_vars = compute_proof_using_for_admitted lemma.proof typ pproofs in
  let universes = Proof_global.get_initial_euctx lemma.proof in
  let ctx = UState.check_univ_decl ~poly universes udecl in
  finish_admitted ~name ~poly ~kind ~scope ~uctx:universes ~hook ~udecl ~impargs ~other_thms (sec_vars, (typ, ctx), None)

(************************************************************************)
(* Saving a lemma-like constant                                         *)
(************************************************************************)

let default_thm_id = Id.of_string "Unnamed_thm"

let check_anonymity id save_ident =
  if not (String.equal (Nameops.atompart_of_id id) (Id.to_string (default_thm_id))) then
    CErrors.user_err Pp.(str "This command can only be used for unnamed theorem.")

let finish_proved idopt po info =
  let open Proof_global in
  let { Info.hook; compute_guard; impargs; other_thms; scope; kind } = info in
  match po with
  | { name; entries=[const]; universes; udecl; poly } ->
    let name = match idopt with
      | None -> name
      | Some { CAst.v = save_id } -> check_anonymity name save_id; save_id in
    let fix_exn = Declare.Internal.get_fix_exn const in
    let () = try
      let mutpe = MutualEntry.adjust_guardness_conditions ~other_thms ~possible_indexes:compute_guard const in
      let hook_data = Option.map (fun hook -> hook, universes, []) hook in
      let ubind = UState.universe_binders universes in
      let _r : Names.GlobRef.t list =
        MutualEntry.declare_mutind ~fix_exn ~scope ~kind ~uctx:universes ~poly ~udecl ?hook_data ~hook ~ubind ~name ~impargs mutpe
      in ()
    with e when CErrors.noncritical e ->
      let e = CErrors.push e in
      iraise (fix_exn e)
    in ()
  | _ ->
    CErrors.anomaly ~label:"finish_proved" Pp.(str "close_proof returned more than one proof term")

let finish_derived ~f ~name ~idopt ~entries =
  (* [f] and [name] correspond to the proof of [f] and of [suchthat], respectively. *)

  if Option.has_some idopt then
    CErrors.user_err Pp.(str "Cannot save a proof of Derive with an explicit name.");

  let f_def, lemma_def =
    match entries with
    | [_;f_def;lemma_def] ->
      f_def, lemma_def
    | _ -> assert false
  in
  (* The opacity of [f_def] is adjusted to be [false], as it
     must. Then [f] is declared in the global environment. *)
  let f_def = Declare.Internal.set_opacity ~opaque:false f_def in
  let f_kind = Decls.(IsDefinition Definition) in
  let f_def = Declare.DefinitionEntry f_def in
  let f_kn = Declare.declare_constant ~name:f ~kind:f_kind f_def in
  let f_kn_term = Constr.mkConst f_kn in
  (* In the type and body of the proof of [suchthat] there can be
     references to the variable [f]. It needs to be replaced by
     references to the constant [f] declared above. This substitution
     performs this precise action. *)
  let substf c = Vars.replace_vars [f,f_kn_term] c in
  (* Extracts the type of the proof of [suchthat]. *)
  let lemma_pretype typ =
    match typ with
    | Some t -> Some (substf t)
    | None -> assert false (* Proof_global always sets type here. *)
  in
  (* The references of [f] are subsituted appropriately. *)
  let lemma_def = Declare.Internal.map_entry_type lemma_def ~f:lemma_pretype in
  (* The same is done in the body of the proof. *)
  let lemma_def = Declare.Internal.map_entry_body lemma_def ~f:(fun ((b,ctx),fx) -> (substf b, ctx), fx) in
  let lemma_def = Declare.DefinitionEntry lemma_def in
  let _ : Names.Constant.t = Declare.declare_constant ~name ~kind:Decls.(IsProof Proposition) lemma_def in
  ()

let finish_proved_equations lid kind proof_obj hook i types wits sigma0 =

  let obls = ref 1 in
  let sigma, recobls =
    CList.fold_left2_map (fun sigma (wit, (evar_env, ev, evi, local_context, type_)) entry ->
        let id =
          match Evd.evar_ident ev sigma0 with
          | Some id -> id
          | None -> let n = !obls in incr obls; Nameops.add_suffix i ("_obligation_" ^ string_of_int n)
        in
        let entry, args = Declare.Internal.shrink_entry local_context entry in
        let cst = Declare.declare_constant ~name:id ~kind (Declare.DefinitionEntry entry) in
        let sigma, app = Evarutil.new_global sigma (GlobRef.ConstRef cst) in
        let sigma = Evd.define ev (EConstr.applist (app, List.map EConstr.of_constr args)) sigma in
        sigma, cst) sigma0
      (CList.combine (List.rev !wits) types) proof_obj.Proof_global.entries
  in
  hook recobls sigma

let finalize_proof idopt proof_obj proof_info =
  let open Proof_global in
  let open Proof_ending in
  match CEphemeron.default proof_info.Info.proof_ending Regular with
  | Regular ->
    finish_proved idopt proof_obj proof_info
  | End_obligation oinfo ->
    DeclareObl.obligation_terminator proof_obj.entries proof_obj.universes oinfo
  | End_derive { f ; name } ->
    finish_derived ~f ~name ~idopt ~entries:proof_obj.entries
  | End_equations { hook; i; types; wits; sigma } ->
    finish_proved_equations idopt proof_info.Info.kind proof_obj hook i types wits sigma

let save_lemma_proved ~lemma ~opaque ~idopt =
  (* Env and sigma are just used for error printing in save_remaining_recthms *)
  let proof_obj = Proof_global.close_proof ~opaque ~keep_body_ucst_separate:false (fun x -> x) lemma.proof in
  finalize_proof idopt proof_obj lemma.info

(***********************************************************************)
(* Special case to close a lemma without forcing a proof               *)
(***********************************************************************)
let save_lemma_admitted_delayed ~proof ~info =
  let open Proof_global in
  let { name; entries; universes; udecl; poly } = proof in
  let { Info.hook; scope; impargs; kind; other_thms } = info in
  if List.length entries <> 1 then
    CErrors.user_err Pp.(str "Admitted does not support multiple statements");
  let { Declare.proof_entry_secctx; proof_entry_type; proof_entry_universes } = List.hd entries in
  let poly = match proof_entry_universes with
    | Entries.Monomorphic_entry _ -> false
    | Entries.Polymorphic_entry (_, _) -> true in
  let typ = match proof_entry_type with
    | None -> CErrors.user_err Pp.(str "Admitted requires an explicit statement");
    | Some typ -> typ in
  let ctx = UState.univ_entry ~poly universes in
  let sec_vars = if get_keep_admitted_vars () then proof_entry_secctx else None in
  finish_admitted ~name ~poly ~kind ~scope ~uctx:universes ~hook ~udecl ~impargs ~other_thms (sec_vars, (typ, ctx), None)

let save_lemma_proved_delayed ~proof ~info ~idopt = finalize_proof idopt proof info
