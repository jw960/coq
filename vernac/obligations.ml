(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Printf
open Names
open Pp
open Util

(* For the records fields, opens should go away one these types are private *)
open DeclareObl
open DeclareObl.Obligation
open DeclareObl.ProgramDecl

let reduce c =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  EConstr.Unsafe.to_constr
    (Reductionops.clos_norm_flags CClosure.betaiota env sigma
       (EConstr.of_constr c))

let explain_no_obligations = function
  | Some ident -> str "No obligations for program " ++ Id.print ident
  | None -> str "No obligations remaining"

module Error = struct
  let no_obligations n = CErrors.user_err (explain_no_obligations n)

  let ambiguous_program id ids =
    CErrors.user_err
      Pp.(
        str "More than one program with unsolved obligations: "
        ++ prlist Id.print ids
        ++ str "; use the \"of\" clause to specify, as in \"Obligation 1 of "
        ++ Id.print id ++ str "\"")

  let unknown_obligation num =
    CErrors.user_err
      (Pp.str (sprintf "Unknown obligation number %i" (succ num)))

  let already_solved num =
    CErrors.user_err
      ( str "Obligation" ++ spc () ++ int num ++ str "already" ++ spc ()
      ++ str "solved." )

  let depends num rem =
    CErrors.user_err
      ( str "Obligation " ++ int num
      ++ str " depends on obligation(s) "
      ++ pr_sequence (fun x -> int (succ x)) rem )
end

let default_tactic = ref (Proofview.tclUNIT ())

let evar_of_obligation o =
  Evd.make_evar (Global.named_context_val ()) (EConstr.of_constr o.obl_type)

open Evd

let goal_kind = Decls.(IsDefinition Definition)
let goal_proof_kind = Decls.(IsProof Lemma)

let kind_of_obligation o =
  match o with
  | Evar_kinds.Define false | Evar_kinds.Expand -> goal_kind
  | _ -> goal_proof_kind

(* Solve an obligation using tactics, return the corresponding proof term *)
let warn_solve_errored =
  CWarnings.create ~name:"solve_obligation_error" ~category:"tactics"
    (fun err ->
      Pp.seq
        [ str "Solve Obligations tactic returned error: "
        ; err
        ; fnl ()
        ; str "This will become an error in the future" ])

let solve_by_tac ?loc name evi t poly uctx =
  (* the status is dropped. *)
  try
    let env = Global.env () in
    let body, types, _, uctx =
      Pfedit.build_by_tactic env ~uctx ~poly ~typ:evi.evar_concl t
    in
    Inductiveops.control_only_guard env (Evd.from_ctx uctx)
      (EConstr.of_constr body);
    Some (body, types, uctx)
  with
  | Refiner.FailError (_, s) as exn ->
    let _ = Exninfo.capture exn in
    CErrors.user_err ?loc ~hdr:"solve_obligation" (Lazy.force s)
  (* If the proof is open we absorb the error and leave the obligation open *)
  | Proof.OpenProof _ -> None
  | e when CErrors.noncritical e ->
    let err = CErrors.print e in
    warn_solve_errored ?loc err;
    None

let get_unique_prog pm prg =
  match State.get_unique_open_prog pm prg with
  | Ok prg -> prg
  | Error [] -> Error.no_obligations None
  | Error (id :: _ as ids) -> Error.ambiguous_program id ids

let rec solve_obligation prg num tac =
  let user_num = succ num in
  let {obls; remaining = rem} = prg.prg_obligations in
  let obl = obls.(num) in
  let remaining = deps_remaining obls obl in
  let () =
    if defined obl then Error.already_solved user_num;
    if not (List.is_empty remaining) then Error.depends user_num remaining
  in
  let obl = subst_deps_obl obls obl in
  let scope = DeclareDef.(Global Declare.ImportNeedQualified) in
  let kind = kind_of_obligation (snd obl.obl_status) in
  let evd = Evd.from_ctx prg.prg_ctx in
  let evd = Evd.update_sigma_env evd (Global.env ()) in
  let auto pm n oblset tac = auto_solve_obligations ~pm n ~oblset tac in
  let proof_ending =
    Lemmas.Proof_ending.End_obligation
      DeclareObl.{name = prg.prg_name; num; auto}
  in
  let info = Lemmas.Info.make ~proof_ending ~scope ~kind () in
  let poly = prg.prg_poly in
  let lemma =
    Lemmas.start_lemma ~name:obl.obl_name ~poly ~info evd
      (EConstr.of_constr obl.obl_type)
  in
  let lemma = fst @@ Lemmas.by !default_tactic lemma in
  let lemma =
    Option.cata (fun tac -> Lemmas.set_endline_tactic tac lemma) lemma tac
  in
  lemma

and obligation ~pm (user_num, name, typ) tac =
  let num = pred user_num in
  let prg = get_unique_prog pm name in
  let {obls; remaining} = prg.prg_obligations in
  if num >= 0 && num < Array.length obls then
    let obl = obls.(num) in
    match obl.obl_body with
    | None -> solve_obligation prg num tac
    | Some r -> Error.already_solved num
  else Error.unknown_obligation num

and solve_obligation_by_tac ~pm prg obls i tac =
  let obl = obls.(i) in
  match obl.obl_body with
  | Some _ -> (pm, None)
  | None ->
    if List.is_empty (deps_remaining obls obl) then (
      let obl = subst_deps_obl obls obl in
      let tac =
        match tac with
        | Some t -> t
        | None -> (
          match obl.obl_tac with Some t -> t | None -> !default_tactic )
      in
      let evd = Evd.from_ctx prg.prg_ctx in
      let evd = Evd.update_sigma_env evd (Global.env ()) in
      (* Would maybe better to open a regular proof and use the std terminator? *)
      match
        solve_by_tac ?loc:(fst obl.obl_location) obl.obl_name
          (evar_of_obligation obl) tac prg.prg_poly
          (Evd.evar_universe_context evd)
      with
      | None -> (pm, None)
      | Some (t, ty, ctx) ->
        let prg = ProgramDecl.set_uctx ~uctx:ctx prg in
        (* Why is uctx not used above? *)
        let uctx = UState.univ_entry ~poly:prg.prg_poly ctx in
        let def, obl' = declare_obligation prg obl t ty uctx in
        obls.(i) <- obl';
        if def && not prg.prg_poly then
          (* Declare the term constraints with the first obligation only *)
          let evd = Evd.from_env (Global.env ()) in
          let evd =
            Evd.merge_universe_subst evd (Evd.universe_subst (Evd.from_ctx ctx))
          in
          let ctx' = Evd.evar_universe_context evd in
          (pm, Some (ProgramDecl.set_uctx ~uctx:ctx' prg))
        else (pm, Some prg) )
    else (pm, None)

and solve_prg_obligations ~pm prg ?oblset tac =
  let {obls; remaining} = prg.prg_obligations in
  let rem = ref remaining in
  let obls' = Array.copy obls in
  let set = ref Int.Set.empty in
  let p =
    match oblset with
    | None -> fun _ -> true
    | Some s ->
      set := s;
      fun i -> Int.Set.mem i !set
  in
  let pm, prg =
    Array.fold_left_i
      (fun i (pm, prg) x ->
        if p i then (
          match solve_obligation_by_tac ~pm prg obls' i tac with
          | pm, None -> (pm, prg)
          | pm, Some prg ->
            let deps = dependencies obls i in
            set := Int.Set.union !set deps;
            decr rem;
            (pm, prg) )
        else (pm, prg))
      (pm, prg) obls'
  in
  update_obls pm prg obls' !rem

and solve_obligations ~pm n tac =
  let prg = get_unique_prog pm n in
  solve_prg_obligations ~pm prg tac

and solve_all_obligations ~pm tac =
  State.fold pm ~init:pm ~f:(fun k v pm ->
      solve_prg_obligations ~pm v tac |> fst)

and try_solve_obligation ~pm n prg tac =
  let prg = get_unique_prog pm prg in
  let {obls; remaining} = prg.prg_obligations in
  let obls' = Array.copy obls in
  match solve_obligation_by_tac ~pm prg obls' n tac with
  | pm, Some prg' ->
    let pm, _r = update_obls pm prg' obls' (pred remaining) in
    pm
  | pm, None -> pm

and try_solve_obligations ~pm n tac =
  let pm, _ = solve_obligations ~pm n tac in
  pm

and auto_solve_obligations ~pm n ?oblset tac : State.t * progress =
  Flags.if_verbose Feedback.msg_info
    (str "Solving obligations automatically...");
  let prg = get_unique_prog pm n in
  solve_prg_obligations ~pm prg ?oblset tac

open Pp

let show_obligations_of_prg ?(msg = true) prg =
  let n = prg.prg_name in
  let {obls; remaining} = prg.prg_obligations in
  let showed = ref 5 in
  if msg then
    Feedback.msg_info (int remaining ++ str " obligation(s) remaining: ");
  Array.iteri
    (fun i x ->
      match x.obl_body with
      | None ->
        if !showed > 0 then (
          decr showed;
          let x = subst_deps_obl obls x in
          let env = Global.env () in
          let sigma = Evd.from_env env in
          Feedback.msg_info
            ( str "Obligation" ++ spc ()
            ++ int (succ i)
            ++ spc () ++ str "of" ++ spc () ++ Id.print n ++ str ":" ++ spc ()
            ++ hov 1
                 ( Printer.pr_constr_env env sigma x.obl_type
                 ++ str "." ++ fnl () ) ) )
      | Some _ -> ())
    obls

let show_obligations ~pm ?(msg = true) n =
  let progs =
    match n with
    | None -> State.all pm
    | Some n -> (
      match State.find pm n with
      | Some prg -> [prg]
      | None -> Error.no_obligations (Some n) )
  in
  List.iter (fun x -> show_obligations_of_prg ~msg x) progs

let show_term ~pm n =
  let prg = get_unique_prog pm n in
  let n = prg.prg_name in
  let env = Global.env () in
  let sigma = Evd.from_env env in
  Id.print n ++ spc () ++ str ":" ++ spc ()
  ++ Printer.pr_constr_env env sigma prg.prg_type
  ++ spc () ++ str ":=" ++ fnl ()
  ++ Printer.pr_constr_env env sigma prg.prg_body

let msg_generating_obl name obls =
  let len = Array.length obls in
  let info = Id.print name ++ str " has type-checked" in
  Feedback.msg_info
    ( if len = 0 then info ++ str "."
    else
      info ++ str ", generating " ++ int len
      ++ str (String.plural len " obligation") )

let add_definition ~pm ~name ?term t ~uctx ?(udecl = UState.default_univ_decl)
    ?(impargs = []) ~poly
    ?(scope = DeclareDef.Global Declare.ImportDefaultBehavior)
    ?(kind = Decls.Definition) ?tactic ?(reduce = reduce) ?hook
    ?(opaque = false) obls =
  let prg =
    ProgramDecl.make ~opaque name ~udecl term t ~uctx [] None [] obls ~impargs
      ~poly ~scope ~kind reduce ?hook
  in
  let {obls; _} = prg.prg_obligations in
  if Int.equal (Array.length obls) 0 then (
    Flags.if_verbose (msg_generating_obl name) obls;
    let pm, cst = DeclareObl.declare_definition pm prg in
    (pm, Defined cst) )
  else
    let () = Flags.if_verbose (msg_generating_obl name) obls in
    let pm = State.add pm name prg in
    let pm, res = auto_solve_obligations ~pm (Some name) tactic in
    match res with
    | Remain rem ->
      Flags.if_verbose (show_obligations ~pm ~msg:false) (Some name);
      (pm, res)
    | _ -> (pm, res)

let add_mutual_definitions ~pm l ~uctx ?(udecl = UState.default_univ_decl)
    ?tactic ~poly ?(scope = DeclareDef.Global Declare.ImportDefaultBehavior)
    ?(kind = Decls.Definition) ?(reduce = reduce) ?hook ?(opaque = false)
    notations fixkind =
  let deps = List.map (fun ({DeclareDef.Recthm.name; _}, _, _) -> name) l in
  let pm =
    List.fold_left
      (fun pm ({DeclareDef.Recthm.name; typ; impargs; _}, b, obls) ->
        let prg =
          ProgramDecl.make ~opaque name ~udecl (Some b) typ ~uctx deps
            (Some fixkind) notations obls ~impargs ~poly ~scope ~kind reduce
            ?hook
        in
        State.add pm name prg)
      pm l
  in
  let pm, _defined =
    List.fold_left
      (fun (pm, finished) x ->
        if finished then (pm, finished)
        else
          let pm, res = auto_solve_obligations ~pm (Some x) tactic in
          match res with
          | Defined _ ->
            (* If one definition is turned into a constant,
               the whole block is defined. *)
            (pm, true)
          | _ -> (pm, false))
      (pm, false) deps
  in
  pm

(* get_any_prog *)
let rec admit_all_obligations ~pm =
  let prg = State.first_pending pm in
  match prg with
  | None -> pm
  | Some prg ->
    let pm, _prog = admit_obligations ~pm prg in
    admit_all_obligations ~pm

let admit_obligations ~pm n =
  match n with
  | None -> admit_all_obligations ~pm
  | Some _ ->
    let prg = get_unique_prog pm n in
    admit_obligations ~pm prg |> fst

let next_obligation ~pm n tac =
  let prg =
    match n with
    | None -> State.first_pending pm |> Option.get
    | Some _ -> get_unique_prog pm n
  in
  let {obls; remaining} = prg.prg_obligations in
  let is_open _ obl =
    (not (defined obl)) && List.is_empty (deps_remaining obls obl)
  in
  let i =
    match Array.findi is_open obls with
    | Some i -> i
    | None -> CErrors.anomaly (Pp.str "Could not find a solvable obligation.")
  in
  solve_obligation prg i tac

let check_program_libraries () =
  Coqlib.check_required_library Coqlib.datatypes_module_name;
  Coqlib.check_required_library ["Coq"; "Init"; "Specif"];
  Coqlib.check_required_library ["Coq"; "Program"; "Tactics"]
