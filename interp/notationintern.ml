(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Notation interpretation pass. Split from constrintern by EJGA *)

open Names
open Libnames
open Globnames
open Notation_term
open Notation_ops
open Constrexpr

(* START Duplicated with constrintern *)

let make_subst ids l =
  let fold accu (id, scl) a = Id.Map.add id (a, scl) accu in
  List.fold_left2 fold Id.Map.empty ids l

let error_not_enough_arguments loc =
  CErrors.user_err ~loc Pp.(str "Abbreviation is not applied enough.")

let split_by_type ids =
  List.fold_right (fun (x,(scl,typ)) (l1,l2,l3) ->
    match typ with
    | NtnTypeConstr | NtnTypeOnlyBinder -> ((x,scl)::l1,l2,l3)
    | NtnTypeConstrList -> (l1,(x,scl)::l2,l3)
    | NtnTypeBinderList -> (l1,l2,(x,scl)::l3)) ids ([],[],[])

let rec wildcards ntn n =
  if Int.equal n (String.length ntn) then []
  else let l = spaces ntn (n+1) in if ntn.[n] == '_' then n::l else l
and spaces ntn n =
  if Int.equal n (String.length ntn) then []
  else if ntn.[n] == ' ' then wildcards ntn (n+1) else spaces ntn (n+1)

let expand_notation_string ntn n =
  let pos = List.nth (wildcards ntn 0) n in
  let hd = if Int.equal pos 0 then "" else String.sub ntn 0 pos in
  let tl =
    if Int.equal pos (String.length ntn) then ""
    else String.sub ntn (pos+1) (String.length ntn - pos -1) in
  hd ^ "{ _ }" ^ tl

let locate_reference qid =
  Smartlocate.global_of_extended_global (Nametab.locate_extended qid)

let global_reference_of_reference ref =
  locate_reference (snd (qualid_of_reference ref))

let check_duplicate loc fields =
  let eq (ref1, _) (ref2, _) = eq_reference ref1 ref2 in
  let dups = CList.duplicates eq fields in
  match dups with
  | [] -> ()
  | (r, _) :: _ ->
    CErrors.user_err ~loc Pp.(str "This record defines several times the field " ++
      pr_reference r ++ str ".")

(** [sort_fields ~complete loc fields completer] expects a list
    [fields] of field assignments [f = e1; g = e2; ...], where [f, g]
    are fields of a record and [e1] are "values" (either terms, when
    interning a record construction, or patterns, when intering record
    pattern-matching). It will sort the fields according to the record
    declaration order (which is important when type-checking them in
    presence of dependencies between fields). If the parameter
    [complete] is true, we require the assignment to be complete: all
    the fields of the record must be present in the
    assignment. Otherwise the record assignment may be partial
    (in a pattern, we may match on some fields only), and we call the
    function [completer] to fill the missing fields; the returned
    field assignment list is always complete. *)
let sort_fields ~complete loc fields completer =
  match fields with
    | [] -> None
    | (first_field_ref, first_field_value):: other_fields ->
        let (first_field_glob_ref, record) =
          try
            let gr = global_reference_of_reference first_field_ref in
            (gr, Recordops.find_projection gr)
          with Not_found ->
            CErrors.user_err ~loc:(loc_of_reference first_field_ref) ~hdr:"intern"
              Pp.(pr_reference first_field_ref ++ str": Not a projection")
        in
        (* the number of parameters *)
        let nparams = record.Recordops.s_EXPECTEDPARAM in
        (* the reference constructor of the record *)
        let base_constructor =
          let global_record_id = ConstructRef record.Recordops.s_CONST in
          try Qualid (loc, Nametab.shortest_qualid_of_global Id.Set.empty global_record_id)
          with Not_found ->
            CErrors.anomaly Pp.(str "Environment corruption for records") in
        let () = check_duplicate loc fields in
        let (end_index,    (* one past the last field index *)
             first_field_index,  (* index of the first field of the record *)
             proj_list)    (* list of projections *)
          =
          (* elimitate the first field from the projections,
             but keep its index *)
          let rec build_proj_list projs proj_kinds idx ~acc_first_idx acc =
            match projs with
              | [] -> (idx, acc_first_idx, acc)
              | (Some name) :: projs ->
                 let field_glob_ref = ConstRef name in
                 let first_field = eq_gr field_glob_ref first_field_glob_ref in
                 begin match proj_kinds with
                    | [] -> CErrors.anomaly Pp.(str "Number of projections mismatch")
                    | (_, regular) :: proj_kinds ->
                       (* "regular" is false when the field is defined
                           by a let-in in the record declaration
                           (its value is fixed from other fields). *)
                       if first_field && not regular && complete then
                         CErrors.user_err ~loc Pp.(str "No local fields allowed in a record construction.")
                       else if first_field then
                         build_proj_list projs proj_kinds (idx+1) ~acc_first_idx:idx acc
                       else if not regular && complete then
                         (* skip non-regular fields *)
                         build_proj_list projs proj_kinds idx ~acc_first_idx acc
                       else
                         build_proj_list projs proj_kinds (idx+1) ~acc_first_idx
                                         ((idx, field_glob_ref) :: acc)
                 end
              | None :: projs ->
                 if complete then
                   (* we don't want anonymous fields *)
                   CErrors.user_err ~loc Pp.(str "This record contains anonymous fields.")
                 else
                   (* anonymous arguments don't appear in proj_kinds *)
                   build_proj_list projs proj_kinds (idx+1) ~acc_first_idx acc
          in
          build_proj_list record.Recordops.s_PROJ record.Recordops.s_PROJKIND 1 ~acc_first_idx:0 []
        in
        (* now we want to have all fields assignments indexed by their place in
           the constructor *)
        let rec index_fields fields remaining_projs acc =
          match fields with
            | (field_ref, field_value) :: fields ->
               let field_glob_ref = try global_reference_of_reference field_ref
               with Not_found ->
                 CErrors.user_err ~loc:(loc_of_reference field_ref) ~hdr:"intern"
                               Pp.(str "The field \"" ++ pr_reference field_ref ++ str "\" does not exist.") in
               let remaining_projs, (field_index, _) =
                 let the_proj (idx, glob_ref) = eq_gr field_glob_ref glob_ref in
                 try CList.extract_first the_proj remaining_projs
                 with Not_found ->
                   CErrors.user_err ~loc 
                     Pp.(str "This record contains fields of different records.")
               in
               index_fields fields remaining_projs ((field_index, field_value) :: acc)
            | [] ->
               (* the order does not matter as we sort them next,
                  List.rev_* is just for efficiency *)
               let remaining_fields =
                 let complete_field (idx, _field_ref) = (idx, completer idx) in
                 List.rev_map complete_field remaining_projs in
               List.rev_append remaining_fields acc
        in
        let unsorted_indexed_fields =
          index_fields other_fields proj_list
            [(first_field_index, first_field_value)] in
        let sorted_indexed_fields =
          let cmp_by_index (i, _) (j, _) = Int.compare i j in
          List.sort cmp_by_index unsorted_indexed_fields in
        let sorted_fields = List.map snd sorted_indexed_fields in
        Some (nparams, base_constructor, sorted_fields)

(* END   Duplicated with constrintern *)

type raw_cases_pattern_expr =
  | RCPatAlias  of Loc.t * raw_cases_pattern_expr * Id.t
  | RCPatCstr   of Loc.t * Globnames.global_reference
    * raw_cases_pattern_expr list * raw_cases_pattern_expr list
  (** [CPatCstr (_, c, l1, l2)] represents ((@c l1) l2) *)
  | RCPatAtom   of Loc.t * Id.t option
  | RCPatOr     of Loc.t * raw_cases_pattern_expr list

let raw_cases_pattern_expr_loc = function
  | RCPatAlias (loc,_,_) -> loc
  | RCPatCstr (loc,_,_,_) -> loc
  | RCPatAtom (loc,_) -> loc
  | RCPatOr (loc,_) -> loc

type notation_resolution_error =
  | NotAConstructor of reference

exception NotationResolutionError of Loc.t * notation_resolution_error

let explain_not_a_constructor ref =
  Pp.(str "Unknown constructor: " ++ pr_reference ref)

let explain_notation_resolution_error e =
  let pp = match e with
    | NotAConstructor ref -> explain_not_a_constructor ref
  in Pp.(pp ++ str ".")

let error_bad_inductive_type ?loc =
  CErrors.user_err ?loc Pp.(str
    "This should be an inductive type applied to patterns.")

(* Auxiliary functions *)
let find_pattern_variable = function
  | Ident (loc,id) -> id
  | Qualid (loc,_) as x -> raise (NotationResolutionError(loc,NotAConstructor x))

let rec subst_pat_iterator y t p = match p with
  | RCPatAtom (_,id) ->
    begin match id with Some x when Id.equal x y -> t | _ -> p end
  | RCPatCstr (loc,id,l1,l2) ->
    RCPatCstr (loc,id,List.map (subst_pat_iterator y t) l1,
 	       List.map (subst_pat_iterator y t) l2)
  | RCPatAlias (l,p,a) -> RCPatAlias (l,subst_pat_iterator y t p,a)
  | RCPatOr (l,pl) -> RCPatOr (l,List.map (subst_pat_iterator y t) pl)

(**********************************************************************)
(* Contracting "{ _ }" in notations *)

let contract_pat_notation ntn (l,ll) =
  let ntn' = ref ntn in
  let rec contract_squash n = function
    | [] -> []
    | CPatNotation (_,"{ _ }",([a],[]),[]) :: l ->
        ntn' := expand_notation_string !ntn' n;
        contract_squash n (a::l)
    | a :: l ->
        a::contract_squash (n+1) l in
  let l = contract_squash 0 l in
  (* side effect; don't inline *)
  !ntn',(l,ll)

(** {5 Notation Resolution for Patterns } *)

(** {6 Elementary bricks } *)

let rec simple_adjust_scopes n scopes =
  (* Note: they can be less scopes than arguments but also more scopes *)
  (* than arguments because extra scopes are used in the presence of *)
  (* coercions to funclass *)
  if Int.equal n 0 then [] else match scopes with
  | [] -> None :: simple_adjust_scopes (n-1) []
  | sc::scopes -> sc :: simple_adjust_scopes (n-1) scopes


let find_remaining_scopes pl1 pl2 ref =
  let impls_st = Impargs.implicits_of_global ref in
  let len_pl1 = List.length pl1 in
  let len_pl2 = List.length pl2 in
  let impl_list = if Int.equal len_pl1 0
    then Impargs.select_impargs_size len_pl2 impls_st
    else CList.skipn_at_least len_pl1 (Impargs.select_stronger_impargs impls_st) in
  let allscs = Notation.find_arguments_scope ref in
  let scope_list = CList.skipn_at_least len_pl1 allscs in
  let rec aux = function
    |[],l -> l
    |_,[] -> []
    |h::t,_::tt when Impargs.is_status_implicit h -> aux (t,tt)
    |_::t,h::tt -> h :: aux (t,tt)
  in ((try CList.firstn len_pl1 allscs with Failure _ -> simple_adjust_scopes len_pl1 allscs),
      simple_adjust_scopes len_pl2 (aux (impl_list,scope_list)))

(** {6 Main Routine for Notation Expansion }

    @returns a raw_case_pattern_expr :
    - no notations and syntactic definition
    - global reference and identifeir instead of reference

*)

let drop_notations_pattern looked_for =
  (* At toplevel, Constructors and Inductives are accepted, in recursive calls
     only constructor are allowed *)
  let ensure_kind top loc g =
    try
      if top then looked_for g else
      match g with ConstructRef _ -> () | _ -> raise Not_found
    with Not_found ->
      Topconstr.error_invalid_pattern_notation ~loc ()
  in
  let test_kind top =
    if top then looked_for else function ConstructRef _ -> () | _ -> raise Not_found
  in
  (** [rcp_of_glob] : from [glob_constr] to [raw_cases_pattern_expr] *)
  let rec rcp_of_glob = let open Glob_term in function
    | GVar (loc,id) -> RCPatAtom (loc,Some id)
    | GHole (loc,_,_,_) -> RCPatAtom (loc,None)
    | GRef (loc,g,_) -> RCPatCstr (loc, g,[],[])
    | GApp (loc,GRef (_,g,_),l) -> RCPatCstr (loc, g, List.map rcp_of_glob l,[])
    | _ -> CErrors.anomaly Pp.(str "Invalid return pattern from Notation.interp_prim_token_cases_pattern_expr ")
  in
  let rec drop_syndef top scopes re pats =
    let (loc,qid) = qualid_of_reference re in
    try
      match Nametab.locate_extended qid with
      | SynDef sp ->
	let (vars,a) = Syntax_def.search_syntactic_definition sp in
	(match a with
	| NRef g ->
          (* Convention: do not deactivate implicit arguments and scopes for further arguments *)
	  test_kind top g;
	  let () = assert (CList.is_empty vars) in
	  let (_,argscs) = find_remaining_scopes [] pats g in
	  Some (g, [], List.map2 (in_pat_sc scopes) argscs pats)
	| NApp (NRef g,[]) -> (* special case: Syndef for @Cstr, this deactivates *)
	      test_kind top g;
              let () = assert (CList.is_empty vars) in
	      Some (g, List.map (in_pat false scopes) pats, [])
	| NApp (NRef g,args) ->
              (* Convention: do not deactivate implicit arguments and scopes for further arguments *)
	      test_kind top g;
	      let nvars = List.length vars in
	      if List.length pats < nvars then error_not_enough_arguments loc;
	      let pats1,pats2 = CList.chop nvars pats in
	      let subst = make_subst vars pats1 in
	      let idspl1 = List.map (in_not false loc scopes (subst, Id.Map.empty) []) args in
	      let (_,argscs) = find_remaining_scopes pats1 pats2 g in
	      Some (g, idspl1, List.map2 (in_pat_sc scopes) argscs pats2)
	| _ -> raise Not_found)
      | TrueGlobal g ->
	  test_kind top g;
	  Dumpglob.add_glob loc g;
	  let (_,argscs) = find_remaining_scopes [] pats g in
	  Some (g,[],List.map2 (fun x -> in_pat false (x,snd scopes)) argscs pats)
    with Not_found -> None
  and in_pat top scopes = function
    | CPatAlias (loc, p, id) -> RCPatAlias (loc, in_pat top scopes p, id)
    | CPatRecord (loc, l) ->
      let sorted_fields =
	sort_fields ~complete:false loc l (fun _idx -> (CPatAtom (loc, None))) in
      begin match sorted_fields with
	| None -> RCPatAtom (loc, None)
	| Some (n, head, pl) ->
          let pl =
            if !Topconstr.asymmetric_patterns then pl else
            let pars = CList.make n (CPatAtom (loc, None)) in
            List.rev_append pars pl in
	  match drop_syndef top scopes head pl with
	    |Some (a,b,c) -> RCPatCstr(loc, a, b, c)
	    |None -> raise (NotationResolutionError (loc,NotAConstructor head))
      end
    | CPatCstr (loc, head, None, pl) ->
      begin
	match drop_syndef top scopes head pl with
	  | Some (a,b,c) -> RCPatCstr(loc, a, b, c)
	  | None -> raise (NotationResolutionError (loc,NotAConstructor head))
      end
     | CPatCstr (loc, r, Some expl_pl, pl) ->
      let g = try Nametab.locate (snd (qualid_of_reference r))
	      with Not_found ->
 	      raise (NotationResolutionError (loc,NotAConstructor r)) in
      if expl_pl == [] then
        (* Convention: (@r) deactivates all further implicit arguments and scopes *)
        RCPatCstr (loc, g, List.map (in_pat false scopes) pl, [])
      else
        (* Convention: (@r expl_pl) deactivates implicit arguments in expl_pl and in pl *)
        (* but not scopes in expl_pl *)
        let (argscs1,_) = find_remaining_scopes expl_pl pl g in
        RCPatCstr (loc, g, List.map2 (in_pat_sc scopes) argscs1 expl_pl @ List.map (in_pat false scopes) pl, [])
    | CPatNotation (loc,"- _",([CPatPrim(_,Numeral p)],[]),[])
      when Bigint.is_strictly_pos p ->
      let (pat, _df) = Notation.interp_prim_token_cases_pattern_expr loc (ensure_kind false loc) (Numeral (Bigint.neg p)) scopes in
      rcp_of_glob pat
    | CPatNotation (_,"( _ )",([a],[]),[]) ->
      in_pat top scopes a
    | CPatNotation (loc, ntn, fullargs,extrargs) ->
      let ntn,(args,argsl as fullargs) = contract_pat_notation ntn fullargs in
      let ((ids',c),df) = Notation.interp_notation loc ntn scopes in
      let (ids',idsl',_) = split_by_type ids' in
      Dumpglob.dump_notation_location (Topconstr.patntn_loc loc fullargs ntn) ntn df;
      let substlist = make_subst idsl' argsl in
      let subst = make_subst ids' args in
      in_not top loc scopes (subst,substlist) extrargs c
    | CPatDelimiters (loc, key, e) ->
      in_pat top (None,Notation.find_delimiters_scope loc key::snd scopes) e
    | CPatPrim (loc,p) ->
      let (pat, _df) = Notation.interp_prim_token_cases_pattern_expr loc (test_kind false) p scopes in
      rcp_of_glob pat
    | CPatAtom (loc, Some id) ->
      begin
	match drop_syndef top scopes id [] with
	  |Some (a,b,c) -> RCPatCstr (loc, a, b, c)
	  |None -> RCPatAtom (loc, Some (find_pattern_variable id))
      end
    | CPatAtom (loc,None) -> RCPatAtom (loc,None)
    | CPatOr (loc, pl) ->
      RCPatOr (loc,List.map (in_pat top scopes) pl)
    | CPatCast (loc,_,_) ->
      (* We raise an error if the pattern contains a cast, due to
         current restrictions on casts in patterns. Cast in patterns
         are supportted only in local binders and only at top
         level. In fact, they are currently eliminated by the
         parser. The only reason why they are in the
         [cases_pattern_expr] type is that the parser needs to factor
         the "(c : t)" notation with user defined notations (such as
         the pair). In the long term, we will try to support such
         casts everywhere, and use them to print the domains of
         lambdas in the encoding of match in constr. This check is
         here and not in the parser because it would require to
         duplicate the levels of the [pattern] rule. *)
      CErrors.user_err ~loc ~hdr:"drop_notations_pattern"
                            (Pp.strbrk "Casts are not supported in this pattern.")
  and in_pat_sc scopes x = in_pat false (x,snd scopes)
  and in_not top loc scopes (subst,substlist as fullsubst) args = function
    | NVar id ->
      let () = assert (CList.is_empty args) in
      begin
	(* subst remembers the delimiters stack in the interpretation *)
	(* of the notations *)
	try
	  let (a,(scopt,subscopes)) = Id.Map.find id subst in
	  in_pat top (scopt,subscopes@snd scopes) a
	with Not_found ->
	  if Id.equal id ldots_var then RCPatAtom (loc,Some id) else
	    CErrors.anomaly Pp.(str "Unbound pattern notation variable: " ++ Id.print id)
      end
    | NRef g ->
      ensure_kind top loc g;
      let (_,argscs) = find_remaining_scopes [] args g in
      RCPatCstr (loc, g, [], List.map2 (in_pat_sc scopes) argscs args)
    | NApp (NRef g,pl) ->
      ensure_kind top loc g;
      let (argscs1,argscs2) = find_remaining_scopes pl args g in
      RCPatCstr (loc, g,
		 List.map2 (fun x -> in_not false loc (x,snd scopes) fullsubst []) argscs1 pl @
		 List.map (in_pat false scopes) args, [])
    | NList (x,y,iter,terminator,lassoc) ->
      if not (CList.is_empty args) then CErrors.user_err ~loc 
        Pp.(strbrk "Application of arguments to a recursive notation not supported in patterns.");
      (try
         (* All elements of the list are in scopes (scopt,subscopes) *)
	 let (l,(scopt,subscopes)) = Id.Map.find x substlist in
         let termin = in_not top loc scopes fullsubst [] terminator in
	 List.fold_right (fun a t ->
           let nsubst = Id.Map.add y (a, (scopt, subscopes)) subst in
           let u = in_not false loc scopes (nsubst, substlist) [] iter in
           subst_pat_iterator ldots_var t u)
           (if lassoc then List.rev l else l) termin
       with Not_found ->
         CErrors.anomaly Pp.(str "Inconsistent substitution of recursive notation"))
    | NHole _ ->
      let () = assert (CList.is_empty args) in
      RCPatAtom (loc, None)
    | t -> Topconstr.error_invalid_pattern_notation ~loc ()
  in in_pat true

let drop_notations_pattern ~inductive_pattern scopes pat = 
  if inductive_pattern then
    try drop_notations_pattern (function (IndRef _ | ConstructRef _) -> () | _ -> raise Not_found) scopes pat
    with NotationResolutionError(loc, NotAConstructor _) -> error_bad_inductive_type ~loc
  else
    try drop_notations_pattern (function ConstructRef _ -> () | _ -> raise Not_found) scopes pat
    with NotationResolutionError (loc,e) ->
      CErrors.user_err ~loc ~hdr:"internalize" (explain_notation_resolution_error e)
