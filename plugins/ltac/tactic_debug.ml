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
open Pp
open Tacexpr

let (ltac_trace_info : ltac_trace Exninfo.t) = Exninfo.make ()

let prtac x =
  let env = Global.env () in
  Pptactic.pr_glob_tactic env x
let prmatchpatt env sigma hyp =
  Pptactic.pr_match_pattern (Printer.pr_constr_pattern_env env sigma) hyp
let prmatchrl env sigma rl =
  Pptactic.pr_match_rule false prtac
    (fun (_,p) -> Printer.pr_constr_pattern_env env sigma p) rl

(* This module intends to be a beginning of debugger for tactic expressions.
   Currently, it is quite simple and we can hope to have, in the future, a more
   complete panel of commands dedicated to a proof assistant framework *)

(* Debug information *)
type debug_info =
  | DebugOn of int
  | DebugOff

(* An exception handler *)
let explain_logic_error e = CErrors.print e
let explain_logic_error_no_anomaly e = CErrors.print_no_report e

(* Communications with the outside world *)
module Comm = struct

  (* TODO: ideally we would check that the debugger hooks are
     correctly set, however we don't do this yet as debugger
     initialiation is unconditionally done for example in coqc, which
     doesn't support the debugger interface. Improving this would
     require some tweaks in tacinterp which are out of scope for the
     current refactoring. *)
  let init () = match DebugHook.Intf.get () with
    | Some _ -> ()
    | None -> ()
      (* CErrors.user_err
       *   (Pp.str "The LTAC debugger is not supported by your user interface") *)

  let hook () = Option.get (DebugHook.Intf.get ())
  let wrap = Proofview.NonLogical.make

  open DebugHook.Intf
  open DebugHook.Answer

  let prompt g = wrap (fun () -> (hook ()).submit_answer (Prompt g))
  let goal g = wrap (fun () -> (hook ()).submit_answer (Goal g))
  let output g = wrap (fun () -> (hook ()).submit_answer (Output g))
  let read = wrap (fun () -> (hook ()).read_cmd ())

end

(* Prints the goal *)

let db_pr_goal gl =
  let env = Proofview.Goal.env gl in
  let concl = Proofview.Goal.concl gl in
  let penv = Termops.Internal.print_named_context env in
  let pc = Printer.pr_econstr_env env (Tacmach.New.project gl) concl in
    str"  " ++ hv 0 (penv ++ fnl () ++
                   str "============================" ++ fnl ()  ++
                   str" "  ++ pc) ++ fnl ()

let db_pr_goal =
  Proofview.Goal.enter begin fun gl ->
  let pg = db_pr_goal gl in
  Proofview.tclLIFT (Comm.goal pg)
  end

(* Prints the commands *)
let help () =
  Comm.output
    (str "Commands: <Enter> = Step" ++ fnl() ++
     str "          h/? = Help" ++ fnl() ++
     str "          r <num> = Run <num> times" ++ fnl() ++
     str "          r <string> = Run up to next idtac <string>" ++ fnl() ++
     str "          s = Skip" ++ fnl() ++
     str "          x = Exit")

(* Prints the goal and the command to be executed *)
let goal_com tac =
  Proofview.tclTHEN
    db_pr_goal
    (Proofview.tclLIFT (Comm.output (str "Going to execute:" ++ fnl () ++ prtac tac)))

(* [run (new_ref _)] gives us a ref shared among [NonLogical.t]
   expressions. It avoids parametrizing everything over a
   reference. *)
let skipped = Proofview.NonLogical.run (Proofview.NonLogical.ref 0)
let skip = Proofview.NonLogical.run (Proofview.NonLogical.ref 0)
let breakpoint = Proofview.NonLogical.run (Proofview.NonLogical.ref None)

let batch = ref false

open Goptions

let () =
  declare_bool_option
    { optdepr  = false;
      optkey   = ["Ltac";"Batch";"Debug"];
      optread  = (fun () -> !batch);
      optwrite = (fun x -> batch := x) }

(* (Re-)initialize debugger *)
let db_initialize =
  let open Proofview.NonLogical in
  make Comm.init >>
  (skip:=0) >> (skipped:=0) >> (breakpoint:=None)

(* Prints the run counter *)
let run ini =
  let open Proofview.NonLogical in
  if not ini then
    begin
      !skipped >>= fun skipped ->
      Comm.output (str "Executed expressions: " ++ int skipped ++ fnl())
    end >>
    !skipped >>= fun x ->
    skipped := x+1
  else
    return ()

(* Prints the prompt *)
let rec prompt level =
  (* spiwack: avoid overriding by the open below *)
  let runtrue = run true in
    let open Proofview.NonLogical in
    Comm.prompt (str "TcDebug (" ++ int level ++ str ") > ") >>
    if Util.(!batch) then return (DebugOn (level+1)) else
    let exit = (skip:=0) >> (skipped:=0) >> raise (Sys.Break, Exninfo.null) in
    Comm.read >>= fun inst ->
    let open DebugHook.Action in
    match inst with
    | Step  -> return (DebugOn (level+1))
    | Skip -> return (DebugOff)
    | Exit -> Proofview.NonLogical.print_char '\b' >> exit
    | Help -> help () >> prompt level
    | RunCnt num -> (skip:=num) >> (skipped:=0) >>
        runtrue >> return (DebugOn (level+1))
    | RunBreakpoint s -> (breakpoint:=(Some s)) >>
        runtrue >> return (DebugOn (level+1))
    | Failed -> prompt level

(* Prints the state and waits for an instruction *)
(* spiwack: the only reason why we need to take the continuation [f]
   as an argument rather than returning the new level directly seems to
   be that [f] is wrapped in with "explain_logic_error". I don't think
   it serves any purpose in the current design, so we could just drop
   that. *)
let debug_prompt lev tac f =
  (* spiwack: avoid overriding by the open below *)
  let runfalse = run false in
  let open Proofview.NonLogical in
  let (>=) = Proofview.tclBIND in
  (* What to print and to do next *)
  let newlevel =
    Proofview.tclLIFT !skip >= fun initial_skip ->
    if Int.equal initial_skip 0 then
      Proofview.tclLIFT !breakpoint >= fun breakpoint ->
      if Option.is_empty breakpoint then Proofview.tclTHEN (goal_com tac) (Proofview.tclLIFT (prompt lev))
      else Proofview.tclLIFT(runfalse >> return (DebugOn (lev+1)))
    else Proofview.tclLIFT begin
      (!skip >>= fun s -> skip:=s-1) >>
      runfalse >>
      !skip >>= fun new_skip ->
      (if Int.equal new_skip 0 then skipped:=0 else return ()) >>
      return (DebugOn (lev+1))
    end in
  newlevel >= fun newlevel ->
  (* What to execute *)
  Proofview.tclOR
    (f newlevel)
    begin fun (reraise, info) ->
      Proofview.tclTHEN
        (Proofview.tclLIFT begin
          (skip:=0) >> (skipped:=0) >>
          Comm.output (str "Level " ++ int lev ++ str ": " ++ explain_logic_error reraise)
        end)
        (Proofview.tclZERO ~info reraise)
    end

let is_debug db =
  let open Proofview.NonLogical in
  !breakpoint >>= fun breakpoint ->
  match db, breakpoint with
  | DebugOff, _ -> return false
  | _, Some _ -> return false
  | _ ->
      !skip >>= fun skip ->
      return (Int.equal skip 0)

(* Prints a constr *)
let db_constr debug env sigma c =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
    Comm.output (str "Evaluated term: " ++ Printer.pr_econstr_env env sigma c)
  else return ()

(* Prints the pattern rule *)
let db_pattern_rule debug env sigma num r =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
  begin
    Comm.output
      (str "Pattern rule " ++ int num ++ str ":" ++ fnl () ++
       str "|" ++ spc () ++ prmatchrl env sigma r)
  end
  else return ()

(* Prints the hypothesis pattern identifier if it exists *)
let hyp_bound = function
  | Anonymous -> str " (unbound)"
  | Name id -> str " (bound to " ++ Id.print id ++ str ")"

(* Prints a matched hypothesis *)
let db_matched_hyp debug env sigma (id,_,c) ido =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
    Comm.output
      (str "Hypothesis " ++ Id.print id ++ hyp_bound ido ++
       str " has been matched: " ++ Printer.pr_econstr_env env sigma c)
  else return ()

(* Prints the matched conclusion *)
let db_matched_concl debug env sigma c =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
    Comm.output
      (str "Conclusion has been matched: " ++ Printer.pr_econstr_env env sigma c)
  else return ()

(* Prints a success message when the goal has been matched *)
let db_mc_pattern_success debug =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
    Comm.output
      (str "The goal has been successfully matched!" ++ fnl() ++
       str "Let us execute the right-hand side part..." ++ fnl())
  else return ()

(* Prints a failure message for an hypothesis pattern *)
let db_hyp_pattern_failure debug env sigma (na,hyp) =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
    Comm.output
      (str "The pattern hypothesis" ++ hyp_bound na ++
       str " cannot match: " ++
       prmatchpatt env sigma hyp)
  else return ()

(* Prints a matching failure message for a rule *)
let db_matching_failure debug =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
    Comm.output
      (str "This rule has failed due to matching errors!" ++ fnl() ++
       str "Let us try the next one...")
  else return ()

(* Prints an evaluation failure message for a rule *)
let db_eval_failure debug s =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
    let s = str "message \"" ++ s ++ str "\"" in
    Comm.output
      (str "This rule has failed due to \"Fail\" tactic (" ++
       s ++ str ", level 0)!" ++ fnl() ++ str "Let us try the next one...")
  else return ()

(* Prints a logic failure message for a rule *)
let db_logic_failure debug err =
  let open Proofview.NonLogical in
  is_debug debug >>= fun db ->
  if db then
  begin
    Comm.output
      (explain_logic_error err) >>
    Comm.output
      (str "This rule has failed due to a logic error!" ++ fnl() ++
       str "Let us try the next one...")
  end
  else return ()

let is_breakpoint brkname s = match brkname, s with
  | Some s, MsgString s'::_ -> String.equal s s'
  | _ -> false

let db_breakpoint debug s =
  let open Proofview.NonLogical in
  !breakpoint >>= fun opt_breakpoint ->
  match debug with
  | DebugOn lev when not (CList.is_empty s) && is_breakpoint opt_breakpoint s ->
      breakpoint:=None
  | _ ->
      return ()

(** Extrating traces *)

let is_defined_ltac trace =
  let rec aux = function
  | (_, Tacexpr.LtacNameCall f) :: _ -> not (Tacenv.is_ltac_for_ml_tactic f)
  | (_, Tacexpr.LtacNotationCall f) :: _ -> true
  | (_, Tacexpr.LtacAtomCall _) :: _ -> false
  | _ :: tail -> aux tail
  | [] -> false in
  aux (List.rev trace)

let explain_ltac_call_trace last trace loc =
  let calls = last :: List.rev_map snd trace in
  let pr_call ck = match ck with
    | Tacexpr.LtacNotationCall kn -> quote (Pptactic.pr_alias_key kn)
  | Tacexpr.LtacNameCall cst -> quote (Pptactic.pr_ltac_constant cst)
  | Tacexpr.LtacMLCall t ->
      quote (prtac t)
  | Tacexpr.LtacVarCall (id,t) ->
      quote (Id.print id) ++ strbrk " (bound to " ++
        prtac t ++ str ")"
  | Tacexpr.LtacAtomCall te ->
      quote (prtac (Tacexpr.TacAtom (CAst.make te)))
  | Tacexpr.LtacConstrInterp (c, { Ltac_pretype.ltac_constrs = vars }) ->
    (* XXX: This hooks into the CErrors's additional error info API so
       it is tricky to provide the right env for now. *)
      let env = Global.env() in
      let sigma = Evd.from_env env in
      quote (Printer.pr_glob_constr_env env sigma c) ++
        (if not (Id.Map.is_empty vars) then
          strbrk " (with " ++
            prlist_with_sep pr_comma
            (fun (id,c) ->
              Id.print id ++ str ":=" ++ Printer.pr_lconstr_under_binders_env env sigma c)
            (List.rev (Id.Map.bindings vars)) ++ str ")"
        else mt())
  in
  match calls with
  | [] -> mt ()
  | [a] -> hov 0 (str "Ltac call to " ++ pr_call a ++ str " failed.")
  | _ ->
    let kind_of_last_call = match List.last calls with
    | Tacexpr.LtacConstrInterp _ -> ", last term evaluation failed."
    | _ -> ", last call failed."
    in
    hov 0 (str "In nested Ltac calls to " ++
           pr_enum pr_call calls ++ strbrk kind_of_last_call)

let skip_extensions trace =
  let rec aux = function
  | (_,Tacexpr.LtacNotationCall _ as tac) :: (_,Tacexpr.LtacMLCall _) :: tail ->
     (* Case of an ML defined tactic with entry of the form <<"foo" args>> *)
     (* see tacextend.mlp *)
     tac :: aux tail
  | t :: tail -> t :: aux tail
  | [] -> [] in
  List.rev (aux (List.rev trace))

let extract_ltac_trace ?loc trace =
  let trace = skip_extensions trace in
  let (tloc,c),tail = List.sep_last trace in
  if is_defined_ltac trace then
    (* We entered a user-defined tactic,
       we display the trace with location of the call *)
    let msg = hov 0 (explain_ltac_call_trace c tail loc ++ fnl()) in
    (if Loc.finer loc tloc then loc else tloc), msg
  else
    (* We entered a primitive tactic, we don't display trace but
       report on the finest location *)
    let best_loc =
      (* trace is with innermost call coming first *)
      let rec aux best_loc = function
        | (loc,_)::tail ->
           if Option.is_empty best_loc ||
              not (Option.is_empty loc) && Loc.finer loc best_loc
           then
             aux loc tail
           else
             aux best_loc tail
        | [] -> best_loc in
        aux loc trace in
    best_loc, mt ()

let get_ltac_trace info =
  let ltac_trace = Exninfo.get info ltac_trace_info in
  let loc = Loc.get_loc info in
  match ltac_trace with
  | None -> None
  | Some trace -> Some (extract_ltac_trace ?loc trace)

let () = CErrors.register_additional_error_info get_ltac_trace
