(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Pp
open ConsoleOps

(** Console display of feedback *)

(** Default tags *)
module Tag = struct

  let error   = "message.error"
  let warning = "message.warning"
  let debug   = "message.debug"

end

let msgnl_with ?pre_hdr fmt strm =
  pp_with fmt (strm ++ fnl ());
  Format.pp_print_flush fmt ()

module Emacs = struct

  (* Special chars for emacs, to detect warnings inside goal output *)
  let quote_warning_start = "<warning>"
  let quote_warning_end = "</warning>"

  let quote_info_start = "<infomsg>"
  let quote_info_end = "</infomsg>"

  let quote_emacs q_start q_end msg =
    hov 0 (seq [str q_start; brk(0,0); msg; brk(0,0); str q_end])

  let quote_warning = quote_emacs quote_warning_start quote_warning_end
  let quote_info = quote_emacs quote_info_start quote_info_end

end

let  dbg_hdr = tag Tag.debug   (str "Debug:")   ++ spc ()
let info_hdr = mt ()
let warn_hdr = tag Tag.warning (str "Warning:") ++ spc ()
let  err_hdr = tag Tag.error   (str "Error:")   ++ spc ()

let make_body quoter info ?pre_hdr s =
  pr_opt_no_spc (fun x -> x ++ fnl ()) pre_hdr ++ quoter (hov 0 (info ++ s))

(* The empty quoter *)
let noq x = x
(* Generic logger *)
let gen_logger dbg warn ?pre_hdr level msg = let open Feedback in match level with
  | Debug   -> msgnl_with !std_ft (make_body dbg  dbg_hdr ?pre_hdr msg)
  | Info    -> msgnl_with !std_ft (make_body dbg info_hdr ?pre_hdr msg)
  | Notice  -> msgnl_with !std_ft (make_body noq info_hdr ?pre_hdr msg)
  | Warning -> Flags.if_warn (fun () ->
               msgnl_with !err_ft (make_body warn warn_hdr ?pre_hdr msg)) ()
  | Error   -> msgnl_with !err_ft (make_body noq   err_hdr ?pre_hdr msg)

(** Standard loggers *)

(* We provide a generic clear_log_backend callback for backends
   wanting to do cleanup after the print.
*)
let std_logger_cleanup = ref (fun () -> ())

let std_logger ?pre_hdr level msg =
  gen_logger (fun x -> x) (fun x -> x) ?pre_hdr level msg;
  !std_logger_cleanup ()

(** Color logging. Moved from Ppstyle, it may need some more refactoring  *)

(* Tag map for terminal style *)
let default_tag_map () = let open Terminal in [
  (* Local to console toplevel *)
    "message.error"    , make ~bold:true ~fg_color:`WHITE ~bg_color:`RED ()
  ; "message.warning"  , make ~bold:true ~fg_color:`WHITE ~bg_color:`YELLOW ()
  ; "message.debug"    , make ~bold:true ~fg_color:`WHITE ~bg_color:`MAGENTA ()
  (* Coming from the printer *)
  ; "constr.evar"      , make            ~fg_color:`LIGHT_BLUE ()
  ; "constr.keyword"   , make ~bold:true ()
  ; "constr.type"      , make ~bold:true ~fg_color:`YELLOW ()
  ; "constr.notation"  , make ~fg_color:`WHITE ()
  (* ["constr"; "variable"] is not assigned *)
  ; "constr.reference" , make ~fg_color:`LIGHT_GREEN ()
  ; "constr.path"      , make ~fg_color:`LIGHT_MAGENTA ()
  ; "module.definition", make ~bold:true ~fg_color:`LIGHT_RED ()
  ; "module.keyword"   , make ~bold:true ()
  ; "tactic.keyword"   , make ~bold:true ()
  ; "tactic.primitive" , make ~fg_color:`LIGHT_GREEN ()
  ; "tactic.string"    , make ~fg_color:`LIGHT_RED ()
  ; "diff.added"       , make ~bg_color:(`RGB(0,141,0)) ~underline:true ()
  ; "diff.removed"     , make ~bg_color:(`RGB(170,0,0)) ~underline:true ()
  ; "diff.added.bg"    , make ~bg_color:(`RGB(0,91,0)) ()
  ; "diff.removed.bg"  , make ~bg_color:(`RGB(91,0,0)) ()
  ]

let tag_map = ref CString.Map.empty

let init_tag_map styles =
  let set accu (name, st) = CString.Map.add name st accu in
  tag_map := List.fold_left set !tag_map styles

let default_styles () =
  init_tag_map (default_tag_map ())

let parse_color_config str =
  let styles = Terminal.parse str in
  init_tag_map styles

let dump_tags () = CString.Map.bindings !tag_map

let empty = Terminal.make ()
let default_style = Terminal.reset_style

let get_style tag =
  try CString.Map.find tag !tag_map
  with Not_found -> empty;;

let get_open_seq tags =
  let style = List.fold_left (fun a b -> Terminal.merge a (get_style b)) default_style tags in
  Terminal.eval (Terminal.diff default_style style);;

let get_close_seq tags =
  let style = List.fold_left (fun a b -> Terminal.merge a (get_style b)) default_style tags in
  Terminal.eval (Terminal.diff style default_style);;

let diff_tag_stack = ref []  (* global, just like std_ft *)

(** Not thread-safe. We should put a lock somewhere if we print from
    different threads. Do we? *)
let make_style_stack () =
  (* Default tag is to reset everything *)
  let style_stack = ref [] in
  let peek () = match !style_stack with
  | []      -> default_style  (* Anomalous case, but for robustness *)
  | st :: _ -> st
  in
  let open_tag tag =
    let (tpfx, ttag) = split_tag tag in
    if tpfx = end_pfx then "" else
      let style = get_style ttag in
      (* Merge the current settings and the style being pushed.  This
         allows restoring the previous settings correctly in a pop
         when both set the same attribute.  Example: current settings
         have red FG, the pushed style has green FG.  When popping the
         style, we should set red FG, not default FG. *)
    let style = Terminal.merge (peek ()) style in
    let diff = Terminal.diff (peek ()) style in
    style_stack := style :: !style_stack;
    if tpfx = start_pfx then diff_tag_stack := ttag :: !diff_tag_stack;
    Terminal.eval diff
  in
  let close_tag tag =
    let (tpfx, _) = split_tag tag in
      if tpfx = start_pfx then "" else begin
        if tpfx = end_pfx then diff_tag_stack := (try List.tl !diff_tag_stack with tl -> []);
        match !style_stack with
        | []       -> (* Something went wrong, we fallback *)
                      Terminal.eval default_style
        | cur :: rem -> style_stack := rem;
                      if cur = (peek ()) then "" else
                        if rem = [] then Terminal.reset else
                          Terminal.eval (Terminal.diff cur (peek ()))
      end
  in
  let clear () = style_stack := [] in
  open_tag, close_tag, clear

let make_printing_functions () =
  let print_prefix ft tag =
    let (tpfx, ttag) = split_tag tag in
    if tpfx <> end_pfx then
      let style = get_style ttag in
      match style.Terminal.prefix with Some s -> Format.pp_print_string ft s | None -> () in

  let print_suffix ft tag =
    let (tpfx, ttag) = split_tag tag in
    if tpfx <> start_pfx then
      let style = get_style ttag in
      match style.Terminal.suffix with Some s -> Format.pp_print_string ft s | None -> () in

  print_prefix, print_suffix

let init_output_fns () =
  let reopen_highlight = ref "" in
  let open Format in
  let fns = Format.pp_get_formatter_out_functions !std_ft () in
  let newline () =
    if !diff_tag_stack <> [] then begin
      let close = get_close_seq !diff_tag_stack in
      fns.out_string close 0 (String.length close);
      reopen_highlight := get_open_seq (List.rev !diff_tag_stack);
    end;
    fns.out_string "\n" 0 1 in
  let string s off n =
    if !reopen_highlight <> ""  && String.trim (String.sub s off n) <> "" then begin
      fns.out_string !reopen_highlight 0 (String.length !reopen_highlight);
      reopen_highlight := ""
    end;
    fns.out_string s off n in
  let new_fns = { fns with out_string = string; out_newline = newline } in
  Format.pp_set_formatter_out_functions !std_ft new_fns;;

let init_terminal_output ~color =
  let open_tag, close_tag, clear_tag = make_style_stack () in
  let print_prefix, print_suffix = make_printing_functions () in
  let tag_handler ft = {
    Format.mark_open_tag   = open_tag;
    Format.mark_close_tag  = close_tag;
    Format.print_open_tag  = print_prefix ft;
    Format.print_close_tag = print_suffix ft;
  } in
  if color then
    (* Use 0-length markers *)
    begin
      std_logger_cleanup := clear_tag;
      init_output_fns ();
      Format.pp_set_mark_tags !std_ft true;
      Format.pp_set_mark_tags !err_ft true
    end
  else
    (* Use textual markers *)
    begin
      Format.pp_set_print_tags !std_ft true;
      Format.pp_set_print_tags !err_ft true
    end;
  Format.pp_set_formatter_tag_functions !std_ft (tag_handler !std_ft);
  Format.pp_set_formatter_tag_functions !err_ft (tag_handler !err_ft)

(* Rules for emacs:
   - Debug/info: emacs_quote_info
   - Warning/Error: emacs_quote_err
   - Notice: unquoted
 *)
let emacs_logger = gen_logger Emacs.quote_info Emacs.quote_warning

(* This is specific to the toplevel *)

type execution_phase =
  | ParsingCommandLine
  | Initialization
  | LoadingPrelude
  | LoadingRcFile
  | InteractiveLoop
  | CompilationPhase

let default_phase = ref InteractiveLoop

let in_phase ~phase f x =
  let op = !default_phase in
  default_phase := phase;
  try
    let res = f x in
    default_phase := op;
    res
  with exn ->
    let iexn = Backtrace.add_backtrace exn in
    default_phase := op;
    Util.iraise iexn

let pr_loc loc =
    let fname = loc.Loc.fname in
    match fname with
    | Loc.ToplevelInput ->
      Loc.(str"Toplevel input, characters " ++ int loc.bp ++
           str"-" ++ int loc.ep ++ str":")
    | Loc.InFile fname ->
      Loc.(str"File " ++ str "\"" ++ str fname ++ str "\"" ++
           str", line " ++ int loc.line_nb ++ str", characters " ++
           int (loc.bp-loc.bol_pos) ++ str"-" ++ int (loc.ep-loc.bol_pos) ++
           str":")

let pr_phase ?loc () =
  match !default_phase, loc with
  | LoadingRcFile, loc ->
     (* For when all errors go through feedback:
     str "While loading rcfile:" ++
     Option.cata (fun loc -> fnl () ++ pr_loc loc) (mt ()) loc *)
     Option.map pr_loc loc
  | LoadingPrelude, loc ->
     Some (str "While loading initial state:" ++ Option.cata (fun loc -> fnl () ++ pr_loc loc) (mt ()) loc)
  | _, Some loc -> Some (pr_loc loc)
  | ParsingCommandLine, _
  | Initialization, _
  | CompilationPhase, _ ->
    None
  | InteractiveLoop, _ ->
     (* Note: interactive messages such as "foo is defined" are not located *)
     None

let print_err_exn any =
  let (e, info) = CErrors.push any in
  let loc = Loc.get_loc info in
  let pre_hdr = pr_phase ?loc () in
  let msg = CErrors.iprint (e, info) ++ fnl () in
  std_logger ?pre_hdr Feedback.Error msg

(* Functions to print underlined locations from an input buffer. *)
module TopErr = struct

(* Given a location, returns the list of locations of each line. The last
   line is returned separately. It also checks the location bounds. *)

let get_bols_of_loc ibuf (bp,ep) =
  let add_line (b,e) lines =
    if b < 0 || e < b then CErrors.anomaly (Pp.str "Bad location.");
    match lines with
      | ([],None) -> ([], Some (b,e))
      | (fl,oe) -> ((b,e)::fl, oe)
  in
  let rec lines_rec ba after = function
    | []                  -> add_line (0,ba) after
    | ll::_ when ll <= bp -> add_line (ll,ba) after
    | ll::fl              ->
        let nafter = if ll < ep then add_line (ll,ba) after else after in
        lines_rec ll nafter fl
  in
  let (fl,ll) = lines_rec ibuf.len ([],None) ibuf.bols in
  (fl,Option.get ll)

let dotted_location (b,e) =
  if e-b < 3 then
    ("", String.make (e-b) ' ')
  else
    (String.make (e-b-1) '.', " ")

let blanch_utf8_string s bp ep = let open Bytes in
  let s' = make (ep-bp) ' ' in
  let j = ref 0 in
  for i = bp to ep - 1 do
    let n = Char.code (get s i) in
    (* Heuristic: assume utf-8 chars are printed using a single
    fixed-size char and therefore contract all utf-8 code into one
    space; in any case, preserve tabulation so
    that its effective interpretation in terms of spacing is preserved *)
    if get s i == '\t' then set s' !j '\t';
    if n < 0x80 || 0xC0 <= n then incr j
  done;
  Bytes.sub_string s' 0 !j

let adjust_loc_buf ib loc = let open Loc in
  { loc with ep = loc.ep - ib.start; bp = loc.bp - ib.start }

let print_highlight_location ib loc =
  let (bp,ep) = Loc.unloc loc in
  let highlight_lines =
    match get_bols_of_loc ib (bp,ep) with
      | ([],(bl,el)) ->
          let shift = blanch_utf8_string ib.str bl bp in
          let span = String.length (blanch_utf8_string ib.str bp ep) in
          (str"> " ++ str(Bytes.sub_string ib.str bl (el-bl-1)) ++ fnl () ++
           str"> " ++ str(shift) ++ str(String.make span '^'))
      | ((b1,e1)::ml,(bn,en)) ->
          let (d1,s1) = dotted_location (b1,bp) in
          let (dn,sn) = dotted_location (ep,en) in
          let l1 = (str"> " ++ str d1 ++ str s1 ++
                      str(Bytes.sub_string ib.str bp (e1-bp))) in
          let li =
            prlist (fun (bi,ei) ->
                      (str"> " ++ str(Bytes.sub_string ib.str bi (ei-bi)))) ml in
          let ln = (str"> " ++ str(Bytes.sub_string ib.str bn (ep-bn)) ++
                      str sn ++ str dn) in
          (l1 ++ li ++ ln)
  in
  highlight_lines

let valid_buffer_loc ib loc =
  let (b,e) = Loc.unloc loc in b-ib.start >= 0 && e-ib.start < ib.len && b<=e

(* Toplevel error explanation. *)
let error_info_for_buffer ?loc buf =
  match loc with
  | None -> pr_phase ?loc ()
  | Some loc ->
      let fname = loc.Loc.fname in
        (* We are in the toplevel *)
      match fname with
      | Loc.ToplevelInput ->
        let nloc = adjust_loc_buf buf loc in
        if valid_buffer_loc buf loc then
          match pr_phase ~loc:nloc () with
          | None -> None
          | Some hd -> Some (hd ++ fnl () ++ print_highlight_location buf nloc)
        (* in the toplevel, but not a valid buffer *)
        else pr_phase ~loc ()
      (* we are in batch mode, don't adjust location *)
      | Loc.InFile _ -> pr_phase ~loc ()

(* Actual printing routine *)
let print_error_for_buffer ~buffer ~emacs ?loc lvl msg =
  let pre_hdr = error_info_for_buffer ?loc buffer in
  if emacs
  then emacs_logger ?pre_hdr lvl msg
  else std_logger   ?pre_hdr lvl msg

(*
let print_toplevel_parse_error (e, info) buf =
  let loc = Loc.get_loc info in
  let lvl = Feedback.Error in
  let msg = CErrors.iprint (e, info) in
  print_error_for_buffer ?loc lvl msg buf
*)
end

let extract_default_loc loc doc_id sid : Loc.t option =
  match loc with
  | Some _ -> loc
  | None ->
    try
      let doc = Stm.get_doc doc_id in
      Option.cata fst None Stm.(get_ast ~doc sid)
    with _ -> loc

(** Coqtop / Coqc Console feedback handler *)
let console_feed ~emacs ~buffer (fb : Feedback.feedback) = let open Feedback in
  match fb.contents with
  | Processed   -> ()
  | Incomplete  -> ()
  | Complete    -> ()
  | ProcessingIn _ -> ()
  | InProgress _ -> ()
  | WorkerStatus (_,_) -> ()
  | AddedAxiom  -> ()
  | GlobRef (_,_,_,_,_) -> ()
  | GlobDef (_,_,_,_) -> ()
  | FileDependency (_,_) -> ()
  | FileLoaded (_,_) -> ()
  | Custom (_,_,_) -> ()
  (* Re-enable when we switch back to feedback-based error printing *)
  | Message (Error,loc,msg) -> ()
  (* TopErr.print_error_for_buffer ?loc lvl msg top_buffer *)
  | Message (Warning,loc,msg) ->
    let loc = extract_default_loc loc fb.doc_id fb.span_id in
    TopErr.print_error_for_buffer ?loc ~emacs ~buffer Warning msg
  | Message (lvl,loc,msg) ->
    TopErr.print_error_for_buffer ?loc ~emacs ~buffer lvl msg
