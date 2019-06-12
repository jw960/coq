(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Util
open Pp
open CErrors

type typed_vernac =
  | VtDefault of (unit -> unit)

  | VtNoProof of (unit -> unit)
  | VtCloseProof of (lemma:Lemmas.t -> unit)
  | VtOpenProof of (unit -> Lemmas.t)
  | VtModifyProof of (pstate:Proof_global.t -> Proof_global.t)
  | VtReadProofOpt of (pstate:Proof_global.t option -> unit)
  | VtReadProof of (pstate:Proof_global.t -> unit)

type vernac_command = atts:Attributes.vernac_flags -> typed_vernac

type plugin_args = Genarg.raw_generic_argument list

(* Table of vernac entries *)
let vernac_tab =
  (Hashtbl.create 211 :
    (Vernacexpr.extend_name, bool * (plugin_args -> vernac_command)) Hashtbl.t)

let vinterp_add depr s f =
  try
    Hashtbl.add vernac_tab s (depr, f)
  with Failure _ ->
    user_err ~hdr:"vinterp_add"
      (str"Cannot add the vernac command " ++ str (fst s) ++ str" twice.")

let vinterp_map s =
  try
    Hashtbl.find vernac_tab s
  with Failure _ | Not_found ->
    user_err ~hdr:"Vernac Interpreter"
      (str"Cannot find vernac command " ++ str (fst s) ++ str".")

let warn_deprecated_command =
  let open CWarnings in
  create ~name:"deprecated-command" ~category:"deprecated"
         (fun pr -> str "Deprecated vernacular command: " ++ pr)

(* Interpretation of a vernac command *)

let type_vernac opn converted_args ~atts =
  let phase = ref "Looking up command" in
  try
    let depr, callback = vinterp_map opn in
    let () = if depr then
      let rules = Egramml.get_extend_vernac_rule opn in
      let pr_gram = function
      | Egramml.GramTerminal s -> str s
      | Egramml.GramNonTerminal _ -> str "_"
      in
      let pr = pr_sequence pr_gram rules in
      warn_deprecated_command pr;
    in
    phase := "Checking arguments";
    let hunk = callback converted_args in
    phase := "Executing command";
    hunk ~atts
  with
    | reraise ->
        let reraise = CErrors.push reraise in
        if !Flags.debug then
          Feedback.msg_debug (str"Vernac Interpreter " ++ str !phase);
        iraise reraise

(** VERNAC EXTEND registering *)

type _ ty_sig =
| TyNil : vernac_command ty_sig
| TyTerminal : string * 'r ty_sig -> 'r ty_sig
| TyNonTerminal : ('a, 'b, 'c) Extend.ty_user_symbol * 'r ty_sig -> ('a -> 'r) ty_sig

type ty_ml = TyML : bool * 'r ty_sig * 'r -> ty_ml

let type_error () = CErrors.anomaly (Pp.str "Ill-typed VERNAC EXTEND")

(** Stupid GADTs forces us to duplicate the definition just for typing *)
let rec untype_command : type r. r ty_sig -> r -> plugin_args -> vernac_command = function
| TyNil -> fun f args ->
  begin match args with
  | [] -> f
  | _ :: _ -> type_error ()
  end
| TyTerminal (_, ty) -> fun f args -> untype_command ty f args
| TyNonTerminal (tu, ty) -> fun f args ->
  let open Genarg in
  begin match args with
  | [] -> type_error ()
  | GenArg (Rawwit tag, v) :: args ->
    match genarg_type_eq tag (Egramml.proj_symbol tu) with
    | None -> type_error ()
    | Some Refl -> untype_command ty (f v) args
  end

let rec untype_user_symbol : type s a b c. (a, b, c) Extend.ty_user_symbol -> (s, Extend.norec, a) Extend.symbol =
  let open Extend in function
| TUlist1 l -> Alist1 (untype_user_symbol l)
| TUlist1sep (l, s) -> Alist1sep (untype_user_symbol l, Atoken (CLexer.terminal s))
| TUlist0 l -> Alist0 (untype_user_symbol l)
| TUlist0sep (l, s) -> Alist0sep (untype_user_symbol l, Atoken (CLexer.terminal s))
| TUopt o -> Aopt (untype_user_symbol o)
| TUentry a -> Aentry (Pcoq.genarg_grammar (Genarg.ExtraArg a))
| TUentryl (a, i) -> Aentryl (Pcoq.genarg_grammar (Genarg.ExtraArg a), string_of_int i)

let rec untype_grammar : type r. r ty_sig -> 'a Egramml.grammar_prod_item list = function
| TyNil -> []
| TyTerminal (tok, ty) -> Egramml.GramTerminal tok :: untype_grammar ty
| TyNonTerminal (tu, ty) ->
  let t = Genarg.rawwit (Egramml.proj_symbol tu) in
  let symb = untype_user_symbol tu in
  Egramml.GramNonTerminal (Loc.tag (t, symb)) :: untype_grammar ty

let vernac_extend ~command ?entry ext =
  let iter i (TyML (depr, ty, f)) =
    let f = untype_command ty f in
    let r = untype_grammar ty in
    let () = vinterp_add depr (command, i) f in
    Egramml.extend_vernac_command_grammar (command, i) entry r
  in
  List.iteri iter ext

(** VERNAC ARGUMENT EXTEND registering *)

type 'a argument_rule =
| Arg_alias of 'a Pcoq.Entry.t
| Arg_rules of 'a Extend.production_rule list

type 'a vernac_argument = {
  arg_printer : Environ.env -> Evd.evar_map -> 'a -> Pp.t;
  arg_parsing : 'a argument_rule;
}

let vernac_argument_extend ~name arg =
  let wit = Genarg.create_arg name in
  let entry = match arg.arg_parsing with
  | Arg_alias e ->
    let () = Pcoq.register_grammar wit e in
    e
  | Arg_rules rules ->
    let e = Pcoq.create_generic_entry Pcoq.utactic name (Genarg.rawwit wit) in
    let () = Pcoq.grammar_extend e None (None, [(None, None, rules)]) in
    e
  in
  let pr = arg.arg_printer in
  let pr x = Genprint.PrinterBasic (fun env sigma -> pr env sigma x) in
  let () = Genprint.register_vernac_print0 wit pr in
  (wit, entry)
