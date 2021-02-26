(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* registration of debugger hooks *)

type debugger_action =
  | DbStep
  | DbSkip
  | DbExit
  | DbHelp
  | DbRunCnt of int
  | DbRunBreakpoint of string
  | DbFailure

type debugger_hooks = {
  read_cmd : unit -> debugger_action;   (* read a debugger command from the client *)
  print_notice : Pp.t -> unit;           (* print a notice *)
  print_debug  : Pp.t -> unit;           (* print a debug *)
  print_prompt : Pp.t -> unit           (* print the debugger prompt *)
}

let debugger_hooks : debugger_hooks option ref = ref None

let register_debugger_hooks hooks = debugger_hooks := Some hooks

let get_debugger_hooks () =
  match !debugger_hooks with
  | Some hooks -> hooks
  | None -> failwith "get_debugger_hooks"

(* todo: didn't understand the point of "Exninfo.capture e"
   in the monadic form of the following *)

let rec drop_spaces inst i =
  if String.length inst > i && inst.[i] == ' ' then drop_spaces inst (i+1)
  else i

let possibly_unquote s =
  if String.length s >= 2 && s.[0] == '"' && s.[String.length s - 1] == '"' then
    String.sub s 1 (String.length s - 2)
  else
    s

let check_positive n =
  if n < 0 then
    raise (Invalid_argument "number must be positive")
  else
    ()

let run_invalid_arg () =
  raise (Invalid_argument "run_com")

(* Gives the number of steps or next breakpoint of a run command *)
let run_com inst =
  let first_char = String.get inst 0 in
  if first_char = 'r' then
    let i = drop_spaces inst 1 in
    if String.length inst > i then
      let s = String.sub inst i (String.length inst - i) in
      if inst.[0] >= '0' && inst.[0] <= '9' then
        let num = int_of_string s in
        check_positive num;
        DbRunCnt num
      else
        DbRunBreakpoint (possibly_unquote s)
    else
      run_invalid_arg ()
  else
    run_invalid_arg ()

let parse_cmd inst =
  match inst with
  | ""  -> DbStep
  | "s" -> DbSkip
  | "x" -> DbExit
  | "h"| "?" -> DbHelp
  | _ ->
    try run_com inst with
      | Failure _ | Invalid_argument _ -> DbFailure
