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
  print_notice : Pp.t -> unit;          (* print a notice *)
  print_debug  : Pp.t -> unit;          (* print a debug *)
  print_prompt : Pp.t -> unit           (* print the debugger prompt *)
}

val register_debugger_hooks : debugger_hooks -> unit

val get_debugger_hooks : unit -> debugger_hooks

val parse_cmd : string -> debugger_action
