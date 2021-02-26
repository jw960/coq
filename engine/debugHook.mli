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
type debugger_hooks = {
  (* read a debugger command from the client *)
  read_cmd : unit -> string;
  (* print the debugger prompt *)
  print_prompt : Pp.t -> unit
}

val register_debugger_hooks : debugger_hooks -> unit

val get_debugger_hooks : unit -> debugger_hooks
