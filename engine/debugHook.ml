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

let debugger_hooks : debugger_hooks option ref = ref None

let register_debugger_hooks hooks = debugger_hooks := Some hooks

let get_debugger_hooks () =
  match !debugger_hooks with
  | Some hooks -> hooks
  | None -> failwith "get_debugger_hooks"
