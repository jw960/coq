(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* registration of the routine that reads debugger commands from the client *)
let forward_read_debug_cmd = ref (None : (unit -> string) option)

let register_debug_cmd_reader f = forward_read_debug_cmd := Some f

let get_debug_cmd_reader () = !forward_read_debug_cmd
