(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Initialization. *)
val set_debug : unit -> unit

(** LoadPath for toploop toplevels *)
val toplevel_init_load_path : unit -> Mltop.coq_path list

(** LoadPath for Coq user libraries *)
val libs_init_load_path : load_init:bool -> Mltop.coq_path list

val init_ocaml_path : unit -> unit

(** [load_init_vernaculars opts ~state] Load vernaculars from the init (rc) file *)
val load_init_vernaculars
  :  opts:Coqargs.t
  -> state:Vernac.State.t
  -> Vernac.State.t

type init_fn = opts:Coqargs.t -> string list -> Coqargs.t * string list

(** [init_toplevel ~help ~init custom_init arg_list]
    Common Coq initialization and argument parsing *)
val init_toplevel
  :  help:(unit -> unit)
  -> init:Coqargs.t
  -> init_fn
  -> string list
  -> Coqargs.t * string list
