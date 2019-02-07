(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** [compile_files opts] compile files specified in [opts] *)
val compile_files : Coqargs.t -> Coqcargs.t -> unit

(** [do_vio opts] process [.vio] files in [opts] *)
val do_vio : Coqargs.t -> Coqcargs.t -> unit
