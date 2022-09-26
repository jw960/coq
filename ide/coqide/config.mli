(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

val configdir : string  (* where configuration files are installed *)
val datadir : string    (* where extra data files are installed *)

val arch : string       (* architecture *)
val gtk_platform : [`QUARTZ | `WIN32 | `X11]

val browser : string
(** default web browser to use, may be overridden by environment
    variable COQREMOTEBROWSER *)
