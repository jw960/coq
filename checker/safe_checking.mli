(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(*i*)
open Declarations
open Environ
open Safe_typing
(*i*)

val get_env : unit -> env

type compiled_library

val set_engagement : engagement -> unit
val import         :
  CUnix.physical_path -> compiled_library -> Univ.ContextSet.t -> vodigest -> unit
val unsafe_import  :
  CUnix.physical_path -> compiled_library -> Univ.ContextSet.t -> vodigest -> unit
