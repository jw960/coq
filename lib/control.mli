(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Global control of Coq. *)

val timeout : int -> (unit -> 'a) -> exn -> 'a
(** [timeout n f e] tries to compute [f], and if it fails to do so before [n]
    seconds, it raises [e] instead. *)

type timeout = { timeout : 'a. int -> (unit -> 'a) -> exn -> 'a }

val set_timeout : timeout -> unit
(** Set a particular timeout function. *)
