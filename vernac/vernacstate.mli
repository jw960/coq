(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

module Parser : sig
  type state

  val init : unit -> state
  val cur_state : unit -> state

  val parse : state -> 'a Pcoq.Entry.t -> Pcoq.Parsable.t -> 'a

end

type t =
  { parsing : Parser.state
  ; system  : States.state          (* summary + libstack *)
  ; lemmas  : Lemmas.Stack.t option (* proofs of lemmas currently opened *)
  ; shallow : bool                  (* is the state trimmed down (libstack) *)
  }

val freeze_interp_state : stack:Lemmas.Stack.t option -> marshallable:bool -> t
val unfreeze_interp_state : t -> unit

val make_shallow : t -> t

(* WARNING: Do not use, it will go away in future releases *)
val invalidate_cache : unit -> unit
