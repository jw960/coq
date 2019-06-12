(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

module Parser = struct

  type state = Pcoq.frozen_t

  let init () = Pcoq.freeze ~marshallable:false

  let cur_state () = Pcoq.freeze ~marshallable:false

  let parse ps entry pa =
    Pcoq.unfreeze ps;
    try Pcoq.Entry.parse entry pa
    with e when CErrors.noncritical e ->
      let (e, info) = CErrors.push e in
      Exninfo.iraise (e, info)

end

type t = {
  parsing : Parser.state;
  system  : States.state;          (* summary + libstack *)
  lemmas  : Lemmas.Stack.t option; (* proofs of lemmas currently opened *)
  shallow : bool                   (* is the state trimmed down (libstack) *)
}

let s_cache = ref None

let invalidate_cache () =
  s_cache := None

let update_cache rf v =
  rf := Some v; v

let do_if_not_cached rf f v =
  match !rf with
  | None ->
    rf := Some v; f v
  | Some vc when vc != v ->
    rf := Some v; f v
  | Some _ ->
    ()

let freeze_interp_state ~stack ~marshallable =
  { system = update_cache s_cache (States.freeze ~marshallable);
    lemmas = stack;
    shallow = false;
    parsing = Parser.cur_state ();
  }

let unfreeze_interp_state { system; lemmas; parsing } =
  do_if_not_cached s_cache States.unfreeze system;
  Pcoq.unfreeze parsing

let make_shallow st =
  let lib = States.lib_of_state st.system in
  { st with
    system = States.replace_lib st.system @@ Lib.drop_objects lib;
    shallow = true;
  }
