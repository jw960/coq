(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Constr
open Declarations

(** {6 Cooking the constants. } *)

type work_list = (Univ.Instance.t * Names.Id.t array) Names.Cmap.t *
  (Univ.Instance.t * Names.Id.t array) Names.Mindmap.t

type cooking_info = {
  modlist : work_list;
  abstract : Constr.named_context * Univ.Instance.t * Univ.AUContext.t }

type recipe = { from : constant_body; info : cooking_info }

type inline = bool

type result = {
  cook_body : constant_def;
  cook_type : types;
  cook_universes : constant_universes;
  cook_private_univs : Univ.ContextSet.t option;
  cook_inline : inline;
  cook_context : Constr.named_context option;
}

val cook_constant : hcons:bool -> recipe -> result
val cook_constr : cooking_info -> constr -> constr

(** {6 Utility functions used in module [Discharge]. } *)

val expmod_constr : work_list -> constr -> constr
