(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** The idea is that *)

(** [init ?path] sets the default path of coqfind. Calling is optional *)
val set_default_path : path:string -> unit

(** We follow Dune's location method:
    [from https://github.com/ocaml/dune/pull/1226]

    - if [init] was called, we look into that directory.
    - else, we use "$OPAM_SWITCH_PREFIX"/lib
    - else, we use Findlib.default_location () [or findlib.search_path ?]
    - last $(which coqtop)/../lib/coq/

    We always use COQPATH and OCAMLPATH
*)

val search_path : unit -> string list

type coqlib_flag =
  | VoPath of { unix_path : string;      (* Filesystem path containing vo/ml files *)
                coq_path  : string list; (* Coq Prefix for the path                *)
                implicit  : bool;        (* [implicit = true] avoids qualification *)
              }

(** [query_pkg "pkg"] will check that a [dune-package] file exists *)
val query_pkg : string -> coqlib_flag list
