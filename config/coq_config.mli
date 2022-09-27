(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* The fields below are absolute paths *)
val coqlib : string     (* where the std library is installed *)
val docdir : string     (* where the doc is installed *)

(* The fields below are paths relative to the installation prefix *)
(* However, if an absolute path, it means discarding the actual prefix *)
val coqlibsuffix : string    (* std library relative to installation prefix *)
val configdirsuffix : string (* config files relative to installation prefix *)
val datadirsuffix : string   (* data files relative to installation prefix *)
val docdirsuffix : string    (* doc directory relative to installation prefix *)

val ocamlfind : string

val caml_flags : string     (* arguments passed to ocamlc (ie. CAMLFLAGS) *)

val arch_is_win32 : bool

val version : string    (* version number of Coq *)
val caml_version : string    (* OCaml version used to compile Coq *)
val caml_version_nums : int list    (* OCaml version used to compile Coq by components *)
val vo_version : int32

val all_src_dirs : string list

val exec_extension : string (* "" under Unix, ".exe" under MS-windows *)

val has_natdynlink : bool

val wwwcoq : string
val wwwrefman : string
val wwwbugtracker : string
val wwwstdlib : string
val localwwwrefman : string

val bytecode_compiler : bool
type native_compiler = NativeOff | NativeOn of { ondemand : bool }
val native_compiler : native_compiler
