(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Names

(** This module provides functions to load, open and save
  libraries. Libraries correspond to the subclass of modules that
  coincide with a file on disk (the ".vo" files). Libraries on the
  disk comes with checksums (obtained with the [Digest] module), which
  are checked at loading time to prevent inconsistencies between files
  written at various dates.
*)

(** Type of libraries loaded in memory *)
type library_t

(** {6 ... }
    Require = load in the environment *)
val require_library_from_dirpath : library_t list -> unit

val require_library_syntax_from_dirpath
  :  lib_resolver:(DirPath.t -> CUnix.physical_path)
  -> DirPath.t list
  -> library_t list

(** {6 Start the compilation of a library } *)

type 'doc todo_proofs =
 | ProofsTodoNone (* for .vo *)
 | ProofsTodoSome of Future.UUIDSet.t (* for .vos *)

(** End the compilation of a library and save it to a ".vo" file, or a
    ".vos" file, depending on the todo_proofs argument.
    [output_native_objects]: when producing vo objects, also compile
    the native-code version. *)
val save_library_to :
  'document todo_proofs ->
  output_native_objects:bool ->
  DirPath.t -> string -> unit

(** {6 Interrogate the status of libraries } *)

  (** - Tell if a library is loaded *)
val library_is_loaded : DirPath.t -> bool

  (** - Tell which libraries are loaded *)
val loaded_libraries : unit -> DirPath.t list

(** {6 Native compiler. } *)
val native_name_from_filename : string -> string

(** {6 Opaque accessors} *)
val indirect_accessor : Global.indirect_accessor
