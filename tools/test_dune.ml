(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* LICENSE NOTE: This file is dually MIT/LGPL 2.1+ licensed. MIT license:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(* test_dune: generate dune rules for Coq's test-suite *)

type file = string
type reason = string
type stanza = string

let replace_ext ~file ~newext =
  Filename.(remove_extension file) ^ newext

module type Test = sig

  type t

  val make : file -> t          (* .v file with the test *)
  val deps : t -> file list
  val rule : t -> stanza

end

module Output : Test = struct

  type t = file

  let make file = file
  let deps t = [t]
  let rule in_file =
    let vo_file  = replace_ext ~file:in_file ~newext:".vo" in
    let log_file = replace_ext ~file:in_file ~newext:".v.log" in
    let out_file = replace_ext ~file:in_file ~newext:".out" in
    let output_coqc = "coqc -q -test-mode -async-proofs-cache force" in
    let sed_command = Format.asprintf "sed -i \"s/File \\\"[^\\\"]*\\\"/File \\\"stdin\\\"/\" %s" log_file in
    Format.asprintf
      "(rule (targets %s %s) (deps %s) (action (progn (with-outputs-to %s (run %s %s)) (run %s) (diff %s %s))))"
      vo_file log_file in_file log_file output_coqc in_file sed_command out_file log_file

end

module CoqExitCode : Test = struct

  type t = { file : file
           ; code : bool
           (** true = 0; false = 1; in the future we want to generalize to
               arbitrary codes *)
           }

  let make file = { file; code = true }
  let deps t = [t.file]
  let rule t =
    let in_file = t.file in
    let vo_file  = replace_ext ~file:in_file ~newext:".vo" in
    let log_file = replace_ext ~file:in_file ~newext:".v.log" in
    let success_coqc = "coqc -q" in
    Format.asprintf
      "(rule (targets %s %s) (deps %s) (action (progn (with-outputs-to %s (run %s %s)))))"
      vo_file log_file in_file log_file success_coqc in_file

end

let read_v_files ~dir =
  let is_vo file = Filename.check_suffix file ".v" in
  let all = Sys.readdir dir in
  Array.fold_left (fun vs file -> if is_vo file then file :: vs else vs) [] all

let mk_output file = Output.make file

let gen_dune (module M : Test) ~dir =
  let dir = Filename.concat "test-suite" dir in
  let files = read_v_files ~dir in
  let tests = List.map M.make files in
  let out = Stdlib.open_out (Filename.concat dir "dune") in
  let fmt = Format.formatter_of_out_channel out in
  List.iter (fun t -> Format.fprintf fmt "%s@\n" (M.rule t)) tests;
  let log_files = List.map (fun file -> replace_ext ~file ~newext:".v.log") files in
  Format.fprintf fmt "(alias (name runtest) (deps %a))"
    Format.(pp_print_list ~pp_sep:pp_print_space pp_print_string) log_files;
  Format.pp_print_flush fmt ();
  Stdlib.close_out out

let _ =
  gen_dune (module Output) ~dir:"output";
  let success_dirs = [ "ssr"; "success"; "micromega"; "modules"; "primitive/float"; "primitive/uint63"; "ltac2"] in
  List.iter (fun dir -> gen_dune (module CoqExitCode) ~dir) success_dirs
