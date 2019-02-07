(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Util
open Pp
open Coqargs

let ( / ) s1 s2 = Filename.concat s1 s2

(** Coq Initialization *)

let set_debug () =
  let () = Backtrace.record_backtrace true in
  Flags.debug := true

(* Loading of the ressource file.
   rcfile is either $XDG_CONFIG_HOME/.coqrc.VERSION, or $XDG_CONFIG_HOME/.coqrc if the first one
  does not exist. *)

(************************************************************************)
(* Loading of RC Files                                                  *)
(************************************************************************)

let rcdefaultname = "coqrc"

let load_rcfile ?rcfile ~state =
    try
      match rcfile with
      | Some rcfile ->
        if CUnix.file_readable_p rcfile then
          Vernac.load_vernac ~echo:false ~interactive:false ~check:true ~state rcfile
        else raise (Sys_error ("Cannot read rcfile: "^ rcfile))
      | None ->
	try
	  let warn x = Feedback.msg_warning (str x) in
	  let inferedrc = List.find CUnix.file_readable_p [
	    Envars.xdg_config_home warn / rcdefaultname^"."^Coq_config.version;
	    Envars.xdg_config_home warn / rcdefaultname;
	    Envars.home ~warn / "."^rcdefaultname^"."^Coq_config.version;
	    Envars.home ~warn / "."^rcdefaultname
	  ] in
          Vernac.load_vernac ~echo:false ~interactive:false ~check:true ~state inferedrc
        with Not_found -> state
	(*
	Flags.if_verbose
	  mSGNL (str ("No coqrc or coqrc."^Coq_config.version^
			 " found. Skipping rcfile loading."))
	*)
    with reraise ->
      let reraise = CErrors.push reraise in
      let () = Feedback.msg_info (str"Load of rcfile failed.") in
      iraise reraise

(************************************************************************)
(* Initial Load Path                                                    *)
(************************************************************************)

(* Recursively puts dir in the LoadPath if -nois was not passed *)
let build_stdlib_path ~load_init ~unix_path ~coq_path ~with_ml =
  let open Mltop in
  let add_ml = if with_ml then AddRecML else AddNoML in
  { recursive = true;
    path_spec = VoPath { unix_path; coq_path ; has_ml = add_ml; implicit = load_init }
  }

let build_userlib_path ~unix_path =
  let open Mltop in
  { recursive = true;
    path_spec = VoPath {
        unix_path;
        coq_path = Libnames.default_root_prefix;
        has_ml = Mltop.AddRecML;
        implicit = false;
      }
  }

let ml_path_if c p =
  let open Mltop in
  let f x = { recursive = false; path_spec = MlPath x } in
  if c then List.map f p else []

(* LoadPath for developers *)
let toplevel_init_load_path () =
  let coqlib = Envars.coqlib () in
  (* NOTE: These directories are searched from last to first *)
  (* first, developer specific directory to open *)
  ml_path_if Coq_config.local [coqlib/"dev"]

(* LoadPath for Coq user libraries *)
let libs_init_load_path ~load_init =

  let open Mltop in
  let coqlib = Envars.coqlib () in
  let user_contrib = coqlib/"user-contrib" in
  let xdg_dirs = Envars.xdg_dirs ~warn:(fun x -> Feedback.msg_warning (str x)) in
  let coqpath = Envars.coqpath in
  let coq_path = Names.DirPath.make [Libnames.coq_root] in

  (* current directory (not recursively!) *)
  [ { recursive = false;
      path_spec = VoPath { unix_path = ".";
                           coq_path = Libnames.default_root_prefix;
                           implicit = false;
                           has_ml = AddTopML }
    } ] @

  (* then standard library and plugins *)
  [build_stdlib_path ~load_init ~unix_path:(coqlib/"theories") ~coq_path ~with_ml:false;
   build_stdlib_path ~load_init ~unix_path:(coqlib/"plugins")  ~coq_path ~with_ml:true ] @

  (* then user-contrib *)
  (if Sys.file_exists user_contrib then
     [build_userlib_path ~unix_path:user_contrib] else []
  ) @

  (* then directories in XDG_DATA_DIRS and XDG_DATA_HOME and COQPATH *)
  List.map (fun s -> build_userlib_path ~unix_path:s) (xdg_dirs @ coqpath)

(* Initialises the Ocaml toplevel before launching it, so that it can
   find the "include" file in the *source* directory *)
let init_ocaml_path () =
  let open Mltop in
  let lp s = { recursive = false; path_spec = MlPath s } in
  let add_subdir dl =
    Mltop.add_coq_path (lp (List.fold_left (/) Envars.coqroot [dl]))
  in
    Mltop.add_coq_path (lp (Envars.coqlib ()));
    List.iter add_subdir Coq_config.all_src_dirs

(******************************************************************************)
(* Interactive Load File Simulation                                           *)
(******************************************************************************)
let load_vernacular ~vernac_file_list ~state =
  List.fold_left
    (fun state (f_in, echo) ->
      let s = Loadpath.locate_file f_in in
      (* Should make the beautify logic clearer *)
      let load_vernac f = Vernac.load_vernac ~echo ~interactive:false ~check:true ~state f in
      if !Flags.beautify
      then Flags.with_option Flags.beautify_file load_vernac f_in
      else load_vernac s
    ) state vernac_file_list

let load_init_vernaculars ~opts ~state =
  let rcfile = opts.rcfile in
  let vernac_file_list = List.rev opts.load_vernacular_list in
  let state =
    Topfmt.(in_phase ~phase:LoadingRcFile) (fun () ->
        load_rcfile ?rcfile ~state) ()
  in
  load_vernacular ~vernac_file_list ~state

(******************************************************************************)
(* Color Options                                                              *)
(******************************************************************************)
let init_color opts =
  let has_color = match opts.color with
  | `OFF -> false
  | `ON -> true
  | `AUTO ->
    Terminal.has_style Unix.stdout &&
    Terminal.has_style Unix.stderr &&
    (* emacs compilation buffer does not support colors by default,
       its TERM variable is set to "dumb". *)
    try Sys.getenv "TERM" <> "dumb" with Not_found -> false
  in
  let term_color =
    if has_color then begin
      let colors = try Some (Sys.getenv "COQ_COLORS") with Not_found -> None in
      match colors with
      | None -> Topfmt.default_styles (); true        (* Default colors *)
      | Some "" -> false                              (* No color output *)
      | Some s -> Topfmt.parse_color_config s; true   (* Overwrite all colors *)
    end
    else
      false
  in
  if not term_color then
    Proof_diffs.write_color_enabled term_color;
  if Proof_diffs.show_diffs () && not term_color then
    (prerr_endline "Error: -diffs requires enabling -color"; exit 1);
  Topfmt.init_terminal_output ~color:term_color

let print_style_tags opts =
  let () = init_color opts in
  let tags = Topfmt.dump_tags () in
  let iter (t, st) =
    let opt = Terminal.eval st ^ t ^ Terminal.reset ^ "\n" in
    print_string opt
  in
  let make (t, st) =
    let tags = List.map string_of_int (Terminal.repr st) in
    (t ^ "=" ^ String.concat ";" tags)
  in
  let repr = List.map make tags in
  let () = Printf.printf "COQ_COLORS=\"%s\"\n" (String.concat ":" repr) in
  let () = List.iter iter tags in
  flush_all ()

(******************************************************************************)
(* Header                                                                     *)
(******************************************************************************)
let ( / ) = Filename.concat

let get_version_date () =
  try
    let ch = open_in (Envars.coqlib () / "revision") in
    let ver = input_line ch in
    let rev = input_line ch in
    let () = close_in ch in
    (ver,rev)
  with e when CErrors.noncritical e ->
    (Coq_config.version,Coq_config.date)

let print_header () =
  let (ver,rev) = get_version_date () in
  Feedback.msg_notice (str "Welcome to Coq " ++ str ver ++ str " (" ++ str rev ++ str ")");
  flush_all ()

(******************************************************************************)
(* Input/Output State                                                         *)
(******************************************************************************)
let inputstate opts =
  Option.iter (fun istate_file ->
    let fname = Loadpath.locate_file (CUnix.make_suffix istate_file ".coq") in
    States.intern_state fname) opts.inputstate


(******************************************************************************)
(* Main Coq Initialization Routine                                            *)
(******************************************************************************)

(** GC tweaking *)

(** Coq is a heavy user of persistent data structures and symbolic ASTs, so the
    minor heap is heavily solicited. Unfortunately, the default size is far too
    small, so we enlarge it a lot (128 times larger).

    To better handle huge memory consumers, we also augment the default major
    heap increment and the GC pressure coefficient.
*)

let init_gc () =
  try
    (* OCAMLRUNPARAM environment variable is set.
     * In that case, we let ocamlrun to use the values provided by the user.
     *)
    ignore (Sys.getenv "OCAMLRUNPARAM")

  with Not_found ->
    (* OCAMLRUNPARAM environment variable is not set.
     * In this case, we put in place our preferred configuration.
     *)
    Gc.set { (Gc.get ()) with
             Gc.minor_heap_size = 33554432; (* 4M *)
             Gc.space_overhead = 120}

(** Memory stats *)
let memory_stat = ref false
let print_memory_stat () =
  begin (* -m|--memory from the command-line *)
    if !memory_stat then
    Feedback.msg_notice
      (str "total heap size = " ++ int (CObj.heap_size_kb ()) ++ str " kbytes" ++ fnl ());
  end;
  begin
    (* operf-macro interface:
       https://github.com/OCamlPro/operf-macro *)
    try
      let fn = Sys.getenv "OCAML_GC_STATS" in
      let oc = open_out fn in
      Gc.print_stat oc;
      close_out oc
    with _ -> ()
  end

let _ = at_exit print_memory_stat

(** Main init routine *)
let init_toplevel ~help ~init custom_init arglist =
  (* Coq's init process, phase 1:
     OCaml parameters, basic structures, and IO
   *)
  CProfile.init_profile ();
  init_gc ();
  Sys.catch_break false; (* Ctrl-C is fatal during the initialisation *)

  Lib.init();

  (* Coq's init process, phase 2:
     Basic Coq environment, load-path, plugins.
   *)
  let opts, extras = parse_args ~help ~init arglist in
  memory_stat := opts.memory_stat;

  (* If we have been spawned by the Spawn module, this has to be done
   * early since the master waits us to connect back *)
  Spawned.init_channels ();
  Envars.set_coqlib ~fail:(fun msg -> CErrors.user_err Pp.(str msg));
  if opts.print_where then begin
    print_endline (Envars.coqlib ());
    exit (exitcode opts)
  end;
  if opts.print_config then begin
    Envars.print_config stdout Coq_config.all_src_dirs;
    exit (exitcode opts)
  end;
  if opts.print_tags then begin
    print_style_tags opts;
    exit (exitcode opts)
  end;
  if opts.filter_opts then begin
    print_string (String.concat "\n" extras);
    exit 0;
  end;
  let top_lp = toplevel_init_load_path () in
  List.iter Mltop.add_coq_path top_lp;

  let opts, extras = custom_init ~opts extras in
  Flags.if_verbose print_header ();
  Mltop.init_known_plugins ();

  Global.set_engagement opts.impredicative_set;
  Global.set_indices_matter opts.indices_matter;
  Global.set_VM opts.enable_VM;
  Global.set_native_compiler
    (match opts.native_compiler with NativeOff -> false | NativeOn _ -> true);

  Global.set_allow_sprop opts.allow_sprop;
  if opts.cumulative_sprop then Global.make_sprop_cumulative ();

  (* Allow the user to load an arbitrary state here *)
  inputstate opts;

  (* This state will be shared by all the documents *)
  Stm.init_core ();

  (* Coq init process, phase 3: Stm initialization, backtracking state.

     It is essential that the module system is in a consistent
     state before we take the first snapshot. This was not
     guaranteed in the past, but now is thanks to the STM API.
  *)
  opts, extras

type init_fn = opts:Coqargs.t -> string list -> Coqargs.t * string list
