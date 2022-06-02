(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Pp
open CErrors
open Util

module Dyn = Dyn.Make ()

module Stage = struct

type t = Synterp | Interp

let equal x y =
  match x, y with
  | Synterp, Synterp -> true
  | Synterp, Interp -> false
  | Interp, Interp -> true
  | Interp, Synterp -> false

end

type 'a summary_declaration = {
  stage : Stage.t;
  freeze_function : marshallable:bool -> 'a;
  unfreeze_function : 'a -> unit;
  init_function : unit -> unit }

module Decl = struct type 'a t = 'a summary_declaration end
module DynMap = Dyn.Map(Decl)

type ml_modules = (string option * string) list

let sum_mod : ml_modules summary_declaration option ref = ref None
let sum_map = ref DynMap.empty

let mangle id = id ^ "-SUMMARY"

let declare_ml_modules_summary decl =
  sum_mod := Some decl

let check_name sumname = match Dyn.name sumname with
| None -> ()
| Some (Dyn.Any tag) ->
  anomaly ~label:"Summary.declare_summary"
    (str "Colliding summary names: " ++ str sumname ++ str " vs. " ++ str (Dyn.repr tag) ++ str ".")

let declare_summary_tag sumname decl =
  let () = check_name (mangle sumname) in
  let tag = Dyn.create (mangle sumname) in
  let () = sum_map := DynMap.add tag decl !sum_map in
  tag

let declare_summary sumname decl =
  ignore(declare_summary_tag sumname decl)

module ID = struct type 'a t = 'a end
module Frozen = Dyn.Map(ID)

type frozen = {
  summaries : Frozen.t;
  (** Ordered list w.r.t. the first component. *)
  ml_module : ml_modules option;
  (** Special handling of the ml_module summary. *)
}

let empty_frozen = { summaries = Frozen.empty; ml_module = None }

module HMap = Dyn.HMap(Decl)(ID)

let freeze_staged_summaries stage ~marshallable : frozen =
  let filter = { HMap.filter = fun tag decl -> Stage.equal decl.stage stage } in
  let map = { HMap.map = fun tag decl -> decl.freeze_function ~marshallable } in
  { summaries = HMap.map map (HMap.filter filter !sum_map);
    ml_module =
      match stage with
      | Stage.Synterp ->
        Option.map (fun decl -> decl.freeze_function ~marshallable) !sum_mod
      | _ ->
        None;
  }

let freeze_summaries ~marshallable : frozen =
  let map = { HMap.map = fun tag decl -> decl.freeze_function ~marshallable } in
  { summaries = HMap.map map !sum_map;
    ml_module = Option.map (fun decl -> decl.freeze_function ~marshallable) !sum_mod;
  }

let warn_summary_out_of_scope =
  let name = "summary-out-of-scope" in
  let category = "dev" in
  let default = CWarnings.Disabled in
  CWarnings.create ~name ~category ~default (fun name ->
    Pp.str (Printf.sprintf
      "A Coq plugin was loaded inside a local scope (such as a Section). It is recommended to load plugins at the start of the file. Summary entry: %s"
      name)
    )

let unfreeze_summaries ?(partial=false) { summaries; ml_module } =
  (* The unfreezing of [ml_modules_summary] has to be anticipated since it
   * may modify the content of [summaries] by loading new ML modules *)
  begin match !sum_mod with
  | None -> anomaly (str "Undeclared ML-MODULES summary.")
  | Some decl -> Option.iter decl.unfreeze_function ml_module
  end;
  (* We must be independent on the order of the map! *)
  let ufz (DynMap.Any (name, decl)) =
    try decl.unfreeze_function Frozen.(find name summaries)
    with Not_found ->
      if not partial then begin
        warn_summary_out_of_scope (Dyn.repr name);
        decl.init_function ()
      end;
  in
  (* String.Map.iter unfreeze_single !sum_map *)
  DynMap.iter ufz !sum_map

let init_summaries () =
  DynMap.iter (fun (DynMap.Any (_, decl)) -> decl.init_function ()) !sum_map

(** For global tables registered statically before the end of coqtop
    launch, the following empty [init_function] could be used. *)

let nop () = ()

(** Summary projection *)
let project_from_summary { summaries } tag =
  Frozen.find tag summaries

let modify_summary st tag v =
  let () = assert (Frozen.mem tag st.summaries) in
  let summaries = Frozen.add tag v st.summaries in
  {st with summaries}

let remove_from_summary st tag =
  let summaries = Frozen.remove tag st.summaries in
  {st with summaries}

(** All-in-one reference declaration + registration *)

let ref_tag ?(stage=Stage.Interp) ?(freeze=fun ~marshallable r -> r) ~name x =
  let r = ref x in
  let tag = declare_summary_tag name
    { stage;
      freeze_function = (fun ~marshallable -> freeze ~marshallable !r);
      unfreeze_function = ((:=) r);
      init_function = (fun () -> r := x) } in
  r, tag

let ref ?stage ?freeze ~name x = fst @@ ref_tag ?stage ?freeze ~name x

module Local = struct

type 'a local_ref = 'a CEphemeron.key ref * 'a CEphemeron.key Dyn.tag

let set (r, tag) v = r := CEphemeron.create v

let get (key, name) =
  try CEphemeron.get !key
  with CEphemeron.InvalidKey ->
    let { init_function } = DynMap.find name !sum_map in
    init_function ();
    CEphemeron.get !key

let ref (type a) ?(stage=Stage.Interp) ~name (init : a) : a local_ref =
  let () = check_name (mangle name) in
  let tag : a CEphemeron.key Dyn.tag = Dyn.create (mangle name) in
  let r = pervasives_ref (CEphemeron.create init) in
  let () = sum_map := DynMap.add tag
    { stage;
      freeze_function = (fun ~marshallable -> !r);
      unfreeze_function = (fun v -> r := v);
      init_function = (fun () -> r := CEphemeron.create init) } !sum_map
  in
  (r, tag)

let (!) = get
let (:=) = set

end

let dump = Dyn.dump

let grab_stats_summary_entry (Frozen.Any (tag, value)) =
  Dyn.repr tag, Obj.(reachable_words (magic value))

let print_stats fmt (name, size) =
  Format.fprintf fmt "@[%s: %d@]" name size

let print_stats_summary_entry stats =
  (* Filter size 0 *)
  let stats = List.filter (fun (_,s) -> not (Int.equal s 0)) stats in
  let stats = List.sort (fun (_,s1) (_,s2) -> - (Int.compare s1 s2)) stats in
  Format.eprintf "  @[<v>%a@]@\n%!" (Format.pp_print_list print_stats) stats

let print_stats { summaries : Frozen.t } =
  Format.eprintf " [summary] mem reach: %d@\n%!" Obj.(reachable_words (magic summaries));
  let stats = Frozen.fold (fun s acc -> s :: acc) summaries [] in
  let stats = List.map grab_stats_summary_entry stats in
  print_stats_summary_entry stats;
  ()
