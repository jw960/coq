(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Names
open Univ
open Constr
open Mod_subst

type work_list = (Instance.t * Id.t array) Cmap.t * 
  (Instance.t * Id.t array) Mindmap.t

type cooking_info = {
  modlist : work_list;
  abstract : Constr.named_context * Univ.Instance.t * Univ.AUContext.t }

type indirect_accessor = {
  access_proof : DirPath.t -> int -> constr option;
  access_discharge : cooking_info list -> int -> constr -> constr;
}

type proofterm = (constr * Univ.ContextSet.t) Lazy.t
type universes = int
type opaque =
  | Indirect of substitution list * DirPath.t * int (* subst, lib, index *)
  | Direct of universes * cooking_info list * proofterm
type opaquetab = {
  opaque_val : (int * cooking_info list * proofterm) Int.Map.t;
  (** Actual proof terms *)
  opaque_len : int;
  (** Size of the above map *)
  opaque_dir : DirPath.t;
}
let empty_opaquetab = {
  opaque_val = Int.Map.empty;
  opaque_len = 0;
  opaque_dir = DirPath.initial;
}

let not_here () =
  CErrors.user_err Pp.(str "Cannot access opaque delayed proof")

let create ~univs cu = Direct (univs, [],cu)

let turn_indirect dp o tab = match o with
  | Indirect (_,_,i) ->
      if not (Int.Map.mem i tab.opaque_val)
      then CErrors.anomaly (Pp.str "Indirect in a different table.")
      else CErrors.anomaly (Pp.str "Already an indirect opaque.")
  | Direct (nunivs, d, cu) ->
    (* Invariant: direct opaques only exist inside sections, we turn them
      indirect as soon as we are at toplevel. At this moment, we perform
      hashconsing of their contents, potentially as a future. *)
      let hcons (c, u) =
        let c = Constr.hcons c in
        let u = Univ.hcons_universe_context_set u in
        (c, u)
      in
      let cu = Lazy.from_fun (fun () -> hcons (Lazy.force cu)) in
      let id = tab.opaque_len in
      let opaque_val = Int.Map.add id (nunivs, d,cu) tab.opaque_val in
      let opaque_dir =
        if DirPath.equal dp tab.opaque_dir then tab.opaque_dir
        else if DirPath.equal tab.opaque_dir DirPath.initial then dp
        else CErrors.anomaly
          (Pp.str "Using the same opaque table for multiple dirpaths.") in
      let ntab = { opaque_val; opaque_dir; opaque_len = id + 1 } in
      Indirect ([],dp,id), ntab

let subst_opaque sub = function
  | Indirect (s,dp,i) -> Indirect (sub::s,dp,i)
  | Direct _ -> CErrors.anomaly (Pp.str "Substituting a Direct opaque.")

let discharge_direct_opaque ci = function
  | Indirect _ -> CErrors.anomaly (Pp.str "Not a direct opaque.")
  | Direct (n, d, cu) ->
      Direct (n, ci :: d, cu)

let join cu = Lazy.force cu

let join_opaque { opaque_val = prfs; opaque_dir = odp; _ } = function
  | Direct (_,_,cu) -> ignore(join cu)
  | Indirect (_,dp,i) ->
    if DirPath.equal dp odp then
      let (_, _, fp) = Int.Map.find i prfs in
      ignore(join fp)

let force_proof access { opaque_val = prfs; opaque_dir = odp; _ } = function
  | Direct (n, d, cu) ->
    let (c, _) = Lazy.force cu in
    access.access_discharge d n c
  | Indirect (l,dp,i) ->
      let c =
        if DirPath.equal dp odp
        then
          let (n, d, cu) = Int.Map.find i prfs in
          let (c, _) = Lazy.force cu in
          access.access_discharge d n c
        else match access.access_proof dp i with
        | None -> not_here ()
        | Some v -> v
      in
      force_constr (List.fold_right subst_substituted l (from_val c))

let force_constraints _access { opaque_val = prfs; opaque_dir = odp; _ } = function
  | Direct (_,_,cu) ->
    snd(Lazy.force cu)
  | Indirect (_,dp,i) ->
      if DirPath.equal dp odp
      then
        let (_, _, cu) = Int.Map.find i prfs in
        snd (Lazy.force cu)
      else Univ.ContextSet.empty

let get_direct_constraints = function
| Indirect _ -> CErrors.anomaly (Pp.str "Not a direct opaque.")
| Direct (_, _, cu) -> Lazy.from_fun (fun () -> snd (Lazy.force cu))
