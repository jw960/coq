(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Declaration of section variables and local definitions *)
type variable_declaration =
  | SectionLocalDef of Evd.side_effects Declare.proof_entry
  | SectionLocalAssum of { typ:Constr.types; impl:Glob_term.binding_kind; }

(* This object is only for things which iterate over objects to find
   variables (only Prettyp.print_context AFAICT) *)
let inVariable : unit -> Libobject.obj =
  let open Libobject in
  declare_object { (default_object "VARIABLE") with
    classify_function = (fun () -> Dispose)}

let declare_variable ~name ~kind d =
  (* Constr raisonne sur les noms courts *)
  if Decls.variable_exists name then
    raise (Declare.AlreadyDeclared (None, name));

  let open Entries in
  let open Declare in
  let impl,opaque = match d with (* Fails if not well-typed *)
    | SectionLocalAssum {typ;impl} ->
      let () = Global.push_named_assum (name,typ) in
      impl, true
    | SectionLocalDef (de) ->
      (* The body should already have been forced upstream because it is a
         section-local definition, but it's not enforced by typing *)
      let (body, eff) = Future.force de.proof_entry_body in
      let ((body, uctx), export) = Global.export_private_constants (body, eff.Evd.seff_private) in
      let eff = Declare.get_roles export eff in
      let () = List.iter Declare.register_side_effect eff in
      let poly, univs = match de.proof_entry_universes with
        | Monomorphic_entry uctx -> false, uctx
        | Polymorphic_entry (_, uctx) -> true, Univ.ContextSet.of_context uctx
      in
      let univs = Univ.ContextSet.union uctx univs in
      (* We must declare the universe constraints before type-checking the
         term. *)
      let () = declare_universe_context ~poly univs in
      let se = {
        secdef_body = body;
        secdef_secctx = de.proof_entry_secctx;
        secdef_feedback = de.proof_entry_feedback;
        secdef_type = de.proof_entry_type;
      } in
      let () = Global.push_named_def (name, se) in
      Glob_term.Explicit, de.proof_entry_opaque
  in
  Nametab.push (Nametab.Until 1) (Libnames.make_path Names.DirPath.empty name) (Names.GlobRef.VarRef name);
  Decls.(add_variable_data name {opaque;kind});
  ignore(Lib.add_leaf name (inVariable ()) : Libobject.object_name);
  Impargs.declare_var_implicits ~impl name;
  Notation.declare_ref_arguments_scope Evd.empty (Names.GlobRef.VarRef name)

let assumption_message id =
  (* Changing "assumed" to "declared", "assuming" referring more to
  the type of the object than to the name of the object (see
  discussion on coqdev: "Chapter 4 of the Reference Manual", 8/10/2015) *)
  Flags.if_verbose Feedback.msg_info Pp.(Names.Id.print id ++ str " is declared")
