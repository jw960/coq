type t =
  { proof_label : string
  ; goal : string
  ; proof_step: string
  ; proof_step_hash : string
  ; parent_hash : string list
  } [@@deriving yojson]

let default =
  { proof_label = ""
  ; goal = ""
  ; proof_step = ""
  ; proof_step_hash = ""
  ; parent_hash = []
  }

let make (pf : Proof.t) =
  let { Proof.goals; sigma; _ } = Proof.data pf in
  match goals with
  | [gl] ->
    let ei = Evd.find sigma gl in
    let goal = Printer.pr_evar sigma (gl,ei) |> Pp.string_of_ppcmds in
    { default with goal }
  | _ ->
    default
