type t =
  { proof_label : string
  ; goal : string
  ; proof_step: string
  ; proof_step_hash : string
  ; parent_hash : string list
  } [@@deriving yojson]

let make _ =
  { proof_label = ""
  ; goal = ""
  ; proof_step = ""
  ; proof_step_hash = ""
  ; parent_hash = []
  }
