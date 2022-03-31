type t =
  { proof_label : string
  ; goal : string
  ; proof_step: string
  ; proof_step_hash : string
  ; parent_hash : string list
  } [@@deriving yojson]

val make : Proof.t -> t
