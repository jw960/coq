module Kind : sig
  type t = UnFreeze | Factorize | Remove | Extend | Compare
end

val time : ('a -> 'b) -> 'a -> 'b * float

val get : kind:Kind.t -> float
val record : kind:Kind.t -> f:('a -> 'b) -> 'a -> 'b * float

val dump : unit -> string

val reset : unit -> unit
