(* Hash type *)
type t

val zero : t
val one : t
val two : t
val three : t
val four : t

val succ : t -> t

val equal : t -> t -> bool
val compare : t -> t -> bool

val of_int : int -> t
val to_int : t -> int

val (+) : t -> t -> t
val ( * ) : t -> t -> t

val ( land ) : t -> t -> t

type _t = t

module type Type =
sig
  type t
  val hash : t -> _t
end
