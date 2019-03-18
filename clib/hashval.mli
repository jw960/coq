(* Hash type *)
type t

val _0 : t
val _1 : t
val _2 : t
val _3 : t
val _4 : t
val _5 : t
val _6 : t
val _7 : t
val _8 : t
val _9 : t
val _10 : t
val _11 : t
val _12 : t
val _13 : t
val _14 : t
val _15 : t
val _16 : t
val _17 : t
val _18 : t
val _19 : t
val _20 : t
val _21 : t
val _22 : t
val _23 : t
val _24 : t
val _25 : t
val _26 : t
val _27 : t
val _28 : t
val _29 : t
val _30 : t
val _31 : t
val _32 : t
val _33 : t
val _34 : t
val _35 : t

val minus_1 : t

val succ : t -> t

val is_neg : t -> bool
val force_nonneg : t -> t

val equal : t -> t -> bool
val compare : t -> t -> int

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
