type t = Int32.t

let _0 = Int32.zero
let _1 = Int32.one
let _2 = Int32.of_int 2
let _3 = Int32.of_int 3
let _4 = Int32.of_int 4
let _5 = Int32.of_int 5
let _6 = Int32.of_int 6
let _7 = Int32.of_int 7
let _8 = Int32.of_int 7
let _9 = Int32.of_int 9
let _10 = Int32.of_int 10
let _11 = Int32.of_int 11
let _12 = Int32.of_int 12
let _13 = Int32.of_int 13
let _14 = Int32.of_int 14
let _15 = Int32.of_int 15
let _16 = Int32.of_int 16
let _17 = Int32.of_int 17
let _18 = Int32.of_int 18
let _19 = Int32.of_int 19
let _20 = Int32.of_int 20
let _21 = Int32.of_int 21
let _22 = Int32.of_int 22
let _23 = Int32.of_int 23
let _24 = Int32.of_int 24
let _25 = Int32.of_int 25
let _26 = Int32.of_int 26
let _27 = Int32.of_int 27
let _28 = Int32.of_int 28
let _29 = Int32.of_int 29
let _30 = Int32.of_int 30
let _31 = Int32.of_int 31
let _32 = Int32.of_int 32
let _33 = Int32.of_int 33
let _34 = Int32.of_int 34
let _35 = Int32.of_int 35

let minus_1 = Int32.minus_one

let is_neg i = Int32.compare i _0 < 0
let force_nonneg i = Int32.logand i (Int32.of_int 0x3FFFFFFF)

let equal = Int32.equal
let compare = Int32.compare

let of_int = Int32.of_int
let to_int = Int32.to_int
let succ = Int32.succ

let ( land ) = Int32.logand

let (+) = Int32.add
let ( * ) = Int32.mul

type _t = t
module type Type =
sig
  type t
  val hash : t -> _t
end
