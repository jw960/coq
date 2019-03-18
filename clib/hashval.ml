type t = Int32.t

let zero = Int32.zero
let one = Int32.one
let two = Int32.of_int 2
let three = Int32.of_int 3
let four = Int32.of_int 4

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
