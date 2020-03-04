(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

module type ZArith = sig
  type t

  val zero : t
  val one : t
  val two : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val neg : t -> t
  val sign : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val power_int : t -> int -> t
  val quomod : t -> t -> t * t
  val ppcm : t -> t -> t
  val gcd : t -> t -> t
  val lcm : t -> t -> t
  val to_string : t -> string
end

module Z_old = struct
  type t = Big_int.big_int

  open Big_int

  let zero = zero_big_int
  let one = unit_big_int
  let two = big_int_of_int 2
  let add = Big_int.add_big_int
  let sub = Big_int.sub_big_int
  let mul = Big_int.mult_big_int
  let div = Big_int.div_big_int
  let neg = Big_int.minus_big_int
  let sign = Big_int.sign_big_int
  let equal = eq_big_int
  let compare = compare_big_int
  let power_int = power_big_int_positive_int
  let quomod = quomod_big_int

  let ppcm x y =
    let g = gcd_big_int x y in
    let x' = div_big_int x g in
    let y' = div_big_int y g in
    mult_big_int g (mult_big_int x' y')

  let gcd = gcd_big_int

  let lcm x y =
    if eq_big_int x zero && eq_big_int y zero then zero
    else abs_big_int (div_big_int (mult_big_int x y) (gcd x y))

  let to_string = string_of_big_int
end

module Z_new = struct
  include Z

  (* Workaround https://github.com/ocaml/Zarith/issues/58 , remove
     when zarith 1.9.2 is released *)
  let gcd x y = Z.abs (Z.gcd x y)
  let lcm x y = Z.abs (Z.lcm x y)

  (* Constants *)
  let two = Z.of_int 2
  let ten = Z.of_int 10
  let power_int = Big_int_Z.power_big_int_positive_int
  let quomod = Big_int_Z.quomod_big_int

  let ppcm x y =
    let g = Z.gcd x y in
    let x' = Z.div x g in
    let y' = Z.div y g in
    Z.mul g (Z.mul x' y')

  let lcm x y =
    if Z.equal x zero && Z.equal y zero then zero
    else Z.abs (Z.div (Z.mul x y) (gcd x y))
end

module type QArith = sig
  module Z : ZArith

  type t

  val of_int : int -> t
  val zero : t
  val one : t
  val two : t
  val ten : t
  val neg_one : t

  module Notations : sig
    val ( // ) : t -> t -> t
    val ( +/ ) : t -> t -> t
    val ( -/ ) : t -> t -> t
    val ( */ ) : t -> t -> t
    val ( =/ ) : t -> t -> bool
    val ( <>/ ) : t -> t -> bool
    val ( >/ ) : t -> t -> bool
    val ( >=/ ) : t -> t -> bool
    val ( </ ) : t -> t -> bool
    val ( <=/ ) : t -> t -> bool
  end

  val compare : t -> t -> int
  val make : Z.t -> Z.t -> t
  val den : t -> Z.t
  val num : t -> Z.t
  val of_bigint : Z.t -> t
  val to_bigint : t -> Z.t
  val neg : t -> t

  (* val inv : t -> t *)
  val max : t -> t -> t
  val min : t -> t -> t
  val sign : t -> int
  val abs : t -> t
  val mod_ : t -> t -> t
  val floor : t -> t

  (* val floorZ : t -> Z.t *)
  val ceiling : t -> t
  val round : t -> t
  val pow2 : int -> t
  val pow10 : int -> t
  val power : int -> t -> t
  val to_string : t -> string
  val of_string : string -> t
  val to_float : t -> float
end

module Q_new : QArith with module Z = Z_new = struct
  module Z = Z_new

  let pow_check_exp x y =
    let z_res =
      if y = 0 then Z.one
      else if y > 0 then Z.pow x y
      else (* s < 0 *)
        Z.pow x (abs y)
    in
    let z_res = Q.of_bigint z_res in
    if 0 <= y then z_res else Q.inv z_res

  include Q

  let two = Q.(of_int 2)
  let ten = Q.(of_int 10)
  let neg_one = Q.(neg one)

  module Notations = struct
    let ( // ) = Q.div
    let ( +/ ) = Q.add
    let ( -/ ) = Q.sub
    let ( */ ) = Q.mul
    let ( =/ ) = Q.equal
    let ( <>/ ) x y = not (Q.equal x y)
    let ( >/ ) = Q.gt
    let ( >=/ ) = Q.geq
    let ( </ ) = Q.lt
    let ( <=/ ) = Q.leq
  end

  (* XXX: review / improve *)
  let floorZ q : Z.t = Z.fdiv (num q) (den q)
  let floor q : t = floorZ q |> Q.of_bigint
  let ceiling q : t = Z.cdiv (Q.num q) (Q.den q) |> Q.of_bigint

  let half = Q.make Z.one Z.two
  (* Num round is to the nearest *)
  let round q = floor (Q.add half q)

  (* XXX: review / improve *)
  let quo x y =
    let s = sign y in
    let res = floor (x / abs y) in
    if Int.equal s (-1) then neg res else res

  let mod_ x y = x - (y * quo x y)

  (* XXX: review / improve *)
  (* Note that Z.pow doesn't support negative exponents *)
  let pow2 y = pow_check_exp Z.two y
  let pow10 y = pow_check_exp Z.ten y

  let power (x : int) (y : t) : t =
    let y =
      try Q.to_int y
      with Z.Overflow ->
        (* XXX: make doesn't link Pp / CErrors for csdpcert, that could be fixed *)
        raise (Invalid_argument "[micromega] overflow in exponentiation")
        (* CErrors.user_err (Pp.str "[micromega] overflow in exponentiation") *)
    in
    pow_check_exp (Z.of_int x) y
end

module Q_old : QArith with module Z = Z_old = struct
  module Z = Z_old

  type t = Num.num

  open Num

  let of_int x = Int x
  let zero = Int 0
  let one = Int 1
  let two = Int 2
  let ten = Int 10
  let neg_one = Int (-1)

  module Notations = struct
    let ( // ) = div_num
    let ( +/ ) = add_num
    let ( -/ ) = sub_num
    let ( */ ) = mult_num
    let ( =/ ) = eq_num
    let ( <>/ ) = ( <>/ )
    let ( >/ ) = ( >/ )
    let ( >=/ ) = ( >=/ )
    let ( </ ) = ( </ )
    let ( <=/ ) = ( <=/ )
  end

  let compare = compare_num
  let make x y = Big_int x // Big_int y

  let numdom r =
    let r' = Ratio.normalize_ratio (ratio_of_num r) in
    (Ratio.numerator_ratio r', Ratio.denominator_ratio r')

  let num x = numdom x |> fst
  let den x = numdom x |> snd
  let of_bigint x = Big_int x
  let to_bigint = big_int_of_num
  let neg = minus_num

  (* let inv =  *)
  let max = max_num
  let min = min_num
  let sign = sign_num
  let abs = abs_num
  let mod_ = mod_num
  let floor = floor_num
  let ceiling = ceiling_num
  let round = round_num
  let pow2 n = power_num two (Int n)
  let pow10 n = power_num ten (Int n)
  let power x = power_num (Int x)
  let to_string = string_of_num
  let of_string = num_of_string
  let to_float = float_of_num
end

(* XXX *)
let _ = Q_new.zero
let _ = Q_old.zero

module Z_mix : ZArith with type t = Z_old.t * Z_new.t = struct
  module Zo = Z_old
  module Zn = Z_new

  type t = Zo.t * Zn.t

  let eq_ name (x : Zo.t) (y : Zn.t) : unit =
    let x_ = Zo.to_string x in
    let y_ = Zn.to_string y in
    let a = String.equal x_ y_ in
    if a then ()
    else
      let () = Format.eprintf "assertion failure at %s, [%s] vs [%s]@\n%!" name x_ y_ in
      assert false

  let eqg name x y : unit =
    let a = x = y in
    if a then ()
    else
      let () = Format.eprintf "assertion failure at %s@\n%!" name in
      assert false

  let lift1 name fo fn x =
    let y_old = fo (fst x) in
    let y_new = fn (snd x) in
    eq_ name y_old y_new; (y_old, y_new)

  let lift1b name fo fn x =
    let y_old = fo (fst x) in
    let y_new = fn (snd x) in
    eqg name y_old y_new; y_old

  let lift2b name fo fn x y =
    let r_old = fo (fst x) (fst y) in
    let r_new = fn (snd x) (snd y) in
    eqg name r_old r_new; r_old

  let lift2 name fo fn x y =
    let r_old = fo (fst x) (fst y) in
    let r_new = fn (snd x) (snd y) in
    eq_ name r_old r_new; (r_old, r_new)

  let lift22 name fo fn x y =
    let r_old1, r_old2 = fo (fst x) (fst y) in
    let r_new1, r_new2 = fn (snd x) (snd y) in
    eq_ name r_old1 r_new1;
    eq_ name r_old2 r_new2;
    ((r_old1, r_new1), (r_old2, r_new2))

  let zero = (Z_old.zero, Z_new.zero)
  let one = (Z_old.one, Z_new.one)
  let two = (Z_old.two, Z_new.two)
  let add = lift2 "add" Zo.add Zn.add
  let sub = lift2 "sub" Zo.sub Zn.sub
  let mul = lift2 "mul" Zo.mul Zn.mul
  let div = lift2 "div" Zo.div Zn.div
  let neg = lift1 "new" Zo.neg Zn.neg
  let sign = lift1b "sign" Zo.sign Zn.sign
  let equal = lift2b "eq" Zo.equal Zn.equal
  let compare = lift2b "compare" Zo.compare Zn.compare
  let power_int x i = lift2 "power_int" Zo.power_int Zn.power_int x (i, i)
  let quomod : t -> t -> t * t = lift22 "quomod" Zo.quomod Zn.quomod
  let ppcm = lift2 "ppcm" Zo.ppcm Zn.ppcm
  let gcd = lift2 "gcd" Zo.gcd Zn.gcd
  let lcm = lift2 "lcm" Zo.lcm Zn.lcm
  let to_string = lift1b "to_string" Zo.to_string Zn.to_string
end

module Q_old_z_new : QArith with module Z = Z_mix = struct
  module Z = Z_mix

  type t = Num.num

  open Num

  let of_int x = Int x
  let zero = Int 0
  let one = Int 1
  let two = Int 2
  let ten = Int 10
  let neg_one = Int (-1)

  module Notations = struct
    let ( // ) = div_num
    let ( +/ ) = add_num
    let ( -/ ) = sub_num
    let ( */ ) = mult_num
    let ( =/ ) = eq_num
    let ( <>/ ) = ( <>/ )
    let ( >/ ) = ( >/ )
    let ( >=/ ) = ( >=/ )
    let ( </ ) = ( </ )
    let ( <=/ ) = ( <=/ )
  end

  let compare = compare_num
  let make x y = Big_int (fst x) // Big_int (fst y)

  let numdom r =
    let r' = Ratio.normalize_ratio (ratio_of_num r) in
    (Ratio.numerator_ratio r', Ratio.denominator_ratio r')

  let conv x = Z_new.of_string (Z_old.to_string x)
  let num x = numdom x |> fst |> fun x -> (x, conv x)
  let den x = numdom x |> snd |> fun x -> (x, conv x)
  let of_bigint x = Big_int (fst x)
  let to_bigint x = (big_int_of_num x, conv (big_int_of_num x))
  let neg = minus_num

  (* let inv =  *)
  let max = max_num
  let min = min_num
  let sign = sign_num
  let abs = abs_num
  let mod_ = mod_num
  let floor = floor_num
  let ceiling = ceiling_num
  let round = round_num
  let pow2 n = power_num two (Int n)
  let pow10 n = power_num ten (Int n)
  let power x = power_num (Int x)
  let to_string = string_of_num
  let of_string = num_of_string
  let to_float = float_of_num
end

let _ = Q_old_z_new.zero

module Z = Z_mix
module Q = Q_old_z_new
