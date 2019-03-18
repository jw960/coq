(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

type t =
  | Int63head0
  | Int63tail0
  | Int63add
  | Int63sub
  | Int63mul
  | Int63div
  | Int63mod
  | Int63lsr
  | Int63lsl
  | Int63land
  | Int63lor
  | Int63lxor
  | Int63addc
  | Int63subc
  | Int63addCarryC
  | Int63subCarryC
  | Int63mulc
  | Int63diveucl
  | Int63div21
  | Int63addMulDiv
  | Int63eq
  | Int63lt
  | Int63le
  | Int63compare

let equal (p1 : t) (p2 : t) =
  p1 == p2

let hash = function
  | Int63head0 -> Hashval.of_int 1
  | Int63tail0 -> Hashval.of_int 2
  | Int63add -> Hashval.of_int 3
  | Int63sub -> Hashval.of_int 4
  | Int63mul -> Hashval.of_int 5
  | Int63div -> Hashval.of_int 6
  | Int63mod -> Hashval.of_int 7
  | Int63lsr -> Hashval.of_int 8
  | Int63lsl -> Hashval.of_int 9
  | Int63land -> Hashval.of_int 10
  | Int63lor -> Hashval.of_int 11
  | Int63lxor -> Hashval.of_int 12
  | Int63addc -> Hashval.of_int 13
  | Int63subc -> Hashval.of_int 14
  | Int63addCarryC -> Hashval.of_int 15
  | Int63subCarryC -> Hashval.of_int 16
  | Int63mulc -> Hashval.of_int 17
  | Int63diveucl -> Hashval.of_int 18
  | Int63div21 -> Hashval.of_int 19
  | Int63addMulDiv -> Hashval.of_int 20
  | Int63eq -> Hashval.of_int 21
  | Int63lt -> Hashval.of_int 22
  | Int63le -> Hashval.of_int 23
  | Int63compare -> Hashval.of_int 24

(* Should match names in nativevalues.ml *)
let to_string = function
  | Int63head0 -> "head0"
  | Int63tail0 -> "tail0"
  | Int63add -> "add"
  | Int63sub -> "sub"
  | Int63mul -> "mul"
  | Int63div -> "div"
  | Int63mod -> "rem"
  | Int63lsr -> "l_sr"
  | Int63lsl -> "l_sl"
  | Int63land -> "l_and"
  | Int63lor -> "l_or"
  | Int63lxor -> "l_xor"
  | Int63addc -> "addc"
  | Int63subc -> "subc"
  | Int63addCarryC -> "addCarryC"
  | Int63subCarryC -> "subCarryC"
  | Int63mulc -> "mulc"
  | Int63diveucl -> "diveucl"
  | Int63div21 -> "div21"
  | Int63addMulDiv -> "addMulDiv"
  | Int63eq -> "eq"
  | Int63lt -> "lt"
  | Int63le -> "le"
  | Int63compare -> "compare"

type arg_kind =
  | Kparam (* not needed for the evaluation of the primitive when it reduces *)
  | Kwhnf  (* need to be reduced in whnf before reducing the primitive *)
  | Karg   (* no need to be reduced in whnf. example: [v] in [Array.set t i v] *)

type args_red = arg_kind list

(* Invariant only argument of type int63 or an inductive can
   have kind Kwhnf *)

let kind = function
  | Int63head0 | Int63tail0 -> [Kwhnf]

  | Int63add | Int63sub | Int63mul
  | Int63div | Int63mod
  | Int63lsr | Int63lsl
  | Int63land | Int63lor | Int63lxor
  | Int63addc | Int63subc
  | Int63addCarryC | Int63subCarryC  | Int63mulc | Int63diveucl
  | Int63eq | Int63lt | Int63le | Int63compare -> [Kwhnf; Kwhnf]

  | Int63div21 | Int63addMulDiv -> [Kwhnf; Kwhnf; Kwhnf]

let arity = function
  | Int63head0 | Int63tail0 -> 1
  | Int63add | Int63sub | Int63mul
  | Int63div | Int63mod
  | Int63lsr | Int63lsl
  | Int63land | Int63lor | Int63lxor
  | Int63addc | Int63subc
  | Int63addCarryC | Int63subCarryC | Int63mulc | Int63diveucl
  | Int63eq | Int63lt | Int63le
  | Int63compare -> 2

  | Int63div21 | Int63addMulDiv -> 3

(** Special Entries for Register **)

type prim_ind =
  | PIT_bool
  | PIT_carry
  | PIT_pair
  | PIT_cmp

type prim_type =
  | PT_int63

type op_or_type =
  | OT_op of t
  | OT_type of prim_type

let prim_ind_to_string = function
  | PIT_bool -> "bool"
  | PIT_carry -> "carry"
  | PIT_pair -> "pair"
  | PIT_cmp -> "cmp"

let prim_type_to_string = function
  | PT_int63 -> "int63_type"

let op_or_type_to_string = function
  | OT_op op -> to_string op
  | OT_type t -> prim_type_to_string t
