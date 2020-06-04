(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Univ

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
  | Float64opp
  | Float64abs
  | Float64eq
  | Float64lt
  | Float64le
  | Float64compare
  | Float64classify
  | Float64add
  | Float64sub
  | Float64mul
  | Float64div
  | Float64sqrt
  | Float64ofInt63
  | Float64normfr_mantissa
  | Float64frshiftexp
  | Float64ldshiftexp
  | Float64next_up
  | Float64next_down
  | Arraymake
  | Arrayget
  | Arraydefault
  | Arrayset
  | Arraycopy
  | Arrayreroot
  | Arraylength

let parse = function
  | "int63_head0" -> Int63head0
  | "int63_tail0" -> Int63tail0
  | "int63_add" -> Int63add
  | "int63_sub" -> Int63sub
  | "int63_mul" -> Int63mul
  | "int63_div" -> Int63div
  | "int63_mod" -> Int63mod
  | "int63_lsr" -> Int63lsr
  | "int63_lsl" -> Int63lsl
  | "int63_land" -> Int63land
  | "int63_lor" -> Int63lor
  | "int63_lxor" -> Int63lxor
  | "int63_addc" -> Int63addc
  | "int63_subc" -> Int63subc
  | "int63_addcarryc" -> Int63addCarryC
  | "int63_subcarryc" -> Int63subCarryC
  | "int63_mulc" -> Int63mulc
  | "int63_diveucl" -> Int63diveucl
  | "int63_div21" -> Int63div21
  | "int63_addmuldiv" -> Int63addMulDiv
  | "int63_eq" -> Int63eq
  | "int63_lt" -> Int63lt
  | "int63_le" -> Int63le
  | "int63_compare" -> Int63compare
  | "float64_opp" -> Float64opp
  | "float64_abs" -> Float64abs
  | "float64_eq" -> Float64eq
  | "float64_lt" -> Float64lt
  | "float64_le" -> Float64le
  | "float64_compare" -> Float64compare
  | "float64_classify" -> Float64classify
  | "float64_add" -> Float64add
  | "float64_sub" -> Float64sub
  | "float64_mul" -> Float64mul
  | "float64_div" -> Float64div
  | "float64_sqrt" -> Float64sqrt
  | "float64_of_int63" -> Float64ofInt63
  | "float64_normfr_mantissa" -> Float64normfr_mantissa
  | "float64_frshiftexp" -> Float64frshiftexp
  | "float64_ldshiftexp" -> Float64ldshiftexp
  | "float64_next_up" -> Float64next_up
  | "float64_next_down" -> Float64next_down
  | "array_make" -> Arraymake
  | "array_get" -> Arrayget
  | "array_default" -> Arraydefault
  | "array_set" -> Arrayset
  | "array_length" -> Arraylength
  | "array_copy" -> Arraycopy
  | "array_reroot" -> Arrayreroot
  | _ -> raise Not_found

let equal (p1 : t) (p2 : t) =
  p1 == p2

let hash = function
  | Int63head0 -> 1
  | Int63tail0 -> 2
  | Int63add -> 3
  | Int63sub -> 4
  | Int63mul -> 5
  | Int63div -> 6
  | Int63mod -> 7
  | Int63lsr -> 8
  | Int63lsl -> 9
  | Int63land -> 10
  | Int63lor -> 11
  | Int63lxor -> 12
  | Int63addc -> 13
  | Int63subc -> 14
  | Int63addCarryC -> 15
  | Int63subCarryC -> 16
  | Int63mulc -> 17
  | Int63diveucl -> 18
  | Int63div21 -> 19
  | Int63addMulDiv -> 20
  | Int63eq -> 21
  | Int63lt -> 22
  | Int63le -> 23
  | Int63compare -> 24
  | Float64opp -> 25
  | Float64abs -> 26
  | Float64compare -> 27
  | Float64classify -> 28
  | Float64add -> 29
  | Float64sub -> 30
  | Float64mul -> 31
  | Float64div -> 32
  | Float64sqrt -> 33
  | Float64ofInt63 -> 34
  | Float64normfr_mantissa -> 35
  | Float64frshiftexp -> 36
  | Float64ldshiftexp -> 37
  | Float64next_up -> 38
  | Float64next_down -> 39
  | Float64eq -> 40
  | Float64lt -> 41
  | Float64le -> 42
  | Arraymake -> 43
  | Arrayget -> 44
  | Arraydefault -> 45
  | Arrayset -> 46
  | Arraycopy -> 47
  | Arrayreroot -> 48
  | Arraylength -> 49

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
  | Float64opp -> "fopp"
  | Float64abs -> "fabs"
  | Float64eq -> "feq"
  | Float64lt -> "flt"
  | Float64le -> "fle"
  | Float64compare -> "fcompare"
  | Float64classify -> "fclassify"
  | Float64add -> "fadd"
  | Float64sub -> "fsub"
  | Float64mul -> "fmul"
  | Float64div -> "fdiv"
  | Float64sqrt -> "fsqrt"
  | Float64ofInt63 -> "float_of_int"
  | Float64normfr_mantissa -> "normfr_mantissa"
  | Float64frshiftexp -> "frshiftexp"
  | Float64ldshiftexp -> "ldshiftexp"
  | Float64next_up    -> "next_up"
  | Float64next_down  -> "next_down"
  | Arraymake -> "arraymake"
  | Arrayget -> "arrayget"
  | Arraydefault -> "arraydefault"
  | Arrayset -> "arrayset"
  | Arraycopy -> "arraycopy"
  | Arrayreroot -> "arrayreroot"
  | Arraylength -> "arraylength"

type const =
  | Arraymaxlength

let const_to_string = function
  | Arraymaxlength -> "arraymaxlength"

let const_of_string = function
  | "array_max_length" -> Arraymaxlength
  | _ -> raise Not_found

let const_univs = function
  | Arraymaxlength -> AUContext.empty

type 'a prim_type =
  | PT_int63 : unit prim_type
  | PT_float64 : unit prim_type
  | PT_array : (Instance.t * ind_or_type) prim_type

and 'a prim_ind =
  | PIT_bool : unit prim_ind
  | PIT_carry : ind_or_type prim_ind
  | PIT_pair : (ind_or_type * ind_or_type) prim_ind
  | PIT_cmp : unit prim_ind
  | PIT_f_cmp : unit prim_ind
  | PIT_f_class : unit prim_ind

and ind_or_type =
  | PITT_ind : 'a prim_ind * 'a -> ind_or_type
  | PITT_type : 'a prim_type * 'a -> ind_or_type
  | PITT_param : int -> ind_or_type (* DeBruijn index referring to prenex type quantifiers *)

let one_univ =
  AUContext.make Names.[|Name (Id.of_string "u")|] Constraint.empty

let typ_univs (type a) (t : a prim_type) = match t with
  | PT_int63 -> AUContext.empty
  | PT_float64 -> AUContext.empty
  | PT_array -> one_univ

type prim_type_ex = PTE : 'a prim_type -> prim_type_ex

type prim_ind_ex = PIE : 'a prim_ind -> prim_ind_ex

let types =
  let int_ty = PITT_type (PT_int63, ()) in
  let float_ty = PITT_type (PT_float64, ()) in
  let array_param =
    PITT_type
      (PT_array,
       (Instance.of_array [|Level.var 0|],
        PITT_param 1))
  in
  function
  | Int63head0 | Int63tail0 -> [[int_ty]; [int_ty]]
  | Int63add | Int63sub | Int63mul
  | Int63div | Int63mod
  | Int63lsr | Int63lsl
  | Int63land | Int63lor | Int63lxor -> [[int_ty]; [int_ty]; [int_ty]]
  | Int63addc | Int63subc | Int63addCarryC | Int63subCarryC ->
     [[int_ty]; [int_ty]; [PITT_ind (PIT_carry, int_ty)]]
  | Int63mulc | Int63diveucl ->
     [[int_ty]; [int_ty]; [PITT_ind (PIT_pair, (int_ty, int_ty))]]
  | Int63eq | Int63lt | Int63le -> [[int_ty]; [int_ty]; [PITT_ind (PIT_bool, ())]]
  | Int63compare -> [[int_ty]; [int_ty]; [PITT_ind (PIT_cmp, ())]]
  | Int63div21 ->
     [[int_ty]; [int_ty]; [int_ty]; [PITT_ind (PIT_pair, (int_ty, int_ty))]]
  | Int63addMulDiv -> [[int_ty]; [int_ty]; [int_ty]; [int_ty]]
  | Float64opp | Float64abs | Float64sqrt
  | Float64next_up | Float64next_down -> [[float_ty]; [float_ty]]
  | Float64ofInt63 -> [[int_ty]; [float_ty]]
  | Float64normfr_mantissa -> [[float_ty]; [int_ty]]
  | Float64frshiftexp -> [[float_ty]; [PITT_ind (PIT_pair, (float_ty, int_ty))]]
  | Float64eq | Float64lt | Float64le -> [[float_ty]; [float_ty]; [PITT_ind (PIT_bool, ())]]
  | Float64compare -> [[float_ty]; [float_ty]; [PITT_ind (PIT_f_cmp, ())]]
  | Float64classify -> [[float_ty]; [PITT_ind (PIT_f_class, ())]]
  | Float64add | Float64sub | Float64mul
  | Float64div -> [[float_ty]; [float_ty]; [float_ty]]
  | Float64ldshiftexp -> [[float_ty]; [int_ty]; [float_ty]]
  | Arraymake -> [[int_ty]; [PITT_param 1]; [array_param]]
  | Arrayget -> [[array_param]; [int_ty]; [PITT_param 1]]
  | Arraydefault -> [[array_param]; [PITT_param 1]]
  | Arrayset -> [[array_param]; [int_ty]; [PITT_param 1]; [array_param]]
  | Arraycopy -> [[array_param]; [array_param]]
  | Arrayreroot -> [[array_param]; [array_param]]
  | Arraylength -> [[array_param]; [int_ty]]

let one_param =
  (* currently if there's a parameter it's always this *)
  let a_annot = Context.nameR (Names.Id.of_string "A") in
  let ty = Constr.mkType (Universe.make (Level.var 0)) in
  Context.Rel.Declaration.[LocalAssum (a_annot, ty)]

let params = function
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
  | Float64opp
  | Float64abs
  | Float64eq
  | Float64lt
  | Float64le
  | Float64compare
  | Float64classify
  | Float64add
  | Float64sub
  | Float64mul
  | Float64div
  | Float64sqrt
  | Float64ofInt63
  | Float64normfr_mantissa
  | Float64frshiftexp
  | Float64ldshiftexp
  | Float64next_up
  | Float64next_down -> []

  | Arraymake
  | Arrayget
  | Arraydefault
  | Arrayset
  | Arraycopy
  | Arrayreroot
  | Arraylength -> one_param

let nparams x = List.length (params x)

let univs = function
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
  | Float64opp
  | Float64abs
  | Float64eq
  | Float64lt
  | Float64le
  | Float64compare
  | Float64classify
  | Float64add
  | Float64sub
  | Float64mul
  | Float64div
  | Float64sqrt
  | Float64ofInt63
  | Float64normfr_mantissa
  | Float64frshiftexp
  | Float64ldshiftexp
  | Float64next_up
  | Float64next_down -> AUContext.empty

  | Arraymake
  | Arrayget
  | Arraydefault
  | Arrayset
  | Arraycopy
  | Arrayreroot
  | Arraylength -> one_univ

type arg_kind =
  | Kparam (* not needed for the evaluation of the primitive when it reduces *)
  | Kwhnf  (* need to be reduced in whnf before reducing the primitive *)
  | Karg   (* no need to be reduced in whnf. example: [v] in [Array.set t i v] *)

type args_red = arg_kind list

(* Invariant only argument of type int63, float or an inductive can
   have kind Kwhnf *)

let arity t = let sign = types t in nparams t + List.length sign - 1

let kind t =
  let rec params n = if n <= 0 then [] else Kparam :: params (n - 1) in
  let args = function [PITT_type _] | [PITT_ind _] -> Kwhnf | _ -> Karg in
  params (nparams t) @ List.map args (CList.drop_last (types t))

let types t = params t, types t

(** Special Entries for Register **)

type op_or_type =
  | OT_op of t
  | OT_type : 'a prim_type -> op_or_type
  | OT_const of const

let prim_ind_to_string (type a) (p : a prim_ind) = match p with
  | PIT_bool -> "bool"
  | PIT_carry -> "carry"
  | PIT_pair -> "pair"
  | PIT_cmp -> "cmp"
  | PIT_f_cmp -> "f_cmp"
  | PIT_f_class -> "f_class"

let prim_type_to_string (type a) (ty : a prim_type) = match ty with
  | PT_int63 -> "int63_type"
  | PT_float64 -> "float64_type"
  | PT_array -> "array_type"

let op_or_type_to_string = function
  | OT_op op -> to_string op
  | OT_type t -> prim_type_to_string t
  | OT_const c -> const_to_string c

let prim_type_of_string = function
  | "int63_type" -> PTE PT_int63
  | "float64_type" -> PTE PT_float64
  | "array_type" -> PTE PT_array
  | _ -> raise Not_found

let op_or_type_of_string s =
  match prim_type_of_string s with
  | PTE ty -> OT_type ty
  | exception Not_found ->
    begin try OT_op (parse s)
      with Not_found -> OT_const (const_of_string s)
    end

let parse_op_or_type ?loc s =
  try op_or_type_of_string s
  with Not_found ->
    CErrors.user_err ?loc Pp.(str ("Built-in #"^s^" does not exist."))

let op_or_type_univs = function
  | OT_op t -> univs t
  | OT_type t -> typ_univs t
  | OT_const c -> const_univs c

let body_of_prim_const = function
  | Arraymaxlength ->
    Constr.mkInt (Parray.max_length)
