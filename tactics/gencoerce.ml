open Util
open Genarg
open Geninterp
open Stdarg

exception CannotCoerceTo of string

let val_tag wit = match val_tag wit with
| Val.Base t -> t
| _ -> assert false

let has_type : type a. Val.t -> a typed_abstract_argument_type -> bool = fun v wit ->
  let Val.Dyn (t, _) = v in
  match Val.eq t (val_tag wit) with
  | None -> false
  | Some Refl -> true

(** All the types considered here are base types *)
let prj : type a. a Val.typ -> Val.t -> a option = fun t v ->
  let Val.Dyn (t', x) = v in
  match Val.eq t t' with
  | None -> None
  | Some Refl -> Some x

let in_gen wit v = Val.Dyn (val_tag wit, v)
let out_gen wit v = match prj (val_tag wit) v with None -> assert false | Some x -> x

let coerce_to_int (v : Val.t) =
  if has_type v (topwit wit_int) then
    out_gen (topwit wit_int) v
  else raise (CannotCoerceTo "an integer")

(* Should be in some other place, such as genprint *)
let unbox : type a. Val.t -> a Val.typ -> a =
  fun (Val.Dyn (tag, x)) t ->
  match Val.eq tag t with
  | None -> assert false
  | Some Refl -> x

let rec pr_value lev (v : Val.t) : Pp.t =
  let open Pp in
  if has_type v Val.typ_list then
    pr_sequence (fun x -> pr_value lev x) (unbox v Val.typ_list)
  else if has_type v Val.typ_opt then
    pr_opt_no_spc (fun x -> pr_value lev x) (unbox v Val.typ_opt)
  else if has_type v Val.typ_pair then
    let (v1, v2) = unbox v Val.typ_pair in
    str "(" ++ pr_value lev v1 ++ str ", " ++ pr_value lev v2 ++ str ")"
  else
    let Val.Dyn (tag, x) = v in
    let name = Val.repr tag in
    let default = str "<" ++ str name ++ str ">" in
    match ArgT.name name with
    | None -> default
    | Some (ArgT.Any arg) ->
      let wit = ExtraArg arg in
      match val_tag (Topwit wit) with
      | Val.Base t ->
        begin match Val.eq t tag with
          | None -> default
          | Some Refl ->
            let open Genprint in
            match generic_top_print (in_gen (Topwit wit) x) with
            | TopPrinterBasic pr -> pr ()
            | TopPrinterNeedsContext pr ->
              let env = Global.env() in
              pr env (Evd.from_env env)
            | TopPrinterNeedsContextAndLevel { default_ensure_surrounded; printer } ->
              let env = Global.env() in
              printer env (Evd.from_env env) default_ensure_surrounded
        end
      | _ -> default

let error_ltac_variable ?loc id env v s =
   CErrors.user_err ?loc Pp.(
      str "Ltac variable " ++ Names.Id.print id ++
      strbrk " is bound to" ++ spc () ++ pr_value env v ++ spc () ++
      strbrk "which cannot be coerced to " ++ str s ++ str".")
