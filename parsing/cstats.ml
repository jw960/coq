(************************************************************************)
(* Coq Language Server Protocol                                         *)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1 / GPL3+      *)
(* Copyright 2022 Inria           -- Dual License LGPL 2.1 / GPL3+      *)
(* Written by: Emilio J. Gallego Arias                                  *)
(************************************************************************)
(* Status: Experimental                                                 *)
(************************************************************************)

module Kind = struct
  type t = UnFreeze | Factorize | Remove | Extend | Compare
end

let stats = Hashtbl.create 1000

let find kind = Hashtbl.find_opt stats kind |> Option.default 0.0

let bump kind time =
  let acc = find kind in
  Hashtbl.replace stats kind (acc +. time)

let time f x =
  let before = Unix.gettimeofday () in
  let res = f x in
  let after = Unix.gettimeofday () in
  res, after -. before

let record ~kind ~f x =
  let res, time = time f x in
  bump kind time;
  res, time

let get ~kind = find kind

let dump () =
  Format.asprintf "freeze: %f | factorize: %f | remove: %f | extend: %f | compare: %f"
    (find Kind.UnFreeze)
    (find Kind.Factorize)
    (find Kind.Remove)
    (find Kind.Extend)
    (find Kind.Compare)

let reset () =
  Hashtbl.remove stats Kind.UnFreeze;
  Hashtbl.remove stats Kind.Factorize;
  Hashtbl.remove stats Kind.Remove;
  Hashtbl.remove stats Kind.Extend;
  Hashtbl.remove stats Kind.Compare;
  ()
