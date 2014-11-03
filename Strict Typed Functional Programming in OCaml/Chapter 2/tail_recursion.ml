open Core

let strlst list =
  let rec helper = function
    | []    -> ")"
    | h::[] -> (string_of_int h) ^ ")"
    | h::t  -> (string_of_int h) ^ ";" ^ (helper t)
  in
  "(" ^ (helper list)

(* sum up all list elements *)
(* can process up to ~250000 elements *)
let rec sum lst =
  match lst with
    | []    -> 0
    | x::xs -> x+(sum xs)

(* sum up all list elements tail-recursive *)
(* this function is optimized and can process an infinite amount of elements *)
let rec tsum lst acc =
  match lst with
    | []    -> acc
    | x::xs -> tsum xs (x+acc)

(* generate a list tail-recursive *)
let gen_list f n = 
  let () = assert (n >= 0) (* index out of range *) in
  let rec helper n list =
    match n with
      | 0 -> list
      | n -> (helper (n-1) ((f n) :: list))
  in
  helper n []

let rec map f = function
  | []   -> []
  | h::t -> (f h)::(map f t)

let tmap f lst =
  let rec helper lst res = 
    match lst with
      | []   -> res
      | h::t -> helper t ((f h)::res)
  in
  helper lst []

let main =
  let ()  = print_endline "generate inkrementing list with 250000 elements startting at 1" in
  let lst = gen_list (fun x -> x) 250000 in
  let ()  = Std.printf "sum using sum:  %d\n" (sum lst) in
  let ()  = Std.printf "sum using tsum: %d\n" (tsum lst 0) in
  Std.printf "sum using List.left_fold: %d\n" (List.fold_left (fun x y -> x + y) 0 lst)