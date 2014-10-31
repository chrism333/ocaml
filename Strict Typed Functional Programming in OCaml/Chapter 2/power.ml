open Core.Std

printf "Does it work? %B\n" (test_powers 2)

let rec power_naive x = function
  | 0 -> 1
  | n when (n < 0) -> assert false (* negative power is not supported *)
  | n -> x * (power_naive x (n-1))

let rec power_fast x = function
  | 0 -> 1
  | 1 -> x
  | n when n > 0 -> 
    let tmp = power_fast x (n/2) in            (* reduce problem recursively*)
    tmp * tmp * (if n mod 2 = 0 then 1 else x) (* and reuse the result*)
  | _ -> assert false (* index out of range *)

(* compare results for x^0 to x^1000 *)
let test_powers x = 
  let rec c = function
    | 1001 -> true
    | n   -> ((power_fast x n) = (power_naive x n)) && (c (n+1))
  in
  c 0
