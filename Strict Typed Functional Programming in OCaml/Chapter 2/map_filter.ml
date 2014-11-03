
let rec filter f = function
  | []   -> []
  | h::t -> if (f h) then h::(filter f t) else (filter f t)

let rec map f = function
  | []   -> []
  | h::t -> (f h)::(map f t)

let strlst list =
  let rec helper = function
    | []    -> ")"
    | h::[] -> (string_of_int h) ^ ")"
    | h::t  -> (string_of_int h) ^ ";" ^ (helper t)
  in
  "(" ^ (helper list)

let main =
  let lst = [3;11;55;8;23;88;6] in
  let newlst = filter (fun x -> x > 100)
                      (map (fun x -> x * 10) lst) in
  print_endline (strlst newlst)
