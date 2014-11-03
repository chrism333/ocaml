open Core.Std

let rec mklist = function
  | [] -> []
  | h::t -> if h > 10 then (h * 10) :: (mklist t) else (mklist t)

let strlst list =
  let rec helper = function
    | []    -> ")"
    | h::[] -> (string_of_int h) ^ ")"
    | h::t  -> (string_of_int h) ^ ";" ^ (helper t)
  in
  "(" ^ (helper list)

let main =
  print_endline (strlst (mklist [3;11;55;8;23;88;6]))
