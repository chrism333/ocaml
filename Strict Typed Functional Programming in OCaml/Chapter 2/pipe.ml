
let main =
  let lst = [3;11;55;8;23;88;6] in
  let strlist = (List.map (fun x -> x * 10) lst) |> 
                (List.filter (fun x -> x > 100)) |>
                (List.map string_of_int) |>
                (String.concat ";") in
  print_endline ("(" ^ strlist ^ ")")
