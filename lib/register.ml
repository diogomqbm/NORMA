type t = (int * int)

let initialize n =
  for i = 0 to n do
    let to_print = Int.to_string i in
    print_endline to_print
  done

