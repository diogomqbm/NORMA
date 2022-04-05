type value = int * int
type t = { register_number : int; n : int; value : value }

let make ~register_number ~n = { register_number; n; value = (0, n) }
let string_of_value value = Format.asprintf "(%d, %d)" (fst value) (snd value)

let print t operator operation =
  Format.printf "%s: reg_%d_%d_(%s) - %s\n" operation t.register_number t.n
    operator (string_of_value t.value)

let simple_print t operation letter =
  Format.printf "%s: reg_%s - %s\n" operation letter (string_of_value t.value)

let print_result t =
  Format.printf "O resultado eh: %s\n" (string_of_value t.value)

let set_value register value = { register with value }

let is_zero register =
  match register.value with
  | (0,0) -> true
  | _ -> false

let ( ++ ) register =
  let new_value =
    match register.value with
    | 1, 0 -> (0, 0)
    | 1, some_n -> (1, some_n + 1)
    | 0, 0 -> (0, 1)
    | _, some_n -> (0, some_n + 1)
  in
  set_value register new_value

let ( -- ) register =
  let new_value =
    match register.value with
    | 0, 0 -> (1, 1)
    | 0, 1 -> (0, 0)
    | 0, some_n -> (0, some_n - 1)
    | _, some_n -> (1, some_n - 1)
  in
  set_value register new_value
