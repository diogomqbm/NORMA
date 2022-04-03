type value = int * int
type t = { register_number : int; n : int; value : value }

let make ~register_number ~n = { register_number; n; value = (0, n) }
let string_of_value value = Format.asprintf "(%d, %d)" (fst value) (snd value)

let print t operator =
  Format.printf "reg_%d_%d_(%s) - %s\n" t.register_number t.n operator
    (string_of_value t.value)

let set_value register value = { register with value }
