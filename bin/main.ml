open V1
let test : Register.t = Register.make ~register_number:1 ~n:5

(* Add (Init 5, Init 3) *)
let () = 
  print_endline "Entre com uma expressao(Ex: 5+5, 4*5):";
  let input = Scanf.scanf "%s" (fun x -> x) in
  input |> Expressions.parse |> Expressions.run ~registers_count:0
