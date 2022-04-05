type init = Init of int
type t = Sub of init * init | Add of init * init | Mult of init * init

exception Operator_Not_Supported

let get_operator expression =
  let contains_add = String.contains expression '+' in
  let contains_sub = String.contains expression '-' in
  let contains_mult = String.contains expression '*' in
  let contains_div = String.contains expression '/' in
  match (contains_add, contains_sub, contains_mult, contains_div) with
  | true, _, _, _ -> '+'
  | _, true, _, _ -> '-'
  | _, _, true, _ -> '*'
  | _, _, _, true -> '/'
  | false, false, false, false -> raise Operator_Not_Supported

let parse expression =
  let operator = get_operator expression in
  let elem_list = String.split_on_char operator expression in
  let fst = List.hd elem_list in
  let snd = List.nth elem_list 1 in
  match operator with
  | '+' -> Add (Init (int_of_string fst), Init (int_of_string snd))
  | '-' -> Sub (Init (int_of_string fst), Init (int_of_string snd))
  | '*' -> Mult (Init (int_of_string fst), Init (int_of_string snd))
  | _ -> raise Operator_Not_Supported

let run exp ~registers_count =
  match exp with
  | Add (Init n1, Init n2) ->
      let register1 =
        Register.make ~register_number:(registers_count + 1) ~n:n1
        |> Operations.initialize
      in
      let register2 =
        Register.make ~register_number:(registers_count + 2) ~n:n2
        |> Operations.initialize
      in
      Operations.sum register1 register2
  | Sub (Init n1, Init n2) ->
      let register1 =
        Register.make ~register_number:(registers_count + 1) ~n:n1
        |> Operations.initialize
      in
      let register2 =
        Register.make ~register_number:(registers_count + 2) ~n:n2
        |> Operations.initialize
      in
      Operations.sub register1 register2
  | Mult (Init n1, Init n2) ->
      let register1 =
        Register.make ~register_number:(registers_count + 1) ~n:n1
        |> Operations.initialize
      in
      let register2 =
        Register.make ~register_number:(registers_count + 2) ~n:n2
        |> Operations.initialize
      in
      Operations.mult register1 register2
