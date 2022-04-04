open V1

let test : Register.t = Register.make ~register_number:1 ~n:5
let exp = Expressions.parse "5*5"
