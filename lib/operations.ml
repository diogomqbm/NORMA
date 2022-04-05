open Register

let initialize register =
  let new_register = ref register in
  for i = 1 to register.n do
    new_register := set_value new_register.contents (0, i);
    print new_register.contents "-" "Inicializa"
  done;
  new_register.contents

let sum reg1 reg2 =
  let new_reg1 = ref reg1 in
  let new_reg2 = ref reg2 in
  while not (is_zero new_reg2.contents) do
    new_reg1 := ( ++ ) new_reg1.contents;
    new_reg2 := ( -- ) new_reg2.contents;
    print new_reg1.contents "+" "Soma";
    print new_reg2.contents "+" "Soma"
  done;
  print_result new_reg1.contents;;

let sub reg1 reg2 =
  let new_reg1 = ref reg1 in
  let new_reg2 = ref reg2 in
  while not (is_zero new_reg2.contents) do
    new_reg1 := ( -- ) new_reg1.contents;
    new_reg2 := ( -- ) new_reg2.contents;
    print new_reg1.contents "-" "Subtrai";
    print new_reg2.contents "-" "Subtrai"
  done;
  print_result new_reg1.contents;;

let sum_using reg1 reg2 letter operator =
  let reg_temp = ref (make ~register_number:0 ~n:0) in
  let operation = Format.sprintf "AB_usando_%s" letter in
  while not (is_zero reg2.contents) do
    reg1 := ( ++ ) reg1.contents;
    reg_temp := ( ++ ) reg_temp.contents;
    reg2 := ( -- ) reg2.contents;
    print reg1.contents operator operation;
    print reg2.contents operator operation;
    simple_print reg_temp.contents operation letter;
  done;
  while not (is_zero reg_temp.contents) do
    reg2 := ( ++ ) reg2.contents;
    reg_temp := ( -- ) reg_temp.contents;
    print reg2.contents operator operation;
    simple_print reg_temp.contents operation letter;
  done

let mult reg1 reg2 =
  let reg_temp = ref (make ~register_number:0 ~n:0) in
  let new_reg1 = ref reg1 in
  let new_reg2 = ref reg2 in
  while not (is_zero new_reg1.contents) do
    reg_temp := ( ++ ) reg_temp.contents;
    new_reg1 := ( -- ) new_reg1.contents;
    print new_reg1.contents "*" "Multiplica";
    simple_print reg_temp.contents "Multiplica" "C"
  done;
  while not (is_zero reg_temp.contents) do
    sum_using new_reg1 new_reg2 "D" "*";
    reg_temp := ( -- ) reg_temp.contents;
    print new_reg1.contents "*" "Multiplica";
    simple_print reg_temp.contents "Multiplica" "C"
  done;
  print_result new_reg1.contents
