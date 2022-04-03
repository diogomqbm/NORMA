let initialize (register : Register.t) =
  let new_register = ref register in
  for i = 0 to register.n do
    new_register := Register.set_value new_register.contents (0, i);
    Register.print new_register.contents "-"
  done