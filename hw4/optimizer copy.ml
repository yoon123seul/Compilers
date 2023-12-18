let rec rm_skip : T.program -> T.program -> T.program
  = fun program o_program -> 
    match program with 
    | [] -> o_program
    | (label, SKIP) :: (0, code1) :: remain -> rm_skip remain (o_program @ [(label, code1)])  
    | (label, cmd) :: remain -> rm_skip remain (o_program @ [(label, cmd)])

let rec int_replace1 : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (0, COPYC(var1, i)) :: (0, COPY(y, var2)) :: remain -> 
    if var1 == var2 then 
      int_replace1 remain (o_program @ [(0, COPYC(y, i))])  
    else 
      int_replace1 remain (o_program @ [(0, T.COPYC(var1, i))] @ [(0, COPY(y, var2))])
  | (0, COPYC(temp, i)) :: (0, ASSIGNV(x, bop, y, z)) :: remain -> 
    if temp == z then 
      int_replace1 remain (o_program @ [(0, T.ASSIGNC(x, bop, y, i))])  
    else 
      int_replace1 remain (o_program @ [(0, T.COPYC(temp, i))] @ [(0, ASSIGNV(x, bop, y, z))])
    

  | (label, cmd) :: remain -> int_replace1 remain (o_program @ [(label, cmd)])
  



  
let optimize : T.program -> T.program
= fun t -> (*raise (Failure "Not implemented") (* TODO *)*)
  let wo_skip = rm_skip t [] in 
  int_replace1 wo_skip []