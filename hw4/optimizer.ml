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
  | (label0, COPYC(var, i)) :: (0, COPY(y, var0)) :: remain -> 
    if var == var0 then 
      int_replace1 remain (o_program @ [(label0, COPYC(y, i))])  
    else 
      int_replace1 remain (o_program @ [(label0, T.COPYC(var, i))] @ [(0, COPY(y, var0))])
  | (label0, COPYC(temp, i)) :: (0, ASSIGNV(x, bop, y, z)) :: remain -> 
    if temp == z then 
      int_replace1 remain (o_program @ [(label0, T.ASSIGNC(x, bop, y, i))])  
    else 
      int_replace1 remain (o_program @ [(label0, T.COPYC(temp, i))] @ [(0, ASSIGNV(x, bop, y, z))])
  

  | (label, cmd) :: remain -> int_replace1 remain (o_program @ [(label, cmd)])


  


let rec temp_replace1 : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (label, COPY(temp_var1, var1)) :: (0, COPY(y1, var2)) :: remain1 -> 
    if temp_var1 == var2 && temp_var1.[0] = 't' then 
      begin
        (* print_string temp_var1; *)
        (* print_string "\n";  *)
        (* print_string var2; *)
        temp_replace1 remain1 (o_program @ [(label, COPY(y1, var1))])  
      end
    else 
      begin
        (* print_string "temp_var1 is "; *)
        (* print_string temp_var1; *)
        (* print_string "\n";  *)
        (* print_string "var2 is :"; *)
        (* print_string var2; *)
        (* print_string "\n";  *)
        temp_replace1 ( [(0, T.COPY(y1, var2))]@ remain1) (o_program @  [(label, T.COPY(temp_var1, var1))])
      end
  | (label, COPY(temp_var1, var1)) :: (0, ASSIGNV(x1, bop1, y1, z1)) :: remain1 -> 
    if temp_var1 == z1 && temp_var1.[0] = 't' then 
      temp_replace1 remain1 (o_program @ [(label, ASSIGNV(x1, bop1, y1, var1))])  
    else 
      temp_replace1 remain1 (o_program @ [(label, T.COPY(temp_var1, var1))] @ [(0, ASSIGNV(x1, bop1, y1, z1))])
    

  | (label1, cmd1) :: remain1 -> temp_replace1 remain1 (o_program @ [(label1, cmd1)])














let rec temp_replace2 : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (label2, COPY(temp_var2, var3)) :: (0, ASSIGNV(x2, bop2, y2, z2)) :: remain2 -> 
    if temp_var2 == y2 (*&& temp_var.[0] = 't'*) then 
      temp_replace2 remain2 (o_program @ [(label2, T.ASSIGNV(x2, bop2, var3, z2))])  
    else 
      temp_replace2 remain2 (o_program @ [(label2, T.COPY(temp_var2, var3))] @ [(0, ASSIGNV(x2, bop2, y2, z2))])
  
  | (label2, COPY(temp_var2, var3)) :: (0, ASSIGNC(x2, bop2, y2, i2)) :: remain2 -> 
    if temp_var2 == y2 (*&& temp_var.[0] = 't'*) then 
      temp_replace2 remain2 (o_program @ [(label2, T.ASSIGNC(x2, bop2, var3, i2))])  
    else 
      temp_replace2 remain2 (o_program @ [(label2, T.COPY(temp_var2, var3))] @ [(0, ASSIGNC(x2, bop2, y2, i2))])
    
  | (label2, ASSIGNU(temp_var2, op, var3)) :: (0, COPY(x2, y2)) :: remain2 -> 
    if temp_var2 == y2 (*&& temp_var.[0] = 't'*) then 
      temp_replace2 remain2 (o_program @ [(label2, T.ASSIGNU(x2, op, var3))])  
    else 
      temp_replace2 ( [(0, T.COPY(x2, y2))] @ remain2) (o_program @ [(label2, T.ASSIGNU(temp_var2, op, var3))] )

  | (label2, COPY(x2, y2)) :: (0, ASSIGNU(temp_var2, op, var3)) :: remain2 -> 
    if x2 == var3 (*&& temp_var.[0] = 't'*) then 
      temp_replace2 remain2 (o_program @ [(label2, T.ASSIGNU(temp_var2, op, y2))])  
    else 
      temp_replace2 ([(0, T.ASSIGNU(temp_var2, op, var3))] @ remain2) (o_program @ [(label2, T.COPY(x2, y2))] )

  | (label2, cmd2) :: remain -> temp_replace2 remain (o_program @ [(label2, cmd2)])












let rec temp_replace3 : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (label2, ASSIGNV(x2, bop2, y2, z2)) :: (0, COPY(temp_var2, var3)) :: remain2 -> 
    if x2 == var3 (*&& temp_var.[0] = 't'*) then 
      temp_replace3 remain2 (o_program @ [(label2, T.ASSIGNV(temp_var2, bop2, y2, z2))])  
    else 
      temp_replace3 remain2 (o_program @ [(label2, T.ASSIGNV(x2, bop2, y2, z2))] @ [(0, COPY(temp_var2, var3))])
  | (label2, ASSIGNC(x2, bop2, y2, z2)) :: (0, COPY(temp_var2, var3)) :: remain2 -> 
    if x2 == var3 (*&& temp_var.[0] = 't'*) then 
      temp_replace3 remain2 (o_program @ [(label2, T.ASSIGNC(temp_var2, bop2, y2, z2))])  
    else 
      temp_replace3 remain2 (o_program @ [(label2, T.ASSIGNC(x2, bop2, y2, z2))] @ [(0, COPY(temp_var2, var3))])
  | (label2, cmd2) :: remain -> temp_replace3 remain (o_program @ [(label2, cmd2)])
    
    








let rec for_write : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (label, COPY(temp_var, var1)) :: (0, WRITE(x)) :: remain -> 
    if temp_var == x (*&& temp_var.[0] = 't'*) then 
      for_write remain (o_program @ [(label, T.WRITE(var1))])  
    else 
      for_write remain (o_program @ [(label, T.COPY(temp_var, var1))] @ [(0, WRITE(x))])
    

  | (label, cmd) :: remain -> for_write remain (o_program @ [(label, cmd)])



let rec rm_copyc : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (label, COPYC(var1, i1)) :: (0, COPYC(var2, i2)) :: remain -> 
    if var1 = var2  && i1 == i2(*&& temp_var.[0] = 't'*) then 
      rm_copyc remain (o_program @ [(label, COPYC(var1, i1))])  
    else 
      rm_copyc ([(0, T.COPYC(var2, i2))] @ remain) (o_program @ [(label, T.COPYC(var1, i1))] )
    

  | (label, cmd) :: remain -> for_write remain (o_program @ [(label, cmd)])







let rec rm_index : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (label2, COPY(x, y)) :: (0, STORE(arr, var)) :: remain2 -> 
    if x == var (*&& temp_var.[0] = 't'*) then 
      rm_index remain2 (o_program @ [(label2, T.STORE(arr, y))])  
    else 
      rm_index remain2 (o_program @ [(label2, T.COPY(x, y))] @ [(0, STORE(arr, y))])

  | (label2, cmd2) :: remain -> rm_index remain (o_program @ [(label2, cmd2)])
    

let rec rm_init : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (0, COPYC(x, -989112)) :: remain -> rm_init remain (o_program)  
  | (label, cmd) :: remain -> rm_skip remain (o_program @ [(label, cmd)])



let rec rm_last_jmp : T.program -> T.program -> T.program
= fun program o_program -> 
  match program with 
  | [] -> o_program
  | (0, UJUMP(target)) :: (goal, cmd) :: remain -> 
      if target = goal then rm_last_jmp  remain (o_program @ [(goal, cmd)])
      else rm_last_jmp remain (o_program @ [0, T.UJUMP(target)] @ [(goal, cmd)])
  | (label, cmd) :: remain -> rm_last_jmp remain (o_program @ [(label, cmd)])



let optimize : T.program -> T.program
= fun t -> (*raise (Failure "Not implemented") (* TODO *)*)
  let wo_skip = rm_skip t [] in 
  let wo_int = int_replace1 wo_skip [] in 
  let wo_temp = temp_replace1 wo_int [] in 
  let pp = temp_replace1 (int_replace1 wo_temp []) [] in 
  let ppp = temp_replace2 pp [] in
  let wo_write = (for_write ppp []) in 
  let wo_copyc = rm_copyc wo_write [] in 
  let pppp = temp_replace3 wo_copyc [] in 
  (* rm_init (rm_index pppp []) [] *)
  rm_last_jmp (rm_index pppp [])[]



  (* temp_replace1 t [] *)
