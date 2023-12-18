let translate : S.program -> T.program
= fun s_program -> (*raise (Failure "Not implemented")  TODO *)
  let get_new_label = 
    let count_label = ref 0 in 
    fun () -> 
      let label = !count_label in
      count_label := !count_label + 1;
      label
  in
  let defalut_label = get_new_label() in

  let get_new_var =
    let count_var = ref 0 in 
    fun () -> 
      let var = "t" ^ string_of_int !count_var in 
      count_var := !count_var + 1;
      var
  in

  let rec trans_e : S.exp -> T.var * T.linstr list
    = fun e -> match e with
        | NUM n -> 
          let t = get_new_var() in 
          ( t, [(defalut_label, COPYC (t, n))])

        | LV (ID id) -> 
          let t = get_new_var() in
          (t, [(defalut_label, COPY (t, id))])

        | LV (ARR (arr, index)) ->
          let t2 = get_new_var() in
          let (t1, code) = trans_e index in
          (t2, code @ [(defalut_label, LOAD (t2, (arr, t1)))]) (*이 부분도 문제 있음*)

        | ADD (e1, e2) -> 
            let (t1, code1) = trans_e(e1) in
            let (t2, code2) = trans_e(e2) in 
            let t3 = get_new_var() in 
            (t3, code1@code2@[(defalut_label,ASSIGNV (t3, ADD, t1, t2))])
            (*ASSIGNV of var * bop * var * var (* x = y bop z *)*)

        | SUB (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in 
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label,ASSIGNV (t3, SUB, t1, t2))])

        | MUL (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in 
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label,ASSIGNV (t3, MUL, t1, t2))])

        | DIV (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in 
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label,ASSIGNV (t3, DIV, t1, t2))])

        | MINUS e -> 
          let (t1, code1) = trans_e(e) in
          let t2 = get_new_var() in 
          (t2, code1@[(defalut_label, ASSIGNU (t2, MINUS, t1))]);
          (*ASSIGNU of var * uop * var  (* x = uop y *)*)

        | NOT e -> 
          let (t1, code1) = trans_e(e) in
          let t2 = get_new_var() in 
          (t2, code1@[(defalut_label, ASSIGNU (t2, NOT, t1))]);

        | LT (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label, ASSIGNV (t3, LT, t1, t2))]);

        | LE (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label, ASSIGNV (t3, LE, t1, t2))]);
        
        | GT (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label, ASSIGNV (t3, GT, t1, t2))]);

        | GE (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label, ASSIGNV (t3, GE, t1, t2))]);

        | EQ (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label, ASSIGNV (t3, EQ, t1, t2))]);

        | AND (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label, ASSIGNV (t3, AND, t1, t2))]);

        | OR (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(defalut_label, ASSIGNV (t3, OR, t1, t2))])

  in 

    
  let rec trans_d : S.decl -> T.linstr list 
    = fun d -> match d with 
        | (TINT, id) -> 
          [(defalut_label, COPYC(id, 0))]

        | (TARR n, id) -> 
          [(defalut_label, ALLOC(id, n))]
  in
  
  let rec trans_s : S.stmt -> T.linstr list 
    = fun s -> match s with
        | ASSIGN (lv, e) ->
          (match lv with 
          | ID id -> 
            let (t1, code1) = trans_e(e) in
            code1@[(defalut_label, COPY (id, t1))]
          | ARR (arr, index) -> 
            let (i, code1) = trans_e index in
            let (t1, code2) = trans_e (e) in  (* 이 부분 문제 있음 !!!!!!!!!*)
            code1 @ code2 @ [(defalut_label, STORE ((arr, i), t1))])

        | IF (e, s1, s2) -> 
          let (t1, code1) = trans_e e in 
          let code_t = trans_s s1 in
          let code_f = trans_s s2 in
          let label_t = get_new_label() in 
          let label_f = get_new_label () in
          let label_x = get_new_label () in 
          code1 @ [(defalut_label, T.CJUMP(t1, label_t))] @ [(defalut_label, T.UJUMP(label_f))] 
          @ [(label_t, T.SKIP)] @ code_t @ [(defalut_label, T.UJUMP(label_x))] @ [(label_f, T.SKIP)]
          @ code_f @ [(defalut_label, T.UJUMP(label_x))] @ [(label_x, T.SKIP)]

        | WHILE (e, s) -> 
          let (t1, code1) = trans_e e in 
          let code_b = trans_s s in 
          let label_e = get_new_label() in 
          let label_x = get_new_label () in 
          [(label_e, T.SKIP)] @ code1 @ [(defalut_label, T.CJUMPF(t1, label_x))] @ code_b @
          [(defalut_label, T.UJUMP(label_e))] @ [(label_x), SKIP]

        | DOWHILE (s, e)->
          (trans_s s) @ (trans_s (WHILE(e, s)))

        | READ id ->
          [defalut_label, READ(id)]
  
        | PRINT e->
          let (t1, code1) = trans_e e in 
          code1 @ [defalut_label, WRITE t1]

        | BLOCK (decls, stmts) -> 
            let code_decl = List.flatten (List.map trans_d decls) in
            let code_stmt = List.flatten (List.map trans_s stmts) in
            code_decl @ code_stmt
  in

  let rec trans_b : S.block -> T.linstr list
    = fun (decls, stmts) -> 
      let code_decl = List.flatten (List.map trans_d decls) in
      let code_stmt = List.flatten (List.map trans_s stmts) in
      code_decl @ code_stmt
  in

  trans_b s_program @ [0,HALT]


(* t, [(0, COPYC (t, n))])
stmt  = ASSIGN of lv * exp       (* lv = exp *)
        | IF of exp * stmt * stmt 
        | WHILE of exp * stmt
        | DOWHILE of stmt * exp
        | READ of id
        | PRINT of exp 
        | BLOCK of block *)

