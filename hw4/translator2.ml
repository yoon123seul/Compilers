let translate : S.program -> T.program
= fun s -> (*raise (Failure "Not implemented")  TODO *)
  let get_new_label = 
    let count_label = ref 0 in 
    fun () -> 
      let label = !count_label in
      count_label := !count_label + 1;
      label
  in

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
          ( t, [(0, COPYC (t, n))])

        | LV (ID id) -> 
          let t = get_new_var() in
          (t, [(0, COPY (t, id))])

        | LV (ARR (arr, index)) ->
          let t2 = get_new_var() in
          let (t1, code) = trans_e index in
          (t2, code @ [(0, LOAD (t2, (arr, t2)))])

        | ADD (e1, e2) -> 
            let (t1, code1) = trans_e(e1) in
            let (t2, code2) = trans_e(e2) in 
            let t3 = get_new_var() in 
            (t3, code1@code2@[(0,ASSIGNV (t3, ADD, t1, t2))])
            (*ASSIGNV of var * bop * var * var (* x = y bop z *)*)

        | SUB (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in 
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0,ASSIGNV (t3, SUB, t1, t2))])

        | MUL (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in 
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0,ASSIGNV (t3, MUL, t1, t2))])

        | DIV (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in 
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0,ASSIGNV (t3, DIV, t1, t2))])

        | MINUS e -> 
          let (t1, code1) = trans_e(e) in
          let t2 = get_new_var() in 
          (t2, code1@[(0, ASSIGNU (t2, MINUS, t1))]);
          (*ASSIGNU of var * uop * var  (* x = uop y *)*)

        | NOT e -> 
          let (t1, code1) = trans_e(e) in
          let t2 = get_new_var() in 
          (t2, code1@[(0, ASSIGNU (t2, NOT, t1))]);

        | LT (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0, ASSIGNV (t3, LT, t1, t2))]);

        | LE (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0, ASSIGNV (t3, LE, t1, t2))]);
        
        | GT (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0, ASSIGNV (t3, GT, t1, t2))]);

        | GE (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0, ASSIGNV (t3, GE, t1, t2))]);

        | EQ (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0, ASSIGNV (t3, EQ, t1, t2))]);

        | AND (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0, ASSIGNV (t3, AND, t1, t2))]);

        | OR (e1, e2) -> 
          let (t1, code1) = trans_e(e1) in
          let (t2, code2) = trans_e(e2) in
          let t3 = get_new_var() in 
          (t3, code1@code2@[(0, ASSIGNV (t3, OR, t1, t2))])

  in 
  
  let rec trans_s : S.stmt -> T.linstr list 
    = fun s -> match s with
        | ASSIGN (lv, e) ->
          (match lv with 
          | ID id -> 
            let (t1, code1) = trans_e(e) in
            code1@[(0, COPY (id, t1))]
          | ARR (arr, index) -> 
            let (t1, code1) = trans_e (e) in 
            let (i, _) = trans_e index in
            code1 @ [(0, STORE ((arr, i), t1))])

        | IF (e, s1, s2) -> 
          let (t1, code1) = trans_e e in 
          let code_t = trans_s s1 in
          let code_f = trans_s s2 in
          let label_t = get_new_label() in 
          let label_f = get_new_label () in
          let label_x = get_new_label () in 
          code1 @ [(0, CJUMP(t1, label_t))] @ [(0, CJUMPF(t1, label_f))] 
          @ [(label_t, SKIP)] @ code_t @ [(0, UJUMP(label_x))] @ [(label_f, SKIP)]
          @ code_f @ [(0, UJUMP(label_x))] @ [(label_x, SKIP)]

        | WHILE (e, s) -> 
          let (t1, code1) = trans_e e in 
          let code_b = trans_s s in 
          let label_e = get_new_label() in 
          let label_x = get_new_label () in 
          [(label_e, SKIP)] @ code1 @ [(0, CJUMPF(t1, label_x))] @ code_b @
          [(0, UJUMP(label_e))] @ [(label_x), SKIP]

        | DOWHILE (s, e)->
          (trans_s s) @ (trans_s (WHILE(e, s)))

        | READ id ->
          [0, READ(id)]
  
        | PRINT e->
          let (t1, code1) = trnas_e e in 
          code1 @ [0, WRITE t1]

        | BLOCK block -> 
          trans_b block 
  in

(* t, [(0, COPYC (t, n))])
stmt  = ASSIGN of lv * exp       (* lv = exp *)
        | IF of exp * stmt * stmt 
        | WHILE of exp * stmt
        | DOWHILE of stmt * exp
        | READ of id
        | PRINT of exp 
        | BLOCK of block *)

