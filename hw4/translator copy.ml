let translate : S.program -> T.program
= fun s -> (*raise (Failure "Not implemented")  TODO *)
  let get_new_label = 
    let count = ref 0 in 
    fun () -> 
      let label = !count in
      count := !count + 1;
      label
  in

  let get_new_var =
    let count = ref 0 in 
    fun () -> 
      let var = "t" ^ string_of_int !count in 
      count := count + 1;
      var
  in

  let lec trans_e : S.exp -> T.var * T.linstr
    = fun e -> 
        match e with
        | NUM n -> let new_var = get_new_var() in
                    ( new_var, COPYC (new_var, n)) 
        | 
                
