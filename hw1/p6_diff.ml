exception NotImplemented

type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> match exp with  
                  | Const a -> Const 0
                  | Var a -> if a = x then Const 1 else Const 0
                  | Power (v , n) -> if v = x then Times [Const n; Power(v, n - 1)]
                                     else Const 0 
                 (* | Times list -> 
                    let rec pro_diff = fun list (h::ori)  ans -> match list with
                                                  | [] -> ans
                                                  | h::t -> 
                                                    let h_diff = diff(h, x) in
                                                    pro_diff t (ori @ [h]) (Sum[Times ([h_diff] @ ori); ans]) in
                                                    pro_diff list list (Const 1) *)
                  | Times list -> 
                    let rec pro_diff = fun list  list2  ans -> match list with
                                                  | [] -> ans
                                                  | h::t -> 
                                                    let (h2,ori) = match list2 with [] -> assert false | hd::tl -> (hd,tl) in
                                                    let h_diff = diff(h, x) in
                                                    pro_diff t (ori @ [h2]) (Sum[Times ([h_diff] @ ori @ [ans])]) in
                                                    pro_diff list list (Const 1) 
                                                    
                  | Sum list -> 
                    let rec sum_diff = fun list r -> match list with
                                                  | [] -> r
                                                  | h::t -> 
                                                    let h_diff = diff(h, x) in
                                                    sum_diff t (Sum ([h_diff; r])) in
                                                    sum_diff list (Const 0)
                                                  ;;

                                                  