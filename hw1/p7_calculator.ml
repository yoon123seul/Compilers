exception NotImplemented
exception Diveded_by_zreo
exception Sigma_bound_error
exception No_value




type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


  let rec calculator : exp -> int
= fun exp -> match exp with
            | X -> raise No_value
            | INT n -> n
            | ADD (e1, e2) -> calculator e1 + calculator e2
            | SUB (e1, e2) -> calculator e1 - calculator e2
            | MUL (e1, e2) -> calculator e1 * calculator e2
            | DIV (e1, e2) ->
                if calculator e2 = 0 then raise Diveded_by_zreo
                else calculator e1 / calculator e2 
            | SIGMA (start, end', body) -> if start > end' then raise Sigma_bound_error else 
                    let rec eval exp number = match exp with
                                | X -> number
                                | INT a -> a
                                | ADD (e1, e2) -> eval (e1) number + eval e2 number
                                | SUB (e1, e2) -> eval (e1) number - eval e2 number
                                | MUL (e1, e2) -> eval (e1) number * eval e2 number
                                | DIV (e1, e2) -> 
                                    if eval e2 number = 0 then raise Diveded_by_zreo
                                    else eval (e1) number / eval e2 number
                                | SIGMA (e1, e2, e3) -> calculator (exp) 
              in 
              let rec sigma_loop i ans = 
                if i > calculator end' then ans
                else sigma_loop (i+1) (ans + eval body (calculator (INT i)))
              in sigma_loop (calculator start) 0
            ;;

