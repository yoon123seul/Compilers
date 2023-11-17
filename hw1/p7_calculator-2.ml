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
            | SIGMA (start, end', body) -> if calculator start > calculator end' then raise Sigma_bound_error else 
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



let tcs =
  [
    (*(try calculator (INT 10) = 10 with _ -> false); 
    (try calculator (ADD (INT 1, INT 3)) = 4 with _ -> false);
    (try calculator (ADD (ADD (INT 1, INT 2), ADD (INT 3, INT 4))) = 10 with _ -> false);
    (try calculator (SUB (ADD (INT 1, INT 3), ADD (INT 2, INT 4))) = -2 with _ -> false);

    (try calculator
      (DIV
         ( SUB (INT 4, ADD (INT 1, INT 2)),
           ADD (SUB (INT 4, INT 3), DIV (INT 4, INT 2)) ))
    = 0
     with _ -> false);

    (try calculator
      (MUL (MUL (INT 3, INT 10), ADD (SUB (INT 3, INT 1), DIV (INT 10, INT 2))))
    = 210
     with _ -> false);

    (try calculator
      (MUL
         ( SIGMA (INT 1, INT 10, SUB (MUL (X, X), INT 1)),
           SIGMA (INT 1, INT 10, SUB (MUL (X, X), INT 1)) ))
    = 140625
     with _ -> false);

    (try calculator
      (DIV
         ( ADD (INT 4, ADD (INT 4, INT 4)),
           MUL (INT 1, MUL (INT 2, SUB (INT 4, INT 2))) ))
    = 3
     with _ -> false);

    (try calculator (SIGMA (INT 0, INT 10, X)) = 55 with _ -> false);
    (try calculator (SIGMA (ADD (INT 1, INT 0), SUB (INT 11, INT 1), X)) = 55 with _ -> false); *)
    (try calculator (SIGMA (INT 1, INT 1, SIGMA (X, X, INT 1))) = 1 with _ -> false);
    (try calculator (SIGMA (INT 1, INT 2, SIGMA (INT 1, ADD (X, INT 2), INT 1))) = 7 with _ -> false);

    (try calculator (SIGMA (INT 1, INT 2, SIGMA (SUB (X, INT 1), ADD (X, INT 1), INT 3)))
    = 18
     with _ -> false);

    (try calculator (SIGMA (INT 1, INT 2, SIGMA (SUB (X, INT 1), ADD (X, INT 1), X))) = 9
     with _ -> false);

    (try calculator
      (SIGMA (DIV (INT 10, INT 2), MUL (ADD (INT 3, INT 10), INT 1), SUB (X, INT 1)))
    = 72
     with _ -> false);

    (*(try calculator
      (SIGMA (SIGMA (INT 1, INT 3, X), SIGMA (INT 3, INT 6, X), MUL (X, MUL (X, X))))
    = 29016
     with _ -> false);

    (try calculator
      (SIGMA
         ( SIGMA (INT 1, SIGMA (INT 1, INT 2, X), X),
           SIGMA (INT 3, INT 5, X),
           SIGMA (INT 6, INT 7, X) ))
    = 91
     with _ -> false);

    (try calculator (ADD (SIGMA (INT 1, INT 10, X), SIGMA (INT 1, INT 5, X))) = 70 with _ -> false);*)

    (try calculator
      (MUL (SIGMA (DIV (INT 10, INT 3), MUL (INT 10, INT 3), INT 3), INT 10))
    = 840
     with _ -> false);

    (try calculator
      (SIGMA
         ( INT 1,
           INT 5,
           SIGMA
             ( X,
               ADD (X, INT 5),
               SIGMA
                 ( X,
                   ADD (X, INT 5),
                   SIGMA (ADD (X, INT 5), ADD (X, INT 5), DIV (X, INT 2)) ) ) ))
    = 1125
     with _ -> false);
  ]

let _ = List.iter (fun t -> t |> string_of_bool |> print_endline) tcs
