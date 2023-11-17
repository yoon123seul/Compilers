exception NotImplemented

type formula =
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp


  let rec cal : exp -> int
  = fun e -> match e with
             | Num a -> a
             | Plus (a, b) -> cal(a) + cal(b)
             | Minus (a,b) -> cal(a) - cal(b)


let rec eval : formula -> bool
= fun f -> match f with
           | True -> true
           | False -> false
           | Not f-> if ((eval f) = true) then false else true
           | OrElse (f1, f2) -> if eval(f1) = false && eval(f2) = false then false else true
           | AndAlso (f1, f2) -> if eval(f1) = true && eval(f2) = true then true else false
           | Imply (f1, f2) -> if eval(f1) = false then true else if eval(f2) = true then true else false
           | Equal (e1, e2) -> if cal e1 = cal e2 then true else false;;

           