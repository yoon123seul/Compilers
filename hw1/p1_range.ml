exception NotImplemented

let rec range: int -> int -> int list
= fun n m -> if n > m then [] (* TODO *)
else if n = m then [n]
else n :: range (n + 1) m;;

