

exception NotImplemented

type graph = (vertex * vertex) list
and vertex = int




let rec reach : graph * vertex -> vertex list
= fun (g,v) -> let rec search g ans v= 
                    if List.mem v ans then ans
                    else 
                      let find_edge (start, _) = start = v in
                      let find_node (_, node) = node in
                      List.fold_left (search g) (v::ans) (List.map find_node (List.filter find_edge g))
                in search g [] v
                
              ;;

