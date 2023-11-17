exception NotImplemented

type graph = (vertex * vertex) list
and vertex = int




let rec reach : graph * vertex -> vertex list
= fun (g,v) -> match g with 
              | [] -> assert false
              | h::t -> let find_edge (start, _) i_v = start = i_v in
                        let find_node (_, node) = node in
                        let visited = [v] in
                        let v' = [v] in
                        if find_edge h v then (find_node h)::visited else visited 
                        