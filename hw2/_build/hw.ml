open Regex 

exception Not_implemented

let rec regex2nfa : Regex.t -> Nfa.t 
= fun regex -> 
  match regex with
  | Empty -> 
    let nfa = Nfa.create_new_nfa () in
    (* let inital_state = Nfa.get_initial_state nfa in *)
    let final_state, nfa' = Nfa.create_state nfa in
    (* Nfa.print (Nfa.add_final_state nfa' final_state); *)
    Nfa.add_final_state nfa' final_state

  | Epsilon -> 
    let nfa = Nfa.create_new_nfa () in
    let inital_state = Nfa.get_initial_state nfa in
    let final_state, nfa' = Nfa.create_state nfa in
    let updated_nfa = Nfa.add_final_state nfa' final_state in
    let updated_nfa' = Nfa.add_epsilon_edge updated_nfa (inital_state, final_state) in
    (* Nfa.print (updated_nfa'); *)
    updated_nfa'

  | Alpha a -> 
    let nfa = Nfa.create_new_nfa () in
    let inital_state = Nfa.get_initial_state nfa in
    let final_state, nfa' = Nfa.create_state nfa in
    let updated_nfa = Nfa.add_final_state nfa' final_state in
    let updated_nfa' = Nfa.add_edge updated_nfa (inital_state, a, final_state) in
    (* Nfa.print (updated_nfa'); *)
    updated_nfa'

  | OR (r1, r2) -> 
    let nfa' = Nfa.create_new_nfa () in
    let inital_state = Nfa.get_initial_state nfa' in
    let (final_state, nfa'') = Nfa.create_state nfa' in
    let nfa = Nfa.add_final_state nfa'' final_state in
    let nfa1 = (regex2nfa r1) in
    let nfa2 = (regex2nfa r2) in
    let nfa1_states = Nfa.get_states nfa1 in
    let nfa2_states = Nfa.get_states nfa2 in
    let nfa1_edge = Nfa.get_edges nfa1 in 
    let nfa2_edge = Nfa.get_edges nfa2 in 
    let nfa1_final = BatSet.any (Nfa.get_final_states nfa1) in 
    let nfa2_final = BatSet.any (Nfa.get_final_states nfa2) in 
    let concat_nfa' = Nfa.add_states (Nfa.add_states nfa nfa1_states) nfa2_states in
    let concat_nfa = Nfa.add_edges (Nfa.add_edges concat_nfa' nfa1_edge) nfa2_edge in
    let final_nfa = Nfa.add_epsilon_edge (Nfa.add_epsilon_edge concat_nfa (nfa1_final, final_state)) (nfa2_final, final_state) in
    let result = Nfa.add_epsilon_edge (Nfa.add_epsilon_edge final_nfa (inital_state, Nfa.get_initial_state nfa1)) (inital_state, Nfa.get_initial_state nfa2) in
    (* Nfa.print (result); *)
    result

  | CONCAT (r1, r2) -> 
    let nfa = Nfa.create_new_nfa () in 
    let inital_state = Nfa.get_initial_state nfa in
    let nfa1 = (regex2nfa r1) in
    let nfa2 = (regex2nfa r2) in
    let nfa1_states = Nfa.get_states nfa1 in
    let nfa2_states = Nfa.get_states nfa2 in
    let nfa1_edge = Nfa.get_edges nfa1 in 
    let nfa2_edge = Nfa.get_edges nfa2 in 
    let nfa1_final = BatSet.any (Nfa.get_final_states nfa1) in 
    let nfa2_final = BatSet.any (Nfa.get_final_states nfa2) in
    let concat_nfa' = Nfa.add_states (Nfa.add_states nfa nfa1_states) nfa2_states in
    let concat_nfa = Nfa.add_edges (Nfa.add_edges concat_nfa' nfa1_edge) nfa2_edge in
    let final_nfa = Nfa.add_final_state concat_nfa nfa2_final in
    let result = Nfa.add_epsilon_edge (Nfa.add_epsilon_edge final_nfa (nfa1_final, (Nfa.get_initial_state nfa2))) (inital_state, (Nfa.get_initial_state nfa1)) in
    (* Nfa.print (result); *)
    result

| STAR r -> 
    let nfa' = Nfa.create_new_nfa () in
    let inital_state = Nfa.get_initial_state nfa' in
    let (final_state, nfa'') = Nfa.create_state nfa' in
    let nfa = Nfa.add_final_state nfa'' final_state in
    let nfa1 = (regex2nfa r) in
    let nfa1_states = Nfa.get_states nfa1 in
    let nfa1_edge = Nfa.get_edges nfa1 in 
    let nfa1_init = Nfa.get_initial_state nfa1 in
    let nfa1_final = BatSet.any (Nfa.get_final_states nfa1) in
    let concat_nfa' = Nfa.add_states nfa nfa1_states in
    let concat_nfa = Nfa.add_edges concat_nfa' nfa1_edge in
    let final_nfa = Nfa.add_epsilon_edge (Nfa.add_epsilon_edge (Nfa.add_epsilon_edge (Nfa.add_epsilon_edge concat_nfa (inital_state, nfa1_init)) (inital_state, final_state)) (nfa1_final, nfa1_init)) (nfa1_final, final_state)in
    (* Nfa.print (final_nfa); *)
    final_nfa
        
let rec nfa2dfa : Nfa.t -> Dfa.t
= fun nfa ->  (*아직 파이널 스테이트는 안함*)

    let rec epsilon_closure : Nfa.t -> Nfa.state -> Nfa.states 
    = fun nfa state -> 
      let rec ec_closure visited_states current_state =
        if BatSet.mem current_state visited_states then visited_states
        else
          let reachable_states = Nfa.get_next_state_epsilon nfa current_state in
          let visited_states' = BatSet.add current_state visited_states in
          let helper_fun state curr_state = ec_closure curr_state state in
          BatSet.fold helper_fun reachable_states visited_states'
      in
      ec_closure BatSet.empty state in

    let rec get_next : Nfa.t -> Nfa.states -> Regex.alphabet -> Nfa.states -> Nfa.states
    = fun nfa states alpha results -> 
    if BatSet.is_empty states then results
    else 
      let (curr_state, ramain_states) = BatSet.pop_min states in
      (* print_string "is there problem?"; *)
      let next_states = Nfa.get_next_state nfa curr_state alpha in
      (* print_string "is there problem?"; *)
        let results' = try BatSet.union (epsilon_closure nfa (BatSet.any next_states)) results
                      with _ -> results
          in
          (* 문제 발생!!!!!! *)
      (* print_string "problem fixed"; *)
      get_next nfa ramain_states alpha results' in

    let dfa_init_state = epsilon_closure nfa (Nfa.get_initial_state nfa) in
    let dfa = Dfa.create_new_dfa dfa_init_state in 
    (* Dfa.print dfa ; *)
    (* print_int 123; *)
    
    let done_list = BatSet.singleton dfa_init_state in
    let work_list = BatSet.singleton dfa_init_state in
    (* let work_list = BatSet.empty in *)
    (* print_int 123; *)

    let rec build_dfa : Nfa.t -> Dfa.t -> 'a BatSet.t -> 'a BatSet.t -> Dfa.t 
    = fun nfa dfa done_list work_list -> 
      (* print_int 123123123123 ; *)
      if BatSet.is_empty work_list then dfa 
      else 
        let (curr_work, new_work_list) = BatSet.pop_min work_list in
        (* print_int 123; *)
        (* if curr_work contains final state of Nfa then add curr_work to dfa_final_state *)
        let dfa_add_final nfa dfa dfa_state 
        = let nfa_final = BatSet.any (Nfa.get_final_states nfa) in
        if BatSet.mem nfa_final dfa_state then Dfa.add_final_state dfa dfa_state
        else dfa 
      in
      let dfa''' = dfa_add_final nfa dfa curr_work in 
        
        (* print_string Regex.A ; *)

        let dfa_next_state_a = get_next nfa curr_work Regex.A BatSet.empty in
        (* Nfa.t -> Nfa.states -> Regex.alphabet -> Nfa.states -> Nfa.states *)
        (* 여기서 문제가 생긴 듯 함 *)
        let dfa' = Dfa.add_state dfa''' dfa_next_state_a in
        let dfa'' = Dfa.add_edge dfa' (curr_work, Regex.A, dfa_next_state_a) in
        
        (* Dfa.print dfa'' ; *)
        let new_work_list = if BatSet.mem dfa_next_state_a done_list then new_work_list else BatSet.add dfa_next_state_a new_work_list in
        let done_list = BatSet.add dfa_next_state_a done_list in

        let dfa_next_state_b = get_next nfa curr_work Regex.B BatSet.empty in
        let dfa''' = Dfa.add_state dfa'' dfa_next_state_b in
        (* Dfa.print dfa''' ; *)
        let dfa'''' = Dfa.add_edge dfa''' (curr_work, Regex.B, dfa_next_state_b) in
        let new_work_list = if BatSet.mem dfa_next_state_b done_list then new_work_list else BatSet.add dfa_next_state_b new_work_list in
        let done_list = BatSet.add dfa_next_state_b done_list in
        build_dfa nfa dfa'''' done_list new_work_list in

        
    let final_dfa = build_dfa nfa dfa done_list work_list in
    (* Dfa.print final_dfa ; *)
    final_dfa
    

(* Do not modify this function *)
let regex2dfa : Regex.t -> Dfa.t
= fun regex ->
  let nfa = regex2nfa regex in
  let dfa = nfa2dfa nfa in
  (* Dfa.print dfa ; *)
  (* 아니 과제 뭐가 이렇게 어려워 *)
  dfa

let rec run_dfa : Dfa.t -> alphabet list -> bool
= fun dfa str -> 
  let start_state = Dfa.get_initial_state dfa in
  let rec do_search : Dfa.t -> alphabet list -> Dfa.state -> Dfa.state
  = fun dfa str_list curr_state ->
    match str_list with
    [] -> curr_state
    | h::t -> 
      let next_state = Dfa.get_next_state dfa curr_state h in
      do_search dfa t next_state
    in 
    let final_state = do_search dfa str start_state in
    if Dfa.is_final_state dfa final_state then true else false