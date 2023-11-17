open Regex

let regex2nfa : Regex.t -> Nfa.t =
  let rec regex2nfa nfa regex =
    match regex with
    | Regex.Empty ->
      (* Create an NFA with one start state and no transitions or final states *)
      Nfa.create_new_nfa ()
    | Regex.Epsilon ->
      (* Create an NFA with one start state, an epsilon transition, and the same state as a final state *)
      let nfa = Nfa.create_new_nfa () in
      let init = Nfa.get_initial_state nfa in
      Nfa.add_epsilon_edge nfa (init, init)
    | Regex.Alpha a ->
      let nfa = Nfa.create_new_nfa () in
      (* Create an NFA with two states, a transition on alphabet 'a', and the second state as a final state *)
      let init = Nfa.get_initial_state nfa in
      let final, nfa = Nfa.create_state nfa in
      let final_state = Nfa.get_initial_state nfa in
      Nfa.add_edge nfa (init, a, final_state);
      Nfa.add_final_state nfa final;
      nfa
    | Regex.OR (r1, r2) ->
      (* Create NFAs for both subexpressions and merge them with epsilon transitions to a new start state *)
      let nfa1 = regex2nfa nfa r1 in
      let nfa2 = regex2nfa nfa r2 in
      let start = Nfa.create_new_nfa () in
      let init1 = Nfa.get_initial_state nfa1 in
      let init2 = Nfa.get_initial_state nfa2 in
      Nfa.add_epsilon_edge start (init1, init2);
      Nfa.add_states start (Nfa.get_states nfa1);
      Nfa.add_states start (Nfa.get_states nfa2)
    | Regex.CONCAT (r1, r2) ->
      (* Create NFAs for both subexpressions and concatenate them by making the final state of the first NFA lead to the start state of the second NFA *)
      let nfa1 = regex2nfa nfa r1 in
      let nfa2 = regex2nfa nfa r2 in
      let final1 = Nfa.get_final_states nfa1 in
      let init2 = Nfa.get_initial_state nfa2 in
      Nfa.add_epsilon_edge nfa1 (final1, init2);
      Nfa.add_states nfa1 (Nfa.get_states nfa2)
    | Regex.STAR r ->
      (* Create an NFA for the subexpression and add epsilon transitions for repetition *)
      let nfa1 = regex2nfa nfa r in
      let init1 = Nfa.get_initial_state nfa1 in
      let final1 = Nfa.get_final_states nfa1 in
      let final, nfa1 = Nfa.create_state nfa1 in
      Nfa.add_epsilon_edge nfa1 (final, init1);
      Nfa.add_states nfa1 (Nfa.get_states nfa1);
    in
    regex2nfa (Nfa.create_new_nfa ()) regex
  

let nfa2dfa : Nfa.t -> Dfa.t =
    fun nfa ->
            let module NfaSet = BatSet.Make (struct
              type t = Nfa.state
        
              let compare = compare
            end) in
            let module DfaSet = BatSet.Make (struct
              type t = NfaSet.t
        
              let compare = compare
            end) in
            let initial_state = Nfa.get_initial_state nfa in
            let epsilon_closure s =
              let rec eclosure visited states =
                let next_states =
                  NfaSet.fold
                    (fun state acc ->
                      let epsilon_neighbors = Nfa.get_next_state_epsilon nfa state in
                      NfaSet.union epsilon_neighbors acc
                    )
                    states NfaSet.empty
                in
                let new_states = NfaSet.union states next_states in
                if NfaSet.is_empty next_states || NfaSet.subset new_states visited then
                  new_states
                else eclosure (NfaSet.union visited new_states) new_states
              in
              eclosure NfaSet.empty s
            in
            let dfa_states = ref DfaSet.empty in
            let dfa_delta = ref BatMap.empty in
            let dfa_final = ref DfaSet.empty in
            let worklist = ref [epsilon_closure (NfaSet.singleton initial_state)] in
            while !worklist <> [] do
              let current_nfa_states = List.hd !worklist in
              worklist := List.tl !worklist;
              if not (DfaSet.mem current_nfa_states !dfa_states) then (
                dfa_states := DfaSet.add current_nfa_states !dfa_states;
                if Nfa.is_final_state nfa current_nfa_states then
                  dfa_final := DfaSet.add current_nfa_states !dfa_final;
                let transitions = ref BatMap.empty in
                NfaSet.iter
                  (fun nfa_state ->
                    for a = A to B do
                      let next_nfa_states = Nfa.get_next_state nfa nfa_state a in
                      let next_nfa_states_eclosure = epsilon_closure next_nfa_states in
                      transitions :=
                        BatMap.add (current_nfa_states, a)
                          next_nfa_states_eclosure !transitions
                    done
                  )
                  current_nfa_states;
                dfa_delta := BatMap.merge
                  (fun key value1 value2 ->
                    match (value1, value2) with
                    | Some _, Some _ -> value1
                    | Some _, None -> value1
                    | None, Some _ -> value2
                    | None, None -> None
                  )
                  !dfa_delta !transitions;
                DfaSet.iter
                  (fun state ->
                    if not (DfaSet.mem state !dfa_states) then
                      worklist := state :: !worklist
                  )
                  !transitions
              )
            done;
            let dfa_init = epsilon_closure (NfaSet.singleton initial_state) in
            { Dfa.states = !dfa_states; Dfa.delta = !dfa_delta; Dfa.init = dfa_init; Dfa.final = !dfa_final }
        ;
        
        
        
        (* Add the implementations of nfa2dfa and run_dfa to your code *)
        
        

(* Do not modify this function *)
let (regex2dfa) : Regex.t -> Dfa.t
= fun regex ->
  let nfa = regex2nfa regex in
  let dfa = nfa2dfa nfa in
  dfa
;
let run_dfa : Dfa.t -> alphabet list -> bool
= fun dfa input ->
  let rec process_input current_state remaining_input =
    match remaining_input with
    | [] -> Dfa.is_final_state dfa current_state
    | symbol :: rest ->
      let next_state = Dfa.get_next_state dfa current_state symbol in
      process_input next_state rest
  in
  process_input (Dfa.get_initial_state dfa) input