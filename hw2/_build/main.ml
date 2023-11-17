open Regex
open Hw

let testcases : (Regex.t * alphabet list) list = 
  [ 
    (* (CONCAT (Alpha A, (CONCAT (Alpha B, STAR (OR (Alpha A, Alpha B))))), [A]); *)
    (* (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;B]); *)
    (* (CONCAT (STAR (Alpha A), Alpha B), [B]); *)
    (* (STAR (Alpha A), [A]); *)
    (* (CONCAT (Alpha A, Alpha B), [B]); *)
    (OR (Alpha A, Empty), []);
    (STAR (CONCAT (Epsilon, Alpha A)), [A;A;A]);
    (OR (Empty, Alpha A), [A]);
    (OR (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), [A;A]);
    (OR (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), [A;A;B;B]);
    (OR (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), []);
    (OR (Alpha A, Empty), [A]);
    (Empty, []);
    (Epsilon, []);
    (Alpha A, [A]);
    (Alpha A, [B]);
    (OR (Alpha A, Alpha B), [B]);
    (CONCAT (STAR (Alpha A), Alpha B), [B]);
    (CONCAT (STAR (Alpha A), Alpha B), [A;B]);
    (CONCAT (STAR (Alpha A), Alpha B), [A;A;B]);
    (CONCAT (STAR (Alpha A), Alpha B), [A;B;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B;B;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;A;B;B;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;B;B;B]);
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;A;B;B;B;A;A;A])
  ]

let match_regex : Regex.t -> alphabet list -> bool
= fun regex input -> Hw.run_dfa (Hw.regex2dfa regex) input

(* run testcases *)
let _ =
  List.iter (fun (regex, str) ->
    prerr_endline (string_of_bool (match_regex regex str))
  ) testcases
