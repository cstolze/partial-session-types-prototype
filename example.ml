open Session
open Interface

module SimpleExamples = struct
  (* These two examples give examples of the syntax *)
  let syntaxexample1 = parse "((0 + omega) & end) & close"
  let syntaxexample2 = parse "p -> q,r : %inj1; p -> q : <q -> p : <end>; end>; ?p : <0>"

  (* parse composed with session_to_string is the identity *)
  let () = assert (parse (session_to_string syntaxexample2) = syntaxexample2)

  (* The first way to print session is preferred because it manages indentation correctly (I guess) *)
  let printexample1 () = Format.printf "%a\n" fprintf_session syntaxexample2
  let printexample2 () = print_endline (session_to_string syntaxexample2)
end

module Ex1 = struct (* Example (section 6) *)
  let p_type = parse "p -> q : %inj1; p -> r : <end>; close +
                      p -> q : %inj2; close"
  let q_type = parse "p -> q : %inj1; q -> r : %inj1; close &
                      p -> q : %inj2; q -> r : %inj2; close"
  let r_type = parse "q -> r : %inj1; p -> r : <close>; close &
                      q -> r : %inj2; close"

  let pq_type = merge ["p"] ["q"] p_type q_type
  let pr_type = merge ["p"] ["r"] p_type r_type
  let qr_type = merge ["q"] ["r"] q_type r_type

  let pqr_type = merge ["p";"q"] ["r"] pq_type r_type

  let run () = Format.printf "Section 6:@.- type of p:@ @[<2>%a@]@.- type of q:@ @[<2>%a@]@.- type of r:@ @[<2>%a@]@.- type of p|q:@ @[<2>%a@]@.- type of p|r:@ @[<2>%a@]@.- type of q|r:@ @[<2>%a@]@.- type of p|q|r:@ @[<2>%a@]@.@." fprintf_session p_type fprintf_session q_type fprintf_session r_type fprintf_session pq_type fprintf_session pr_type fprintf_session qr_type fprintf_session pqr_type
end

module Ex2 = struct (* Example (section A.1) *)
  let p_type = parse "p -> q,r : <end>; p -> q : <end>; close"
  let q_type = parse "p -> q : <close>; p -> q : <close>; close"
  let r_type = parse "p -> r : <close>; close"

  let pq_type = merge ["p"] ["q"] p_type q_type
  let pr_type = merge ["p"] ["r"] p_type r_type
  let qr_type = merge ["q"] ["r"] q_type r_type

  let pqr_type = merge ["p"] ["q";"r"] p_type qr_type

  let run () = Format.printf "Section A.1:@.- type of p:@ @[<2>%a@]@.- type of q:@ @[<2>%a@]@.- type of r:@ @[<2>%a@]@.- type of p|q:@ @[<2>%a@]@.- type of p|r:@ @[<2>%a@]@.- type of q|r:@ @[<2>%a@]@.- type of p|q|r:@ @[<2>%a@]@.@." fprintf_session p_type fprintf_session q_type fprintf_session r_type fprintf_session pq_type fprintf_session pr_type fprintf_session qr_type fprintf_session pqr_type
end

module Ex3 = struct (* Example (section A.2) *)
  let p_type = parse "?p : <0>"
  let q_type = parse "?p : <q -> r : %inj1; close>"
  let r_type = parse "?p : <r -> q : %inj1; close>"

  let pq_type = merge ["p"] ["q"] p_type q_type
  let pr_type = merge ["p"] ["r"] p_type r_type
  let qr_type = merge ["q"] ["r"] q_type r_type

  let pqr_type = merge ["p"] ["q"; "r"] p_type qr_type

  let run () = Format.printf "Section A.2:@.- type of p:@ @[<2>%a@]@.- type of q:@ @[<2>%a@]@.- type of r:@ @[<2>%a@]@.- type of p|q:@ @[<2>%a@]@.- type of p|r:@ @[<2>%a@]@.- type of q|r:@ @[<2>%a@]@.- type of p|q|r:@ @[<2>%a@]@.@." fprintf_session p_type fprintf_session q_type fprintf_session r_type fprintf_session pq_type fprintf_session pr_type fprintf_session qr_type fprintf_session pqr_type
end

module Ex4 = struct (* Example from "Multiparty Session Types as Coherence proofs" by Carbone et al. *)

  let gsub_b1 = "b1 -> s : %inj1; b1 -> s : string; end + b1 -> s : %inj2; b1 -> s : string; end"
  let b1_type = parse @@ "b1 -> s : string; s -> b1 : int; b1 -> b2 : int; (b2 -> b1 : %inj1; close & b2 -> b1 : %inj2; b1 -> s : <" ^ gsub_b1 ^ ">; b1 -> s : string; close)"

  let b2_type = parse "s -> b2 : int; b1 -> b2 : int; (b2 -> b1,s : %inj1; b2 -> s : string; close + b2 -> b1,s : %inj2; b2 -> s : string; close)"

  let gsub_s = "b1 -> s : %inj1; b1 -> s : string; close & b1 -> s : %inj2; b1 -> s : string; close"
  let s_type = parse @@ "b1 -> s : string; s -> b1, b2 : int; (b2 -> s : %inj1; b2 -> s : string; end & b2 -> s : %inj2; b1 -> s : <" ^ gsub_s ^ ">; b1 -> s : string; b2 -> s : string; end)"

  let b1b2_type = simplify_nothing_everything_session @@ merge ["b1"] ["b2"] b1_type b2_type

  (* unreadable *)
  let b1b2s_type = merge ["b1";"b2"] ["s"] b1b2_type s_type

  (* the result is of the form "G1 + (G2 & G2 & G2)" *)
  let b1b2s_type' = simplify_nothing_everything_session @@ b1b2s_type
end
