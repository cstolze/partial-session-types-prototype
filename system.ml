type msg =
  | Inj1
  | Inj2
  | Int
  | String
  | Sub of session
and main_comm =
  | Msg of (string * string list * msg)
and end_comm =
  | End
  | Close
  | Client of string * session
and session =
  | Internal of session * session
  | External of session * session
  | Cons of (main_comm * session)
  | Nothing
  | Everything
  | Nil of end_comm

let rec sender_string =
  function
  | [] -> assert false
  | [x] -> x
  | [x;y] -> x ^ ", " ^ y
  | x :: l -> x ^ ", " ^ sender_string l

let rec msg_to_string =
  function
  | Inj1 -> "%inj_1"
  | Inj2 -> "%inj_2"
  | Int -> "int"
  | String -> "string"
  | Sub s -> "<" ^ session_to_string 0 s ^ ">"
and main_to_string (Msg (s1,s2,m)) =
  s1 ^ " -> " ^ sender_string s2 ^ " : " ^ msg_to_string m
and end_to_string =
  function
  | End -> "end"
  | Close -> "close"
  | Client (s,g) -> "?" ^ s ^ " : <" ^ session_to_string 0 g ^ ">"
and session_to_string precedence =
  let paren trigger txt =
    if precedence < trigger then txt else "(" ^ txt ^ ")"
  in
  function
  | Internal (g1,g2) -> paren 1 (session_to_string 1 g1 ^ " + " ^ session_to_string 0 g2)
  | External (g1,g2) -> paren 2 (session_to_string 2 g1 ^ " & " ^ session_to_string 1 g2)
  | Cons (c,g) -> main_to_string c ^ "; " ^ session_to_string 2 g
  | Nothing -> "0"
  | Everything -> "Omega"
  | Nil c -> end_to_string c

let rec normalize_msg =
  function
  | Sub s -> Sub (normalize_session s)
  | x -> x
and normalize_main (Msg (s1,s2,m)) = Msg (s1,s2, normalize_msg m)
and normalize_end =
  function
  | Client (s,g) -> Client (s, normalize_session g)
  | x -> x
and distr_ext g1 g2 =
  match (g1, g2) with
  | (Internal (g11,g12), _) -> Internal(distr_ext g11 g2, distr_ext g12 g2)
  | (_, Internal (g21,g22)) -> Internal(distr_ext g1 g21, distr_ext g1 g22)
  | _ -> External (g1, g2)
and distr_main c =
  function
  | Internal (g1, g2) -> Internal(distr_main c g1, distr_main c g2)
  | External (g1, g2) -> External(distr_main c g1, distr_main c g2)
  | g -> Cons (c, g)
and normalize_session =
  function
  | Internal (g1, g2) -> let g1 = normalize_session g1 in
                         let g2 = normalize_session g2 in
                         Internal (g1, g2)
  | External (g1, g2) -> let g1 = normalize_session g1 in
                         let g2 = normalize_session g2 in
                         distr_ext g1 g2
  | Cons (c,g) -> distr_main (normalize_main c) (normalize_session g)
  | Nil c -> Nil (normalize_end c)
  | x -> x

let rec simplify_nothing_everything_msg =
  function
  | Sub s -> Sub (simplify_nothing_everything_session s)
  | x -> x
and simplify_nothing_everything_main (Msg (s1,s2,m)) =
  Msg (s1,s2, simplify_nothing_everything_msg m)
and simplify_nothing_everything_end =
  function
  | Client (s,g) -> Client (s, simplify_nothing_everything_session g)
  | x -> x
and simplify_nothing_everything_session =
  function
  | Internal (g1, g2) -> let g1 = simplify_nothing_everything_session g1 in
                         let g2 = simplify_nothing_everything_session g2 in
                         begin
                           match (g1,g2) with
                           | (Everything, g) | (g, Everything) -> Everything
                           | (Nothing, g) | (g, Nothing) -> g
                           | _ -> Internal (g1,g2)
                         end
  | External (g1, g2) -> let g1 = simplify_nothing_everything_session g1 in
                         let g2 = simplify_nothing_everything_session g2 in
                         begin
                           match (g1,g2) with
                           | (Nothing, g) | (g, Nothing) -> Nothing
                           | (Everything, g) | (g, Everything) -> g
                           | _ -> External (g1,g2)
                         end
  | Cons (c,g) ->
     begin
       match simplify_nothing_everything_session g with
       | Nothing | Everything as x -> x
       | g -> Cons (simplify_nothing_everything_main c, g)
     end
  | Nil c -> Nil (simplify_nothing_everything_end c)
  | x -> x

let rec belong x =
  function
  | [] -> false
  | y :: l when x = y -> true
  | _ :: l -> belong x l

let subseteq l1 l2 = List.fold_left (&&) true (List.map (fun x -> belong x l2) l1)

let rec inter s1 =
  function
  | [] -> []
  | x :: s2 -> if belong x s1 then x :: inter s1 s2 else inter s1 s2

let rec cup s1 =
  function
  | [] -> s1
  | x :: s2 -> if belong x s1 then cup s1 s2 else x :: cup s1 s2

let rec finalized_msg m s =
  match m with
  | Sub g -> finalized_session g s
  | _ -> true
and finalized_main (Msg(p,q,m)) s = belong p s && subseteq q s && finalized_msg m (p::q)
and finalized_end e s =
  match e with
  | End -> true
  | Close -> false
  | Client (p,g) -> belong p s && finalized_session g s
and finalized_session g s =
  match g with
  | Internal (g1,g2) -> finalized_session g1 s && finalized_session g2 s
  | External (g1,g2) -> finalized_session g1 s && finalized_session g2 s
  | Cons (c,g) -> finalized_main c s && finalized_session g s
  | Nothing -> true
  | Everything -> false
  | Nil e -> finalized_end e s

let indep s a b =
  match a with
  | None -> true
  | Some (Msg(p1,q1,_)) ->
     match b with
     | None -> true
     | Some (Msg(p2,q2,_)) -> match inter s (inter (p1::q1) (p2::q2)) with
                              | [] -> true
                              | _ :: _ -> false

let cons x l = (* useful? *)
  match x with
  | None -> l
  | Some x -> Cons(x,l)

let rec prefix_list s =
  function
  | Cons(m,c) -> let l = prefix_list s c in
                 (Some m, c) :: List.map (fun (x,c) -> (x,Cons(m,c))) (List.filter (fun (x,_) -> indep s (Some m) x) l)
  | Nil _ as c -> [(None, c)]
  | _ -> failwith "prefix_list"

(* takes chains of communications as input *)
(* returns a list of triples (c, g1, g2) where c is the merged prefix, and g1, g2 have yet to be merged *)
let sync s1 s2 f c1 c2 =
  let l1 = prefix_list s1 c1 in
  let l2 = prefix_list s2 c2 in
  let rec foo l (c1,g1) =
    match l with
    | [] -> []
    | (c2,g2) :: l -> match c1,c2 with
                      | None, None -> foo l (c1,g1)
                      | _ -> match f c1 c2 with
                             | None -> foo l (c1,g1)
                             | Some c -> (c, g1, g2) :: foo l (c1,g1)
  in List.concat (List.map (foo l2) l1)

let rec my_fold f g =
  function
  | [] -> failwith "don't call my_fold with an empty string"
  | [x] -> g x
  | x :: l -> f x (my_fold f g l)

(* like in the article, g1 and g2 are supposed to be in normal form, but here we have 2 auxiliary functions: fm merges main_comm, and fe merges end_comm *)
let rec map s1 s2 fm fe g1 g2 =
  match g1, g2 with
  | Nothing, _ | _, Nothing -> Nothing
  | Everything, _ | _, Everything -> Everything
  | Internal (g,g'), _ -> Internal(map s1 s2 fm fe g g2, map s1 s2 fm fe g' g2)
  | _, Internal (g,g') -> Internal(map s1 s2 fm fe g1 g, map s1 s2 fm fe g1 g')
  | External (g,g'), _ -> External(map s1 s2 fm fe g g2, map s1 s2 fm fe g' g2)
  | _, External (g,g') -> External(map s1 s2 fm fe g1 g, map s1 s2 fm fe g1 g')
  | Cons(_,_), _ | _, Cons(_,_) -> let l = sync s1 s2 fm g1 g2 in
                                   begin
                                     match l with
                                     | [] -> Everything
                                     | _ -> my_fold (fun (c,g1,g2) l -> External(Cons(c,map s1 s2 fm fe g1 g2), l)) (fun (c,g1,g2) -> Cons(c,map s1 s2 fm fe g1 g2)) l
                                   end
  | Nil e1, Nil e2 -> match fe e1 e2 with
                      | None -> Everything
                      | Some e -> Nil e

let rec merge_end_comm s1 s2 fm e1 e2 =
  match e1, e2 with
  | Close, e | e, Close -> Some e
  | End, End -> Some End
  | Client (p1, g1), Client (p2, g2) when p1 = p2 -> Some (Client (p1, map s1 s2 fm (merge_end_comm s1 s2 fm) g1 g2))
  | _ -> None

let rec merge_main_comm s1 s2 c1 c2 =
  match c1, c2 with
  | None, None -> failwith "the case (1, 1) should never be called"
  | None, Some(Msg(p,q,m)) ->
     begin
       match inter s1 (p::q) with
       | [] -> Some(Msg(p,q,m))
       | _ -> None
     end
  | Some(Msg(p,q,m)), None ->
     begin
       match inter s2 (p::q) with
       | [] -> Some (Msg(p,q,m))
       | _ -> None
     end
  | Some(Msg(p1,q1,m1)), Some(Msg(p2,q2,m2)) ->
         let guard1 = if belong p1 s1 then subseteq q2 q1 else subseteq (inter s1 q2) q1 in
         let guard2 = if belong p2 s2 then subseteq q1 q2 else subseteq (inter s2 q1) q2 in
         if guard1 && guard2 && p1 = p2 then
           match m1, m2 with
           | Sub g1, Sub g2 -> let q = cup q1 q2 in
                               let g = map s1 s2 (merge_main_comm s1 s2) (merge_end_comm s1 s2 (merge_main_comm s1 s2)) g1 g2 in
                               let s = cup s1 s2 in
                               if subseteq (p1::q) s then
                                 let g = simplify_nothing_everything_session g in
                                 if finalized_session g s then
                                   Some (Msg(p1,q,Sub g))
                                 else None
                               else Some (Msg(p1,q,Sub g))
           | x, y when x = y -> Some(Msg(p1,cup q1 q2, x))
           | _ -> None
         else None

let merge s1 s2 g1 g2 = map s1 s2 (merge_main_comm s1 s2) (merge_end_comm s1 s2 (merge_main_comm s1 s2)) (normalize_session (simplify_nothing_everything_session g1)) (normalize_session (simplify_nothing_everything_session g2))

(* Example (section 6) *)
let g1 = Cons(Msg("p",["q"],Inj1), Cons(Msg("p",["r"], Sub(Nil End)), Nil Close))
let g2 = Cons(Msg("p",["q"],Inj2), Nil Close)
let g1' = Cons(Msg("p",["q"],Inj1), Cons(Msg("q",["r"], Inj1), Nil Close))
let g2' = Cons(Msg("p",["q"],Inj2), Cons(Msg("q",["r"], Inj2), Nil Close))
let g1'' = Cons(Msg("q",["r"],Inj1), Cons(Msg("p",["r"], Sub(Nil Close)), Nil Close))
let g2'' = Cons(Msg("q",["r"],Inj2), Nil Close)

let p_type = Internal(g1,g2)
let q_type = External(g1',g2')
let r_type = External(g1'',g2'')

let pq_type = merge ["p"] ["q"] p_type q_type
let pr_type = merge ["p"] ["r"] p_type r_type
let qr_type = merge ["q"] ["r"] q_type r_type

let pqr_type = merge ["p";"q"] ["r"] pq_type r_type

let () = Printf.printf "Section 6:\ntype of p:\n\t%s\ntype of q:\n\t%s\ntype of r:\n\t%s\ntype of p|q:\n\t%s\ntype of p|r:\n\t%s\ntype of q|r:\n\t%s\ntype of p|q|r:\n\t%s\n\n" (session_to_string 0 p_type) (session_to_string 0 q_type) (session_to_string 0 r_type) (session_to_string 0 pq_type) (session_to_string 0 pr_type) (session_to_string 0 qr_type) (session_to_string 0 pqr_type)

(* Example (section A.1) *)
let p_type = Cons(Msg("p",["q";"r"], Sub(Nil End)), Cons(Msg("p",["q"], Sub(Nil End)), Nil Close))
let q_type = Cons(Msg("p",["q"], Sub(Nil Close)), Cons(Msg("p",["q"], Sub(Nil Close)), Nil Close))
let r_type = Cons(Msg("p",["r"], Sub(Nil Close)), Nil Close)

let pq_type = merge ["p"] ["q"] p_type q_type
let pr_type = merge ["p"] ["r"] p_type r_type
let qr_type = merge ["q"] ["r"] q_type r_type

let pqr_type = merge ["p"] ["q";"r"] p_type qr_type

let () = Printf.printf "Section A.1:\ntype of p:\n\t%s\ntype of q:\n\t%s\ntype of r:\n\t%s\ntype of p|q:\n\t%s\ntype of p|r:\n\t%s\ntype of q|r:\n\t%s\ntype of p|q|r:\n\t%s\n\n" (session_to_string 0 p_type) (session_to_string 0 q_type) (session_to_string 0 r_type) (session_to_string 0 pq_type) (session_to_string 0 pr_type) (session_to_string 0 qr_type) (session_to_string 0 pqr_type)

(* Example (section A.2) *)
let p_type = Nil (Client("p", Nothing))
let q_type = Nil (Client("p", Cons(Msg("q",["r"], Inj1), Nil Close)))
let r_type = Nil (Client("p", Cons(Msg("r",["q"], Inj1), Nil Close)))

let pq_type = merge ["p"] ["q"] p_type q_type
let pr_type = merge ["p"] ["r"] p_type r_type
let qr_type = merge ["q"] ["r"] q_type r_type

let pqr_type = merge ["p"] ["q"; "r"] p_type qr_type

let () = Printf.printf "Section A.2:\ntype of p:\n\t%s\ntype of q:\n\t%s\ntype of r:\n\t%s\ntype of p|q:\n\t%s\ntype of p|r:\n\t%s\ntype of q|r:\n\t%s\ntype of p|q|r:\n\t%s\n\n" (session_to_string 0 p_type) (session_to_string 0 q_type) (session_to_string 0 r_type) (session_to_string 0 pq_type) (session_to_string 0 pr_type) (session_to_string 0 qr_type) (session_to_string 0 pqr_type
)
