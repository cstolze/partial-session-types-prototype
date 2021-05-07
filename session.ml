let sort_simple l =
  let compare_default x y = if x < y then -1 else if x = y then 0 else 1 in
  List.sort_uniq compare_default l

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

let rec cupcup s1 =
  function
  | [] -> []
  | x :: s2 -> List.map (cup x) s1 @ cupcup s1 s2

let rec my_fold f g =
  function
  | [] -> failwith "don't call my_fold with an empty string"
  | [x] -> g x
  | x :: l -> f x (my_fold f g l)

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
and chain =
  | Cons of (main_comm * chain)
  | Nil of end_comm
and session = chain list list

let indep s a b =
  match a with
  | None -> true
  | Some (Msg(p1,q1,_)) ->
     match b with
     | None -> true
     | Some (Msg(p2,q2,_)) -> match inter s (inter (p1::q1) (p2::q2)) with
                              | [] -> true
                              | _ :: _ -> false

let rec cons c = List.map (List.map (fun x -> Cons (c, x)))

(* bubble sort for chain of communication *)
let rec insert_in_chain s m1 g =
  match g with
  | Nil e -> Cons (m1, Nil e)
  | Cons (m2, g) -> if m2 < m1 && indep s (Some m1) (Some m2) then
                      Cons (m2, insert_in_chain s m1 g)
                    else Cons (m1, Cons (m2, g))
let rec canonical_msg s =
  function
  | Sub l -> Sub (canonical_session s l)
  | _ as x -> x
and canonical_main s (Msg (p, l, m)) = Msg (p, sort_simple l, canonical_msg s m)
and canonical_end s =
  function
  | Client (p, l) -> Client (p, canonical_session s l)
  | _ as x -> x
and canonical_chain s g =
  match g with
  | Nil e -> Nil e
  | Cons (m1, g) -> insert_in_chain s m1 (canonical_chain s g)
and canonical_session s l = sort_simple @@ List.map (fun x -> sort_simple @@ List.map (canonical_chain s) x) l

(* ASSERT THAT THE ARGUMENT IS IN CANONICAL FORM *)
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
and finalized_chain g s =
  match g with
  | Cons (c,g) -> finalized_main c s && finalized_chain g s
  | Nil e -> finalized_end e s
and finalized_session g s =
  match g with
  | [[]] -> false (* THE TYPE IS SUPPOSED TO BE IN CANONICAL FORM *)
  | _ as l -> List.fold_left (&&) true (List.map (fun l -> List.fold_left (&&) true (List.map (fun x -> finalized_chain x s) l)) l)

let rec prefix_list s =
  function
  | Cons(m,c) -> let l = prefix_list s c in
                 (Some m, c) :: List.map (fun (x,c) -> (x,Cons(m,c))) (List.filter (fun (x,_) -> indep s (Some m) x) l)
  | Nil _ as c -> [(None, c)]

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

(* like in the article, g1 and g2 are supposed to be in normal form, but here we have 2 auxiliary functions: fm merges main_comm, and fe merges end_comm *)
(* the input sessions are not necessarily canonical, and the output is not canonical *)
let rec map_chain s1 s2 fm fe g1 g2 =
  match g1, g2 with
  | Cons(_,_), _ | _, Cons(_,_) -> let l = sync s1 s2 fm g1 g2 in
                                   List.concat (List.map (fun (c, g1, g2) -> List.map (fun x -> Cons (c,x)) (map_chain s1 s2 fm fe g1 g2)) l)
  | Nil e1, Nil e2 -> match fe e1 e2 with
                      | None -> []
                      | Some e -> [Nil e]
let rec map s1 s2 fm fe g1 g2 =
  List.concat @@ List.map (fun g1 -> List.map (fun g2 -> List.concat @@ List.map (fun g1 -> List.concat @@ List.map (fun g2 -> map_chain s1 s2 fm fe g1 g2) g2) g1) g2) g1

let rec merge_end_comm s1 s2 fm e1 e2 =
  match e1, e2 with
  | Close, e | e, Close -> Some e
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
                                 let g = canonical_session (cup s1 s2) g in (* IMPORTANT *)
                                 if finalized_session g s then
                                   Some (Msg(p1,q,Sub g))
                                 else None
                               else Some (Msg(p1,q,Sub g))
           | x, y when x = y -> Some(Msg(p1,cup q1 q2, x))
           | _ -> None
         else None

let merge s1 s2 g1 g2 = canonical_session (cup s1 s2) @@ map s1 s2 (merge_main_comm s1 s2) (merge_end_comm s1 s2 (merge_main_comm s1 s2)) g1 g2
