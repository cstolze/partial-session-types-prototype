open Session
open Format

let rec sender_string =
  function
  | [] -> assert false
  | [x] -> x
  | x :: l -> x ^ "," ^ sender_string l

let rec my_fold f g o =
  function
  | [] -> o
  | [x] -> g x
  | x :: l -> f x (my_fold f g o l)

let rec msg_to_string =
  function
  | Inj1 -> "%inj1"
  | Inj2 -> "%inj2"
  | Int -> "int"
  | String -> "string"
  | Sub s -> "<" ^ session_to_string s ^ ">"
and main_to_string (Msg (s1,s2,m)) =
  s1 ^ " -> " ^ sender_string s2 ^ " : " ^ msg_to_string m
and end_to_string =
  function
  | End -> "end"
  | Close -> "close"
  | Client (s,g) -> "?" ^ s ^ " : <" ^ session_to_string g ^ ">"
and chain_to_string =
  function
  | Cons (c,g) -> main_to_string c ^ "; " ^ chain_to_string g
  | Nil c -> end_to_string c
and session_to_string l =
  let foo l = my_fold (fun x y -> x ^ " & " ^ y) (fun x -> x) "omega" (List.map chain_to_string l) in
  my_fold (fun x y -> x ^ " + " ^ y) (fun x -> x) "0" (List.map foo l)

let string_to_session s = Parser.s Lexer.read (Lexing.from_string s)
let parse = string_to_session

let rec fprintf_list ppf =
  function
  | [] -> assert false
  | [x] -> fprintf ppf "%s" x
  | x :: l -> fprintf ppf "%s,@,%a" x fprintf_list l

let rec fprintf_msg ppf =
  function
  | Inj1 -> fprintf ppf "%%inj1"
  | Inj2 -> fprintf ppf "%%inj2"
  | Int ->  fprintf ppf "int"
  | String -> fprintf ppf "string"
  | Sub s -> fprintf ppf "@[<2><%a>@]" fprintf_session s
and fprintf_main ppf (Msg (s1,s2,m)) =
  fprintf ppf "%s@ ->@ %a@ :@ %a" s1 fprintf_list s2 fprintf_msg m
and fprintf_end ppf =
  function
  | End -> fprintf ppf "end"
  | Close -> fprintf ppf "close"
  | Client (s,g) -> fprintf ppf "?%s@ :@ @[<2><%a>@]" s fprintf_session g
and fprintf_chain ppf =
  function
  | Cons (c,g) -> fprintf ppf "@[<2>%a;@ %a@]" fprintf_main c fprintf_chain g
  | Nil c -> fprintf_end ppf c
and fprintf_session ppf l =
  let foo l = my_fold (fun x y () -> fprintf ppf "@[<2>"; x (); fprintf ppf "@ &@ "; y (); fprintf ppf "@]") (fun x -> x) (fun () -> fprintf ppf "omega") (List.map (fun x () -> fprintf_chain ppf x) l) in
  my_fold (fun x y () -> fprintf ppf "@[<2>"; x (); fprintf ppf "@ +@ "; y (); fprintf ppf "@]") (fun x -> x) (fun () -> fprintf ppf "omega") (List.map foo l) ()
