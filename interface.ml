open Session
open Format

let rec sender_string =
  function
  | [] -> assert false
  | [x] -> x
  | x :: l -> x ^ "," ^ sender_string l

let rec msg_to_string =
  function
  | Inj1 -> "%inj1"
  | Inj2 -> "%inj2"
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
  | Internal (g1,g2) -> paren 1 (session_to_string 0 g1 ^ " + " ^ session_to_string 1 g2)
  | External (g1,g2) -> paren 2 (session_to_string 1 g1 ^ " & " ^ session_to_string 2 g2)
  | Cons (c,g) -> main_to_string c ^ "; " ^ session_to_string 2 g
  | Nothing -> "0"
  | Everything -> "omega"
  | Nil c -> end_to_string c

let session_to_string = session_to_string 0

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
  | Sub s -> fprintf ppf "@[<2><%a>@]" (fprintf_session 0) s
and fprintf_main ppf (Msg (s1,s2,m)) =
  fprintf ppf "%s@ ->@ %a@ :@ %a" s1 fprintf_list s2 fprintf_msg m
and fprintf_end ppf =
  function
  | End -> fprintf ppf "end"
  | Close -> fprintf ppf "close"
  | Client (s,g) -> fprintf ppf "?%s@ :@ @[<2><%a>@]" s (fprintf_session 0) g
and fprintf_session precedence ppf =
  function
  | Internal (g1,g2) -> if precedence < 1  then fprintf ppf "@[<2>%a@ +@ %a@]" (fprintf_session 0) g1 (fprintf_session 1) g2 else fprintf ppf "@[<2>(%a@ +@ %a)@]" (fprintf_session 0) g1 (fprintf_session 1) g2
  | External (g1,g2) -> if precedence < 2 then fprintf ppf "@[<2>%a@ &@ %a@]" (fprintf_session 1) g1 (fprintf_session 2) g2 else fprintf ppf "@[<2>(%a@ &@ %a)@]" (fprintf_session 1) g1 (fprintf_session 2) g2
  | Cons (c,g) -> fprintf ppf "@[<2>%a;@ %a@]" fprintf_main c (fprintf_session 2) g
  | Nothing -> fprintf ppf "0"
  | Everything -> fprintf ppf "omega"
  | Nil c -> fprintf_end ppf c

let fprintf_session = fprintf_session 0
