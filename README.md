# partial-session-types-prototype
Proof-of-concept implementation in OCaml of a proposed merging algorithm for partial session types.
This prototype is intended as an artefact for an article submitted at the CONCUR 2021 conference called "Composable Partial Multiparty Session Types".

## Required softwares:
The standard ocaml tools, including ocamlbuild, ocamllex, and ocamlyacc.

## How to
- Run `make`
- Launch `ocaml`
- Type `Ex1.run ();;`
- Type `Ex2.run ();;`
- Type `Ex3.run ();;`
- You can declare your own session types and merge them. For instance:
```
let p_type = parse "p -> q : %inj1; q -> p : int; close"
let q_type = parse "p -> q : %inj1; q -> p : int; close & p -> q : %inj2; close"
let pq_type = merge ["p"] ["q"] p_type q_type;;
```
- You can also test the different implemented functions.

The different .ml files have self-explanatory names. You can also look at the *.mli files and at the .ocamlinit file.