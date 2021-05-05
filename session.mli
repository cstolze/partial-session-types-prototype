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

val cons : main_comm -> session -> session
val cupcup : session -> session -> session
val cup : session -> session -> session

(* I have chosen to expose most of the internal functions below, but in a non-prototype implementation the only interesting function would be merge *)

(** arbitrary unique representation of a session *)
val canonical_session : string list -> session -> session

(** indep s c1 c2 decides whether c1 I_s c2 (Definition 2) *)
val indep : string list -> main_comm option -> main_comm option -> bool

(** finalized_session g s returns true iff the session g is finalized for the set of participants s (Definition 8) *)
val finalized_session : session -> string list -> bool
val finalized_msg : msg -> string list -> bool
val finalized_main : main_comm -> string list -> bool
val finalized_end : end_comm -> string list -> bool

(** prefix_list s g (where s is the set of participants) computes the set A_i described in Definition 10 *)
val prefix_list : string list -> chain -> (main_comm option * chain) list

(** function sync of Definition 10 *)
val sync : string list -> string list -> (main_comm option -> main_comm option -> main_comm option) -> chain -> chain -> (main_comm * chain * chain) list

(** function map of Definition 11, where g1 and g2 are supposed to be in normal form, but here we have 2 auxiliary functions: fm merges main_comm, and fe merges end_comm *)
val map : string list -> string list ->
          (main_comm option -> main_comm option -> main_comm option) -> (* fm *)
          (end_comm -> end_comm -> end_comm option) -> (* fe *)
          session -> session -> session

(** merge_end_comm implements part of Definitions 13 and 15 *)
val merge_end_comm : string list -> string list ->
                     (main_comm option -> main_comm option -> main_comm option) ->
                     end_comm -> end_comm -> end_comm option

(** merge_main_comm implements part of Definition 13 and 15 *)
val merge_main_comm : string list -> string list ->
                      main_comm option -> main_comm option -> main_comm option

(** merge composes everything nicely *)
val merge : string list -> string list -> session -> session -> session
