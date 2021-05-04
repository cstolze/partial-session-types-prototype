open Session

val session_to_string : session -> string
val string_to_session : string -> session

(** other name for string_to_session *)
val parse : string -> session

(** function used for pretty-printing sessions in the REPL *)
val fprintf_session : Format.formatter -> session -> unit
