open Session

(** Some dummy examples *)
module SimpleExamples : sig
  val syntaxexample1 : session
  val syntaxexample2 : session

  val printexample1 : unit -> unit
  val printexample2 : unit -> unit
end

(** Example from Section 6 *)
module Ex1 : sig
  val p_type : session
  val q_type : session
  val r_type : session
  val pq_type : session
  val pr_type : session
  val qr_type : session
  val pqr_type : session

  val run : unit -> unit
end

(** Example from Section A.1 *)
module Ex2 : sig
  val p_type : session
  val q_type : session
  val r_type : session
  val pq_type : session
  val pr_type : session
  val qr_type : session
  val pqr_type : session

  val run : unit -> unit
end

(** Example from Section A.2 *)
module Ex3 : sig
  val p_type : session
  val q_type : session
  val r_type : session
  val pq_type : session
  val pr_type : session
  val qr_type : session
  val pqr_type : session

  val run : unit -> unit
end

(** Example from "Multiparty Session Types as Coherence proofs" by Carbone et al. *)
module Ex4 : sig
  val b1_type : session
  val b2_type : session
  val s_type : session

  val b1b2_type : session
  val b1b2s_type : session

  val b1b2s_type' : session
end
