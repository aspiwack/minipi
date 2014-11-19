(** [('v,'r) Typed.t] is the type of well-typed terms. ['r] is the
    static name of the typing context. The [ttype] component is a
    cache of the type of [term]. *)
module Typed : sig

  type ('v,'r) t

  val term : ('v,'r) t -> 'v Term.t
  val ttype : ('v,'r) t -> 'v Term.t

end

(** [('v,'r) env] is the type of (well-formed) typing contexts for ['v
    Term.t]. The type ['r] is a static name assigned to the
    environment. *)
type ('v,'e) env

(** Generate environment names. *)
type _ benv =
  | B : ('v,'r) env -> 'v benv

(** Generate environement extension names. *)
type (_,_) eenv =
  | E : ('v,'r*'t) env -> ('v,'t) eenv

(** Creates a typing context for the base variables. Well-formedness
    of this environment is the responsibility of the user. *)
val base : ('v -> ('v,'r) Typed.t) -> 'v benv

(** [ext env u] pushed well-typed term [u] in the typing context. It
    preserves well-formedness. *)
val ext : ('v,'r) env -> ('v,'r) Typed.t -> ('v Term.l,'r) eenv


(** Typing rules *)

exception TypeError

type 'c tconstraint
type 'c checked

type _ newconstraint =
  | N : 'c tconstraint * ('c checked -> 'c0 checked) -> 'c0 newconstraint

type (_,_) under_constraint =
  | U : 'c tconstraint * ('c checked -> ('v,'r) Typed.t)
      -> ('c,'r) under_constraint

(** Type inference *)

val var : ('v,'r) env -> 'v -> ('v,'r) Typed.t
val app : ('v,'r) env -> ('v,'r) Typed.t -> 'v Term.u -> ('v,'r) under_constraint

(** Type checking *)

val lam : 'c tconstraint -> 'c newconstraint
