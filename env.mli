(** [('v,'r) Typed.t] is the type of well-typed terms. ['r] is the
    static name of the typing context. The [ttype] component is a
    cache of the type of [term]. *)
module Typed : sig

  type ('v,'r) t

  val term : ('v,'r) t -> 'v Term.t
  val ttype : ('v,'r) t -> 'v Term.t

end

(** [('v,'r) env] is the type of typing contexts for ['v Term.t]. The
    type ['r] is a static name assigned to the environment. *)
type ('v,'e) env

(** Generate environment names. *)
type _ benv =
  | B : ('v,'r) env -> 'v benv

(** Generate environement extension names. *)
type (_,_) eenv =
  | E : ('v,'r*'t) env -> ('v,'t) eenv


val base : ('v -> 'v Term.t) -> 'v benv
val ext : ('v,'r) env -> ('v,'r) Typed.t -> ('v Term.l,'r) eenv
