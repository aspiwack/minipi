(** [('v,'r) Typed.t] is the type of well-typed terms. ['r] is the
    static name of the typing context. The [ttype] component is a
    cache of the type of [term]. *)
module Typed = struct

  type ('v,'r) t = {
    term :'v Term.t;
    ttype:'v Term.t;
  }

  let term t = t.term
  let ttype t = t.ttype

end

(** [('v,'r) env] is the type of typing contexts for ['v Term.t]. The
    type ['r] is a static name assigned to the environment. *)
type (_,_) env =
  | Base : ('v -> 'v Term.t) -> ('v,'r) env
  | Ext  : ('v,'r) env * ('v,'r) Typed.t -> ('v Term.l,'r*'t) env

(** Generate environment names. *)
type _ benv =
  | B : ('v,'r) env -> 'v benv

(** Generate environement extension names. *)
type (_,_) eenv =
  | E : ('v,'r*'t) env -> ('v,'t) eenv

let base (type v) (e:v -> v Term.t) : v benv = B (Base e)
let ext (type v) (type r) (e:(v,r) env) (u:(v,r) Typed.t) : (v Term.l,r) eenv =
  E (Ext(e,u))
