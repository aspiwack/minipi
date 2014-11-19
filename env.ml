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
  | Base : ('v -> ('v,'r) Typed.t) -> ('v,'r) env
  | Ext  : ('v,'r) env * ('v,'r) Typed.t -> ('v Term.l,'r*'t) env

(** Generate environment names. *)
type _ benv =
  | B : ('v,'r) env -> 'v benv

(** Generate environement extension names. *)
type (_,_) eenv =
  | E : ('v,'r*'t) env -> ('v,'t) eenv

let base (type v) (e:v -> (v,_) Typed.t) : v benv = B (Base e)
let ext (type v) (type r)
    (e:(v,r) env) (u:(v,r) Typed.t) : (v Term.l,r) eenv =
  E (Ext(e,u))



(** Typing rules *)

exception TypeError

type _ tconstraint =
  | C : 'v benv * 'v Term.u * 'v Term.t -> 'c tconstraint

type 'c checked = Checked

let check Checked = Checked

type _ newconstraint =
  | N : 'c tconstraint * ('c checked -> 'c0 checked) -> 'c0 newconstraint

type (_,_) under_constraint =
  | U : 'c tconstraint * ('c checked -> ('v,'r) Typed.t)
      -> ('c,'r) under_constraint

let weaken (type v) (type r) (t:(v,r) Typed.t) : (v Term.l , r*'t) Typed.t =
  let open Typed in
  let term = Term.lift t.term in
  let ttype = Term.lift t.ttype in
  { term ; ttype }

let rec var_type : type v r. (v,r) env -> v -> v Term.t = fun e v ->
  let open Typed in
  match e , v with
  | Base e , v -> (e v).term
  | Ext (_,d) , Term.Z -> Term.lift (d.term)
  | Ext (e,_) , Term.S v -> Term.lift (var_type e v)

let var e v = { Typed.term=Term.Var v ; ttype=var_type e v }

let rec app (type v) (type r)
    (e:(v,r) env) (u:(v,r) Typed.t) (t:v Term.u) : (v,r) under_constraint =
      let open Typed in
      match u.ttype with
      | Term.Pi(a,b) ->
          let t' Checked = Term.Cast(t,a) in
          let ttype ch = Term.subst0 b (t' ch) in
          let c = C (B e,t,a) in
          U ( c , fun ch -> { term=Term.App(u.term,t) ; ttype=ttype ch } )
      | _ -> raise TypeError
      

let lam (type c) (c:c tconstraint) : c newconstraint =
  let open Term in
  match c with
  | C(B env, Lam u , Pi(a,b)) ->
      let env =
        match ext env Typed.({term=a; ttype=Type}) with
        | E e -> B e
      in
      let n = C( env , u , b ) in
      N ( n , check )
  | _ -> raise TypeError






