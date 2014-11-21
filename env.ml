(** [('v,'r) Typed.t] is the type of well-typed terms. ['r] is the
    static name of the typing context. The [ttype] component is a
    cache of the type of [term]. *)
module Typed = struct

  type ('v,'r,'n) u = {
    term :'v Term.t;
    ttype:'v Term.t;
  }

  type ('v,'r) t = N : ('v,'r,'n) u -> ('v,'r) t

  let term (N t) = t.term
  let ttype (N t) = t.ttype

end

(** [('v,'r) env] is the type of typing contexts for ['v Term.t]. The
    type ['r] is a static name assigned to the environment. *)
type (_,_) env =
  | Base : ('v -> ('v,'r) Typed.t) -> ('v,'r) env
  | Ext  : ('v,'r) env * ('v,'r,'t) Typed.u -> ('v Term.l,'r*'t) env

(** Generate environment names. *)
type _ benv =
  | B : ('v,'r) env -> 'v benv

(** Generate environement extension names. *)
type (_,_) eenv =
  | E : ('v,'r*'t) env -> ('v,'r) eenv

let base (type v) (e:v -> (v,_) Typed.t) : v benv = B (Base e)
let extn (type v) (type r) (type n)
    (e:(v,r) env) (u:(v,r,n) Typed.u) : (v Term.l,r*n) env =
  Ext(e,u)
let ext (type v) (type r)
    (e:(v,r) env) ((Typed.N u):(v,r) Typed.t) : (v Term.l,r) eenv =
  E (extn e u)



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
  let term = Term.lift (term t) in
  let ttype = Term.lift (ttype t) in
  N { term ; ttype }

let rec var_type : type v r. (v,r) env -> v -> v Term.t = fun e v ->
  let open Typed in
  match e , v with
  | Base e , v -> term (e v)
  | Ext (_,d) , Term.Z -> Term.lift (d.term)
  | Ext (e,_) , Term.S v -> Term.lift (var_type e v)

let var e v = Typed.N { Typed.term=Term.Var v ; ttype=var_type e v }

let app (type v) (type r)
    (e:(v,r) env) (u:(v,r) Typed.t) (t:v Term.u) : (v,r) under_constraint =
  let open Typed in
  match ttype u with
  | Term.Pi(a,b) ->
      let t' Checked = Term.Cast(t,a) in
      let ttype ch = Term.subst0 b (t' ch) in
      let c = C (B e,t,a) in
      U ( c , fun ch -> N { term=Term.App(term u,t) ; ttype=ttype ch } )
  | _ -> raise TypeError

let cast (type v) (type r)
    (e:(v,r) env) (u:v Term.u) (a:(v,r) Typed.t) : (v,r) under_constraint =
  let open Typed in
  let c = C( B e , u , term a ) in
  U ( c , fun Checked -> N { term=Term.Cast(u,term a) ; ttype=term a } )
      

let ktype (type v) (type r) (e:(v,r) env) : (v,r) Typed.t =
  let open Typed in
  N { term = Term.Type ; ttype = Term.Kind }

let max_kind k1 k2 =
  let open Term in
  match k1,k2 with
  | (Type|Kind) , Kind
  | Kind , Type         -> Kind
  | Type , Type         -> Type
  | _ , _               -> raise TypeError

let pi (type v) (type r) (type n)
    (e:(v,r) env) (a:(v,r,n) Typed.u) (b:(v Term.l, r*n) Typed.t) : (v,r) Typed.t =
  let open Typed in
  N { term = Term.Pi (a.term,term b) ; ttype = max_kind a.term (term b) }

let lam (type c) (c:c tconstraint) : c newconstraint =
  let open Term in
  match c with
  | C(B env, Lam u , Pi(a,b)) ->
      let env =
        match ext env Typed.(N {term=a; ttype=Type}) with
        | E e -> B e
      in
      let n = C( env , u , b ) in
      N ( n , check )
  | _ -> raise TypeError

let convertible (type v) (u:v Term.t) (t:v Term.t) : bool =
  (* TODO *)
  u = t

type infer = { infer:'v 'r. ('v,'r) env -> 'v term -> ('v,'r) typed }

let check (type v) (type r) (c:c tconstraint) (i:infer) : c checked =
  let open Term in
  match c with
  | C(B env, Check u, t) ->
      let tu = i u in
      (* arnaud: the typed term is never (==) to its original thingy
         with the rules I wrote *)
      if term tu == u && convertible (ttype tu) t then Checked
      else raise TypeError
  | _ -> TypeError
