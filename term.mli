type 'a l = Z | S of 'a


(** Terms indexed by the type of their variables. The terms are split
    into two categories: those whose type can be inferred and those whose
    type can be checked. Typing is then done in a bidirectional way. *)
type +'v t =
  (** Terms *)
  | Var of 'v
  | App of 'v t * 'v u
  | Cast of 'v u * 'v t
  (** Types *)
  | Type
  | Kind
  | Pi of 'v t * 'v l t

and +'v u =
  | Lam of 'v l u
  | Check of 'v t


val map  : ('a -> 'b) -> 'a t -> 'b t
val mapu : ('a -> 'b) -> 'a u -> 'b u

(** [return v] is a synonymous for [Var v]. *)
val return : 'a -> 'a t

(** ['a t] is a monad. The monadic bind corresponds to
    substitution. *)
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val substu : 'a u -> ('a -> 'b t) -> 'b u

(** [lift u] returns [u] in a listed environment (all variable are
    lifted by [S]). *)
val lift : 'a t -> 'a l t

(** [subst0 u a] substitutes [a] for the first variable of [u]. It
    effectively implements beta-reduction. *)
val subst0 : 'v l t -> 'v t -> 'v t
