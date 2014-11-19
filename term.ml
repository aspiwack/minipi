type 'a l = Z | S of 'a

module L = struct
  type 'a t = 'a l

  let map (type a) (type b) (f:a->b) : a l -> b l = function
    | Z -> Z
    | S x -> S (f x) 

end


type 'v t =
  (** Terms *)
  | Var of 'v
  | App of 'v t * 'v u
  | Cast of 'v u * 'v t
  (** Types *)
  | Type
  | Kind
  | Pi of 'v t * 'v l t

and 'v u =
  | Lam of 'v l u
  | Check of 'v t

let rec map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t = fun f -> function 
  | Var v -> Var (f v)
  | App(u,v) -> App ( map f u , mapu f v )
  | Cast (u,t) -> Cast (mapu f u,map f t)
  | (Type|Kind) as k -> k
  | Pi (t,u) -> Pi (map f t, map (L.map f) u)
and mapu : 'a 'b. ('a -> 'b) -> 'a u -> 'b u = fun f -> function
  | Lam u -> Lam (mapu (L.map f) u)
  | Check u -> Check (map f u)

let return v = Var v

let shift (type a) (type b) (f:a->b t) : a l -> b l t = function
  | Z -> Var Z
  | S x -> map (fun v -> S v) (f x)

let rec (>>=) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t = fun u s ->
  match u with
  | Var v -> s v
  | App (u,v) -> App( u>>=s , substu v s )
  | Cast (u,t) -> Cast ( substu u s , t>>=s )
  | (Type|Kind) as k -> k
  | Pi (t,u) -> Pi ( t>>=s , u >>= shift s )

and substu : 'a 'b. 'a u -> ('a -> 'b t) -> 'b u = fun u s ->
  match u with
  | Lam u -> Lam ( substu u (shift s) )
  | Check u -> Check ( u>>=s )
