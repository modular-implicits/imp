open Any

module type Functor = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end
(** A type is a functor if it is a data type with one type parameter,
    and the data types' contained values of that type can be mapped over using fmap.
    For example, list is a functor, because you can map over the values of type 't in
    a list of type 't list using the function List.map - so fmap for lists is List.map.

    It should satisfy the following laws:
    * Identity:
      fmap id = id
      (i.e., for all s, fmap (fun x -> x) s = s)
    * Composition:
      fmap (compose f g) = compose (fmap f) (fmap g)
      where compose f g x = f (g x)
      (i.e., for all s, fmap (fun x -> f (g x))) s = fmap f (fmap g s)

    On Applicatives and Monads, where the "contents" of the "structure"
    are actually the results of effectful computations,
    fmap means you map over the results of the computation.
 *)

val fmap : {F : Functor} -> ('a -> 'b) -> 'a F.t -> 'b F.t
(** fmap maps a function over the contents of a functor *)

module type Applicative = sig
  include Functor
  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end
(** Applicative is used to model computations that have effects, and sequence them.
    Applicative is less powerful than monad, in that you can't make a choice of what effects to have
    based on the result of a previous effectful computation.
 *)

let return : {M : Applicative} -> 'a -> 'a M.t
(** return lifts a value to the applicative context, without performing any effects *)

let apply : {M : Applicative} -> ('a -> 'b) M.t -> 'a M.t -> 'b M.t
(** apply is like fmap, but it takes a function which is already in the applicative context.
    It is used to call functions with multiple arguments in an applicative context.
    If you have (outside the applicative context) f x y z
    then you can do this in the applicative context with apply (apply (fmap f x) y) z
    Or, in infix notation, f <$> x <*> y <*> z
    Note: Those operators aren't defined in this module, but they are:
    (<$>) = fmap
    (<*>) = apply
 *)

module type Monad = sig
  include Applicative
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end
(** Monads are a powerful way of modelling and combining computations with effects.
    They are notoriously hard to understand - look elsewhere for an explanation.
 *)

val bind : {M : Monad} -> 'a M.t -> ('a -> 'b M.t) -> 'b M.t
(** bind is the fundamental Monad combinator. Its type signature explains it pretty well I think.
    The effects of (bind f x) are: first, the effects of x, then the effects of f
 *)

(** Convenience functions *)

let pure : {M : Applicative} -> 'a -> 'a M.t
(** pure is an alias for return. It is arguably better-named,
    because "pure x" returns x without any effects (i.e., purely)
 *)

val (>>=) : {M : Monad} -> 'a M.t -> ('a -> 'b M.t) -> 'b M.t
(** infix alias for bind *)

(* Define polymorphic monad functions *)

(** TODO *)

let join {M : Monad} (x : 'a M.t M.t) = M.bind x (fun x -> x)

let (>>|) {M : Monad} (m : 'a M.t) (k : 'a -> 'b) = bind m (fun x -> return (k x))

let sequence {M : Monad} (ms : 'a M.t list) =
  List.fold_right
    (fun m m' ->
       m  >>= fun x ->
       m' >>= fun xs ->
         return (x :: xs))
    ms
    (return [])

module Monad(M : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end)
: Monad with type 'a t = 'a M.t
(** Create a functor, applicative, and monad from just return and bind,
    using default functor and applicative implementation
 *)

module type Monad_plus = sig
  include Monad
  val mzero : 'a t
  val mplus : 'a t -> 'a t -> 'a t
end

module Monad_plus = struct
  let mzero {M : Monad_plus} = M.mzero
  let mplus {M : Monad_plus} = M.mplus

  let mguard {M : Monad_plus} b =
    if b then
      M.return ()
    else
      M.mzero
end

module type Foldable = sig
  type 'a t
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Foldable = struct
  let fold {F : Foldable} = F.fold
end

module type Traversable = sig
  include Functor
  val traverse : {F : Applicative} ->
                 ('a -> 'b F.t) -> 'a t -> 'b t F.t
end

module Traversable = struct
  let traverse {T : Traversable} = T.traverse
end

implicit module Option: sig
  include Functor with type 'a t = 'a option
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include Monad_plus with type 'a t := 'a t
  include Foldable with type 'a t := 'a t
  include Traversable with type 'a t := 'a t
end = struct
  type 'a t = 'a option

  (* Functor *)
  let fmap f = function
    | None -> None
    | Some a -> Some (f a)

  (* Applicative *)
  let return x = Some x
  let apply f x = match f, x with
    | Some f, Some x -> Some (f x)
    | _, _ -> None

  (* Monad *)
  let bind x f = match x with
    | None -> None
    | Some x -> f x

  (* Monad_plus *)
  let mzero = None
  let mplus a b = match a with
    | None -> b
    | Some _ -> a

  (* Foldable *)
  let fold f t acc = match t with
    | None -> acc
    | Some x -> f x acc

  (* Traversable *)
  let traverse (type a) (type b) {F : Applicative} (f : a -> b F.t)  : a option -> b option F.t = function
    | None -> F.return None
    | Some x -> F.fmap (fun x -> Some x) (f x)
end

implicit module List : sig
  include Functor with type 'a t = 'a list
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include Monad_plus with type 'a t := 'a t
  include Foldable with type 'a t := 'a t
  include Traversable with type 'a t := 'a t
end = struct
  type 'a t = 'a list

  (* Functor *)
  let fmap = List.map

  (* Applicative *)
  let return x = [x]
  let apply fs xs =
    List.concat (List.map (fun f -> List.map (fun x -> f x) xs) fs)

  (* Monad *)
  let bind x f = List.concat (List.map f x)

  (* Monad_plus *)
  let mzero = []
  let mplus = (@)

  (* Foldable *)
  let fold f xs a = List.fold_left (fun x y -> f y x) a xs

  (* Traversable *)
  let traverse {F : Applicative} f t =
    let cons x ys = F.apply (F.fmap (fun x xs -> x :: xs) (f x)) ys in
    List.fold_right cons t (F.return [])
end

implicit module Function {A : Any} : sig
  include Functor with type 'b t = A.t_for_any -> 'b
  include Applicative with type 'b t := 'b t
  include Monad with type 'b t := 'b t
end = struct
  type 'b t = A.t_for_any -> 'b

  (* Functor *)
  let fmap m f x = m (f x)

  (* Applicative *)
  let return x _ = x
  let apply f g x = f x (g x)

  (* Monad *)
  let bind g f x = f (g x) x
end
(** (a -> b) is an instance of Monad b - it behaves like the reader monad *)

implicit module Pair {A : Any} : Functor with type 'b t = A.t_for_any * 'b = struct
  type 'b t = A.t_for_any * 'b

  let fmap m (a, b) = (a, m b)
end

type ('a, 'b) const = Const of 'a

implicit module Const {A : Any}: sig
  include Functor with type 'b t = (A.t_for_any, 'b) const
end = struct
  type 'b t = (A.t_for_any, 'b) const
  let fmap _ (Const x) = (Const x)
end

type 'b identity = Identity of 'b

implicit module Identity: sig
  include Functor with type 'b t = 'b identity
  include Applicative with type 'b t := 'b t
  include Monad with type 'b t := 'b t
end = struct
  type 'b t = 'b identity
  let fmap f (Identity b) = Identity (f b)
  let return b = Identity b
  let apply (Identity f) (Identity x) = Identity (f x)
  let bind (Identity x) f = f x
end
