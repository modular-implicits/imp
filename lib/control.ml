open Any

(* Functor, Applicative and Monad module types *)
module type Functor = sig
  type +'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end;;

let fmap {F : Functor} = F.fmap;;

module type Applicative = sig
  include Functor
  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end;;

let return {M : Applicative} = M.return;;
let apply {M : Applicative} = M.apply;;

let liftA2 {M : Applicative} (f : 'a -> 'b -> 'c) (x : 'a M.t) (y : 'b M.t) : 'c M.t = M.apply (M.fmap f x) y;;
let liftA3 {M : Applicative} (f : 'a -> 'b -> 'c -> 'd) (x : 'a M.t) (y : 'b M.t) (z : 'c M.t) : 'd M.t = M.apply (liftA2 {M} f x y) z;;


module type Monad = sig
  include Applicative
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end;;

let bind {M : Monad} = M.bind;;

(* Convenience functions *)

let pure {M : Monad} = M.return;;
let (>>=) {M : Monad} = M.bind;;

(* Define polymorphic monad functions *)

let join {M : Monad} (x : 'a M.t M.t) =
  M.bind x (fun x -> x);;

let (>>|) {M : Monad} (m : 'a M.t) (k : 'a -> 'b) =
  bind m (fun x -> return (k x));;

let sequence {M : Monad} (ms : 'a M.t list) =
  List.fold_right
    (fun m m' ->
       m  >>= fun x ->
       m' >>= fun xs ->
         return (x :: xs))
    ms
    (return [])

(* Create a functor, applicative, and monad from just return and bind,
   using default functor and applicative implementation *)
module Monad(M : sig
                   type +'a t
                   val return : 'a -> 'a t
                   val bind : 'a t -> ('a -> 'b t) -> 'b t
                 end): Monad with type 'a t = 'a M.t = struct
  type +'a t = 'a M.t
  (* Functor *)
  let fmap f m = M.bind m (fun x -> M.return (f x))
  (* Applicative *)
  let return = M.return
  let apply fm xm =
    M.bind fm (fun f ->
    M.bind xm (fun x ->
      M.return (f x)))
  (* Monad *)
  let bind = M.bind
end;;

module type Monad_plus = sig
  include Monad
  val mzero : 'a t
  val mplus : 'a t -> 'a t -> 'a t
end

module type Monad_fix = sig 
  include Monad 
  val mfix : ('a -> 'a t) -> 'a t
end

let mfix {M : Monad_fix} = M.mfix

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

let traverse {T : Traversable} = T.traverse

type ('a, 'b) either = Left of 'a | Right of 'b

implicit module Either {A : Any} : sig
  include Functor with type 'a t = (A.t, 'a) either
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include Foldable with type 'a t := 'a t
  include Traversable with type 'a t := 'a t
end = struct
  type 'a t = (A.t, 'a) either

  (* Functor *)
  let fmap f = function
    | (Left x) -> (Left x)
    | (Right x) -> (Right (f x))

  (* Applicative *)
  let return x = Right x
  let apply f x = match f, x with
    | (Left e), _ -> (Left e)
    | (Right f), x -> fmap f x

  (* Monad *)
  let bind x f = match x with
    | (Left e) -> (Left e)
    | (Right v) -> f v

  (* Foldable *)
  let fold f t acc = match t with
    | (Left _) -> acc
    | (Right x) -> f x acc

  (* Traversable *)
  let traverse (type a) (type b) {F : Applicative} (f : a -> b F.t)  : (A.t, a) either -> ((A.t, b) either) F.t = function
    | (Left x) -> F.return (Left x)
    | (Right y) -> F.fmap (fun x -> Right x) (f y)
end

implicit module Option: sig
  include Functor with type 'a t = 'a option
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include Monad_plus with type 'a t := 'a t
  include Monad_fix with type 'a t := 'a t
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

  let mfix (f : ('a -> 'a option)) : 'a option = 
    let rec loop x = 
      match f x with
      | None -> raise (Failure "Reached None")
      | Some x' -> if x = x' then Some x else loop x'
    in loop (Obj.magic ())


  (* Foldable *)
  let fold f t acc = match t with
    | None -> acc
    | Some x -> f x acc

  (* Traversable *)
  let traverse (type a) (type b) {F : Applicative} (f : a -> b F.t)  : a option -> b option F.t = function
    | None -> F.return None
    | Some x -> F.fmap (fun x -> Some x) (f x)
end

let fix (f : 'a -> 'a) : 'a = 
  let rec loop x = let x' = f x in
     if x = x' then x else loop x' in loop (Obj.magic ())

exception Head
exception Tail

implicit module List : sig
  include Functor with type 'a t = 'a list
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include Monad_plus with type 'a t := 'a t
  include Monad_fix with type 'a t := 'a t
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

  let head = function
    | (x::_) -> x
    | [] -> raise Head
  let tail = function
    | (_::xs) -> xs
    | [] -> raise Tail

  (* Monad_fix *)
  let rec mfix (f : 'a -> 'a list) : 'a list = match fix (fun x -> f (head x)) with 
    | [] -> []
    | (x::_) -> x :: mfix (fun x -> tail (f x))

  (* Foldable *)
  let fold f xs a = List.fold_left (fun x y -> f y x) a xs

  (* Traversable *)
  let traverse {F : Applicative} f t =
    let cons x ys = F.apply (F.fmap (fun x xs -> x :: xs) (f x)) ys in
    List.fold_right cons t (F.return [])
end

implicit module Function {A : Any} : sig
  include Functor with type 'b t = A.t -> 'b
  include Applicative with type 'b t := 'b t
  include Monad with type 'b t := 'b t
end = struct
  type 'b t = A.t -> 'b

  (* Functor *)
  let fmap m f x = m (f x)

  (* Applicative *)
  let return x _ = x
  let apply f g x = f x (g x)

  (* Monad *)
  let bind g f x = f (g x) x

end
(** (a -> b) is an instance of Monad b - it behaves like the reader monad *)

implicit module Pair {A : Any} : Functor with type 'b t = A.t * 'b = struct
  type 'b t = A.t * 'b

  let fmap m (a, b) = (a, m b)
end

type ('a, 'b) const = Const of 'a

implicit module Const {A : Any}: sig
  include Functor with type 'b t = (A.t, 'b) const
end = struct
  type 'b t = (A.t, 'b) const
  let fmap _ (Const x) = (Const x)
end

implicit module Const_Applicative {A: Data.Monoid}: Applicative with type 'b t = (A.t, 'b) const
= struct
  type 'b t = (A.t, 'b) const
  let fmap _ (Const x) = (Const x)
  let return _ = Const (Data.Monoid.empty ())
  let apply (Const a) (Const a') = Const (Data.Monoid.append a a')
end

type 'b identity = Identity of 'b

let runIdentity (Identity x) = x

implicit module Identity: sig
  include Functor with type 'b t = 'b identity
  include Applicative with type 'b t := 'b t
  include Monad with type 'b t := 'b t
include Monad_fix with type 'b t := 'b t
end = struct
  type 'b t = 'b identity
  let fmap f (Identity b) = Identity (f b)
  let return b = Identity b
  let apply (Identity f) (Identity x) = Identity (f x)
  let bind (Identity x) f = f x
  let mfix f = let f' = (fun x -> runIdentity (f x)) in Identity (fix f')
end

type 'a pair = Pair of 'a * 'a

let fst (Pair (x, _)) = x
let snd (Pair (_, y)) = y

implicit module PairSame : sig 
  include Functor with type 'a t = 'a pair
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
end = struct
  type 'a t = 'a pair
  let fmap f (Pair (x, y)) = Pair (f x, f y)
  let return x = Pair (x, x)
  let apply (Pair (f, g)) (Pair (x, y)) = Pair (f x, g y)
  let bind (Pair (x, y)) f = Pair (fst (f x), snd (f y))
end
