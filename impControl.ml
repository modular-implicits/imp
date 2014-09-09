(* Funtor, Idiom and Monad module types *)
module type Functor = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end;;

let fmap (implicit F : Functor) = F.fmap;;

module type Idiom = sig
  include Functor
  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end;;

let return (implicit M : Monad) = M.return;;
let apply (implicit M : Idiom) = M.apply;;

module type Monad = sig
  include Idiom
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end;;

let bind (implicit M : Monad) = M.bind;;

(* Convenience functions *)

let pure (implicit M : Monad) = M.return;;
let (>>=) (implicit M : Monad) = M.bind;;

(* Define polymorphic monad functions *)

let join (implicit M : Monad) (x : 'a M.t M.t) =
  M.bind x (fun x -> x);;

let (>>|) (implicit M : Monad) (m : 'a M.t) (k : 'a -> 'b) =
  bind m (fun x -> return (k x));;

let sequence (implicit M : Monad) (ms : 'a M.t list) =
  fold_right
    (fun m m' ->
       m >>= fun x ->
       m' >>= fun xs ->
         return (x :: xs))
    (return [])
    ms

(* Create a monad using default functor and idiom implementation *)
module Monad(M : sig
                   type 'a t
                   val return : 'a -> 'a t
                   val bind : 'a t -> ('a -> 'b t) -> 'b t
                 end) = struct
  (* Functor *)
  let fmap f m = M.bind m (fun x -> M.return (f x))
  (* Idiom *)
  let return = M.return
  let apply fm xm =
    M.bind fm (fun f ->
    M.bind xm (fun x ->
      M.return (f x)))
  (* Monad *)
  let bind = M.bind
end;;

(* Monad for option *)
implicit module MonadOption = Monad(struct
  type 'a t = 'a option
  let return x = Some x
  let bind x f = match x with
    | None -> None
    | Some x -> f x
end);;

(* Monad for list *)
implicit module MonadList = Monad(struct
  type 'a t = 'a list
  let return x = [x]
  let bind x f =
    let rec aux acc = function
      | x :: xs -> aux (x @ acc) xs
      | [] -> acc
    in
      aux [] (List.rev_map f x)
end);;

module type Monad_plus = sig
  include Monad
  val mzero : unit -> 'a t
  val mplus : 'a t -> 'a t -> 'a t
end

module Monad_plus = struct
  let mzero (implicit M : Monad_plus) = M.mzero
  let mplus (implicit M : Monad_plus) = M.mplus

  let mguard (implicit M : Monad_plus) b =
    if b then
      M.return ()
    else
      M.mzero ()
end

module type Foldable = sig
  type 'a t
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Foldable = struct
  let fold (implicit F : Foldable) = F.fold
end

module type Traversable = sig
  include Functor
  val traverse : (implicit F : Applicative) ->
                 ('a -> 'b F.t) -> 'a t -> 'b t F.t
end

module Traversable = struct
  let traverse (implicit T : Traversable) = T.traverse
end

implicit module Option = struct
  type 'a t = 'a option

  (* Functor *)
  let fmap f = function
    | None -> None
    | Some a -> Some (f a)

  (* Applicative *)
  let pure x = Some x
  let apply f x = match f, x with
    | Some f, Some x -> Some (f x)
    | _, _ -> None

  (* Monad *)
  let return x = Some x
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
  let traverse (implicit F : Applicative) f = function
    | None -> F.pure None
    | Some x -> F.apply (F.pure (fun x -> Some x)) (f x)
end

implicit module List = struct
  type 'a t = 'a list

  (* Functor *)
  let fmap = List.map

  (* Monad *)
  let return x = [x]
  let bind x f =
    let rec aux acc = function
      | x :: xs -> aux (x @ acc) xs
      | [] -> acc in
    aux [] (List.rev_map f x)

  (* Applicative *)
  let pure x = [x]
  let apply fs xs = bind fs (bind xs)

  (* Monad_plus *)
  let mzero = []
  let mplus = (@)

  (* Foldable *)
  let rec fold f t acc = match t with
    | [] -> acc
    | x :: xs -> fold f xs (f x acc)

  (* Traversable *)
  let traverse (implicit F : Applicative) f t =
    let cons = F.pure (fun x xs -> x :: xs) in
    let cons x xs = F.apply (F.apply cons (F.pure (f x))) xs in
    fold cons t (F.pure [])
end
