open Any

module type Eq = sig
  type t
  val ( = ) : t -> t -> bool
end

module Eq = struct
  let ( = ) {M : Eq} = M.(=)
end

module type Ord = sig
  type t
  val compare : t -> t -> int
end

module Ord = struct
  let ( =  ) {M : Ord} a b = M.compare a b =  0
  let ( <> ) {M : Ord} a b = M.compare a b <> 0
  let ( <  ) {M : Ord} a b = M.compare a b <  0
  let ( <= ) {M : Ord} a b = M.compare a b <= 0
  let ( >  ) {M : Ord} a b = M.compare a b >  0
  let ( >= ) {M : Ord} a b = M.compare a b >= 0
  let compare {M : Ord} = M.compare
end

module type Num = sig
  type t
  val zero : t
  val one : t
  val of_int : int -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val (~- ) : t -> t
end

module Num = struct
  let zero   {M : Num} () = M.zero
  let one    {M : Num} () = M.one
  let of_int {M : Num} = M.of_int
  let ( + )  {M : Num} = M.( + )
  let ( - )  {M : Num} = M.( - )
  let ( * )  {M : Num} = M.( * )
  let ( / )  {M : Num} = M.( / )
  let (~- )  {M : Num} = M.(~- )
end

module type Bounded = sig
  type t
  val bounds : t * t
end

module Bounded = struct
  let bounds {M : Bounded} () = M.bounds
end

module type Enum = sig
  include Ord
  val succ : t -> t
  val pred : t -> t
end

module Enum = struct
  let succ {M : Enum} x = M.succ x
  let pred {M : Enum} x = M.pred x

  let rec fold_enum_to
    : {M : Enum} -> M.t -> M.t -> (M.t -> 'a -> 'a) -> 'a -> 'a
    = fun {M : Enum} a b f acc ->
    if M.compare a b < 0 then
      fold_enum_to (M.succ a) b f (f a acc)
    else
      acc

  let rec fold_enum_downto
    : {M : Enum} -> M.t -> M.t -> (M.t -> 'a -> 'a) -> 'a -> 'a
    = fun {M : Enum} a b f acc ->
    if M.compare b a < 0 then
      fold_enum_downto (M.pred a) b f (f a acc)
    else
      acc

  let list_enum_to {M : Enum} (a : M.t) b =
    List.rev (fold_enum_to a b (fun x acc -> x :: acc) [])

  let list_enum_downto {M : Enum} (a : M.t) b =
    List.rev (fold_enum_downto a b (fun x acc -> x :: acc) [])
end

module type Monoid = sig
  type t
  val empty : t
  val append : t -> t -> t
end

module Monoid = struct
  let empty {M : Monoid} () = M.empty
  let append {M : Monoid} = M.append
end

(* Instances *)

implicit module Int = struct
  type t = int

  (* Eq *)
  let ( = ) (a : int) b = a = b

  (* Ord *)
  let compare (a : int) b = compare a b

  (* Num *)
  let zero = 0
  let one  = 1
  let of_int x = x
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let (~- ) = (~- )

  (* Bounded *)
  let bounds = (min_int, max_int)

  (* Enum *)
  let succ = succ
  let pred = pred

  (* Monoid, addition *)
  let empty = 0
  let append = (+)
end

implicit module Float = struct
  type t = float

  (* Eq *)
  let ( = ) (a : float) b = a = b

  (* Ord *)
  let compare (a : float) b = compare a b

  (* Num *)
  let zero = 0.
  let one  = 1.
  let of_int = float_of_int
  let ( + ) = ( +.)
  let ( - ) = ( -.)
  let ( * ) = ( *.)
  let ( / ) = ( /.)
  let (~- ) = (~-.)

  (* Bounded *)
  let bounds = (neg_infinity, infinity)

  (* Monoid, addition *)
  let empty = 0.
  let append = (+.)
end

implicit module Bool = struct
  type t = bool

  (* Eq *)
  let ( = ) (a : bool) b = a = b

  (* Ord *)
  let compare (a : bool) b = compare a b

  (* Bounded *)
  let bounds = (neg_infinity, infinity)

  (* Enum *)
  let succ = function
    | false -> true
    | true  -> invalid_arg "Bool.succ"

  let pred = function
    | true  -> false
    | false -> invalid_arg "Bool.pred"

  (* Monoid, addition *)
  let empty = false
  let append = (||)
end

implicit module Char = struct
  type t = char

  (* Eq *)
  let ( = ) (a : char) b = a = b

  (* Ord *)
  let compare (a : char) b = compare a b

  (* Bounded *)
  let bounds = ('\000', '\255')

  (* Enum *)
  let succ = function
    | '\255' -> invalid_arg "Char.succ"
    | n -> Char.chr (succ (Char.code n))

  let pred = function
    | '\000' -> invalid_arg "Char.succ"
    | n -> Char.chr (pred (Char.code n))
end

implicit module String = struct
  type t = string

  (* Eq *)
  let ( = ) (a : string) b = a = b

  (* Ord *)
  let compare (a : string) b = compare a b

  (* Monoid *)
  let empty = ""
  let append = (^)
end

module List {A : Any} : Monoid with type t = A.t_for_any list = struct
  type t = A.t_for_any list

  (* Monoid *)
  let empty = []
  let append = (@)
end

implicit module Int32 = struct
  type t = int32

  (* Eq *)
  let ( = ) (a : int32) b = a = b

  (* Ord *)
  let compare = Int32.compare

  (* Num *)
  let zero = 0l
  let one  = 1l
  let of_int = Int32.of_int
  let ( + ) = Int32.add
  let ( - ) = Int32.sub
  let ( * ) = Int32.mul
  let ( / ) = Int32.div
  let (~- ) = Int32.neg

  (* Bounded *)
  let bounds = (Int32.min_int, Int32.max_int)

  (* Enum *)
  let succ = Int32.succ
  let pred = Int32.pred

  (* Monoid, addition *)
  let empty = 0l
  let append = Int32.add
end

implicit module Int64 = struct
  type t = int64

  (* Eq *)
  let ( = ) (a : int64) b = a = b

  (* Ord *)
  let compare = Int64.compare

  (* Num *)
  let zero = 0L
  let one  = 1L
  let of_int = Int64.of_int
  let ( + ) = Int64.add
  let ( - ) = Int64.sub
  let ( * ) = Int64.mul
  let ( / ) = Int64.div
  let (~- ) = Int64.neg

  (* Bounded *)
  let bounds = (Int64.min_int, Int64.max_int)

  (* Enum *)
  let succ = Int64.succ
  let pred = Int64.pred

  (* Monoid, addition *)
  let empty = 0L
  let append = Int64.add
end

implicit module Nativeint = struct
  type t = nativeint

  (* Eq *)
  let ( = ) (a : nativeint) b = a = b

  (* Ord *)
  let compare = Nativeint.compare

  (* Num *)
  let zero = 0L
  let one  = 1L
  let of_int = Nativeint.of_int
  let ( + ) = Nativeint.add
  let ( - ) = Nativeint.sub
  let ( * ) = Nativeint.mul
  let ( / ) = Nativeint.div
  let (~- ) = Nativeint.neg

  (* Bounded *)
  let bounds = (Nativeint.min_int, Nativeint.max_int)

  (* Enum *)
  let succ = Nativeint.succ
  let pred = Nativeint.pred

  (* Monoid, addition *)
  let empty = 0L
  let append = Nativeint.add
end

implicit module Unit = struct
  type t = unit

  (* Monoid *)
  let empty = ()
  let append () () = ()
end
