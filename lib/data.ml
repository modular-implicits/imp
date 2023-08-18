open Any

module type Eq = sig
  type t
  val ( = ) : t -> t -> bool
end

module Eq = struct
  let ( = ) {M : Eq} = M.(=)
end

type ordering = LT | GT | EQ

module type Ord = sig
  type t
  val compare : t -> t -> ordering
end

module Ord = struct
  let translateCompare (compare : 'a -> 'a -> int) =
    fun x y -> let n = compare x y in
    if n < 0 then LT
    else if n = 0 then EQ
    else (* if n > 0 then *) GT
  let ( <  ) {M : Ord} a b = M.compare a b = LT
  let ( <= ) {M : Ord} a b = M.compare a b <> GT
  let ( >  ) {M : Ord} a b = M.compare a b = GT
  let ( >= ) {M : Ord} a b = M.compare a b <> LT
  let ( =  ) {M : Ord} a b = M.compare a b =  EQ
  let ( <> ) {M : Ord} a b = M.compare a b <> EQ
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
  let zeroG {M : Num} () = M.zero
  let one    {M : Num} () = M.one
  let oneG   {M : Num} () = M.one
  let of_int {M : Num} = M.of_int
  let of_intG {M : Num} = M.of_int
  let ( + )  {M : Num} = M.( + )
  let ( +@ )  {M : Num} = M.( + )
  let ( - )  {M : Num} = M.( - )
  let ( -@ )  {M : Num} = M.( - )
  let ( * )  {M : Num} = M.( * )
  let ( *@ )  {M : Num} = M.( * )
  let ( / )  {M : Num} = M.( / )
  let ( /@ )  {M : Num} = M.( / )
  let (~- )  {M : Num} = M.(~- )
  let (~-@ )  {M : Num} = M.(~- )
end

module type Fractional = sig
include Num
val of_fractional : float -> t
val fdiv : t -> t -> t
end

module Fractional = struct
  let of_fractional {M : Fractional} = M.of_fractional
  let of_fractionalG {M : Fractional} = M.of_fractional
  let fdiv  {M : Fractional} = M.fdiv
  let fdivG  {M : Fractional} = M.fdiv
end

module type Floating = sig 
include Fractional
  val pi : t
  val exp : t -> t
  val log : t -> t
  val sin : t -> t
  val cos : t -> t
  val asin : t -> t
  val acos : t -> t
  val atan : t -> t
  val sinh : t -> t
  val cosh : t -> t
  (*
  val asinh : t -> t
  val acosh : t -> t
  val atanh : t -> t
  *)
end

module Floating = struct
  let pi {M : Floating}= M.pi
  let piG {M : Floating} = M.pi
  let exp {M : Floating} = M.exp
  let expG {M : Floating} = M.exp
  let log {M : Floating} = M.log
  let logG {M : Floating} = M.log
  let sin {M : Floating} = M.sin
  let sinG {M : Floating} = M.sin
  let cos {M : Floating} = M.cos
  let cosG {M : Floating} = M.cos
  let asin {M : Floating} = M.asin
  let asinG {M : Floating} = M.asin
  let acos {M : Floating} = M.acos
  let acosG {M : Floating} = M.acos
  let atan {M : Floating} = M.atan
  let atanG {M : Floating} = M.atan
  let sinh {M : Floating} = M.sinh
  let sinhG {M : Floating} = M.sinh
  let cosh {M : Floating} = M.cosh
  let coshG {M : Floating} = M.cosh
  (*
  let asinh {M : Floating} = M.asinh
  let acosh {M : Floating} = M.acosh
  let atanh {M : Floating} = M.atanh
  *)
end


module type Bounded = sig
  type t
  val bounds : t * t
end

module Bounded = struct
  let bounds {M : Bounded} () = M.bounds
  let boundsG {M : Bounded} () = M.bounds
end

module type Enum = sig
  include Ord
  val succ : t -> t
  val pred : t -> t
end

module Enum = struct
  let succ {M : Enum} x = M.succ x
  let succG {M : Enum} x = M.succ x
  let pred {M : Enum} x = M.pred x
  let predG {M : Enum} x = M.pred x

  let rec fold_enum_to
    : {M : Enum} -> M.t -> M.t -> (M.t -> 'a -> 'a) -> 'a -> 'a
    = fun {M : Enum} a b f acc ->
    if M.compare a b = LT then
      fold_enum_to (M.succ a) b f (f a acc)
    else
      acc
   let fold_enum_to : {M : Enum} -> M.t -> M.t -> (M.t -> 'a -> 'a) -> 'a -> 'a = fold_enum_to

  let rec fold_enum_downto
    : {M : Enum} -> M.t -> M.t -> (M.t -> 'a -> 'a) -> 'a -> 'a
    = fun {M : Enum} a b f acc ->
    if M.compare b a = LT then
      fold_enum_downto (M.pred a) b f (f a acc)
    else
      acc
  let fold_enum_downto : {M : Enum} -> M.t -> M.t -> (M.t -> 'a -> 'a) -> 'a -> 'a = fold_enum_downto

  let list_enum_to {M : Enum} (a : M.t) b =
    List.rev (fold_enum_to a b (fun x acc -> x :: acc) [])
  
  let list_enum_to {M : Enum} = list_enum_to {M}

  let list_enum_downto {M : Enum} (a : M.t) b =
    List.rev (fold_enum_downto a b (fun x acc -> x :: acc) [])
  
  let list_enum_downto {M : Enum} = list_enum_downto {M}
end

module type Monoid = sig
  type t
  val empty : t
  val append : t -> t -> t
end

module Monoid = struct
  let empty {M : Monoid} () = M.empty
  let emptyG {M : Monoid} () = M.empty
  let append {M : Monoid} = M.append
  let appendG {M : Monoid} = M.append

  type 'a first = { first: 'a option }
end

(* Instances *)

implicit module First {A: Any} : Monoid with type t = A.t Monoid.first = struct
  open Monoid
  type t = A.t first
  let empty = { first = None }
  let append x y = match x with
    | { first = Some _ } -> x
    | { first = None } -> y
end

implicit module Int: sig
  include Eq with type t = int
  include Ord with type t := t
  include Num with type t := t
  include Bounded with type t := t
  include Enum with type t := t
end = struct
  type t = int

  (* Eq *)
  let ( = ) (a : int) b = a = b

  (* Ord *)
  let compare (a : int) b = Ord.translateCompare compare a b

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
end

implicit module Float: sig
  include Eq with type t = float
  include Ord with type t := t
  include Num with type t := t
  include Bounded with type t := t
  include Fractional with type t := t
  include Floating with type t := t
end = struct
  type t = float

  (* Eq *)
  let ( = ) (a : float) b = a = b

  (* Ord *)
  let compare (a : float) b = Ord.translateCompare compare a b

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

  (* Fractional *)

  let of_fractional x = x
  let fdiv = ( /. )

  (* Floating *)

  let pi = 4. *. atan 1.
  let exp = exp
  let log = log
  let sin = sin
  let cos = cos
  let asin = asin
  let acos = acos
  let atan = atan
  let sinh = sinh
  let cosh = cosh

end

implicit module Bool: sig
  include Eq with type t = bool
  include Ord with type t := t
  include Bounded with type t := t
  include Enum with type t := t
end = struct
  type t = bool

  (* Eq *)
  let ( = ) (a : bool) b = a = b

  (* Ord *)
  let compare (a : bool) b = Ord.translateCompare compare a b

  (* Bounded *)
  let bounds = (false, true)

  (* Enum *)
  let succ = function
    | false -> true
    | true  -> invalid_arg "Bool.succ"

  let pred = function
    | true  -> false
    | false -> invalid_arg "Bool.pred"
end

implicit module Char: sig
  include Eq with type t = char
  include Ord with type t := t
  include Bounded with type t := t
  include Enum with type t := t
end = struct
  type t = char

  (* Eq *)
  let ( = ) (a : char) b = a = b

  (* Ord *)
  let compare (a : char) b = Ord.translateCompare compare a b

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

implicit module String: sig
  include Eq with type t = string
  include Ord with type t := t
  include Monoid with type t := t
end = struct
  type t = string

  (* Eq *)
  let ( = ) (a : string) b = a = b

  (* Ord *)
  let compare (a : string) b = Ord.translateCompare compare a b

  (* Monoid *)
  let empty = ""
  let append = (^)
end

module List {A : Any} : Monoid with type t = A.t list = struct
  type t = A.t list

  (* Monoid *)
  let empty = []
  let append = (@)
end

implicit module Int32: sig
  include Eq with type t = int32
  include Ord with type t := t
  include Num with type t := t
  include Bounded with type t := t
  include Enum with type t := t
end = struct
  type t = int32

  (* Eq *)
  let ( = ) (a : int32) b = a = b

  (* Ord *)
  let compare a b = Ord.translateCompare Int32.compare a b

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
end

implicit module Int64: sig
  include Eq with type t = int64
  include Ord with type t := t
  include Num with type t := t
  include Bounded with type t := t
  include Enum with type t := t
end = struct
  type t = int64

  (* Eq *)
  let ( = ) (a : int64) b = a = b

  (* Ord *)
  let compare a b = Ord.translateCompare Int64.compare a b

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
end

implicit module Nativeint: sig
  include Eq with type t = nativeint
  include Ord with type t := t
  include Num with type t := t
  include Bounded with type t := t
  include Enum with type t := t
end = struct
  type t = nativeint

  (* Eq *)
  let ( = ) (a : nativeint) b = a = b

  (* Ord *)
  let compare a b = Ord.translateCompare Nativeint.compare a b

  (* Num *)
  let zero = Nativeint.of_int 0
  let one  = Nativeint.of_int 1
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
end

implicit module Unit: Monoid with type t = unit = struct
  type t = unit

  (* Monoid *)
  let empty = ()
  let append () () = ()
end

type 'b nonEmpty = NonEmpty of 'b * 'b list
