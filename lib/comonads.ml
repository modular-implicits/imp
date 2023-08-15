open Any
open Data
open Control

module type CoMonad = sig
  include Functor
  val extract : 'a t -> 'a
  val duplicate : 'a t -> 'a t t
end

let extract {C : CoMonad} x = C.extract x
let duplicate {C : CoMonad} x = C.duplicate x
let extend {C : CoMonad} f x = C.fmap f (C.duplicate x)

implicit module CoIdentity : sig 
include Functor with type 'b t = 'b identity
include CoMonad with type 'b t := 'b t
end = struct 
  type 'b t = 'b identity
  (* Strange that I have to re-define fmap? Is it worth considering every Functor/Applicative/Monad has its own implpcit?*)
  let fmap f (Identity b) = Identity (f b)
  let extract (Identity b) = b
  let duplicate (Identity b) = Identity (Identity b)
end 

implicit module CoNonEmpty : sig 
  include Functor with type 'b t = 'b nonEmpty
  include CoMonad with type 'b t := 'b t
  end = struct 
    type 'b t = 'b nonEmpty
    (* Strange that I have to re-define fmap? Is it worth considering every Functor/Applicative/Monad has its own implpcit?*)
    let fmap f (NonEmpty (b, bs)) = NonEmpty (f b, List.fmap f bs)
    let extract (NonEmpty (b, _)) = b
    let duplicate ((NonEmpty (b, bs)) : 'b nonEmpty) : 'b nonEmpty nonEmpty = NonEmpty (NonEmpty (b, bs), List.fmap (fun x -> NonEmpty (x, bs)) bs)
  end

implicit module CoPair {A : Any} : sig
include Functor with type 'b t = A.t * 'b
include CoMonad with type 'b t := 'b t
end = struct
  type 'b t = A.t * 'b
  let fmap f (a, b) = (a, f b)
  let extract (_, b) = b
  let duplicate (a, b) = (a, (a, b))
end
