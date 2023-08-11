open Any
open Control

type ('r, +'am) readerT
(** The Reader and ReaderT monads.
    For ReaderT, transforming an inner monad m, use ('r, 'a m) readerT
    For Reader, with no inner monad, use ('r 'a) readerT
 *)

val runReaderT : {M : Monad} -> ('r, 'a M.t) readerT -> 'r -> 'a M.t
(** Run a readerT monad using the given read-only state value *)

val runReader : ('r, 'a) readerT -> 'r -> 'a
(** Run a reader monad using the given read-only state value *)

val liftReader : {M : Monad} -> 'a M.t -> ('r, 'a M.t) readerT
(** Lift a value from the inner monad m to the transformed monad, ('r 'a m) readerT *)

module type MonadReader = sig
  include Monad
  type r
  val asks : (r -> 'b) -> 'b t
  val local : (r -> r) -> r t -> r t
end
(** MonadReader allows the functions ask, asks, and local
    to be generalised to both reader and readerT
    (both those types implement MonadReader)
 *)

val asks : {M : MonadReader} -> (M.r -> 'b) -> 'b M.t
(** asks reads the state, applying a given transformation to it *)

val local : {M : MonadReader} -> (M.r -> M.r) -> M.r M.t -> M.r M.t
(** local runs the given reader monad using a transformed version of the state. *)

val ask : {M : MonadReader} -> M.r M.t
(** ask reads the state of the monad *)

implicit module ReaderT {R : Any} {M : Monad} : sig
  include Functor with type 'a t = (R.t_for_any, 'a M.t) readerT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include MonadReader with type 'a t := 'a t and type r = R.t_for_any
end

implicit module Reader {R : Any} : sig
  include Functor with type 'a t = (R.t_for_any, 'a) readerT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include MonadReader with type 'a t := 'a t and type r = R.t_for_any
end

type ('s, +'asm) stateT
(** The State and StateT monads.
    For StateT, transforming an inner monad m, use ('s, 'a m) stateT
    For State, with no inner monad, use ('s 'a) stateT
 *)

val runStateT : {M : Monad} -> ('s, 'a M.t) stateT -> 's -> 'a M.t
(** Run a stateT monad using the given initial state value.
    Returns the computation's result, and the final state
 *)

val runState : ('s, 'a * 's) stateT -> 's -> 'a * 's
(** Run a state monad using the given initial state value.
    Returns the computation's result, and the final state
 *)

val liftState : {M : Monad} -> 'a M.t -> ('s, ('a * 's) M.t) stateT
(** Lift a value from the inner monad m to the transformed monad, ('s 'a m) stateT *)

module type MonadState = sig
  include Monad
  type s
  val get : s t
  val put : s -> unit t
end
(** MonadState allows the functions get and put
    to be generalised to both state and stateT
    (both those types implement MonadState)
 *)

val get : {M: MonadState} -> M.s M.t
val put : {M: MonadState} -> M.s -> unit M.t
val gets : {M: MonadState} -> (M.s -> 't) -> 't M.t
val modify : {M : MonadState} -> (M.s -> M.s) -> unit M.t
(** modify applies a given transformation to the state *)

implicit module StateT {S : Any} {M : Monad} : sig
  include Functor with type 'a t = (S.t_for_any, ('a * S.t_for_any) M.t) stateT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include MonadState with type 'a t := 'a t and type s = S.t_for_any
end

implicit module State {S : Any} : sig
  include Functor with type 'a t = (S.t_for_any, 'a * S.t_for_any) stateT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include MonadState with type 'a t := 'a t and type s = S.t_for_any
end
