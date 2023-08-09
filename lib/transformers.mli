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

val lift : {M : Monad} -> 'a M.t -> ('r, 'a M.t) readerT
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
