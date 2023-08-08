open Any
open Control

type ('r, 'am) readerT

val runReaderT : {M : Monad} -> ('r, 'a M.t) readerT -> 'r -> 'a M.t
val runReader : ('r, 'a) readerT -> 'r -> 'a
val lift : {M : Monad} -> 'a M.t -> ('r, 'a M.t) readerT

module type MonadReader = sig
  include Monad
  type r
  val asks : (r -> 'b) -> 'b t
  val local : (r -> r) -> r t -> r t
end

val asks : {M : MonadReader} -> (M.r -> 'b) -> 'b M.t
val local : {M : MonadReader} -> (M.r -> M.r) -> M.r M.t -> M.r M.t
val ask : {M : MonadReader} -> M.r M.t

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
