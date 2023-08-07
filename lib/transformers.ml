open Any
open Control

type ('r, 'am) readerT = ReaderT of ('r -> 'am)

let runReaderT {M : Monad} (ReaderT f) = f
let runReader (ReaderT r) = r
let lift {M : Monad} m = ReaderT (fun _ -> m)

module type MonadReader = sig
  include Monad
  type r
  val asks : (r -> 'b) -> 'b t
  val local : (r -> r) -> r t -> r t
end

let asks {M : MonadReader} = M.asks
let local {M : MonadReader} = M.local
let ask {M : MonadReader} = asks (fun r -> r)

implicit module ReaderT {R : Any} {M : Monad} : sig
  include Functor with type 'a t = (R.t_for_any, 'a M.t) readerT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include MonadReader with type 'a t := 'a t and type r = R.t_for_any
end = struct
  type 'a t = (R.t_for_any, 'a M.t) readerT
  (* Functor *)
  let fmap f (ReaderT m) = ReaderT (fun r -> fmap {M} f (m r))
  (* Applicative *)
  let return x = ReaderT (fun _ -> return {M} x)
  let apply (ReaderT ff) (ReaderT fx) = ReaderT (fun r -> apply {M} (ff r) (fx r))
  (* Monad *)
  let bind (ReaderT fx) ff = ReaderT (fun r -> bind {M} (fx r) (fun a -> runReaderT {M} (ff a) r))
  (* MonadReader *)
  type r = R.t_for_any
  let asks f = ReaderT (fun r -> M.return (f r))
  let local f (ReaderT g) = ReaderT (fun r -> g (f r))
end

implicit module Reader {R : Any} : sig
  include Functor with type 'a t = (R.t_for_any, 'a) readerT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include MonadReader with type 'a t := 'a t and type r = R.t_for_any
end = struct
  type 'a t = (R.t_for_any, 'a) readerT
  (* Functor *)
  let fmap f (ReaderT m) = ReaderT (fun r -> f (m r))
  (* Applicative *)
  let return x = ReaderT (fun _ -> x)
  let apply (ReaderT ff) (ReaderT fx) = ReaderT (fun r -> ff r (fx r))
  (* Monad *)
  let bind (ReaderT fx) ff = ReaderT (fun r -> runReader (ff (fx r)) r)
  (* MonadReader *)
  type r = R.t_for_any
  let asks f = ReaderT f
  let local f (ReaderT g) = ReaderT (fun r -> g (f r))
end
