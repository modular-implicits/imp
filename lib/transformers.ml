open Any
open Control

type ('r, 'am) readerT = ReaderT of ('r -> 'am)

(* val runReaderT : 'a t -> R.t_for_any -> 'a M.t *)
let runReaderT {R : Any} {M : Monad} (ReaderT f) = f
let lift {R : Any} {M : Monad} m = ReaderT (fun _ -> m)
let ask {R : Any} {M : Monad} = ReaderT (fun r -> return {M} r)
let asks {R : Any} {M : Monad} f = ReaderT (fun r -> return {M} (f r))
let local {R : Any} {M : Monad} f (ReaderT g) = ReaderT (fun r -> g (f r))

implicit module ReaderT {R : Any} {M : Monad} : sig
  include Functor with type 'a t = (R.t_for_any, 'a M.t) readerT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
end = struct
  type 'a t = (R.t_for_any, 'a M.t) readerT
  (* Functor *)
  let fmap f (ReaderT m) = ReaderT (fun r -> fmap {M} f (m r))
  (* Applicative *)
  let return x = ReaderT (fun _ -> return {M} x)
  let apply (ReaderT ff) (ReaderT fx) = ReaderT (fun r -> apply {M} (ff r) (fx r))
  (* Monad *)
  let bind (ReaderT fx) ff = ReaderT (fun r -> bind {M} (fx r) (fun a -> runReaderT {R} {M} (ff a) r))
end
