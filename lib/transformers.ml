open Any
open Control

module type ReaderT = sig
  type r
  type 'b m
  module R : Any with type t_for_any = r
  module M : Monad with type 'b t = 'b m
  type 'a t = ReaderT of (R.t_for_any -> 'a M.t)
end

module ReaderT (R : Any) (M : Monad) : ReaderT = struct
  module R = R
  module M = M
  type r = R.t_for_any
  type 'b m = 'b M.t
  type 'a t = ReaderT of (R.t_for_any -> 'a M.t)
end

(* val runReaderT : 'a t -> R.t_for_any -> 'a M.t *)
let runReaderT {T : ReaderT} (T.ReaderT f) = f
let lift {T : ReaderT} m = T.ReaderT (fun _ -> m)
let ask {T : ReaderT} = T.ReaderT (fun r -> return {T.M} r)
let asks {T : ReaderT} f = T.ReaderT (fun r -> return {T.M} (f r))
let local {T : ReaderT} f (T.ReaderT g) = T.ReaderT (fun r -> g (f r))

implicit module ReaderT_impl {T : ReaderT} : sig
  include Functor with type 'a t = 'a T.t
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
end = struct
  open T
  type 'a t = 'a T.t
  (* Functor *)
  let fmap f (ReaderT m) = ReaderT (fun r -> fmap {M} f (m r))
  (* Applicative *)
  let return x = ReaderT (fun _ -> return {M} x)
  let apply (ReaderT ff) (ReaderT fx) = ReaderT (fun r -> apply {M} (ff r) (fx r))
  (* Monad *)
  let bind (ReaderT fx) ff = ReaderT (fun r -> bind {M} (fx r) (fun a -> runReaderT (ff a) r))
end
