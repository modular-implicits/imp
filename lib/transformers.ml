open Any
open Control

type ('r, 'am) readerT = ReaderT of ('r -> 'am)

let runReaderT {M : Monad} (ReaderT f) = f
let runReader (ReaderT r) = r

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

type ('s, 'asm) stateT = StateT of ('s -> 'asm)

let runStateT {M : Monad} (StateT f) = f
let runState (StateT s) = s

module type MonadState = sig
  include Monad
  type s
  val get : s t
  val put : s -> unit t
end

implicit module StateT {S : Any} {M : Monad} : sig
  include Functor with type 'a t = (S.t_for_any, ('a * S.t_for_any) M.t) stateT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include MonadState with type 'a t := 'a t and type s = S.t_for_any
end = struct
  type 'a t = (S.t_for_any, ('a * S.t_for_any) M.t) stateT
  (* Functor *)
  let fmap (f: 'a -> 'b) (StateT m: 'a t): 'b t = StateT (
    fun s -> M.fmap (fun (x, s') -> (f x, s')) (m s)
  )
  (* Applicative *)
  let return x = StateT (fun s -> M.return (x, s))
  let apply (StateT ff) (StateT fx) = StateT (
    fun s -> M.bind (ff s) @@
    fun (f, s') -> M.bind (fx s') @@
    fun (x, s'') -> M.return (f x, s'')
  )
  (* Monad *)
  let bind (StateT fx) ff = StateT (
    fun s -> M.bind (fx s) @@
    fun (a, s') -> runStateT {M} (ff a) s'
  )
  (* MonadState *)
  type s = S.t_for_any
  let get = StateT (fun s -> M.return (s, s))
  let put s = StateT (fun _ -> M.return ((), s))
end

let get {M: MonadState} = M.get
let put {M: MonadState} = M.put

let gets {M: MonadState} f = fmap f M.get
let modify {M: MonadState} f = bind M.get (fun s -> M.put (f s))

implicit module State {S : Any} : sig
  include Functor with type 'a t = (S.t_for_any, 'a * S.t_for_any) stateT
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
  include MonadState with type 'a t := 'a t and type s = S.t_for_any
end = struct
  type 'a t = (S.t_for_any, 'a * S.t_for_any) stateT
  (* Functor *)
  let fmap (f: 'a -> 'b) (StateT m: 'a t): 'b t = StateT (
    fun s ->
      let (x, s') = m s in (f x, s')
  )
  (* Applicative *)
  let return x = StateT (fun s -> (x, s))
  let apply (StateT ff) (StateT fx) = StateT (
    fun s ->
      let (f, s') = ff s in
      let (x, s'') = fx s' in
      (f x, s'')
  )
  (* Monad *)
  let bind (StateT fx) ff = StateT (
    fun s -> let (a, s') = fx s in
    runState (ff a) s'
  )
  (* MonadState *)
  type s = S.t_for_any
  let get = StateT (fun s -> (s, s))
  let put s = StateT (fun _ -> ((), s))
end

module type MonadTrans = sig
  module M : Monad
  module MT : Monad
  val lift : 'a M.t -> 'a MT.t
end

let lift {T: MonadTrans} = T.lift

implicit module ReaderT_Trans {R: Any} {M: Monad}: MonadTrans
  with type 'a MT.t = (R.t_for_any, 'a M.t) readerT
  and type 'a M.t = 'a M.t
= struct
  module M = M
  module MT = ReaderT {R} {M}
  let lift m = ReaderT (fun _ -> m)
end

implicit module StateT_Trans {S: Any} {M: Monad}: MonadTrans
  with type 'a MT.t = (S.t_for_any, ('a * S.t_for_any) M.t) stateT
  and type 'a M.t = 'a M.t
= struct
  module M = M
  module MT = StateT {S} {M}
  let lift m = StateT (fun s -> M.fmap (fun x -> (x, s)) m)
end
