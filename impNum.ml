module type Num = sig
  type t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val (~- ) : t -> t
  val zero : t
  val one : t
  val of_int : int -> t
end;;

module Num = struct
  let ( + ) (implicit M : Num) = M.( + )
  let ( - ) (implicit M : Num) = M.( - )
  let ( * ) (implicit M : Num) = M.( * )
  let ( / ) (implicit M : Num) = M.( / )
  let (~- ) (implicit M : Num) = M.(~- )
  let zero  (implicit M : Num) () = M.zero
  let one   (implicit M : Num) () = M.one
  let (~~)  (implicit M : Num) = M.of_int
end;;

implicit module Int = struct
  type t = int
  let ( + ),( - ),( * ), ( / ), (~- )
    = ( + ),( - ),( * ), ( / ), (~- )
  let zero = 0
  let one = 1
  let of_int x = x
end;;

implicit module Float = struct
  type t = float
  let ( + ), ( - ), ( * ), ( / ), ( ~- )
    = ( +. ), ( -. ), ( *. ), ( /. ), ( ~-. )
  let zero = 0.
  let one = 1.
  let of_int = float_of_int
  let ( = ), ( <> ), ( < ), ( <= ), ( > ), ( >= )
    = ( = ), ( <> ), ( < ), ( <= ), ( > ), ( >= )
end;;

open Num;;

let x = 1 + one() + one();;

let y = 2.5 + 6.0;;

let sq (implicit N : Num) (x : N.t) (y : N.t) = x * x;;

let z = sq 6.0;;
