module type Any = sig
  type t
  val __any__ : unit
end

implicit module Any_Int = struct
  type t = int
  let __any__ = ()
end

implicit module Any_String = struct
  type t = string
  let __any__ = ()
end

implicit module Any_List {A : Any} = struct
  type t = A.t list
  let __any__ = ()
end

implicit module Any_Pair {A : Any} {B : Any} = struct
  type t = A.t * B.t
  let __any__ = ()
end

implicit module Any_Function {A : Any} {B : Any} = struct
  type t = A.t -> B.t
  let __any__ = ()
end
