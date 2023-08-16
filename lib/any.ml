module type Any = sig
  type t
  val __any__ : unit
end

implicit module Any_Int = struct
  type t = int
  let __any__ = ()
end

implicit module Any_Unit = struct
  type t = unit
  let __any__ = ()
end

implicit module Any_Char = struct
  type t = char
  let __any__ = ()
end

implicit module Any_Bool = struct
  type t = bool
  let __any__ = ()
end

implicit module Any_Float = struct
  type t = float
  let __any__ = ()
end

implicit module Any_Int32 = struct
  type t = int32
  let __any__ = ()
end

implicit module Any_Int64 = struct
  type t = int64
  let __any__ = ()
end

implicit module Any_Nativeint = struct
  type t = nativeint
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

implicit module Any_Exn = struct
  type t = exn
  let __any__ = ()
end
