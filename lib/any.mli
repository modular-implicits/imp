module type Any = sig
  type t
  val __any__ : unit
end
(** Any is an interface that all types can and should implement.
    Note: the type has the extraneous member __any__, because otherwise
    many existing modules could accidentally fit the Any module type,
    which would make implicit resolution ambiguous.
    *)

implicit module Any_Int : Any with type t = int
implicit module Any_Unit : Any with type t = unit
implicit module Any_Char : Any with type t = char
implicit module Any_Bool : Any with type t = bool
implicit module Any_Float : Any with type t = float
implicit module Any_Int32 : Any with type t = int32
implicit module Any_Int64 : Any with type t = int64
implicit module Any_Nativeint : Any with type t = nativeint
implicit module Any_String : Any with type t = string
implicit module Any_List {A : Any} : Any with type t = A.t list
implicit module Any_Pair {A : Any} {B : Any} : Any with type t = A.t * B.t
implicit module Any_Function {A : Any} {B : Any} : Any with type t = A.t -> B.t
implicit module Any_Exn : Any with type t = exn
