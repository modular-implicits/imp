module type Any = sig
  type t_for_any
end

implicit module Any_Int : Any with type t_for_any = int
implicit module Any_String : Any with type t_for_any = string
implicit module Any_List {A : Any} : Any with type t_for_any = A.t_for_any list
implicit module Any_Pair {A : Any} {B : Any} : Any with type t_for_any = A.t_for_any * B.t_for_any
implicit module Any_Function {A : Any} {B : Any} : Any with type t_for_any = A.t_for_any -> B.t_for_any
