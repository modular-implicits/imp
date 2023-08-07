module type Any = sig
  type t_for_any
end

implicit module Any_Int = struct type t_for_any = int end
implicit module Any_String = struct type t_for_any = string end
implicit module Any_List {A : Any} = struct type t_for_any = A.t_for_any list end
implicit module Any_Pair {A : Any} {B : Any} = struct type t_for_any = A.t_for_any * B.t_for_any end
implicit module Any_Function {A : Any} {B : Any} = struct type t_for_any = A.t_for_any -> B.t_for_any end
