module type Any = sig
  type t
end

implicit module Any_Int = struct type t = int end
implicit module Any_String = struct type t = string end
implicit module Any_Pair { A : Any } {B : Any } = struct type t = A.t * B.t end
