module type Show = sig
  type t
  val show : t -> string
end;;

(* Define "overloaded" string function *)
let show {S : Show} x = S.show x;;

(* Function polymorphic in Show *)
let print {S : Show} (x : S.t) = print_string (show x);;

(* Global instances *)
implicit module ShowString = struct
  type t = string
  let show = Printf.sprintf "%S"
end;;

implicit module ShowBool = struct
  type t = bool
  let show = Printf.sprintf "%b"
end;;

implicit module ShowInt = struct
  type t = int
  let show = string_of_int
end;;

implicit module ShowFloat = struct
  type t = float
  let show = string_of_float
end;;

implicit module ShowList {X : Show} = struct
  type t = X.t list
  let show xs = "[" ^ String.concat "; " (List.map X.show xs) ^ "]"
end;;

implicit module ShowPair {A : Show} {B : Show} = struct
  type t = A.t * B.t
  let show (a, b) = "(" ^ A.show a ^ ", " ^ B.show b ^ ")"
end;;

implicit module ShowUnit = struct
  type t = unit
  let show () = "()"
end;;

implicit module Show3Tuple {A : Show} {B : Show} {C : Show} = struct
  type t = A.t * B.t * C.t
  let show (a, b, c) = "(" ^ A.show a ^ ", " ^ B.show b ^ ", " ^ C.show c ^ ")"
end

implicit module Show4Tuple {A : Show} {B : Show} {C : Show} {D : Show} = struct
  type t = A.t * B.t * C.t * D.t
  let show (a, b, c, d) = "(" ^ A.show a ^ ", " ^ B.show b ^ ", " ^ C.show c ^ ", " ^ D.show d ^ ")"
end

implicit module Show5Tuple {A : Show} {B : Show} {C : Show} {D : Show} {E : Show} = struct
  type t = A.t * B.t * C.t * D.t * E.t
  let show (a, b, c, d, e) = "(" ^ A.show a ^ ", " ^ B.show b ^ ", " ^ C.show c ^ ", " ^ D.show d ^ ", " ^ E.show e ^ ")"
end

implicit module Show6Tuple {A : Show} {B : Show} {C : Show} {D : Show} {E : Show} {F : Show} = struct
  type t = A.t * B.t * C.t * D.t * E.t * F.t
  let show (a, b, c, d, e, f) = "(" ^ A.show a ^ ", " ^ B.show b ^ ", " ^ C.show c ^ ", " ^ D.show d ^ ", " ^ E.show e ^ ", " ^ F.show f ^ ")"
end

implicit module Show7Tuple {A : Show} {B : Show} {C : Show} {D : Show} {E : Show} {F : Show} {G : Show} = struct
  type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t
  let show (a, b, c, d, e, f, g) = "(" ^ A.show a ^ ", " ^ B.show b ^ ", " ^ C.show c ^ ", " ^ D.show d ^ ", " ^ E.show e ^ ", " ^ F.show f ^ ", " ^ G.show g ^ ")"
end

implicit module Show8Tuple {A : Show} {B : Show} {C : Show} {D : Show} {E : Show} {F : Show} {G : Show} {H : Show} = struct
  type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t
  let show (a, b, c, d, e, f, g, h) = "(" ^ A.show a ^ ", " ^ B.show b ^ ", " ^ C.show c ^ ", " ^ D.show d ^ ", " ^ E.show e ^ ", " ^ F.show f ^ ", " ^ G.show g ^ ", " ^ H.show h ^ ")"
end
