module type Show = sig
  type t
  val show : t -> string
end;;

(* Define "overloaded" string function *)
let show {S : Show} x = S.show x;;

(* Locally bind implicit instance *)
let g (x : int) =
  let implicit module ShowInt = struct
    type t = int
    let show = string_of_int
  end in
  show x;;

(* Function polymorphic in Show *)
let print {S : Show} (x : S.t) = print_string (show x);;

(* Global instances *)
implicit module ShowString = struct
  type t = string
  let show x = x
end;;

implicit module ShowInt = struct
  type t = int
  let show = string_of_int
end;;

implicit module ShowFloat = struct
  type t = float
  let show = string_of_float
end;;

implicit module ShowList (X : Show) = struct
  type t = X.t list
  let show xs = "[" ^ String.concat "; " (List.map X.show xs) ^ "]"
end;;

implicit module ShowPair (A : Show) (B : Show) = struct
  type t = A.t * B.t
  let show (a, b) = "(" ^ A.show a ^ ", " ^ B.show b ^ ")"
end;;

(* print_endline (g 4);;
 * 
 * print_endline (show "4");;
 * 
 * print_endline (show ("4", 5.5));;
 * 
 * print (4.5 ([9; 10; 11], "hello");; *)
