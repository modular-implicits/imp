let () =
  let open Imp.Data.Num in
  let open implicit Imp.Data in
  begin
    let x = 1 + one() + one() in
    assert (x = 3);
    let y = 2.5 + 6.0 in
    assert (y = 8.5);
    let sq {N : Imp.Data.Num} (x : N.t) = x * x in
    let z = sq 6.0 in
    assert (z = 36.0);
  end

  let () = 
    let open Imp.Control in 
    let x = Some 5 in 
    let y = Some 10 in 
    begin
    assert ((Some 15) = liftA2 ( + ) x y);
    assert (None = liftA2 ( + ) x None);
    end


let () =
  let open Imp.Data.Fractional in
  let open Imp.Data.Floating in
  let open Imp.Data.Num in
  let open implicit Imp.Data in
  let open Imp.Data in
  begin
    let x = 5. in
    assert (1. = (x / x));
    let y = 3.1415 in 
    let x = (pi {Float}) in
    assert ( 0.1 > 6.29 - (y + x));
end


    
let () =
  begin
    (* Locally bind implicit instance *)
    let g (x : int) =
      let implicit module ShowInt = struct
        type t = int
        let show = string_of_int
      end in
     Imp.Show.show x
    in
    let open Imp.Show in
    let open implicit Imp.Show in
    assert (g 4 = "4");

    assert (show "4" = "\"4\"");

    assert (show ("4", 5.5) = "(\"4\", 5.5)");

    assert (show (4.5, ([9; 10; 11], "hello")) =  "(4.5, ([9; 10; 11], \"hello\"))");
  end

let () =
  let open Imp.Data in
  let e : unit = Monoid.empty () in
  assert (Monoid.append e e = ())

let () =
  let open Imp.Data in
  let e : string = Monoid.empty () in
  assert (e = "");
  assert (Monoid.append "abc" "def" = "abcdef")

let () =
  let open Imp.Any in
  let open Imp.Data in
  let e : int list = Monoid.empty {List {Any_Int}} () in
  assert (e = []);
  assert (Monoid.append {List {Any_Int}} [1; 2] [3] = [1; 2; 3])

let () =
  let open Imp.Control in
  assert (fmap (fun x -> x + 1) [1; 2; 3] = [2; 3; 4]);
  assert (return 5 = [5]);
  assert (apply [( * ) 2; ( * ) 3] [1; 2; 3] = [2; 4; 6; 3; 6; 9]);
  assert (bind [1; 2; 3] (fun x -> [x; x * 2]) = [1; 2; 2; 4; 3; 6]);
  assert (Foldable.fold ( * ) [2; 3; 3; 7] 1 = 126);
  assert (traverse (fun x -> Some (x + 1)) [1; 2; 3] = Some [2; 3; 4])
  (* let sequence {F : Applicative} {T : Traversable} = T.traverse {F} (fun x -> x) in *)
  (* assert (sequence {List} {Option} [Some 1; Some 2; Some 3] = Some [1; 2; 3]) *)

let () =
  let open Imp.Control in
  assert (fmap (fun x -> x + 1) (Some 3) = (Some 4));
  assert (return 5 = Some 5);
  assert (apply (Some (fun x -> x + 1)) (Some 3) = (Some 4));
  assert (apply None (Some 3) = None);
  assert (apply (Some (fun x -> x + 1)) None = None);
  assert (bind (Some 3) (fun x -> Some (x + 1)) = Some 4);
  assert (bind (Some 3) (fun _ -> None) = None);
  assert (Foldable.fold ( * ) (Some 5) 1 = 5);
  assert (Foldable.fold ( * ) None 1234 = 1234);
  assert (traverse (fun x -> [x; x + 1]) (Some 3) = [Some 3; Some 4])

let () =
  let open Imp.Control in
  let open implicit Imp.Any in
  let pair x y = (x, y) in
  assert ((fmap (fun x -> x + 1) (int_of_string)) "3" = 4);
  assert ((return 4) "3" = 4);
  assert ((apply pair (fun x -> x * x)) 3 = (3, 9));
  assert ((bind (fun x -> x * x) pair) 3 = (9, 3))

let () =
  let open Imp.Control in
  let open implicit Imp.Any in
  assert (fmap (fun x -> x + 1) ("hello", 3) = ("hello", 4))

let () =
  let open Imp.Any in
  let open Imp.Control in
  let open Imp.Transformers in
  let module R = ReaderT {Any_String} {Option} in
  let test = bind {R} (ask {R}) (fun x -> return [x ^ "!"]) in
  assert (runReaderT {Option} test "hello" = Some ["hello!"]);
  let test = bind {R} (ask {R}) (fun _ -> lift None) in
  assert (runReaderT {Option} test "hello" = None);
  let module R = Reader {Any_String} in
  let test = bind {R} (ask {R}) (fun x -> return {R} [x ^ "!"]) in
  assert (runReader test "hello" = ["hello!"])

let () =
  let open Imp.Any in
  let open Imp.Control in
  let open Imp.Transformers in
  let module S = StateT {Any_String} {Option} in
  let test = bind {S} (get {S}) (fun x -> return [x ^ "!"]) in
  assert (runStateT {Option} test "hello" = Some (["hello!"], "hello"));
  let test = bind {S} (get {S}) (fun _ -> lift None) in
  assert (runStateT {Option} test "hello" = None);
  let module S = State {Any_String} in
  let test = bind {S} (get {S}) (fun x -> return [x ^ "!"]) in
  assert (runState test "hello" = (["hello!"], "hello"));
  let (>>) {M: Monad} x y = M.bind x (fun _ -> y) in
  let test = bind {S} (put {S} "goodbye" >> get {S}) (fun x -> return [x ^ "!"]) in
  assert (runState test "hello" = (["goodbye!"], "goodbye"));
  let test = bind {S} (modify {S} (fun x -> x ^ "!") >> get {S}) (fun x -> return [x ^ "!"]) in
  assert (runState test "hello" = (["hello!!"], "hello!"))

let () = 
  let open Imp.Comonads in
  let open Imp.Control in
  let open Imp.Data in
  assert (5 = extract (Identity 5));
  assert (10 = extract  (NonEmpty (10, [])))

let () =
  let open Imp.Any in
  let open Imp.Data in
  let open Monoid in
  let e : int first = empty {First {Any_Int}} () in
  assert (e = { first = None });
  assert (append e e = e);
  assert (append {First {Any_Int}} { first = Some 2 } e = { first = Some 2 });
  assert (append {First {Any_Int}} { first = Some 2 } { first = Some 3 } = { first = Some 2 })

  let () =
  let open Imp.Control in
  let f = fun x -> Some 5 in
    assert ((mfix f) = (Some 5))
