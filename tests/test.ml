let () =
  let open Imp.Num.Num in
  let open implicit Imp.Num in
  begin
    let x = 1 + one() + one() in
    assert (x = 3);
    let y = 2.5 + 6.0 in
    assert (y = 8.5);
    let sq {N : Imp.Num.Num} (x : N.t) = x * x in
    let z = sq 6.0 in
    assert (z = 36.0);
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
  let open Imp.Data in
  let implicit module IntList = List(struct type t = int end) in
  let e : int list = Monoid.empty () in
  assert (e = []);
  assert (Monoid.append [1; 2] [3] = [1; 2; 3])

let () =
  let open Imp.Control in
  assert (fmap (fun x -> x + 1) [1; 2; 3] = [2; 3; 4]);
  assert (return 5 = [5]);
  assert (apply [( * ) 2; ( * ) 3] [1; 2; 3] = [2; 4; 6; 3; 6; 9]);
  assert (bind [1; 2; 3] (fun x -> [x; x * 2]) = [1; 2; 2; 4; 3; 6]);
  assert (Foldable.fold ( * ) [2; 3; 3; 7] 1 = 126);
  assert (Traversable.traverse (fun x -> Some (x + 1)) [1; 2; 3] = Some [2; 3; 4]);
  assert (Traversable.traverse (fun x -> x) [Some 1; Some 2; None] = None);
  assert (fmap (fun x -> x + 1) (Some 3) = (Some 4));
  assert (return 5 = Some 5);
  assert (apply (Some (fun x -> x + 1)) (Some 3) = (Some 4));
  assert (apply None (Some 3) = None);
  assert (apply (Some (fun x -> x + 1)) None = None);
  assert (bind (Some 3) (fun x -> Some (x + 1)) = Some 4);
  assert (bind (Some 3) (fun _ -> None) = None);
  assert (Foldable.fold ( * ) (Some 5) 1 = 5);
  assert (Foldable.fold ( * ) None 1234 = 1234);
  assert (Traversable.traverse (fun x -> [x; x + 1]) (Some 3) = [Some 3; Some 4]);
