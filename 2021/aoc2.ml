(* Revisiting Advent of Code 2021 *)

(* beacons and scanners *)
(* x,y,z coordinates *)
(* detection radius is a cube, i.e., (r,r) & (r,0) are both maximum range *)
(* if scanners are in an overlapping region, at least 12 beacons will overlap between them *)
(* the rotation and face of each scanner is random : one of 24 possible configurations *)
(* scanner 0 is the reference frame @ 0,0,0 *)
(* how many beacons are there? *)
let problem_19a () =
 let debug = true in
 let example = false in
 let overlap_threshold = 6 in
 let module IntSet = Set.Make(Int) in
 let module XYSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module XYZSet = Set.Make(struct type t = int * int * int let compare = compare end) in
 (* let radius = 1000 in *)

(*
 (* unused forward transformation *)
 let apply_transformation n pset =
  (* lower 3 bits : negation mask (zyx) - applied first *)
  (* upper 3 bits : rot-swap mask 00_0 - xyz, 01_0 - yzx, 10_0 - zxy; 00_1 - xzy, 01_1 - yxz, 10_1 - zyx *)
  (* to preserve rhr, the third bit MUST BE SHARED between the lower 3 and upper 3 bits, making this a 5 bit mask *)
  (* max value = 10111 = 16 + 7 - 23 for 24 total combinations! *)
  XYZSet.map (fun (x,y,z) ->
   (* shift w/ rhr preservation *)
   let (x,y,z) = (match n lsr 3 with 0 -> (x,y,z) | 1 -> (y,z,x) | 2 -> (z,x,y) | _ -> assert false) in
   (* negate w/ rhr preservation *)
   let (x,z) = if n land 1 = 1 then (~-x,~-z) else (x,z) in
   let (y,z) = if n land 2 = 2 then (~-y,~-z) else (y,z) in
   (* flip w/ rhr preservation *)
   let (y,z) = if n land 4 = 4 then (~-z,y) else (y,z) in
   (x,y,z)) pset in
*)

 let apply_transformation_inv n pset =
  (* lower 3 bits : negate-flip mask (zyx) - applied first *)
  (* upper 2 bits : rotation mask 00_0 - xyz, 01_0 - yzx, 10_0 - zxy; 00_1 - xzy, 01_1 - yxz, 10_1 - zyx *)
  (* to preserve rhr, the third bit MUST BE SHARED between the lower 3 and upper 3 bits, making this a 5 bit mask *)
  (* max value = 10111 = 16 + 7 - 23 for 24 total combinations! *)
  XYZSet.map (fun (x,y,z) ->
   (* flip *)
   let (y,z) = if n land 4 = 4 then (~-z,y) else (y,z) in
   (* negate *)
   let (y,z) = if n land 2 = 2 then (~-y,~-z) else (y,z) in
   let (x,z) = if n land 1 = 1 then (~-x,~-z) else (x,z) in
   (* shift *)
   let (x,y,z) = (match n lsr 3 with 0 -> (x,y,z) | 2 -> (y,z,x) | 1 -> (z,x,y) | _ -> assert false) in
   (x,y,z)) pset in

 let shift_sub (x',y',z') pset = XYZSet.map (fun (x,y,z) -> (x-x',y-y',z-z')) pset in
 let shift_add (x',y',z') pset = XYZSet.map (fun (x,y,z) -> (x+x',y+y',z+z')) pset in
 let point_diff (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2) in

 let match_offsets pset1 pset2 =
  Seq.product (XYZSet.to_seq pset1) (XYZSet.to_seq pset2) |>
  Seq.map (fun (p1,p2) -> if XYZSet.inter (shift_sub p1 pset1) (shift_sub p2 pset2) |> XYZSet.cardinal >= overlap_threshold then Some (p1,p2) else None) |>
  Seq.drop_while (Option.is_none) |> Seq.uncons |> Option.map (fun (x,_) -> Option.get x) in
  
 let overlap pset1 pset2 =
  Seq.ints 0 |> Seq.take 24 |>
  Seq.map (fun i -> Option.map (fun (p1,p2) -> (i,p1,p2)) (match_offsets pset1 (apply_transformation_inv i pset2))) |>
  Seq.drop_while (Option.is_none) |> Seq.uncons |> Option.map (fun (x,_) -> Option.get x) in

 let inv_transform_and_shift pset (n,p1,p2) =
  apply_transformation_inv n pset |> shift_add (point_diff p1 p2) in
 
 let input = In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_lines) |> Array.of_list in
 let breaks =
  Seq.ints 1 |> Seq.take (Array.length input) |>
  Seq.filter (fun i -> input.(i-1) = "") |>  Seq.cons 0 |> Fun.flip Seq.append (Array.length input + 1 |> Seq.return) |>
  Array.of_seq in
 let scanners = Array.init (Array.length breaks - 1)
  (fun i -> 
   Array.to_seq input |>
   Seq.drop (breaks.(i) + 1) |>
   Seq.take (breaks.(i+1) - breaks.(i) - 2) |>
   Seq.map (fun s -> String.split_on_char ',' s |> List.map (int_of_string) |> (function [x;y;z] -> (x,y,z) | _ -> raise_notrace (Invalid_argument "Invalid Coordinates!"))) |>
   XYZSet.of_seq) in
 let transformed = ref (IntSet.add 0 IntSet.empty) in
 let universe = IntSet.of_seq (Seq.ints 0 |> Seq.take (Array.length scanners)) in
 let untransformed = IntSet.diff universe !transformed in
 let attempted = ref (XYSet.empty) in
 while IntSet.cardinal !transformed <> Array.length scanners do
  if debug then (
   print_string "Transformed: " ;
   IntSet.iter (Printf.printf "%d,") !transformed ;
   print_newline ()) ;

  Seq.product (IntSet.to_seq !transformed) (IntSet.to_seq untransformed) |>
  Seq.filter (fun (x,y) -> not (XYSet.mem (x,y) !attempted)) |>
  Seq.iter (fun (s0, s1) ->
   attempted := XYSet.add (s0,s1) !attempted ;
   match IntSet.mem s1 !transformed, overlap scanners.(s0) scanners.(s1) with
   | true, _ -> ()
   | _, None -> ()
   | _, Some (i,p1,p2) ->
     transformed := IntSet.add s1 !transformed ;
     scanners.(s1) <- inv_transform_and_shift scanners.(s1) (i,p1,p2)) ;
 done ;
 if debug then
 (
  IntSet.iter (Printf.printf "%d\n") !transformed ;
  print_endline "-----" ;
  for i = 0 to Array.length scanners - 1 do
   XYZSet.iter (fun (x,y,z) -> Printf.printf "%d %d %d\n" x y z) scanners.(i) ;
   print_newline ()
  done ; ()
 ) ;
 Array.fold_left XYZSet.union XYZSet.empty scanners |> XYZSet.cardinal

(* 20s w/ ocamlopt *)
let problem_19b () =
 let debug = true in
 let example = false in
 let overlap_threshold = 12 in
 let module IntSet = Set.Make(Int) in
 let module XYZSet = Set.Make(struct type t = int * int * int let compare = compare end) in
 (* let radius = 1000 in *)
 (* not used, but forward transformation is easier to think about *)
(*
 let apply_transformation n pset =
  (* lower 3 bits : negation mask (zyx) - applied first *)
  (* upper 3 bits : rot-swap mask 00_0 - xyz, 01_0 - yzx, 10_0 - zxy; 00_1 - xzy, 01_1 - yxz, 10_1 - zyx *)
  (* to preserve rhr, the third bit MUST BE SHARED between the lower 3 and upper 3 bits, making this a 5 bit mask *)
  (* max value = 10111 = 16 + 7 - 23 for 24 total combinations! *)
  XYZSet.map (fun (x,y,z) ->
   (* shift *)
   let (x,y,z) = (match n lsr 3 with 0 -> (x,y,z) | 1 -> (y,z,x) | 2 -> (z,x,y) | _ -> assert false) in
   (* negate *)
   let (x,z) = if n land 1 = 1 then (~-x,~-z) else (x,z) in
   let (y,z) = if n land 2 = 2 then (~-y,~-z) else (y,z) in
   (* flip *)
   let (y,z) = if n land 4 = 4 then (~-z,y) else (y,z) in
   (x,y,z)) pset in
*)

 let apply_transformation_inv n pset =
  (* lower 3 bits : negate-flip mask (zyx) - applied first *)
  (* upper 2 bits : rotation mask 00_0 - xyz, 01_0 - yzx, 10_0 - zxy; 00_1 - xzy, 01_1 - yxz, 10_1 - zyx *)
  (* to preserve rhr, the third bit MUST BE SHARED between the lower 3 and upper 3 bits, making this a 5 bit mask *)
  (* max value = 10111 = 16 + 7 - 23 for 24 total combinations! *)
  XYZSet.map (fun (x,y,z) ->
   (* flip *)
   let (y,z) = if n land 4 = 4 then (~-z,y) else (y,z) in
   (* negate *)
   let (y,z) = if n land 2 = 2 then (~-y,~-z) else (y,z) in
   let (x,z) = if n land 1 = 1 then (~-x,~-z) else (x,z) in
   (* shift *)
   let (x,y,z) = (match n lsr 3 with 0 -> (x,y,z) | 2 -> (y,z,x) | 1 -> (z,x,y) | _ -> assert false) in
   (x,y,z)) pset in

 let shift_sub (x',y',z') pset = XYZSet.map (fun (x,y,z) -> (x-x',y-y',z-z')) pset in
 let shift_add (x',y',z') pset = XYZSet.map (fun (x,y,z) -> (x+x',y+y',z+z')) pset in
 let point_diff (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2) in
 let point_add  (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2) in

 let match_offsets pset1 pset2 =
  Seq.product (XYZSet.to_seq pset1) (XYZSet.to_seq pset2) |>
  Seq.map (fun (p1,p2) -> if XYZSet.inter (shift_sub p1 pset1) (shift_sub p2 pset2) |> XYZSet.cardinal >= overlap_threshold then Some (p1,p2) else None) |>
  Seq.drop_while (Option.is_none) |> Seq.uncons |> Option.map (fun (x,_) -> Option.get x) in
  
 let overlap pset1 pset2 =
  Seq.ints 0 |> Seq.take 24 |>
  Seq.map (fun i -> Option.map (fun (p1,p2) -> (i,p1,p2)) (match_offsets pset1 (apply_transformation_inv i pset2))) |>
  Seq.drop_while (Option.is_none) |> Seq.uncons |> Option.map (fun (x,_) -> Option.get x) in

 let inv_transform_and_shift pset (n,p1,p2) =
  apply_transformation_inv n pset |> shift_add (point_diff p1 p2) in
 
 let input = In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_lines) |> Array.of_list in
 let breaks =
  Seq.ints 1 |> Seq.take (Array.length input) |>
  Seq.filter (fun i -> input.(i-1) = "") |>  Seq.cons 0 |> Fun.flip Seq.append (Array.length input + 1 |> Seq.return) |>
  Array.of_seq in
 let scanners = Array.init (Array.length breaks - 1)
  (fun i -> 
   Array.to_seq input |>
   Seq.drop (breaks.(i) + 1) |>
   Seq.take (breaks.(i+1) - breaks.(i) - 2) |>
   Seq.map (fun s -> String.split_on_char ',' s |> List.map (int_of_string) |> (function [x;y;z] -> (x,y,z) | _ -> raise_notrace (Invalid_argument "Invalid Coordinates!"))) |>
   XYZSet.of_seq) in
 let transformed = ref (IntSet.add 0 IntSet.empty) in
 let universe = IntSet.of_seq (Seq.ints 0 |> Seq.take (Array.length scanners)) in
 let untransformed = IntSet.diff universe !transformed in
 let attempted = Array.make (Array.length scanners) 1 in
 let scanner_pos = Array.make (Array.length scanners) (0,0,0) in

 while IntSet.cardinal !transformed <> Array.length scanners do
  if debug then (
   print_string "Transformed: " ;
   IntSet.iter (Printf.printf "%d,") !transformed ;
   print_newline ()) ;

  Seq.product (IntSet.to_seq !transformed) (IntSet.to_seq untransformed) |>
  Seq.filter (fun (x,y) -> attempted.(x) land (1 lsl y) = 0) |>
  Seq.iter (fun (s0, s1) ->
   attempted.(s0) <- attempted.(s0) lor (1 lsl s1);
   match IntSet.mem s1 !transformed, overlap scanners.(s0) scanners.(s1) with
   | true, _ -> ()
   | _, None -> ()
   | _, Some (i,p1,p2) ->
     transformed := IntSet.add s1 !transformed ;
     scanners.(s1) <- inv_transform_and_shift scanners.(s1) (i,p1,p2);
     scanner_pos.(s1) <- point_add scanner_pos.(s1) (point_diff p1 p2))
 done ;
 if debug then
 (
  IntSet.iter (Printf.printf "%d\n") !transformed ;
  print_endline "-----" ;
  for i = 0 to Array.length scanners - 1 do
   XYZSet.iter (fun (x,y,z) -> Printf.printf "%d %d %d\n" x y z) scanners.(i) ;
   print_newline ()
  done ; ()
 ) ;

 let max_dist = ref 0 in
 for i = 0 to Array.length scanner_pos - 2 do for j = i+1 to Array.length scanner_pos - 1 do
  match scanner_pos.(i), scanner_pos.(j) with
  | (x1,y1,z1),(x2,y2,z2) -> max_dist := max !max_dist (abs (x2-x1) + abs (y2-y1) + abs (z2-z1))
 done done;
 !max_dist

(* dumb way of doing things *)
(* switch iterations from 25 to 1 for 20a *)
let problem_20a () =
 let example = true in 
 let debug = true in 
 let input = In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_lines) |> Array.of_list in
 let dim = Array.length input - 2 + 6 in
 let iterations = 1 in
 let midpoint = (dim + (iterations lsl 2)) lsr 1 in
 let image  = Array.make_matrix (dim + (iterations lsl 2)) (dim + (iterations lsl 2)) false in
 let image' = Array.make_matrix (dim + (iterations lsl 2)) (dim + (iterations lsl 2)) false in

 let add_yx (y0,x0) (y1,x1) = (y0+y1,x0+x1) in
 (* order matters *)
 let iterate_in_range pixel =
  [~-1,~-1; ~-1,  0; ~-1,  1;
     0,~-1;   0,  0;   0,  1;
     1,~-1;   1,  0;   1,  1] |> List.to_seq |> Seq.map (add_yx pixel) in

 let enhance forward =
  let i0 = if forward then image else image' in
  let i1 = if forward then image' else image in
  for y = 0 to Array.length i0 - 1 do
   for x = 0 to Array.length i0.(y) - 1 do
    let addr =
     iterate_in_range (y,x) |> Seq.map (fun (y,x) -> try i0.(y).(x) with _ -> i0.(2).(2)) |>
     Seq.fold_left (fun a on -> if on then (a lsl 1) lor 1 else a lsl 1) 0 in
   i1.(y).(x) <- input.(0).[addr] = '#' done done ; in
    
 (* parse / populate initial image *)
 for y = 2 to Array.length input - 1 do
  for x = 0 to String.length input.(y) - 1 do
   if input.(y).[x] = '#' then image.(y+midpoint-(dim lsr 1)+3-2).(x+midpoint-(dim lsr 1)+3) <- true ;
  done done;

 for i = 1 to iterations do 
  enhance true ;
  enhance false ;
 done ;

 if debug then (
  for i = 0 to Array.length image - 1 do
   for j = 0 to Array.length image.(0) - 1 do
    print_char (if image.(i).(j) then '#' else '.');
  done ; print_newline () done ) ;

 let count = ref 0 in
 for i = 0 to Array.length image - 1 do
  for j = 0 to Array.length image.(0) - 1 do
   if image.(i).(j) then incr count else ();
 done ; done ;

 !count

let problem_20b () =
 let example = false in 
 let debug = true in 
 let input = In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_lines) |> Array.of_list in
 let dim = Array.length input - 2 + 6 in
 let iterations = 25 in
 let midpoint = (dim + (iterations lsl 2)) lsr 1 in
 let image  = Array.make_matrix (dim + (iterations lsl 2)) (dim + (iterations lsl 2)) false in
 let image' = Array.make_matrix (dim + (iterations lsl 2)) (dim + (iterations lsl 2)) false in

 let add_yx (y0,x0) (y1,x1) = (y0+y1,x0+x1) in
 (* order matters *)
 let iterate_in_range pixel =
  [~-1,~-1; ~-1,  0; ~-1,  1;
     0,~-1;   0,  0;   0,  1;
     1,~-1;   1,  0;   1,  1] |> List.to_seq |> Seq.map (add_yx pixel) in

 let enhance forward =
  let i0 = if forward then image else image' in
  let i1 = if forward then image' else image in
  for y = 0 to Array.length i0 - 1 do
   for x = 0 to Array.length i0.(y) - 1 do
    let addr =
     iterate_in_range (y,x) |> Seq.map (fun (y,x) -> try i0.(y).(x) with _ -> i0.(2).(2)) |>
     Seq.fold_left (fun a on -> if on then (a lsl 1) lor 1 else a lsl 1) 0 in
   i1.(y).(x) <- input.(0).[addr] = '#' done done ; in
    
 (* parse / populate initial image *)
 for y = 2 to Array.length input - 1 do
  for x = 0 to String.length input.(y) - 1 do
   if input.(y).[x] = '#' then image.(y+midpoint-(dim lsr 1)+3-2).(x+midpoint-(dim lsr 1)+3) <- true ;
  done done;

 for i = 1 to iterations do 
  enhance true ;
  enhance false ;
 done ;

 if debug then (
  for i = 0 to Array.length image - 1 do
   for j = 0 to Array.length image.(0) - 1 do
    print_char (if image.(i).(j) then '#' else '.');
  done ; print_newline () done ) ;

 let count = ref 0 in
 for i = 0 to Array.length image - 1 do
  for j = 0 to Array.length image.(0) - 1 do
   if image.(i).(j) then incr count else ();
 done ; done ;
 !count

(*
#...........#
###A#D#A#B###
  #C#C#D#B#
  #########
*)

(*
#A..B.....B.#
###A#D#.#.###
  #C#C#D#.#
  #########
*)

(*
#A..B.....B.#
###A#.#.#D###
  #C#C#.#D#
  #########
*)

(*
#A..........#
###A#B#.#D###
  #C#B#C#D#
  #########
*)

(*
#AA.........#
###.#B#.#D###
  #C#B#C#D#
  #########
*)

(*
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########
*)

(*
(* solution by hand *)
B 2
A 7
B 7
D 7
D 5
C 6
B 3
B 6
A 2
C 7
A 3
A 3
*)

(*
 2  3  3  7 +++
 2  3  6  7 +++ 10*+
 6  7 + 100*+
 5  7 + 1000*+p
*)

(*13495*)

(*
(* first attempt *)
B 3
B 3
A 7
A 2
D 6
D 6
C 6
C 7
A 3
A 3
B 7
B 7
*)

(*
 2 3 3 7 +++
 3 3 7 7 +++ 10*+
 6 7 + 100*+
 6 6 + 1000*+p
*)
(* 13515 *)

(*
#############
#...........#
###A#D#A#B###
  #D#C#B#A#
  #D#B#A#C#
  #C#C#D#B#
  #########
*)

(*
#############
#AD.D.....BA#
###A#.#.#B###
  #D#C#.#A#
  #D#B#.#C#
  #C#C#.#B#
  #########
*)

(*
A5
B5
A9
D9
D2
C8
B6
C9
B7
B8
B7
A3
C7
B9
D9
D10
A2
D10
D10
C9
A5
A5
A9
A9
*)


(*
2 3 5 5 5 9 9 9 +++++++
5 6 7 7 8 9 +++++ 10*+
7 8 9 9 +++ 100*+
10 10 10 2 9 9 +++++ 1000*+p
*)

(* 53767 *)

(* solved by hand *)
let problem_23a () =
 13515

(* solved by hand *)
let problem_23b () =
 53767

(* use symbolic math to simplify and then solve *)
(* solved by hand *)
let problem_24a () =
(* validation rules *)
(* w4 = w3 + 4
   w7 = w6 - 6
   w8 = w5 - 5
   w9 = w2 - 7
  w10 = w1 + 6
  w12 = w11 + 1
  w13 = w0 *)
 93959993429899

let problem_24b () =
(* validation rules *)
(* w4 = w3 + 4
   w7 = w6 - 6
   w8 = w5 - 5
   w9 = w2 - 7
  w10 = w1 + 6
  w12 = w11 + 1
  w13 = w0 *)
 11815671117121

let problem_25a () =
 let example = false in
 let debug = true in
 let input = In_channel.(with_open_bin (if example then "25e.txt" else "25.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length input.(0) in
 let (.%[;..]) = (fun bs idx -> Bytes.get bs ((idx.(0) mod h) * w + (idx.(1) mod w))) in
 let (.%[;..]<-) = (fun bs idx x -> Bytes.set bs ((idx.(0) mod h) * w + (idx.(1) mod w)) x) in
 let map = Array.init 2 (fun _ -> Bytes.create (h*w)) in
 for y = 0 to h - 1 do Bytes.blit_string input.(y) 0 map.(0) (w*y) w done ;
 Bytes.blit map.(0) 0 map.(1) 0 (w*h) ;

 let cycles = ref 0 in
 let motions = ref 0 in

 let advance_right (y,x) =
  let src = map.(0) in
  let dst = map.(1) in
  if src.%[y;x] = '>' && src.%[y;x+1] = '.' then (dst.%[y;x] <- '.' ; dst.%[y;x+1] <- '>'; incr motions) in

 let advance_down (y,x) =
  let src = map.(1) in
  let dst = map.(0) in
  if src.%[y;x] = 'v' && src.%[y+1;x] = '.' then (dst.%[y;x] <- '.' ; dst.%[y+1;x] <- 'v'; incr motions) in

 let cycle () =
  motions := 0 ;
  for y = 0 to h - 1 do
   for x = 0 to w - 1 do
    advance_right (y,x)
  done done;
 Bytes.blit map.(1) 0 map.(0) 0 (w*h) ;
  for y = 0 to h - 1 do
   for x = 0 to w - 1 do
    advance_down (y,x)
  done done;
 Bytes.blit map.(0) 0 map.(1) 0 (w*h) in

 let print_map () =
  for y = 0 to h - 1 do
   for x = 0 to w - 1 do
    print_char (map.(0).%[y;x])
  done; print_newline (); done in

 motions := ~-1 ;
 while !motions <> 0 do
  cycle ();
  incr cycles
 done ;
 if debug then print_map () ;
 !cycles
  
  
