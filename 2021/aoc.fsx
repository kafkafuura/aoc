(* Advent of Code 2021 - Selected Problems (F#) *)

(* 20s w/ ocamlopt *)
let problem_19b () =
 let debug = true in
 let example = false in
 let overlap_threshold = 12 in

 (* let module IntSet = Set.Make(Int) in *)
 (* let module XYZSet = Set.Make(struct type t = int * int * int let compare = compare end) in *)

 (* let radius = 1000 in *)

(*
 // not used, but forward transformation is easier to think about
 let apply_transformation n pset =
  // lower 3 bits : negation mask (zyx) - applied first
  // upper 3 bits : rot-swap mask 00_0 - xyz, 01_0 - yzx, 10_0 - zxy; 00_1 - xzy, 01_1 - yxz, 10_1 - zyx
  // to preserve rhr, the third bit MUST BE SHARED between the lower 3 and upper 3 bits, making this a 5 bit mask
  // max value = 10111 = 16 + 7 = 23 for 24 total combinations!
  Set.map
   (fun (x,y,z) ->
    // shift
    let (x,y,z) = (match n >>> 3 with 0 -> (x,y,z) | 1 -> (y,z,x) | 2 -> (z,x,y) | _ -> failwith "assert false") in
    // negate
    let (x,z) = if n &&& 1 = 1 then (-x,-z) else (x,z) in
    let (y,z) = if n &&& 2 = 2 then (-y,-z) else (y,z) in
    // flip
    let (y,z) = if n &&& 4 = 4 then (-z,y) else (y,z) in
    (x,y,z)) pset in
*)

 let apply_transformation_inv n pset =
  (* lower 3 bits : negate-flip mask (zyx) - applied first *)
  (* upper 2 bits : rotation mask 00_0 - xyz, 01_0 - yzx, 10_0 - zxy; 00_1 - xzy, 01_1 - yxz, 10_1 - zyx *)
  (* to preserve rhr, the third bit MUST BE SHARED between the lower 3 and upper 3 bits, making this a 5 bit mask *)
  (* max value = 10111 = 16 + 7 = 23 for 24 total combinations! *)
  pset |>
  Set.map
   (fun (x,y,z) ->
    (* flip *)
    let (y,z) = if n &&& 4 = 4 then (-z,y) else (y,z) in
    (* negate *)
    let (y,z) = if n &&& 2 = 2 then (-y,-z) else (y,z) in
    let (x,z) = if n &&& 1 = 1 then (-x,-z) else (x,z) in
    (* shift *)
    let (x,y,z) = (match n >>> 3 with 0 -> (x,y,z) | 2 -> (y,z,x) | 1 -> (z,x,y) | _ -> failwith "assert false") in
    (x,y,z)) in

 let shift_sub (x',y',z') pset = Set.map (fun (x,y,z) -> (x-x',y-y',z-z')) pset in
 let shift_add (x',y',z') pset = Set.map (fun (x,y,z) -> (x+x',y+y',z+z')) pset in
 let point_diff (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2) in
 let point_add  (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2) in

 let match_offsets pset1 pset2 =
  Seq.allPairs (Set.toSeq pset1) (Set.toSeq pset2) |>
  Seq.map (fun (p1,p2) -> if Set.intersect (shift_sub p1 pset1) (shift_sub p2 pset2) |> Set.count >= overlap_threshold then Some (p1,p2) else None) |>
  Seq.skipWhile (Option.isNone) |> Seq.tryHead |> Option.map Option.get in
  
 let overlap pset1 pset2 =
  (* range is inclusive *)
  seq { 0 .. 23 } |>
  Seq.map (fun i -> Option.map (fun (p1,p2) -> (i,p1,p2)) (match_offsets pset1 (apply_transformation_inv i pset2))) |>
  Seq.skipWhile (Option.isNone) |> Seq.tryHead |> Option.map Option.get in

 let inv_transform_and_shift pset (n,p1,p2) =
  apply_transformation_inv n pset |> shift_add (point_diff p1 p2) in
 
 let input = System.IO.File.ReadLines(if example then "19e.txt" else "19.txt") |> Seq.toArray in
 let breaks =
  seq { 1 .. (Array.length input) } |>
  Seq.filter (fun i -> input.[i-1] = "") |>  Seq.insertAt 0 0 |> (fun s -> Seq.append s (Array.length input + 1 |> Seq.singleton)) |>
  Seq.toArray in
 let scanners =
  Array.init (Array.length breaks - 1)
   (fun i -> 
    input |>
    Seq.skip (breaks.[i] + 1) |>
    Seq.take (breaks.[i+1] - breaks.[i] - 2) |>
    Seq.map (fun s -> s.Split([|','|]) |> Seq.map int |> Seq.toList |> (function [x;y;z] -> (x,y,z) | _ -> failwith "Invalid Coordinates!")) |>
    Set.ofSeq) in
 let transformed = ref (Set.singleton 0) in
 let universe = Set.ofSeq (seq {0 .. Array.length scanners - 1}) in
 let untransformed = Set.difference universe transformed.Value in
 let attempted = Array.create (Array.length scanners) 1 in
 let scanner_pos = Array.create (Array.length scanners) (0,0,0) in

 while transformed.Value.Count <> Array.length scanners do
  if debug then (
   printf "Transformed: " ;
   Set.iter (printf "%d,") transformed.Value ;
   printfn "") ;

  Seq.allPairs (Set.toSeq transformed.Value) (Set.toSeq untransformed) |>
  Seq.filter (fun (x,y) -> attempted.[x] &&& (1 <<< y) = 0) |>
  Seq.iter (fun (s0, s1) ->
   attempted.[s0] <- attempted.[s0] ||| (1 <<< s1);
   match Set.contains s1 transformed.Value, overlap scanners.[s0] scanners.[s1] with
   | true, _ -> ()
   | _, None -> ()
   | _, Some (i,p1,p2) ->
     transformed.Value <- Set.add s1 transformed.Value ;
     scanners.[s1] <- inv_transform_and_shift scanners.[s1] (i,p1,p2);
     scanner_pos.[s1] <- point_add scanner_pos.[s1] (point_diff p1 p2))
 done ;

 if debug then
  Set.iter (printfn "%d") transformed.Value ;
  printfn "-----" ;
  for i = 0 to Array.length scanners - 1 do
   Set.iter (fun (x,y,z) -> printfn "%d %d %d" x y z) scanners.[i] ;
   printfn ""
  done ;

 let max_dist = ref 0 in
 for i = 0 to Array.length scanner_pos - 2 do
  for j = i+1 to Array.length scanner_pos - 1 do
   match scanner_pos.[i], scanner_pos.[j] with
   | (x1,y1,z1),(x2,y2,z2) -> max_dist.Value <- max max_dist.Value (abs (x2-x1) + abs (y2-y1) + abs (z2-z1))
  done
 done ;
 max_dist.Value
