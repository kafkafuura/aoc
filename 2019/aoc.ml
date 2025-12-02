(* Advent of Code 2019 *)

let problem_01a () =
 let input = In_channel.(with_open_bin "01.txt" input_lines) |> List.map int_of_string in
 let fuel_of_mass m = m / 3 - 2 in
 input |>
 List.map fuel_of_mass |>
 List.fold_left (+) 0

let problem_01b () =
 let input = In_channel.(with_open_bin "01.txt" input_lines) |> List.map int_of_string in
 let rec fuel_of_mass m = let f = m / 3 - 2 in if f <= 0 then 0 else (+) f (fuel_of_mass f) in
 input |>
 List.map fuel_of_mass |>
 List.fold_left (+) 0

(* Intcode *)
(* Input is a single line separated by ',' *)
(* Opcodes : 1: +, 2: *, 99: HCF *)
(* Text and Data occupy the same memory space *)
let problem_02a () =
 let mem =
  In_channel.(with_open_bin "02.txt" input_lines) |>
  List.hd |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list in
 (* edit program *)
 mem.(1) <- 12 ;
 mem.(2) <- 2 ;
 let rec run i =
  match mem.(i) with
  | 99 -> (* HCF *)
    mem.(0)
  | 1 -> (* ADD *)
    mem.(mem.(i+3)) <- mem.(mem.(i+1)) + mem.(mem.(i+2)) ;
    run (i+4)
  | 2 -> (* MUL *)
    mem.(mem.(i+3)) <- mem.(mem.(i+1)) * mem.(mem.(i+2)) ;
    run (i+4)
  | _ -> raise (Invalid_argument "Illegal Instruction")
 in run 0

let problem_02b () =
 let mem0 =
  In_channel.(with_open_bin "02.txt" input_lines) |>
  List.hd |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list in
 let len = Array.length mem0 in
 let mem = Array.make len 0 in

 (* initialize memory and set noun and verb *)
 let set_nv n v =
  Array.blit mem0 0 mem 0 len ;
  mem.(1) <- n;
  mem.(2) <- v in

 let rec run i =
  match mem.(i) with
  | 99 -> (* HCF *)
    mem.(0)
  | 1 -> (* ADD *)
    mem.(mem.(i+3)) <- mem.(mem.(i+1)) + mem.(mem.(i+2)) ;
    run (i+4)
  | 2 -> (* MUL *)
    mem.(mem.(i+3)) <- mem.(mem.(i+1)) * mem.(mem.(i+2)) ;
    run (i+4)
  | _ -> raise (Invalid_argument "Illegal Instruction")
 in

 let rec test (n,v) =
  if n < 0 then (n,v) else
  if v < 0 then test (n-1,99) else
  (set_nv n v ;
   if run 0 = 19690720 then (n,v) else test (n,v-1)) in

 let (n,v) = test (99,99) in
 n*100+v

let problem_03a () =
 let example = false in
 let debug = false in
 let code_to_yx codes =
  List.fold_left
   (fun a s ->
    let n = String.sub s 1 (String.length s - 1) |> int_of_string in
    let (y,x) = List.hd a in
    match s.[0] with
    | 'U' -> (y-n,x)::a
    | 'D' -> (y+n,x)::a
    | 'L' -> (y,x-n)::a
    | 'R' -> (y,x+n)::a
    | _ -> raise_notrace (Invalid_argument "Illegal Direction"))
   [0,0] codes in
 let intersection ((y1,x1),(y2,x2)) ((y3,x3),(y4,x4)) =
  (* both lines are horizontal *)
  if y1 = y2 && y3 = y4 then
   if y2 <> y3 then None else
   ([ if min x3 x4 <= x1 && x1 <= max x3 x4 then Some x1 else None
    ; if min x3 x4 <= x2 && x2 <= max x3 x4 then Some x2 else None
    ; if min x1 x2 <= x3 && x3 <= max x1 x2 then Some x3 else None
    ; if min x1 x2 <= x4 && x4 <= max x2 x2 then Some x4 else None ] |>
    List.filter_map (Fun.id) |> List.sort (fun a b -> compare (abs a) (abs b)) |>
    (function hd::tl -> Some (y1,hd) | _ -> None))
  (* both lines are vertical *)
  else if x1 = x2 && x3 = x4 then
   if x2 <> x3 then None else
   ([ if min y3 y4 <= y1 && y1 <= max y3 y4 then Some y1 else None
    ; if min y3 y4 <= y2 && y2 <= max y3 y4 then Some y2 else None
    ; if min y1 y2 <= y3 && y3 <= max y1 y2 then Some y3 else None
    ; if min y1 y2 <= y4 && y4 <= max y2 y2 then Some y4 else None ] |>
    List.filter_map (Fun.id) |> List.sort (fun a b -> compare (abs a) (abs b)) |>
    (function hd::tl -> Some (hd,x1) | _ -> None))
   (* lines are perpindicular *)
  else
   (if y1 = y2 && min x1 x2 <= x3 && x3 <= max x1 x2 && min y3 y4 <= y1 && y1 <= max y3 y4 then Some (y1,x3) else
    if y3 = y4 && min x3 x4 <= x1 && x1 <= max x3 x4 && min y1 y2 <= y3 && y3 <= max y1 y2 then Some (y3,x1) else
    None) in
 let wires =
  In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
  List.map (fun s -> s |> String.split_on_char ',' |> code_to_yx) |>
  Array.of_list in
 let set1 = Array.of_seq (Seq.zip (List.to_seq wires.(0)) (List.to_seq wires.(0) |> Seq.drop 1)) in
 let set2 = Array.of_seq (Seq.zip (List.to_seq wires.(1)) (List.to_seq wires.(1) |> Seq.drop 1)) in
 let update a b =
  match a,b with
  | _, Some (0,0)
  | _, None -> a
  | Some (ay,ax), Some (by,bx) when abs ay + abs ax < abs by + abs bx -> a
  | _ -> b in
 let res = ref None in
 for i = 0 to Array.length set1 - 1 do
  for j = 0 to Array.length set2 - 1 do
   let x = intersection set1.(i) set2.(j) in
   if debug then Option.iter (fun (y,x) -> Printf.printf "%d,%d\n" y x) x ;
   res := update !res x
 done done;
 let (y,x) = Option.get !res in
 (abs y + abs x)

let problem_03a2 () =
 let example = false in
 let module YXSet = Set.Make(struct type t = int * int let compare (y,x) (y',x') = (abs y + abs x) - (abs y' + abs x') end) in
 let code_to_yx codes =
  List.fold_left
   (fun a s ->
    let n = String.sub s 1 (String.length s - 1) |> int_of_string in
    let (y,x) = List.hd a in
    match s.[0] with
    | 'U' -> (y-n,x)::a
    | 'D' -> (y+n,x)::a
    | 'L' -> (y,x-n)::a
    | 'R' -> (y,x+n)::a
    | _ -> raise_notrace (Invalid_argument "Illegal Direction"))
   [0,0] codes in
 let intersection ((y1,x1),(y2,x2)) ((y3,x3),(y4,x4)) =
  (* both lines are horizontal *)
  if y1 = y2 && y3 = y4 then
   if y2 <> y3 then None else
   ([ if min x3 x4 <= x1 && x1 <= max x3 x4 then Some x1 else None
    ; if min x3 x4 <= x2 && x2 <= max x3 x4 then Some x2 else None
    ; if min x1 x2 <= x3 && x3 <= max x1 x2 then Some x3 else None
    ; if min x1 x2 <= x4 && x4 <= max x2 x2 then Some x4 else None ] |>
    List.filter_map (Fun.id) |> List.sort (fun a b -> compare (abs a) (abs b)) |>
    (function hd::tl -> Some (y1,hd) | _ -> None))
  (* both lines are vertical *)
  else if x1 = x2 && x3 = x4 then
   if x2 <> x3 then None else
   ([ if min y3 y4 <= y1 && y1 <= max y3 y4 then Some y1 else None
    ; if min y3 y4 <= y2 && y2 <= max y3 y4 then Some y2 else None
    ; if min y1 y2 <= y3 && y3 <= max y1 y2 then Some y3 else None
    ; if min y1 y2 <= y4 && y4 <= max y2 y2 then Some y4 else None ] |>
    List.filter_map (Fun.id) |> List.sort (fun a b -> compare (abs a) (abs b)) |>
    (function hd::tl -> Some (hd,x1) | _ -> None))
   (* lines are perpindicular *)
  else
   (if y1 = y2 && min x1 x2 <= x3 && x3 <= max x1 x2 && min y3 y4 <= y1 && y1 <= max y3 y4 then Some (y1,x3) else
    if y3 = y4 && min x3 x4 <= x1 && x1 <= max x3 x4 && min y1 y2 <= y3 && y3 <= max y1 y2 then Some (y3,x1) else
    None) in
 let wires =
  In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
  List.map (fun s -> s |> String.split_on_char ',' |> code_to_yx) |>
  Array.of_list in
 let set1 = Array.of_seq (Seq.zip (List.to_seq wires.(0)) (List.to_seq wires.(0) |> Seq.drop 1)) in
 let set2 = Array.of_seq (Seq.zip (List.to_seq wires.(1)) (List.to_seq wires.(1) |> Seq.drop 1)) in
 let intersections =
  Array.fold_left
  (fun a line1 ->
   Array.fold_left
   (fun a line2 ->
    match intersection line1 line2 with
     Some pt -> YXSet.add pt a | None -> a)
   a set2)
  YXSet.empty set1 |>
  YXSet.remove (0,0) in
 let (y,x) = YXSet.min_elt intersections in
 (abs y + abs x)

let problem_03b () =
 let example = false in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let code_to_yx codes =
  List.fold_left
   (fun a s ->
    let n = String.sub s 1 (String.length s - 1) |> int_of_string in
    let (y,x) = List.hd a in
    match s.[0] with
    | 'U' -> (y-n,x)::a
    | 'D' -> (y+n,x)::a
    | 'L' -> (y,x-n)::a
    | 'R' -> (y,x+n)::a
    | _ -> raise_notrace (Invalid_argument "Illegal Direction"))
   [0,0] codes |>
  List.rev in
 (* assume only perpindicular intersections for simplicity *)
 let intersection ((y1,x1),(y2,x2)) ((y3,x3),(y4,x4)) =
  (if y1 = y2 && min x1 x2 <= x3 && x3 <= max x1 x2 && min y3 y4 <= y1 && y1 <= max y3 y4 then Some (y1,x3) else
   if y3 = y4 && min x3 x4 <= x1 && x1 <= max x3 x4 && min y1 y2 <= y3 && y3 <= max y1 y2 then Some (y3,x1) else
   None) in
 let raw_input =
  In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
  List.map (String.split_on_char ',') |> Array.of_list in
 let wires =
  Array.map code_to_yx raw_input in
 let wire_lens =
  Array.map (List.map (fun s -> String.sub s 1 (String.length s - 1) |> int_of_string)) raw_input in
 let set1 = Array.of_seq (Seq.zip (List.to_seq wires.(0)) (List.to_seq wires.(0) |> Seq.drop 1)) in
 let set2 = Array.of_seq (Seq.zip (List.to_seq wires.(1)) (List.to_seq wires.(1) |> Seq.drop 1)) in

 let intersection_length ((y1,x1),(y2,x2)) (y3,x3) =
  abs (y3 - y1) + abs (x3 - x1) in

 let intersections =
  Array.to_seq set1 |>
  Seq.fold_lefti
  (fun a i line1 ->
   Array.to_seq set2 |>
   Seq.fold_lefti
   (fun a j line2 ->
    match intersection line1 line2 with
     Some (0,0) -> a | Some pt -> (pt,i,j)::a | None -> a) a) [] in

 let intersection_distances ((y,x),i,j) =
  let path1 =
   (wire_lens.(0) |> List.to_seq |> Seq.take i |> Seq.fold_left (+) 0) +
   intersection_length set1.(i) (y,x) in
  let path2 =
   (wire_lens.(1) |> List.to_seq |> Seq.take j |> Seq.fold_left (+) 0) +
   intersection_length set2.(j) (y,x) in
  (path1,path2) in

 intersections |>
 List.map intersection_distances |>
 List.map (fun (a,b) -> a+b) |>
 List.fold_left min Int.max_int

(* input 265275-781584 *)
(* rules: 2 adjacent digits must be the same, digits cannot decrease, six digits *)
let problem_04a () =
 let rec to_digits a n = if n = 0 then a else to_digits ((n mod 10)::a) (n / 10) in
 let rec has_adj = function
 | h1::h2::tl when h1 = h2 -> true
 | h1::h2::tl -> has_adj (h2::tl)
 | _ -> false in
 let rec no_dec = function
 | h1::h2::tl when h1 <= h2 -> no_dec (h2::tl)
 | h1::h2::tl -> false
 | _ -> true in
 let valid_digits n = no_dec n && has_adj n in
 let input = In_channel.(with_open_bin "04.txt" input_lines) |> List.hd |> String.split_on_char '-' |> List.map int_of_string in
 let low = List.hd input in
 let high = List.nth input 1 in
 let count = ref 0 in
 for n = low to high do
  if valid_digits (to_digits [] n) then incr count ;
 done ;
 !count

let problem_04b () =
 let rec to_digits a n = if n = 0 then a else to_digits ((n mod 10)::a) (n / 10) in
 let rec has_pair = function
 | h1::h2::h3::[] when h2 = h3 && h1 <> h2 -> true
 | h1::h2::h3::h4::tl when h2 = h3 && h1 <> h2 && h3 <> h4 -> true
 | h1::h2::h3::h4::tl -> has_pair (h2::h3::h4::tl)
 | _ -> false in
 let rec no_dec = function
 | h1::h2::tl when h1 <= h2 -> no_dec (h2::tl)
 | h1::h2::tl -> false
 | _ -> true in
 let valid_digits n = no_dec n && has_pair n in
 let input = In_channel.(with_open_bin "04.txt" input_lines) |> List.hd |> String.split_on_char '-' |> List.map int_of_string in
 let low = List.hd input in
 let high = List.nth input 1 in
 let count = ref 0 in
 for n = low to high do
  if valid_digits (0::(to_digits [] n)) then incr count ;
 done ;
 !count

let problem_05a () =
 let mem0 =
  In_channel.(with_open_bin "05.txt" input_lines) |>
  List.hd |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let mem = Array.copy mem0 in

 let outq = Queue.create () in
 let inq = Queue.create () in

 let rec run i =
  match mem.(i) mod 100 with
  | 99 -> (* HCF *)
    mem.(0)
  | 1 -> (* ADD *)
    let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    mem.(mem.(i+3)) <- param1 + param2 ;
    run (i+4)
  | 2 -> (* MUL *)
    let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    mem.(mem.(i+3)) <- param1 * param2 ;
    run (i+4)
  | 3 -> (* READ *)
    mem.(mem.(i+1)) <- Queue.take inq ;
    run (i+2)
  | 4 -> (* WRITE *)
    let param1 = match (mem.(i) / 100) mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    Queue.add param1 outq ;
    run (i+2)
  | _ -> raise (Invalid_argument "Illegal Instruction")
 in

 Queue.add 1 inq ;
 run 0 |> ignore ;
 (* Queue.to_seq outq |> Seq.iter (fun n -> print_int n ; print_newline ()) *)
 Queue.to_seq outq |> Seq.filter ((<>)0) |> Seq.uncons |> Option.get |> fst

let problem_05b () =
 let mem0 =
  In_channel.(with_open_bin "05.txt" input_lines) |>
  List.hd |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let mem = Array.copy mem0 in

 let outq = Queue.create () in
 let inq = Queue.create () in

 let rec run i =
  match mem.(i) mod 100 with
  | 99 -> (* HCF *)
    mem.(0)
  | 1 -> (* ADD *)
    let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    mem.(mem.(i+3)) <- param1 + param2 ;
    run (i+4)
  | 2 -> (* MUL *)
    let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    mem.(mem.(i+3)) <- param1 * param2 ;
    run (i+4)
  | 3 -> (* READ *)
    mem.(mem.(i+1)) <- Queue.take inq ;
    run (i+2)
  | 4 -> (* WRITE *)
    let param1 = match (mem.(i) / 100) mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    Queue.add param1 outq ;
    run (i+2)
  | 5 -> (* JNZ *)
    let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    if param1 <> 0 then run (param2) else run (i+3)
  | 6 -> (* JZ *)
    let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    if param1 = 0 then run (param2) else run (i+3)
  | 7 -> (* LT *)
    let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    mem.(mem.(i+3)) <- if param1 < param2 then 1 else 0 ;
    run (i+4)
  | 8 -> (* EQ *)
    let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
    mem.(mem.(i+3)) <- if param1 = param2 then 1 else 0 ;
    run (i+4)
  | _ -> raise (Invalid_argument "Illegal Instruction")
 in

 Queue.add 5 inq ;
 run 0 |> ignore ;
 (*Queue.to_seq outq |> Seq.iter (fun n -> print_int n ; print_newline ())*)
 Queue.take outq

(* Universal Orbit Map *)
(* COM : Universal Center of Mass *)
(* AAA)BBB = "BBB is in orbit around AAA" *)
(* Step 1: verify number of direct orbits and indirect orbits *)
(* Direct + Indirect Orbits = Tree Depth *)

let problem_06a () =
 let example = false in
 (* use psuedo-hash (36*36*36) to store nodes *)
 let nodes = Array.make (36*36*36) [] in
 let idx_of_char c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'A' .. 'Z' -> Char.code c - Char.code 'A' + 10
  | _ -> 0 (* don't bother with validation here *) in
 let idx_of_string s =
  let s = if String.length s > 3 then String.sub s 0 3 else s in
  String.fold_left (fun a c -> (a * 36) + idx_of_char c) 0 s in
 let add_link s =
  match String.split_on_char ')' s with
  | [left; right] ->
    let idx = idx_of_string left in
    nodes.(idx) <- (idx_of_string right) :: nodes.(idx)
  | _ -> prerr_endline "Warning: Invalid Link Encountered"; () in
 (* dfs : top-down approach *)
 let rec calculate origin depth =
  List.fold_left
   (fun a next ->
     calculate next (depth+1) + a
   ) 0 nodes.(origin) + depth in
 (* add links *)
 In_channel.(with_open_bin
  (if example then "06e.txt" else "06.txt")
  (fun ic ->
   Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.iter add_link)) ;
 calculate (idx_of_string "COM") 0

(* alternative method that uses bottom-up approach w/o non-tail recursion *)
let problem_06a2 () =
 let example = false in
 (* use psuedo-hash (36*36*36) to store parents *)
 let parents = Array.make (36*36*36) None in
 let idx_of_char c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'A' .. 'Z' -> Char.code c - Char.code 'A' + 10
  | _ -> 0 (* don't bother with validation here *) in
 let idx_of_string s =
  let s = if String.length s > 3 then String.sub s 0 3 else s in
  String.fold_left (fun a c -> (a * 36) + idx_of_char c) 0 s in
 let add_link s =
  match String.split_on_char ')' s with
  | [left; right] ->
    let idx_l = idx_of_string left in
    let idx_r = idx_of_string right in
    parents.(idx_r) <- Some (idx_l)
  | _ -> prerr_endline "Warning: Invalid Link Encountered"; () in
 let rec depth cur acc =
  match parents.(cur) with
  | None -> acc
  | Some next -> depth next (acc + 1) in
 (* add links *)
 In_channel.(with_open_bin
  (if example then "06e.txt" else "06.txt")
  (fun ic ->
   Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.iter add_link)) ;
 (* seq version *)
(*
 Seq.ints 0 |> Seq.take (Array.length parents) |>
 Seq.fold_left (fun a cur -> a + depth cur 0) 0
*)
 (* loop version *)
 ( let acc = ref 0 in
   for cur = 0 to Array.length parents - 1 do
    acc := !acc + depth cur 0
   done ; !acc )

let problem_06b () =
 let example = false in
 (* use psuedo-hash (36*36*36) to store nodes and parents *)
 (* nodes not required for this part *)
 (* let nodes = Array.make (36*36*36) [] in *)
 let parents = Array.make (36*36*36) None in
 let idx_of_char c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'A' .. 'Z' -> Char.code c - Char.code 'A' + 10
  | _ -> 0 (* don't bother with validation here *) in
 let idx_of_string s =
  let s = if String.length s > 3 then String.sub s 0 3 else s in
  String.fold_left (fun a c -> (a * 36) + idx_of_char c) 0 s in
 let add_link s =
  match String.split_on_char ')' s with
  | [left; right] ->
    let idx_l = idx_of_string left in
    let idx_r = idx_of_string right in
    (*nodes.(idx_l) <- (idx_r) :: nodes.(idx_l) ;*)
    parents.(idx_r) <- Some (idx_l)
  | _ -> prerr_endline "Warning: Invalid Link Encountered"; () in
 let rec path_to origin acc =
  if origin = List.hd acc then acc else
  match parents.(List.hd acc) with
  | None -> []
  | Some parent -> path_to origin (parent::acc) in
 let rec drop_while_equal s1 s2 =
  match Seq.uncons s1, Seq.uncons s2 with
  | Some (h1,t1), Some (h2,t2) when h1 = h2 -> drop_while_equal t1 t2
  | _ -> (s1, s2) in
 (* add links *)
 In_channel.(with_open_bin
  (if example then "06e.txt" else "06.txt")
  (fun ic ->
   Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.iter add_link)) ;
 let path1 = path_to (idx_of_string "COM") [idx_of_string "YOU"] |> List.to_seq in
 let path2 = path_to (idx_of_string "COM") [idx_of_string "SAN"] |> List.to_seq in
 let path1, path2 = drop_while_equal path1 path2 in
 Seq.length path1 + Seq.length path2 - 2 (* - 2 to remove both YOU and SAN *)

(* Amplifier Controller Software *)
(* each phase setting 0-4 is used exactly once *)
(* memory is not shared between copies of the program *)

(* use permutation function with efficient swaps *)
let problem_07a () =
 let example = false in
 (* range 0 to 119 *)
 let permutation n =
  let box = Array.init 5 (Fun.id) in
  let swap = ref 0 in
  let n = ref n in
  for len = 5 downto 1 do
   let take = !n mod len in
   n := !n / len ;
   if take <> len-1 then (
    swap := box.(len-1) ;
    box.(len-1) <- box.(take) ;
    box.(take) <- !swap
   )
  done ;
  box in
 let program =
  In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run signal phase =
  let mem = Array.copy program in
  let outq = Queue.create () in
  let inq = Queue.create () in

  Queue.add phase inq ;
  Queue.add signal inq ;

  (* Halt at first output! *)
  let rec run i =
   if not (Queue.is_empty outq) then mem.(0) else
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     mem.(mem.(i+3)) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     mem.(mem.(i+3)) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     mem.(mem.(i+1)) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = match (mem.(i) / 100) mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     mem.(mem.(i+3)) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     mem.(mem.(i+3)) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore ;
  Queue.take outq in

 let results =
  Array.init 120
  (fun n ->
   permutation n |>
   Array.fold_left run 0 ) in
 Array.fold_left max 0 results

(* Uses Effects for Interrupts! *)
let problem_07b () =
 let example = false in
 (* range 0 to 119 ; this one uses a box with contents 5-9 instead of 0-4 *)
 let permutation n =
  let box = Array.init 5 ((+)5) in
  let swap = ref 0 in
  let n = ref n in
  for len = 5 downto 1 do
   let take = !n mod len in
   n := !n / len ;
   if take <> len-1 then (
    swap := box.(len-1) ;
    box.(len-1) <- box.(take) ;
    box.(take) <- !swap
   )
  done ;
  box in
 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in
 let program =
  In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  let mem = Array.copy program in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     mem.(mem.(i+3)) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     mem.(mem.(i+3)) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     mem.(mem.(i+1)) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = match (mem.(i) / 100) mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     mem.(mem.(i+3)) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = match (mem.(i) / 100)  mod 10 with 0 -> mem.(mem.(i+1)) | 1 -> mem.(i+1) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     let param2 = match (mem.(i) / 1000) mod 10 with 0 -> mem.(mem.(i+2)) | 1 -> mem.(i+2) | _ -> raise (Invalid_argument "Illegal Param Mode") in
     mem.(mem.(i+3)) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let results =
  Array.init 120
  (fun n ->
   let pipes = Array.init 5 (fun _ -> Queue.create ()) in
   let perm = permutation n in
   let thrusters = ref 0 in
   for i = 0 to 4 do
    Queue.add perm.(i) pipes.(i)
   done ;
   Queue.add 0 pipes.(0) ;
   let processes = Queue.create () in
   for i = 0 to 4 do
    Queue.add (fun () -> run pipes.(i) pipes.((i+1) mod 5)) processes
   done ;
   Inter.go processes ;
   while not (Queue.is_empty pipes.(0)) do thrusters := Queue.take pipes.(0) done ;
   !thrusters
   ) in
 Array.fold_left max 0 results

(* image size: w = 25, h = 6 *)
let problem_08a () =
 let example = false in
 let w = 25 and h = 6 in
 let image = In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_line) |> Option.get in
 let d = String.length image / (w * h) in
 let ans = Array.make 3 (w*h*2) in
 (
  let ns = Array.make 3 0 in
  for z = 0 to d - 1 do
   for i = w*h*z to w*h*(z+1) - 1 do
    if image.[i] = '0' then ns.(0) <- ns.(0) + 1 else
    if image.[i] = '1' then ns.(1) <- ns.(1) + 1 else
    if image.[i] = '2' then ns.(2) <- ns.(2) + 1 else ()
   done ;
   if ns.(0) < ans.(0) then Array.blit ns 0 ans 0 3 ;
   Array.fill ns 0 3 0
  done
 ) ;
 ans.(1) * ans.(2)

(* 0 = black, 1 = white, 2 = transparent *)
(* note: "unsafe" operations are always safe and efficient if you scope things properly *)
let problem_08b () =
 let example = false in
 let w = 25 and h = 6 in
 let image = In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_line) |> Option.get in
 let final =
 (
  let final = Bytes.make (w * h) '2' in
  for i = 0 to String.length image - 1 do
   if Bytes.get final (i mod (w*h)) = '2' && (image.[i] = '0' || image.[i] = '1')
   then Bytes.set final (i mod (w*h)) image.[i]
  done ;
  (* convert to something more visible *)
  for i = 0 to Bytes.length final - 1 do
   if Bytes.get final i = '0' then Bytes.set final i ' ' else
   if Bytes.get final i = '1' then Bytes.set final i '*' else
   Bytes.set final i '_'
  done ;
  Bytes.unsafe_to_string final
 ) in
 (* print image *)
 for y = 0 to h - 1 do
  print_endline (String.sub final (y*w) w)
 done

(* Add Relative Mode to IntCode Interpreter with Large Number Support *)
let problem_09a () =
 let example = false in
 let debug = true in

 (* Interrupt Module : Unused in Problem 9 *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =

  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   (* "Write" params allowed in position 3 *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 Queue.add 1 inq ;
 run inq outq ;
 print_endline "Program Output:" ;
 if Queue.is_empty outq then () else (
  print_int (Queue.take outq) ;
  while not (Queue.is_empty outq) do
   print_char ',' ;
   print_int (Queue.take outq)
  done
 ) ;
 print_newline ()

(* apparently the virtual memory was unnecessary, no pages were created in the final problem! *)
(* an associative dynarray could be used instead of a hashtbl if page creation is minimal *)
let problem_09b () =
 let example = false in
 let debug = true in

 (* Interrupt Module : Unused in Problem 9 *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =

  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 Queue.add 2 inq ;
 run inq outq ;
 print_endline "Program Output:" ;
 if Queue.is_empty outq then () else (
  print_int (Queue.take outq) ;
  while not (Queue.is_empty outq) do
   print_char ',' ;
   print_int (Queue.take outq)
  done
 ) ;
 print_newline ()

let problem_10a () =
 let example = false in
 let map = In_channel.(with_open_bin (if example then "10e.txt" else "10.txt") input_lines) |> Array.of_list in
 let h = Array.length map
 and w = String.length map.(0) in
 let seen = Array.make (w*h) false in

 let reset_seen () =
  Array.fill seen 0 (w*h) false in

 (* unsafe: expects you to do the bounds checking *)
 let detect_unsafe y x =
  if not seen.(y*w+x) then (
   seen.(y*w+x) <- true ;
   map.(y).[x] = '#'
  ) else false in

 (* used to reduce duplicate detect_ray calls *)
 let peek_empty y x = not seen.(y*w+x) in

 (* purposefully does not short-circuit *)
 let rec detect_ray found y x dy dx =
  if not (y >= 0 && y < h && x >= 0 && x < h) then found else
  detect_ray (detect_unsafe y x || found) (y+dy) (x+dx) dy dx in

 (* sphere = box *)
 let detect_sphere y x r =
  let acc = ref 0 in
  (* top *)
  if y - r < 0 then () else
   for x' = max 0 (x - r) to min (w - 1) (x + r) do
    if peek_empty (y-r) x' && detect_ray false (y-r) x' (~-r) (x' - x) then incr acc
   done ;
  (* bottom *)
  if y + r >= h then () else
   for x' = max 0 (x - r) to min (w - 1) (x + r) do
    if peek_empty (y+r) x' && detect_ray false (y+r) x' r (x' - x) then incr acc
   done ;
  (* left *)
  if x - r < 0 then () else
   for y' = max 0 (y - r + 1) to min (h - 1) (y + r - 1) do
    if peek_empty y' (x-r) && detect_ray false y' (x-r) (y' - y) (~-r) then incr acc
   done ;
  (* right *)
  if x + r >= w then () else
   for y' = max 0 (y - r + 1) to min (h - 1) (y + r - 1) do
    if peek_empty y' (x+r) && detect_ray false y' (x+r) (y' - y) r then incr acc
   done ;
   !acc in

 let visible y x =
  reset_seen () ;
  if detect_unsafe y x then (
   let max_r = [y; h - y - 1; x; w - x - 1] |> List.fold_left max 0 in
   Seq.ints 1 |> Seq.take max_r |>
   Seq.fold_left (fun a r -> a + detect_sphere y x r) 0
  ) else 0 in

 let max_visible = ref 0 in
 let station_y = ref ~-1 in
 let station_x = ref ~-1 in
 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   let vis = visible y x in
   if vis > !max_visible
   then (
    max_visible := vis ;
    station_y := y ;
    station_x := x )
  done
 done ;
 print_int !station_y ;
 print_char ',' ;
 print_int !station_x ;
 print_newline () ;
 !max_visible

let problem_10b () =
 let example = false in
 let map = In_channel.(with_open_bin (if example then "10e.txt" else "10.txt") input_lines) |> Array.of_list in
 let h = Array.length map
 and w = String.length map.(0) in
 let seen = Array.make (w*h) false in

 let reset_seen () =
  Array.fill seen 0 (w*h) false in

 (* unsafe: expects you to do the bounds checking *)
 let detect_unsafe y x =
  if not seen.(y*w+x) then (
   seen.(y*w+x) <- true ;
   map.(y).[x] = '#'
  ) else false in

 (* used to reduce duplicate detect_ray calls *)
 let peek_empty y x = not seen.(y*w+x) in

 (* purposefully does not short-circuit *)
 let rec detect_ray found y x dy dx =
  if not (y >= 0 && y < h && x >= 0 && x < h) then found else
  detect_ray (detect_unsafe y x || found) (y+dy) (x+dx) dy dx in

 (* sphere = box *)
 let detect_sphere y x r =
  let acc = ref 0 in
  (* top *)
  if y - r < 0 then () else
   for x' = max 0 (x - r) to min (w - 1) (x + r) do
    if peek_empty (y-r) x' && detect_ray false (y-r) x' (~-r) (x' - x) then incr acc
   done ;
  (* bottom *)
  if y + r >= h then () else
   for x' = max 0 (x - r) to min (w - 1) (x + r) do
    if peek_empty (y+r) x' && detect_ray false (y+r) x' r (x' - x) then incr acc
   done ;
  (* left *)
  if x - r < 0 then () else
   for y' = max 0 (y - r + 1) to min (h - 1) (y + r - 1) do
    if peek_empty y' (x-r) && detect_ray false y' (x-r) (y' - y) (~-r) then incr acc
   done ;
  (* right *)
  if x + r >= w then () else
   for y' = max 0 (y - r + 1) to min (h - 1) (y + r - 1) do
    if peek_empty y' (x+r) && detect_ray false y' (x+r) (y' - y) r then incr acc
   done ;
   !acc in

 let visible y x =
  reset_seen () ;
  if detect_unsafe y x then (
   let max_r = [y; h - y - 1; x; w - x - 1] |> List.fold_left max 0 in
   Seq.ints 1 |> Seq.take max_r |>
   Seq.fold_left (fun a r -> a + detect_sphere y x r) 0
  ) else 0 in

 let sy, sx, sn =
 (
  let max_visible = ref 0 in
  let station_y = ref ~-1 in
  let station_x = ref ~-1 in
  for y = 0 to h - 1 do
   for x = 0 to w - 1 do
    let vis = visible y x in
    if vis > !max_visible
    then (
     max_visible := vis ;
     station_y := y ;
     station_x := x )
   done
  done ;
  !station_y, !station_x, !max_visible
 ) in
 reset_seen () ;

 (* for quadrants, keep in mind: y is INVERTED! *)
 let sin_cmp (y1,x1) (y2,x2) =
  let quad y x =
   if x = 0 && y = 0 then 0
   else if x = 0 then
    if y < 0 then 1 else 3
   else if y = 0 then
    if x > 0 then 2 else 4
   else if x > 0 then
    if y < 0 then 1 else 2
   else
    if y < 0 then 4 else 3 in
   let q1 = quad y1 x1
   and q2 = quad y2 x2 in
   if q1 <> q2 then compare q1 q2
   else if q1 = 1 || q1 = 3 then
    (* magnitude of sin2 inversely related to clockwise progression *)
    compare ((x1*x1+y1*y1)*y2*y2) ((x2*x2+y2*y2)*y1*y1)
   else
    (* magnitude of sin2 related to clockwise progression *)
    compare ((x2*x2+y2*y2)*y1*y1) ((x1*x1+y1*y1)*y2*y2) in

 (* not needed because we're adding shells in order *)
 (* otherwise you could use this function w/ Set.find_opt to replace an element with a higher distance *)
 (*
 let dst_cmp (y1,x1) (y2,x2) = compare (x1*x1+y1*y1) (x2*x2+y2*y2) in
 *)

 let module RadialSet = Set.Make(struct type t = int * int let compare = sin_cmp end) in
 let vaporized = ref RadialSet.empty in

 let add_sphere y x r =
  (* top *)
  if y - r < 0 then () else
   for x' = max 0 (x - r) to min (w - 1) (x + r) do
    if map.(y-r).[x'] = '#' then vaporized := RadialSet.add (~-r,x'-x) !vaporized ;
   done ;
  (* bottom *)
  if y + r >= h then () else
   for x' = max 0 (x - r) to min (w - 1) (x + r) do
    if map.(y+r).[x'] = '#' then vaporized := RadialSet.add (r,x'-x) !vaporized ;
   done ;
  (* left *)
  if x - r < 0 then () else
   for y' = max 0 (y - r + 1) to min (h - 1) (y + r - 1) do
    if map.(y').[x-r] = '#' then vaporized := RadialSet.add (y'-y,~-r) !vaporized ;
   done ;
  (* right *)
  if x + r >= w then () else
   for y' = max 0 (y - r + 1) to min (h - 1) (y + r - 1) do
    if map.(y').[x+r] = '#' then vaporized := RadialSet.add (y'-y,r) !vaporized ;
   done in

 let add_visible y x =
  let max_r = [y; h - y - 1; x; w - x - 1] |> List.fold_left max 0 in
  Seq.ints 1 |> Seq.take max_r |>
  Seq.iter (add_sphere y x) in

 add_visible sy sx ;
 match
  !vaporized |>
  RadialSet.to_seq |>
  Seq.drop 199 |>
  Seq.uncons
 with
 | Some ((dy,dx),_) -> (sx+dx)*100+sy+dy
 | None -> assert false (* add extra calculations if 10a < 200! *)

let problem_11a () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "11e.txt" else "11.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =

  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in
 let module Robot = struct
  type t = {mutable y : int; mutable x : int; mutable dir : int}
  let create () =
   { y = 0; x = 0; dir = 0 }
  let get_yx bot = (bot.y, bot.x)
  let rotate_and_move bot signal =
   if signal = 0 then
    bot.dir <- (bot.dir + 3) mod 4
   else
    bot.dir <- (bot.dir + 1) mod 4 ;
   let dy, dx = [|~-1,0; 0,1; 1,0; 0,~-1|].(bot.dir) in
   bot.y <- bot.y + dy ;
   bot.x <- bot.x + dx
 end in

 let tiles = ref YXMap.empty in
 let robot = Robot.create () in

 Queue.add 0 inq ;

 (match run inq outq with
 | () ->
   let color = Queue.take outq in
   let signal = Queue.take outq in
   tiles := YXMap.update (Robot.get_yx robot) (fun _ -> Some color) !tiles ;
   Robot.rotate_and_move robot signal
 | effect Inter.Yield, k ->
   let color = Queue.take outq in
   let signal = Queue.take outq in
   tiles := YXMap.update (Robot.get_yx robot) (fun _ -> Some color) !tiles ;
   Robot.rotate_and_move robot signal ;
   let color' = YXMap.find_opt (Robot.get_yx robot) !tiles |> Option.value ~default:0 in
   Queue.add color' inq ;
   Effect.Deep.continue k ()) ;
 YXMap.cardinal !tiles

let problem_11b () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "11e.txt" else "11.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =

  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in
 let module Robot = struct
  type t = {mutable y : int; mutable x : int; mutable dir : int}
  let create () =
   { y = 0; x = 0; dir = 0 }
  let get_yx bot = (bot.y, bot.x)
  let rotate_and_move bot signal =
   if signal = 0 then
    bot.dir <- (bot.dir + 3) mod 4
   else
    bot.dir <- (bot.dir + 1) mod 4 ;
   let dy, dx = [|~-1,0; 0,1; 1,0; 0,~-1|].(bot.dir) in
   bot.y <- bot.y + dy ;
   bot.x <- bot.x + dx
 end in

 let tiles = ref YXMap.empty in
 let robot = Robot.create () in

 (* start on white this time *)
 Queue.add 1 inq ;

 (match run inq outq with
 | () ->
   let color = Queue.take outq in
   let signal = Queue.take outq in
   tiles := YXMap.update (Robot.get_yx robot) (fun _ -> Some color) !tiles ;
   Robot.rotate_and_move robot signal
 | effect Inter.Yield, k ->
   let color = Queue.take outq in
   let signal = Queue.take outq in
   tiles := YXMap.update (Robot.get_yx robot) (fun _ -> Some color) !tiles ;
   Robot.rotate_and_move robot signal ;
   let color' = YXMap.find_opt (Robot.get_yx robot) !tiles |> Option.value ~default:0 in
   Queue.add color' inq ;
   Effect.Deep.continue k ()) ;

 let (ymin,ymax,xmin,xmax) =
  YXMap.fold
   (fun (y,x) v (ymin,ymax,xmin,xmax as acc) ->
    if v = 1 then (min y ymin, max y ymax, min x xmin, max x xmax) else acc)
   !tiles (Int.max_int, Int.min_int, Int.max_int, Int.min_int) in

 (* assert to avoid scary memory allocations *)
 assert (ymax-ymin+1 < 50 && xmax-xmin+1 < 500) ;
 let output = Array.init (ymax-ymin+1) (fun _ -> Bytes.make (xmax-xmin+1) ' ') in

 YXMap.iter
  (fun (y,x) v -> if v = 1 then Bytes.set output.(y-ymin) (x-xmin) '#' else ())
  !tiles ;

 Array.iter
  (fun bs -> print_bytes bs ; print_newline ())
  output

let problem_12a () =
 let example = false in
 let module Moon = struct
  type t = {mutable x : int; mutable y : int; mutable z : int; mutable dx : int; mutable dy : int; mutable dz : int}

  let of_string s =
   let res = { x = 0 ; y = 0 ; z = 0 ; dx = 0 ; dy = 0 ; dz = 0 } in
   let rec loop i =
    if i >= String.length s then () else
    match s.[i] with
    | 'x' -> let len = String.index_from s i ',' - i - 2 in
      res.x <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | 'y' -> let len = String.index_from s i ',' - i - 2 in
      res.y <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | 'z' -> let len = String.index_from s i '>' - i - 2 in
      res.z <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | _ -> loop (i+1) in
   loop 0 ;
   res

  let print m =
   Printf.printf "x=%d,y=%d,z=%d,dx=%d,dy=%d,dz=%d\n" m.x m.y m.z m.dx m.dy m.dz

  let update_velocity m1 m2 =
   let cx = compare m1.x m2.x in
   let cy = compare m1.y m2.y in
   let cz = compare m1.z m2.z in
   m1.dx <- m1.dx - cx ;
   m1.dy <- m1.dy - cy ;
   m1.dz <- m1.dz - cz ;
   m2.dx <- m2.dx + cx ;
   m2.dy <- m2.dy + cy ;
   m2.dz <- m2.dz + cz

  let update_position m =
   m.x <- m.x + m.dx ;
   m.y <- m.y + m.dy ;
   m.z <- m.z + m.dz

  let energy m =
   (abs m.x + abs m.y + abs m.z) * (abs m.dx + abs m.dy + abs m.dz)
 end in

 let moons =
  In_channel.(with_open_bin (if example then "12e.txt" else "12.txt") input_lines) |>
  List.map (Moon.of_string) |> Array.of_list in

 assert (Array.length moons = 4) ;

 let step () =
  for i = 0 to 2 do
   for j = i+1 to 3 do
    Moon.update_velocity moons.(i) moons.(j)
   done
  done ;
  for i = 0 to 3 do
   Moon.update_position moons.(i)
  done in

 for i = 1 to 1000 do step () done ;
 Array.fold_left (fun a m -> a + Moon.energy m) 0 moons

(* detect steady-state oscillations! *)
(* x y and z are independent of each other! *)
let problem_12b () =
 let example = false in
 let module Moon = struct
  type t = {mutable x : int; mutable y : int; mutable z : int; mutable dx : int; mutable dy : int; mutable dz : int}

  let of_string s =
   let res = { x = 0 ; y = 0 ; z = 0 ; dx = 0 ; dy = 0 ; dz = 0 } in
   let rec loop i =
    if i >= String.length s then () else
    match s.[i] with
    | 'x' -> let len = String.index_from s i ',' - i - 2 in
      res.x <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | 'y' -> let len = String.index_from s i ',' - i - 2 in
      res.y <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | 'z' -> let len = String.index_from s i '>' - i - 2 in
      res.z <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | _ -> loop (i+1) in
   loop 0 ;
   res

  let print m =
   Printf.printf "x=%d,y=%d,z=%d,dx=%d,dy=%d,dz=%d\n" m.x m.y m.z m.dx m.dy m.dz

  let update_velocity m1 m2 =
   let cx = compare m1.x m2.x in
   let cy = compare m1.y m2.y in
   let cz = compare m1.z m2.z in
   m1.dx <- m1.dx - cx ;
   m1.dy <- m1.dy - cy ;
   m1.dz <- m1.dz - cz ;
   m2.dx <- m2.dx + cx ;
   m2.dy <- m2.dy + cy ;
   m2.dz <- m2.dz + cz

  let update_position m =
   m.x <- m.x + m.dx ;
   m.y <- m.y + m.dy ;
   m.z <- m.z + m.dz

  let energy m =
   (abs m.x + abs m.y + abs m.z) * (abs m.dx + abs m.dy + abs m.dz)
 end in

 let moons =
  In_channel.(with_open_bin (if example then "12e.txt" else "12.txt") input_lines) |>
  List.map (Moon.of_string) |> Array.of_list in

 assert (Array.length moons = 4) ;

 let step () =
  for i = 0 to 2 do
   for j = i+1 to 3 do
    Moon.update_velocity moons.(i) moons.(j)
   done
  done ;
  for i = 0 to 3 do
   Moon.update_position moons.(i)
  done in

 let module MoonSet = Set.Make(struct type t = (int * int) array let compare = compare end) in
 let msx = ref (MoonSet.add (Array.map Moon.(fun m -> m.x,m.dx) moons) MoonSet.empty) in
 let msy = ref (MoonSet.add (Array.map Moon.(fun m -> m.y,m.dy) moons) MoonSet.empty) in
 let msz = ref (MoonSet.add (Array.map Moon.(fun m -> m.z,m.dz) moons) MoonSet.empty) in

 let rec gcd a b = if b = 0 then a else gcd b (a mod b) in

 let period_x = ref 0 in
 let period_y = ref 0 in
 let period_z = ref 0 in
 let i = ref 0 in
 while !period_x = 0 || !period_y = 0 || !period_z = 0 do
  incr i ; step () ;
  if !period_x = 0 then (
   let state_x = Array.map Moon.(fun m -> m.x,m.dx) moons in
   if MoonSet.mem state_x !msx then (period_x := !i; msx := MoonSet.empty) else msx := MoonSet.add state_x !msx) ;
  if !period_y = 0 then (
   let state_y = Array.map Moon.(fun m -> m.y,m.dy) moons in
   if MoonSet.mem state_y !msy then (period_y := !i; msy := MoonSet.empty) else msy := MoonSet.add state_y !msy) ;
  if !period_z = 0 then (
   let state_z = Array.map Moon.(fun m -> m.z,m.dz) moons in
   if MoonSet.mem state_z !msz then (period_z := !i; msz := MoonSet.empty) else msz := MoonSet.add state_z !msz)
 done ;
 let gcd1 = gcd !period_x !period_y in
 let gcd2 = gcd (!period_x / gcd1) !period_z in
 let gcd3 = gcd (!period_z / gcd2) !period_y in
 !period_x / gcd1 * !period_z / gcd2 * !period_y / gcd3

(* faster version using smaller keys (15bit/value) and hashtbl *)
let problem_12b2 () =
 let example = false in
 let module Moon = struct
  type t = {mutable x : int; mutable y : int; mutable z : int; mutable dx : int; mutable dy : int; mutable dz : int}

  let of_string s =
   let res = { x = 0 ; y = 0 ; z = 0 ; dx = 0 ; dy = 0 ; dz = 0 } in
   let rec loop i =
    if i >= String.length s then () else
    match s.[i] with
    | 'x' -> let len = String.index_from s i ',' - i - 2 in
      res.x <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | 'y' -> let len = String.index_from s i ',' - i - 2 in
      res.y <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | 'z' -> let len = String.index_from s i '>' - i - 2 in
      res.z <- int_of_string (String.sub s (i+2) len) ;
      loop (i+2+len+1)
    | _ -> loop (i+1) in
   loop 0 ;
   res

  let print m =
   Printf.printf "x=%d,y=%d,z=%d,dx=%d,dy=%d,dz=%d\n" m.x m.y m.z m.dx m.dy m.dz

  let update_velocity m1 m2 =
   let cx = compare m1.x m2.x in
   let cy = compare m1.y m2.y in
   let cz = compare m1.z m2.z in
   m1.dx <- m1.dx - cx ;
   m1.dy <- m1.dy - cy ;
   m1.dz <- m1.dz - cz ;
   m2.dx <- m2.dx + cx ;
   m2.dy <- m2.dy + cy ;
   m2.dz <- m2.dz + cz

  let update_position m =
   m.x <- m.x + m.dx ;
   m.y <- m.y + m.dy ;
   m.z <- m.z + m.dz

  let energy m =
   (abs m.x + abs m.y + abs m.z) * (abs m.dx + abs m.dy + abs m.dz)
 end in

 let moons =
  In_channel.(with_open_bin (if example then "12e.txt" else "12.txt") input_lines) |>
  List.map (Moon.of_string) |> Array.of_list in

 assert (Array.length moons = 4) ;

 let step () =
  for i = 0 to 2 do
   for j = i+1 to 3 do
    Moon.update_velocity moons.(i) moons.(j)
   done
  done ;
  for i = 0 to 3 do
   Moon.update_position moons.(i)
  done in

 let msx = Hashtbl.create 4096 in
 let msy = Hashtbl.create 4096 in
 let msz = Hashtbl.create 4096 in

 Hashtbl.add msx (Array.fold_left Moon.(fun (a,da) m -> (a lsl 15) lor (m.x land 0x7FFF),(da lsl 15) lor (m.dx land 0x7FFF)) (0,0) moons) ();
 Hashtbl.add msy (Array.fold_left Moon.(fun (a,da) m -> (a lsl 15) lor (m.y land 0x7FFF),(da lsl 15) lor (m.dy land 0x7FFF)) (0,0) moons) ();
 Hashtbl.add msz (Array.fold_left Moon.(fun (a,da) m -> (a lsl 15) lor (m.z land 0x7FFF),(da lsl 15) lor (m.dz land 0x7FFF)) (0,0) moons) ();

 let rec gcd a b = if b = 0 then a else gcd b (a mod b) in

 let period_x = ref 0 in
 let period_y = ref 0 in
 let period_z = ref 0 in
 let i = ref 0 in
 while !period_x = 0 || !period_y = 0 || !period_z = 0 do
  incr i ; step () ;
  if !period_x = 0 then (
   let state_x = Array.fold_left Moon.(fun (a,da) m -> (a lsl 15) lor (m.x land 0x7FFF),(da lsl 15) lor (m.dx land 0x7FFF)) (0,0) moons in
   if Hashtbl.mem msx state_x then (period_x := !i; Hashtbl.reset msx) else Hashtbl.add msx state_x ()) ;
  if !period_y = 0 then (
   let state_y = Array.fold_left Moon.(fun (a,da) m -> (a lsl 15) lor (m.y land 0x7FFF),(da lsl 15) lor (m.dy land 0x7FFF)) (0,0) moons in
   if Hashtbl.mem msy state_y then (period_y := !i; Hashtbl.reset msy) else Hashtbl.add msy state_y ()) ;
  if !period_z = 0 then (
   let state_z = Array.fold_left Moon.(fun (a,da) m -> (a lsl 15) lor (m.z land 0x7FFF),(da lsl 15) lor (m.dz land 0x7FFF)) (0,0) moons in
   if Hashtbl.mem msz state_z then (period_z := !i; Hashtbl.reset msz) else Hashtbl.add msz state_z ())
 done ;
 let gcd1 = gcd !period_x !period_y in
 let gcd2 = gcd (!period_x / gcd1) !period_z in
 let gcd3 = gcd (!period_z / gcd2) !period_y in
 !period_x / gcd1 * !period_z / gcd2 * !period_y / gcd3

let problem_13a () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "13e.txt" else "13.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =

  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in

 let tiles = ref YXMap.empty in

 (* No Input *)

 (match run inq outq with
 | () ->  ()
 | effect Inter.Yield, k ->
   Effect.Deep.discontinue k (Invalid_argument "Input Expected, but Not Given")) ;

 while not (Queue.is_empty outq) do
  let x = Queue.take outq in
  let y = Queue.take outq in
  let id = Queue.take outq in
  tiles := YXMap.update (y,x) (fun _ -> Some id) !tiles
 done ;

 YXMap.fold (fun _ id acc -> if id = 2 then succ acc else acc) !tiles 0

let problem_13b () =
 let example = false in
 let cheat = true in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "13e.txt" else "13.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =

  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in

 let tiles = ref YXMap.empty in

 (* run once to get information about the display *)
 (match run inq outq with
 | () ->  ()
 | effect Inter.Yield, k ->
   Effect.Deep.discontinue k (Invalid_argument "Input Expected, but Not Given")) ;

 while not (Queue.is_empty outq) do
  let x = Queue.take outq in
  let y = Queue.take outq in
  let id = Queue.take outq in
  tiles := YXMap.update (y,x) (fun _ -> Some id) !tiles
 done ;

 let ((h,w),_) = YXMap.max_binding !tiles in
 let (h,w) = (h+1,w+1) in
 Printf.printf "Dimensions: h=%d, w=%d\n" h w ;
 tiles := YXMap.empty ;
 let score = ref 0 in
 let ball_yx = ref (0,0) in
 let player_yx = ref (0,0) in

 let display = Array.init h (fun _ -> Bytes.make w ' ') in
 let show_display () =
  Printf.printf "Score: %d\n" !score ;
  Printf.printf "Ball: %d,%d\n" (fst !ball_yx) (snd !ball_yx);
  Printf.printf "Player: %d,%d\n" (fst !player_yx) (snd !player_yx);
  for y = 0 to h - 1 do
   print_bytes display.(y) ;
   print_newline ()
  done in

 (* insert quarters *)
 program.(0) <- 2 ;

 (* cheat by filling chasm *)
 (* 1542 = program len - 1 - 1 (key) - (w*h) (encrypted score data) - w (skip bottom row) - w (jump to start) + 1 *)
 if cheat then
  for i = 1542 to 1584 do
   program.(i) <- 3
  done ;
 program.(0) <- 2 ;
 (match run inq outq with
 | () ->
   while not (Queue.is_empty outq) do
    let x = Queue.take outq in
    let y = Queue.take outq in
    let id = Queue.take outq in
    if x = ~-1 then score := id else
    let c =
     (match id with
      | 0 -> ' '
      | 1 -> '|'
      | 2 -> '-'
      | 3 -> player_yx := (y,x) ; '='
      | 4 -> ball_yx := (y,x) ; 'o'
      | _ -> ' ') in
    Bytes.set display.(y) x c ;
   done ;
   show_display ()
 | effect Inter.Yield, k ->
   while not (Queue.is_empty outq) do
    let x = Queue.take outq in
    let y = Queue.take outq in
    let id = Queue.take outq in
    if x = ~-1 then score := id else
    let c =
     (match id with
      | 0 -> ' '
      | 1 -> '|'
      | 2 -> '-'
      | 3 -> player_yx := (y,x) ; '='
      | 4 -> ball_yx := (y,x) ; 'o'
      | _ -> ' ') in
    Bytes.set display.(y) x c ;
   done ;
   show_display ();
   let motion =
    (match (read_line ()).[0] with
    | 'a' -> (-1)
    | 'd' -> 1
    | 'q' -> (-2)
    | _ -> 0
    | exception _ -> 0) in
   if motion = (-2) then raise Exit ;
   Queue.add motion inq ;
   (* cheat *)
   if cheat then Seq.repeat 0 |> Seq.take 1000 |> Queue.add_seq inq ;
   Effect.Deep.continue k ()) ;
 !score

let problem_14a () =
 let example = false in
 let formulas = Hashtbl.create 80 in
 let add_formula s =
  let read_pair s = Scanf.sscanf s " %d %s " (fun n lbl -> (n,lbl)) in
  match String.split_on_char '>' s with
  | [left; right] ->
    let (n, lbl) = read_pair right in
    let pairs =
     String.sub left 0 (String.length left - 1) |>
     String.split_on_char ',' |>
     List.map read_pair |>
     List.sort (fun (n1,_) (n2,_) -> compare n2 n1) in
    Hashtbl.add formulas lbl (n, pairs )
  | _ -> raise (Invalid_argument "Improper Formula") in
 In_channel.(with_open_bin (if example then "14e.txt" else "14.txt") input_lines) |> List.iter add_formula ;

 (* topologically sort formulas : Kahn's Algorithm *)
 let module SSet = Set.Make(struct type t = string let compare = compare end) in
 let module NSHeap : sig
  type t
  val create : unit -> t
  val add : t -> (int * string) -> unit
  val pop_min : t -> (int * string) option
 end = struct
  type t = (int * string) Dynarray.t
  let create = Dynarray.create
  let left_child i = 2 * i + 1
  let right_child i = 2 * i + 2
  let parent_node i = (i - 1) / 2
  let ( .!() ) = Dynarray.get
  let ( .!()<- ) = Dynarray.set
  let order h i j =
    compare (fst h.!(i)) (fst h.!(j))
  let swap h i j =
    let v = h.!(i) in
    h.!(i) <- h.!(j);
    h.!(j) <- v
  let rec heap_up h i =
    if i = 0 then () else
    let parent = parent_node i in
    if order h i parent < 0 then
      (swap h i parent; heap_up h parent)
  and heap_down h ~len i =
    let left, right = left_child i, right_child i in
    if left >= len then () else
    let smallest =
      if right >= len then left else
      if order h left right < 0 then left else right
    in
    if order h i smallest > 0 then
      (swap h i smallest; heap_down h ~len smallest)
  let add h s =
    let i = Dynarray.length h in
    Dynarray.add_last h s;
    heap_up h i
  let pop_min h =
    if Dynarray.is_empty h then None
    else begin
      let last = Dynarray.length h - 1 in
      swap h 0 last;
      let best = Dynarray.pop_last h in
      heap_down h ~len:last 0;
      Some best
    end
 end in
 (*let full_set = SSet.add "ORE" (formulas |> Hashtbl.to_seq_keys |> SSet.of_seq) in*)
 let full_set = formulas |> Hashtbl.to_seq_keys |> SSet.of_seq in
 let get_orphans excluding =
  let src_set = SSet.diff full_set excluding in
  let dst_set =
   src_set |>
   SSet.to_seq |>
   Seq.map (fun key -> key |> Hashtbl.find formulas |> snd |> List.to_seq |> Seq.map snd) |>
   Seq.concat |>
   SSet.of_seq in
  SSet.diff src_set dst_set in

 let tsort () =
  let sorted = Dynarray.create () in
  let orphan_heap = NSHeap.create () in
  let removed = ref SSet.empty in
  get_orphans !removed |>
  SSet.to_seq |>
  Seq.map (fun key -> (Hashtbl.find formulas key |> fst), key) |>
  Seq.iter (NSHeap.add orphan_heap) ;
  let rec loop () =
   match NSHeap.pop_min orphan_heap with
   | Some (_,key) ->
     let (_, pairs) = Hashtbl.find formulas key in
     removed := SSet.add key !removed ;
     Dynarray.add_last sorted key ;
     let orphans = get_orphans !removed in
     List.iter (fun (n,lbl) -> if SSet.mem lbl orphans then NSHeap.add orphan_heap (n,lbl)) pairs ;
     loop ()
   | None -> sorted
  in loop () in

 let tsorted = tsort () in

 let buckets = Hashtbl.create 80 in
 let req = Queue.create () in
 let ore = ref 0 in (* terminating factor *)
 Queue.add (1, "FUEL") req ;
 while
  while not (Queue.is_empty req) do
   let (import, lbl) = Queue.take req in
   if lbl = "ORE" then ore := !ore + import
   else (
    let (export, pairs) = Hashtbl.find formulas lbl in
    let total = (Hashtbl.find_opt buckets lbl |> Option.value ~default:0) + import in
    (* check for "insufficient funds" *)
    if export > total && total <> 0 then Hashtbl.replace buckets lbl total
    else (
     let rem = total mod export in
     let factor = total / export in
     if rem = 0 then Hashtbl.remove buckets lbl else Hashtbl.replace buckets lbl rem ;
     List.iter (fun (n, lbl) -> Queue.add (factor*n,lbl) req) pairs
    )
   )
  done ; Hashtbl.length buckets <> 0 do
   (* use topological sort to select leftover to bump *)
   let pop =
    Dynarray.fold_left
    (fun acc s -> match acc with Some _ -> acc | None -> if Hashtbl.mem buckets s then Some s else None)
    None tsorted |> Option.get in
   let (export, _) = Hashtbl.find formulas pop in
   Hashtbl.replace buckets pop export ;
   Queue.add (0,pop) req
  done ;
 !ore

(* ans: 1893569 *)
let problem_14b () =
 let example = false in
 let formulas = Hashtbl.create 80 in
 let add_formula s =
  let read_pair s = Scanf.sscanf s " %d %s " (fun n lbl -> (n,lbl)) in
  match String.split_on_char '>' s with
  | [left; right] ->
    let (n, lbl) = read_pair right in
    let pairs =
     String.sub left 0 (String.length left - 1) |>
     String.split_on_char ',' |>
     List.map read_pair |>
     List.sort (fun (n1,_) (n2,_) -> compare n2 n1) in
    Hashtbl.add formulas lbl (n, pairs )
  | _ -> raise (Invalid_argument "Improper Formula") in
 In_channel.(with_open_bin (if example then "14e.txt" else "14.txt") input_lines) |> List.iter add_formula ;

 (* topologically sort formulas : Kahn's Algorithm *)
 let module SSet = Set.Make(struct type t = string let compare = compare end) in
 let module NSHeap : sig
  type t
  val create : unit -> t
  val add : t -> (int * string) -> unit
  val pop_min : t -> (int * string) option
 end = struct
  type t = (int * string) Dynarray.t
  let create = Dynarray.create
  let left_child i = 2 * i + 1
  let right_child i = 2 * i + 2
  let parent_node i = (i - 1) / 2
  let ( .!() ) = Dynarray.get
  let ( .!()<- ) = Dynarray.set
  let order h i j =
   compare (fst h.!(i)) (fst h.!(j))
  let swap h i j =
   let v = h.!(i) in
   h.!(i) <- h.!(j);
   h.!(j) <- v
  let rec heap_up h i =
   if i = 0 then () else
   let parent = parent_node i in
   if order h i parent < 0 then
    (swap h i parent; heap_up h parent)
  and heap_down h ~len i =
   let left, right = left_child i, right_child i in
   if left >= len then () else
   let smallest =
    if right >= len then left else
    if order h left right < 0 then left else right
   in
   if order h i smallest > 0 then
    (swap h i smallest; heap_down h ~len smallest)
  let add h s =
   let i = Dynarray.length h in
   Dynarray.add_last h s;
   heap_up h i
  let pop_min h =
   if Dynarray.is_empty h then None
   else begin
    let last = Dynarray.length h - 1 in
    swap h 0 last;
    let best = Dynarray.pop_last h in
    heap_down h ~len:last 0;
    Some best
   end
 end in
 (*let full_set = SSet.add "ORE" (formulas |> Hashtbl.to_seq_keys |> SSet.of_seq) in*)
 let full_set = formulas |> Hashtbl.to_seq_keys |> SSet.of_seq in
 let get_orphans excluding =
  let src_set = SSet.diff full_set excluding in
  let dst_set =
   src_set |>
   SSet.to_seq |>
   Seq.map (fun key -> key |> Hashtbl.find formulas |> snd |> List.to_seq |> Seq.map snd) |>
   Seq.concat |>
   SSet.of_seq in
  SSet.diff src_set dst_set in

 let tsort () =
  let sorted = Dynarray.create () in
  let orphan_heap = NSHeap.create () in
  let removed = ref SSet.empty in
  get_orphans !removed |>
  SSet.to_seq |>
  Seq.map (fun key -> (Hashtbl.find formulas key |> fst), key) |>
  Seq.iter (NSHeap.add orphan_heap) ;
  let rec loop () =
   match NSHeap.pop_min orphan_heap with
   | Some (_,key) ->
     let (_, pairs) = Hashtbl.find formulas key in
     removed := SSet.add key !removed ;
     Dynarray.add_last sorted key ;
     let orphans = get_orphans !removed in
     List.iter (fun (n,lbl) -> if SSet.mem lbl orphans then NSHeap.add orphan_heap (n,lbl)) pairs ;
     loop ()
   | None -> sorted
  in loop () in

 let tsorted = tsort () in

 let simulate fuel =
  let buckets = Hashtbl.create 80 in
  let req = Queue.create () in
  let ore = ref 0 in (* terminating factor *)
  Queue.add (fuel, "FUEL") req ;
  while
   while not (Queue.is_empty req) do
    let (import, lbl) = Queue.take req in
    if lbl = "ORE" then ore := !ore + import
    else (
     let (export, pairs) = Hashtbl.find formulas lbl in
     let total = (Hashtbl.find_opt buckets lbl |> Option.value ~default:0) + import in
     (* check for "insufficient funds" *)
     if export > total && total <> 0 then Hashtbl.replace buckets lbl total
     else (
      let rem = total mod export in
      let factor = total / export in
      if rem = 0 then Hashtbl.remove buckets lbl else Hashtbl.replace buckets lbl rem ;
      List.iter (fun (n, lbl) -> Queue.add (factor*n,lbl) req) pairs
     )
    )
   done ; Hashtbl.length buckets <> 0 do
    (* use topological sort to select which leftover to bump first! *)
    let pop =
     Dynarray.fold_left
     (fun acc s -> match acc with Some _ -> acc | None -> if Hashtbl.mem buckets s then Some s else None)
     None tsorted |> Option.get in
    let (export, _) = Hashtbl.find formulas pop in
    Hashtbl.replace buckets pop export ;
    Queue.add (0,pop) req
   done ;
  !ore in
 let trillion = 1_000_000_000_000 in
 let fuel1 = simulate 1 in
 (* a good first guess for lo *)
 let lo = ref (trillion / fuel1) in
 (* a good first guess for hi *)
 let hi = ref (!lo * 2) in
 (* adjust starting points *)
 while simulate !lo > trillion do lo := !lo / 2 done ;
 while simulate !hi < trillion do hi := !hi * 2 done ;
 (* binary search *)
 while !hi - !lo > 1 do
  let mid = (!hi + !lo) / 2 in
  if simulate mid > trillion then hi := mid else lo := mid
 done ;
 !lo

(* use right-hand on wall technique to search for X, and keep track of optimal distances *)
let problem_15a () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "15e.txt" else "15.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in
 let module Dynmat = struct
  type 'a t = {mutable min_x : int ; mutable max_x : int ; mutable min_y : int ; mutable max_y : int ; mat : 'a Dynarray.t}

  let create () =
   { min_x = 0 ; min_y = 0 ; max_x = 0 ; max_y = 0 ; mat = Dynarray.create ()}

  let init r =
   { min_x = ~-r ; min_y = ~-r ; max_x = r ; max_y = r ; mat = Dynarray.create ()}

  let update dm (y,x) =
   dm.min_x <- min x dm.min_x ;
   dm.min_y <- min y dm.min_y ;
   dm.max_x <- max x dm.max_x ;
   dm.max_y <- max y dm.max_y

  let is_dirty dm =
   Dynarray.length dm.mat <> ((dm.max_y - dm.min_y + 1) * (dm.max_x - dm.min_x + 1))

  let set_dirty dm =
   Dynarray.clear dm.mat

  let fill dm c =
   let len = ((dm.max_y - dm.min_y + 1) * (dm.max_x - dm.min_x + 1)) in
   if Dynarray.length dm.mat = len then (
    for i = 0 to len - 1 do Dynarray.set dm.mat i c done
   ) else (
    Dynarray.clear dm.mat ;
    for i = 0 to len - 1 do Dynarray.add_last dm.mat c done )

  let get dm (y,x) =
   Dynarray.get dm.mat ((y-dm.min_y)*(dm.max_x - dm.min_x + 1)+x-dm.min_x)

  let set dm (y,x) c =
   Dynarray.set dm.mat ((y-dm.min_y)*(dm.max_x - dm.min_x + 1)+x-dm.min_x) c

 end in

 let show_display (dm : char Dynmat.t) =
  for y = dm.min_y to dm.max_y do
   for x = dm.min_x to dm.max_x do
    print_char (Dynmat.get dm (y,x))
   done ;
   print_newline ()
  done in

 let show_display_stat (dm : char Dynmat.t) y x =
  Printf.printf "Y: [%d,%d]\nX: [%d,%d]\n" dm.min_y dm.max_y dm.min_x dm.max_x ;
  Dynmat.set dm (y,x) 'D' ;
  show_display dm in

 let refresh_display dm map =
  YXMap.iter (fun k _ -> Dynmat.update dm k) map ;
  (*if Dynmat.is_dirty dm then Dynmat.fill dm ' ' ;*)
  Dynmat.fill dm ' ';
  YXMap.iter (Dynmat.set dm) map in

 let cur_x = ref 0 in
 let cur_y = ref 0 in
 let cur_d = ref 0 in
 let last_motion = ref (-1,0) in
 let total_moves = ref 0 in
 let auto = ref false in
 let goal = ref None in
 let tiles = YXMap.add (!cur_y,!cur_x) '.' YXMap.empty |> ref in
 let dists = YXMap.add (!cur_y,!cur_x) 0 YXMap.empty |> ref in
 (*let display = Dynmat.create () in*)
 let display = Dynmat.init 5 in
 let pending = Queue.create () in

 let rotate lm =
  match !lm with
  | (-1, 0) -> lm := ( 0,-1)
  | ( 0, 1) -> lm := (-1, 0)
  | ( 1, 0) -> lm := ( 0, 1)
  | ( 0,-1) -> lm := ( 1, 0)
  | _ -> assert false in

 let step () =
  if Queue.is_empty outq then Queue.clear pending ;
  let y0 = !cur_y in
  let x0 = !cur_x in
  while not (Queue.is_empty outq) do
   let (dy, dx) = Queue.take pending in
   match Queue.take outq with
   | 0 -> tiles := YXMap.add (!cur_y+dy,!cur_x+dx) '#' !tiles
   | 1 -> tiles := YXMap.add (!cur_y+dy,!cur_x+dx) '.' !tiles ;
          dists :=
           YXMap.update (!cur_y+dy,!cur_x+dx)
           (fun d ->
            cur_d := Option.value d ~default:(!cur_d + 1) ;
            Some (!cur_d)) !dists ;
          cur_y := !cur_y + dy ;
          cur_x := !cur_x + dx ;
          incr total_moves ;
          last_motion := (dy,dx)
   | 2 -> tiles := YXMap.add (!cur_y+dy,!cur_x+dx) 'X' !tiles ;
          dists :=
           YXMap.update (!cur_y+dy,!cur_x+dx)
           (fun d ->
            cur_d := Option.value d ~default:(!cur_d + 1) ;
            Some (!cur_d)) !dists ;
          cur_y := !cur_y + dy ;
          cur_x := !cur_x + dx ;
          incr total_moves ;
          last_motion := (dy,dx) ;
          goal := Some (!cur_y, !cur_x)
   | _ -> raise (Invalid_argument "Invalid Program Response!")
  done ;
  let continue = ref true in
  if !total_moves > 2000 then auto := false ;
  if !auto = false then (
   total_moves := 0 ; (* to allow multiple A runs *)
   refresh_display display !tiles ;
   show_display_stat display !cur_y !cur_x;
   print_string "Enter Motions: " ;
   let input = read_line () in
   String.iter
   (function
    | 'a' -> Queue.add ( 0,-1) pending ; Queue.add 3 inq
    | 's' -> Queue.add ( 1, 0) pending ; Queue.add 2 inq
    | 'd' -> Queue.add ( 0, 1) pending ; Queue.add 4 inq
    | 'w' -> Queue.add (-1, 0) pending ; Queue.add 1 inq
    | 'A' ->
      auto := true ;
      if x0 = !cur_x && y0 = !cur_y then rotate last_motion ;
      (match !last_motion with
      | (-1, 0) -> Queue.add ( 0, 1) pending ; Queue.add 4 inq ; Queue.add (-1, 0) pending ; Queue.add 1 inq
      | ( 0, 1) -> Queue.add ( 1, 0) pending ; Queue.add 2 inq ; Queue.add ( 0, 1) pending ; Queue.add 4 inq
      | ( 1, 0) -> Queue.add ( 0,-1) pending ; Queue.add 3 inq ; Queue.add ( 1, 0) pending ; Queue.add 2 inq
      | ( 0,-1) -> Queue.add (-1, 0) pending ; Queue.add 1 inq ; Queue.add ( 0,-1) pending ; Queue.add 3 inq
      | _ -> assert false)
    | 'Q' -> continue := false
    | _ -> ()) input )
  else (
   if x0 = !cur_x && y0 = !cur_y then rotate last_motion ;
   (match !last_motion with
   | (-1, 0) -> Queue.add ( 0, 1) pending ; Queue.add 4 inq ; Queue.add (-1, 0) pending ; Queue.add 1 inq
   | ( 0, 1) -> Queue.add ( 1, 0) pending ; Queue.add 2 inq ; Queue.add ( 0, 1) pending ; Queue.add 4 inq
   | ( 1, 0) -> Queue.add ( 0,-1) pending ; Queue.add 3 inq ; Queue.add ( 1, 0) pending ; Queue.add 2 inq
   | ( 0,-1) -> Queue.add (-1, 0) pending ; Queue.add 1 inq ; Queue.add ( 0,-1) pending ; Queue.add 3 inq
   | _ -> assert false)
  );
  !continue in

 run_with step ;
 Option.map (fun k -> YXMap.find_opt k !dists) !goal |>
 Option.join
 (*run_with (fun _ -> false)*)

(*
Enter Motions: A
Y: [-21,19]
X: [-21,19]
 ####### ########### ################# #
#.......#...........#.................#.#
#.#####.#.#######.#.#.###.###########.#.#
#.....#.#.#...#...#.#.#...#.........#...#
 ######.#.###.#.###.#.#####.#######.####
#...#...#...#.#.#.....#...#.#...#...#...#
#.#.#.#####.#.#.#.#####.#.#.###.#.###.#.#
#.#...#...#...#.#.#.....#...#...#.#...#.#
#.#####.#.###.#.###.#########.###.#.###.#
#.......#.....#...#.......#...#...#.#...#
 ##############.#.#######.#.#.#.###.#.##
#.....#.......#.#.....#...#.#.#...#.#.#.#
#.###.#.#####.###.#.#.#.###.#.###.#.#.#.#
#.#.....#...#...#.#.#.#.#...#...#.#.#...#
#.#######D#####.###.#.#.#.## ##.#.#.###.#
#.....#...#.........#.#.#...#...#...#...#
#.###.###.#.#########.#.###.#### ####.##
#.#.#...#...#...#.#...#...#.....#...#...#
#.#.###.#.###.#.#.#.#####.#####.#.#.###.#
#.....#.#...#.#.#...#.....#.....#.#.....#
 ####.#.###.#.#.#####.#####.#####.######
#...#.#.#...#.#.....#.#...........#.....#
#.###.#.#####.#####.###.#############.#.#
#.#...#.........#...#...#.............#.#
#.#.#############.###.#########.#.######
#.#.#...#.#.....#...#.#.......#.#.#.....#
#.#.#.#.#.#.###.###.#.#.#####.#.###.###.#
#...#.#...#.#.......#.#.#...#...#.....#.#
#.###.###.#.#######.#.#.#.###.###.#####.#
#.#...#...#.#...#...#.#.#.....#...#.....#
#.#.###.###.#.#.###.#.#.###.###.###.####
#...#...#...#.#...#.#.#...#...#.#.#.....#
 ####.###.###.###.###.#.#.#####.#.#####.#
#...#...#.....#.#...#.#.#.#...........#.#
 ##.###.#######.###.#.###.#.#######.###.#
#...#...#.......#...#...#...#...#X..#...#
#.###.#########.#.#.###.#####.#.#####.#.#
#.#...#.......#.#.#.#...#...#.#...#...#.#
#.#.###.#####.#.#.###.###.#.#.###.#.###.#
#.......#.......#.........#...#.....#...#
 ####### ####### ######### ### ##### ###
Enter Motions: Q
- : int option = Some 230
*)

(* once you find the oxygen tank, start distance calculations *)
(* continue until you find the oxygen tank again! *)
let problem_15b () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "15e.txt" else "15.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in
 let module Dynmat = struct
  type 'a t = {mutable min_x : int ; mutable max_x : int ; mutable min_y : int ; mutable max_y : int ; mat : 'a Dynarray.t}

  let create () =
   { min_x = 0 ; min_y = 0 ; max_x = 0 ; max_y = 0 ; mat = Dynarray.create ()}

  let init r =
   { min_x = ~-r ; min_y = ~-r ; max_x = r ; max_y = r ; mat = Dynarray.create ()}

  let update dm (y,x) =
   dm.min_x <- min x dm.min_x ;
   dm.min_y <- min y dm.min_y ;
   dm.max_x <- max x dm.max_x ;
   dm.max_y <- max y dm.max_y

  let is_dirty dm =
   Dynarray.length dm.mat <> ((dm.max_y - dm.min_y + 1) * (dm.max_x - dm.min_x + 1))

  let set_dirty dm =
   Dynarray.clear dm.mat

  let fill dm c =
   let len = ((dm.max_y - dm.min_y + 1) * (dm.max_x - dm.min_x + 1)) in
   if Dynarray.length dm.mat = len then (
    for i = 0 to len - 1 do Dynarray.set dm.mat i c done
   ) else (
    Dynarray.clear dm.mat ;
    for i = 0 to len - 1 do Dynarray.add_last dm.mat c done )

  let get dm (y,x) =
   Dynarray.get dm.mat ((y-dm.min_y)*(dm.max_x - dm.min_x + 1)+x-dm.min_x)

  let set dm (y,x) c =
   Dynarray.set dm.mat ((y-dm.min_y)*(dm.max_x - dm.min_x + 1)+x-dm.min_x) c

 end in

 let show_display (dm : char Dynmat.t) =
  for y = dm.min_y to dm.max_y do
   for x = dm.min_x to dm.max_x do
    print_char (Dynmat.get dm (y,x))
   done ;
   print_newline ()
  done in

 let show_display_stat (dm : char Dynmat.t) y x =
  Printf.printf "Y: [%d,%d]\nX: [%d,%d]\n" dm.min_y dm.max_y dm.min_x dm.max_x ;
  Dynmat.set dm (y,x) 'D' ;
  show_display dm in

 let refresh_display dm map =
  YXMap.iter (fun k _ -> Dynmat.update dm k) map ;
  (*if Dynmat.is_dirty dm then Dynmat.fill dm ' ' ;*)
  Dynmat.fill dm ' ';
  YXMap.iter (Dynmat.set dm) map in

 let cur_x = ref 0 in
 let cur_y = ref 0 in
 let cur_d = ref 0 in
 let last_motion = ref (-1,0) in
 let total_moves = ref 0 in
 let auto = ref false in
 let goal = ref None in
 let tiles = YXMap.add (!cur_y,!cur_x) '.' YXMap.empty |> ref in
 let dists = YXMap.add (!cur_y,!cur_x) 0 YXMap.empty |> ref in
 (*let display = Dynmat.create () in*)
 let display = Dynmat.init 5 in
 let pending = Queue.create () in

 let rotate lm =
  match !lm with
  | (-1, 0) -> lm := ( 0,-1)
  | ( 0, 1) -> lm := (-1, 0)
  | ( 1, 0) -> lm := ( 0, 1)
  | ( 0,-1) -> lm := ( 1, 0)
  | _ -> assert false in

 let step () =
  if Queue.is_empty outq then Queue.clear pending ;
  let y0 = !cur_y in
  let x0 = !cur_x in
  while not (Queue.is_empty outq) do
   let (dy, dx) = Queue.take pending in
   match Queue.take outq with
   | 0 -> tiles := YXMap.add (!cur_y+dy,!cur_x+dx) '#' !tiles
   | 1 -> tiles := YXMap.add (!cur_y+dy,!cur_x+dx) '.' !tiles ;
          dists :=
           YXMap.update (!cur_y+dy,!cur_x+dx)
           (fun d ->
            cur_d := Option.value d ~default:(!cur_d + 1) ;
            Some (!cur_d)) !dists ;
          cur_y := !cur_y + dy ;
          cur_x := !cur_x + dx ;
          incr total_moves ;
          last_motion := (dy,dx)
   | 2 -> tiles := YXMap.add (!cur_y+dy,!cur_x+dx) 'X' !tiles ;
          if Option.is_none !goal
          then (dists := YXMap.empty ; cur_d := (-1))
          else (print_endline "Solution Found! Okay to Exit!") ;
          dists :=
           YXMap.update (!cur_y+dy,!cur_x+dx)
           (fun d ->
            cur_d := Option.value d ~default:(!cur_d + 1) ;
            Some (!cur_d)) !dists ;
          cur_y := !cur_y + dy ;
          cur_x := !cur_x + dx ;
          incr total_moves ;
          last_motion := (dy,dx) ;
          goal := Some (!cur_y, !cur_x)
   | _ -> raise (Invalid_argument "Invalid Program Response!")
  done ;
  let continue = ref true in
  if !total_moves > 4000 then auto := false ;
  if !auto = false then (
   total_moves := 0 ; (* to allow multiple A runs *)
   refresh_display display !tiles ;
   show_display_stat display !cur_y !cur_x;
   print_string "Enter Motions: " ;
   let input = read_line () in
   String.iter
   (function
    | 'a' -> Queue.add ( 0,-1) pending ; Queue.add 3 inq
    | 's' -> Queue.add ( 1, 0) pending ; Queue.add 2 inq
    | 'd' -> Queue.add ( 0, 1) pending ; Queue.add 4 inq
    | 'w' -> Queue.add (-1, 0) pending ; Queue.add 1 inq
    | 'A' ->
      auto := true ;
      if x0 = !cur_x && y0 = !cur_y then rotate last_motion ;
      (match !last_motion with
      | (-1, 0) -> Queue.add ( 0, 1) pending ; Queue.add 4 inq ; Queue.add (-1, 0) pending ; Queue.add 1 inq
      | ( 0, 1) -> Queue.add ( 1, 0) pending ; Queue.add 2 inq ; Queue.add ( 0, 1) pending ; Queue.add 4 inq
      | ( 1, 0) -> Queue.add ( 0,-1) pending ; Queue.add 3 inq ; Queue.add ( 1, 0) pending ; Queue.add 2 inq
      | ( 0,-1) -> Queue.add (-1, 0) pending ; Queue.add 1 inq ; Queue.add ( 0,-1) pending ; Queue.add 3 inq
      | _ -> assert false)
    | 'Q' -> continue := false
    | _ -> ()) input )
  else (
   if x0 = !cur_x && y0 = !cur_y then rotate last_motion ;
   (match !last_motion with
   | (-1, 0) -> Queue.add ( 0, 1) pending ; Queue.add 4 inq ; Queue.add (-1, 0) pending ; Queue.add 1 inq
   | ( 0, 1) -> Queue.add ( 1, 0) pending ; Queue.add 2 inq ; Queue.add ( 0, 1) pending ; Queue.add 4 inq
   | ( 1, 0) -> Queue.add ( 0,-1) pending ; Queue.add 3 inq ; Queue.add ( 1, 0) pending ; Queue.add 2 inq
   | ( 0,-1) -> Queue.add (-1, 0) pending ; Queue.add 1 inq ; Queue.add ( 0,-1) pending ; Queue.add 3 inq
   | _ -> assert false)
  );
  !continue in

 run_with step ;
 YXMap.fold (fun _ v acc -> max v acc) !dists 0

(*
Enter Motions: A
Solution Found! Okay to Exit!
Y: [-21,19]
X: [-21,19]
 ####### ########### ################# #
#.......#...........#.................#.#
#.#####.#.#######.#.#.###.###########.#.#
#.....#.#.#...#...#.#.#...#.........#...#
 ######.#.###.#.###.#.#####.#######.####
#...#...#...#.#.#.....#...#.#...#...#...#
#.#.#.#####.#.#.#.#####.#.#.###.#.###.#.#
#.#...#...#...#.#.#.....#...#...#.#...#.#
#.#####.#.###.#.###.#########.###.#.###.#
#.......#.....#...#.......#...#...#.#...#
 ##############.#.#######.#.#.#.###.#.##
#.....#.......#.#.....#...#.#.#...#.#.#.#
#.###.#.#####.###.#.#.#.###.#.###.#.#.#.#
#.#.....#...#...#.#.#.#.#...#...#.#.#...#
#.#######.#####.###.#.#.#.## ##.#.#.###.#
#.....#...#.........#.#.#...#...#...#...#
#.###.###.#.#########.#.###.#### ####.##
#.#.#...#...#...#.#...#...#.....#...#...#
#.#.###.#.###.#.#.#.#####.#####.#.#.###.#
#.....#.#...#.#.#...#.....#.....#.#.....#
 ####.#.###.#.#.#####.#####.#####.######
#...#.#.#...#.#.....#.#...........#.....#
#D###.#.#####.#####.###.#############.#.#
#.#...#.........#...#...#.............#.#
#.#.#############.###.#########.#.######
#.#.#...#.#.....#...#.#.......#.#.#.....#
#.#.#.#.#.#.###.###.#.#.#####.#.###.###.#
#...#.#...#.#.......#.#.#...#...#.....#.#
#.###.###.#.#######.#.#.#.###.###.#####.#
#.#...#...#.#...#...#.#.#.....#...#.....#
#.#.###.###.#.#.###.#.#.###.###.###.####
#...#...#...#.#...#.#.#...#...#.#.#.....#
 ####.###.###.###.###.#.#.#####.#.#####.#
#...#...#.....#.#...#.#.#.#...........#.#
 ##.###.#######.###.#.###.#.#######.###.#
#...#...#.......#...#...#...#...#X..#...#
#.###.#########.#.#.###.#####.#.#####.#.#
#.#...#.......#.#.#.#...#...#.#...#...#.#
#.#.###.#####.#.#.###.###.#.#.###.#.###.#
#.......#.......#.........#...#.....#...#
 ####### ####### ######### ### ##### ###
Enter Motions: Q
- : int = 288
*)

(* Problem 16: FFT - Flawed Frequency Transmission *)
(* naive implementation *)
let problem_16a () =
 let example = false in
 let int_of_digit_opt c =
  match c with
  | '0'..'9' -> Some (Char.code c - 0x30)
  | _ -> None in

 (* ipos ~= row; opos ~= col of transformation matrix *)
 let pattern ipos opos =
  let base = [|0;1;0;-1|] in
  base.(((ipos + 1) / (opos + 1)) mod 4) in

 let print_prelude digits =
  for i = 0 to 7 do
   print_char (Char.chr (digits.(i) mod 10 + 0x30))
  done ;
  print_newline () in

 let input =
  In_channel.(with_open_bin (if example then "16e.txt" else "16.txt") input_line) |>
  Option.get |> String.to_seq |> Seq.filter_map int_of_digit_opt |>
  Array.of_seq in
 let swaps = Array.init 2 (fun _ -> Array.copy input) in
 let iterate i =
  let input = swaps.(i land 1) in
  let output = swaps.((i+1) land 1) in
  for opos = 0 to Array.length input - 1 do
   output.(opos) <-
    Array.fold_left
    (fun (ipos,a) n -> ipos+1, a+(pattern ipos opos)*n) (0,0)
    input |>
    (fun (_,n) -> abs n mod 10)
  done in
 for i = 0 to 99 do
  iterate i
 done ;
 print_prelude swaps.(0)

(*
let pattern ipos opos =
 let base = [|0;1;0;-1|] in
 base.(((ipos + 1) / (opos + 1)) mod 4)

80871224585914546619083218645595
*)

(* how the matrix looks like for the example len = 32 ; R format, by_col; upper triangular matrix *)
(* -1 is substituted with X for alignment *)
(*
A <-
 matrix(
  c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    X,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,X,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    X,X,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,0,X,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,1,X,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    X,1,X,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,X,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,0,0,X,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,X,0,X,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    X,X,1,X,X,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,1,0,X,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,0,1,0,X,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,X,X,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    X,1,0,0,X,X,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,1,0,X,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
    1,0,X,1,0,X,X,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
    0,X,X,1,0,X,X,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
    X,X,X,1,0,X,X,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,X,X,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
    1,0,0,0,1,0,X,X,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
    0,1,0,0,1,0,X,X,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
    X,1,1,0,1,0,X,X,X,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
    0,0,1,X,1,0,0,X,X,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
    1,0,1,X,1,0,0,X,X,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
    0,X,0,X,0,1,0,X,X,X,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
    X,X,0,X,0,1,0,X,X,X,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
    0,0,0,0,0,1,0,0,X,X,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
    nrow=32)
*)

(* Problem 16: FFT - Flawed Frequency Transmission *)
(* because transformation matrix A is an upper triangular,
   output i only depends on inputs j, where j >= i.
   if i >= n/2, output i is simply a running sum of inputs i <= j < n mod 10. *)

(* for our input: 5976629 is squarely in the latter half of 6,500,000 *)
(* the same is true for all examples *)

let problem_16b () =
 let example = false in
 let int_of_digit_opt c =
  match c with
  | '0'..'9' -> Some (Char.code c - 0x30)
  | _ -> None in

 (* ipos ~= row; opos ~= col of transformation matrix *)
 (*
 let pattern ipos opos =
  let base = [|0;1;0;-1|] in
  base.(((ipos + 1) / (opos + 1)) mod 4) in
 *)

 let input =
  In_channel.(with_open_bin (if example then "16e.txt" else "16.txt") input_line) |>
  Option.get |> String.to_seq |> Seq.filter_map int_of_digit_opt |>
  Array.of_seq in

 let offset =
  input |> Array.to_seq |> Seq.take 7 |>
  Seq.fold_left (fun a n -> a*10+n) 0 in

 (* the cheat will not work if this assertion fails! *)
 assert (offset >= (Array.length input * 10_000 / 2)) ;

 (* if assert fails: here are more granular methods up to n/6 *)
 (*
 --++--: iterating backwards

 for n/2 <= i < n:
 old(i)+new(i+1) == sum(i)

 for n/3 <= i < n/2:
 sum(i)-sum(i*2+1)

 for n/4 <= i < n/3:
 sum(i)-sum(i*2+1)-sum(i*3+2)

 for n/5 <= i < n/4:
 sum(i)-sum(i*2+1)-sum(i*3+2)+sum(i*4+3)

 for n/6 <= i < n/5:
 sum(i)-sum(i*2+1)-sum(i*3+2)+sum(i*4+3)+sum(i*5+4)
 *)

 let data = Array.init (Array.length input * 10_000 - offset) (fun i -> input.((offset+i) mod (Array.length input))) in

 let iterate () =
  for i = Array.length data - 2 downto 0 do
   data.(i) <- (data.(i) + data.(i+1)) mod 10
  done in

 let print_prelude () =
  for i = 0 to 7 do
   print_char (Char.chr (data.(i) mod 10 + 0x30))
  done ;
  print_newline () in

 for i = 1 to 100 do
  iterate ()
 done ;
 print_prelude ()

(* IntCode: locate scaffolding intersection points *)
let problem_17a () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "17e.txt" else "17.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let step () = false in

 run_with step ;
 let input =
  outq |>
  Queue.to_seq |>
  Seq.map Char.chr |>
  String.of_seq in
 if debug then print_string input ;
 let map =
  input |>
  String.split_on_char '\n' |>
  List.filter (fun s -> String.length s > 1) |> (* remove trailing \n\n *)
  Array.of_list in
 (
  let res = ref 0 in
  for y = 1 to Array.length map - 2 do
   for x = 1 to String.length map.(0) - 2 do
    (* <> '.' is quicker than = '#' or '^' or '<' or '>' or 'v', 'X' should be impossible *)
    if map.(y).[x] <> '.' &&
       map.(y-1).[x] <> '.' &&
       map.(y+1).[x] <> '.' &&
       map.(y).[x-1] <> '.' &&
       map.(y).[x+1] <> '.' then
    res := !res + x * y
   done
  done;
  !res
 )

(* part b is easy, only turn when you have to, not at intersection points *)
(* movement functions can be at most 20 ascii code points! (21 with \n termination) *)
let problem_17b () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "17e.txt" else "17.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let step () = false in

(*
 (* get map *)
 run_with step ;
 let input =
  outq |>
  Queue.to_seq |>
  Seq.map Char.chr |>
  String.of_seq in
 if debug then print_string input ;
 let map =
  input |>
  String.split_on_char '\n' |>
  List.filter (fun s -> String.length s > 1) |> (* remove trailing \n\n *)
  Array.of_list in

 (* first find path (easy), then figure out how to encode it, like a compression algorithm (hard?) *)
 (* you might be able to use BPE *)
 (* piece will start on an edge *)
 (* because we're using ascii, you can move foward a maximum of 9 spaces/char!, due to commas, you only get 10 max moves. *)
 (* for the initial pass, encode L as (-2) and R as (-1), with no upper limit on forward motion. *)
 let moves = Dynarray.create () in
 let cur_y = ref (-1) in
 let cur_x = ref (-1) in
 let cur_dir = ref (-1) in (* 0 = N, 1 = E, 2 = S, 3 = W ; R = +1, L = -1 *)
 for y = 0 to Array.length map - 1 do
  for x = 0 to String.length map.(0) - 1 do
   if map.(y).[x] <> '.' && map.(y).[x] <> '#'
   then (cur_y := y; cur_x := x) else ()
  done
 done ;
 cur_pos :=
  (match map.(!cur_y).[!cur_x] with
   | '^' -> 0
   | '>' -> 1
   | 'v' -> 2
   | '<' -> 3
   | _ -> (-1)) ;
*)

 (* by hand calculation *)
 let main_routine = "A,B,A,C,A,B,C,B,C,B\n" in
 let function_a = "L,10,R,8,L,6,R,6\n" in
 let function_b = "L,8,L,8,R,8\n" in
 let function_c = "R,8,L,6,L,10,L,10\n" in
 let no_feed = "n\n" in

 (* change modes *)
 program.(0) <- 2 ;
 Queue.clear inq ;
 Queue.clear outq ;

 String.iter (fun c -> Queue.push (Char.code c) inq) main_routine ;
 String.iter (fun c -> Queue.push (Char.code c) inq) function_a ;
 String.iter (fun c -> Queue.push (Char.code c) inq) function_b ;
 String.iter (fun c -> Queue.push (Char.code c) inq) function_c ;
 String.iter (fun c -> Queue.push (Char.code c) inq) no_feed ;

 run_with step ;
 Queue.to_seq outq |> Seq.iter (fun n -> if n < 255 then print_char (Char.chr n) else (print_int n ; print_newline ()))

let problem_17b2 () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "17e.txt" else "17.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let step () = false in

 (* get map *)
 run_with step ;
 let input =
  outq |>
  Queue.to_seq |>
  Seq.map Char.chr |>
  String.of_seq in
 if debug then print_string input ;
 let map =
  input |>
  String.split_on_char '\n' |>
  List.filter (fun s -> String.length s > 1) |> (* remove trailing \n\n *)
  Array.of_list in

 let h = Array.length map in
 let w = String.length map.(0) in

 (* find cursor *)
 let cur_y = ref (-1) in
 let cur_x = ref (-1) in
 let cur_d = ref (-1) in (* 0 = N, 1 = E, 2 = S, 3 = W ; R = +1, L = -1 *)
 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   if map.(y).[x] <> '.' && map.(y).[x] <> '#'
   then (cur_y := y; cur_x := x) else ()
  done
 done ;
 cur_d :=
  (match map.(!cur_y).[!cur_x] with
   | '^' -> 0
   | '>' -> 1
   | 'v' -> 2
   | '<' -> 3
   | _ -> (-1)) ;

 (* turn logic *)
 let next_turn y x = function
 | 0 -> (* N *)
   if (x-1) >= 0 && map.(y).[x-1] = '#' then Some (-1)
   else if (x+1) < w && map.(y).[x+1] = '#' then Some 1
   else None
 | 1 -> (* E *)
   if (y-1) >= 0 && map.(y-1).[x] = '#' then Some (-1)
   else if (y+1) < h && map.(y+1).[x] = '#' then Some 1
   else None
 | 2 -> (* S *)
   if (x-1) >= 0 && map.(y).[x-1] = '#' then Some 1
   else if (x+1) < w && map.(y).[x+1] = '#' then Some (-1)
   else None
 | 3 -> (* W *)
   if (y-1) >= 0 && map.(y-1).[x] = '#' then Some 1
   else if (y+1) < h && map.(y+1).[x] = '#' then Some (-1)
   else None
 | _ -> assert false in

 let move_forward y x = function
 | 0 -> (* N *)
   if (y-1) >= 0 && map.(y-1).[x] = '#' then (decr cur_y ; true) else false
 | 1 -> (* E *)
   if (x+1) < w && map.(y).[x+1] = '#' then (incr cur_x ; true) else false
 | 2 -> (* S *)
   if (y+1) < h && map.(y+1).[x] = '#' then (incr cur_y ; true) else false
 | 3 -> (* W *)
   if (x-1) >= 0 && map.(y).[x-1] = '#' then (decr cur_x ; true) else false
 | _ -> assert false in

 let next_forward () =
  let steps = ref 0 in
  while move_forward !cur_y !cur_x !cur_d do incr steps done;
  !steps in

 (* make sure we need to turn first! *)
 assert (next_forward () = 0) ;

 (* find path *)
 let moves = Dynarray.create () in
 while
  match next_turn !cur_y !cur_x !cur_d with
   | Some (-1) -> (* left *)
     Dynarray.add_last moves (-2) ;
     cur_d := (!cur_d + 3) mod 4 ;
     true
   | Some (1) -> (* right *)
     Dynarray.add_last moves (-1) ;
     cur_d := (!cur_d + 1) mod 4 ;
     true
   | None -> false
   | _ -> assert false
 do
  Dynarray.add_last moves (next_forward ())
 done ;

 (* check moves : valid *)
 if debug then Dynarray.iter (fun n -> print_int n ; print_newline ()) moves ;

 let move_list = Dynarray.to_list moves in

 let gen_freq stream =
  let tbl = Hashtbl.create 20 in
  stream |>
  List.fold_left
  (fun last n ->
   match last with
   | None -> Some n
   | Some p ->
     let count = Hashtbl.find_opt tbl (p,n) |> Option.value ~default:0 in
     Hashtbl.replace tbl (p,n) (succ count) ;
     Some n) None |> ignore ;
  tbl in

 let collapse_pair (l,r) m stream =
  Seq.zip (List.to_seq stream) (Seq.append (List.to_seq stream |> Seq.drop 1) (Seq.return m)) |>
  Seq.fold_left
  (fun (a,s) (n1,n2) ->
   if s then (a,false) else
   if n1 = l && n2 = r then (m::a,true) else (n1::a,false)) ([],false) |> fst |>
  List.rev in

 let encodings = Dynarray.create () in
 Hashtbl.iter (fun (l,r) _ -> if l < 0 then Dynarray.add_last encodings (l,r) else ()) (gen_freq move_list) ;
 let compressed =
  Dynarray.fold_left (fun (a,m) (l,r) -> collapse_pair (l,r) m a, m-1) (move_list,-3) encodings |> fst |> ref in

 let uniq = Dynarray.create () in
 while
  Dynarray.clear uniq ;
  List.iter (fun n -> if Dynarray.mem n uniq |> not then Dynarray.add_last uniq n) !compressed ;
  Dynarray.length uniq > 3
 do
  let freq = gen_freq !compressed in
  (* NOTE: edge cases not covered ! *)
  (* TODO: add dependancy chain logic for more robustness *)
  (* TODO: add function memory overflow check using add_encoding to prevent overmerging *)
  let (l,r,n)  = Hashtbl.fold (fun (l,r) n ((_,_,an) as acc) -> if n > an then (l,r,n) else acc) freq (0,0,0) in
  Dynarray.add_last encodings (l,r) ;
  let m = ~-(Dynarray.length encodings + 2) in
  compressed := collapse_pair (l,r) m !compressed
 done ;

 if debug then List.iter (fun n -> print_int n ; print_newline ()) !compressed ;

 let rec add_encoding sb n =
  if n >= 0 then (Buffer.add_string sb (string_of_int n) ; Buffer.add_char sb ',')
  else if n = (-2) then (Buffer.add_char sb 'L' ; Buffer.add_char sb ',')
  else if n = (-1) then (Buffer.add_char sb 'R' ; Buffer.add_char sb ',')
  else (
   let (l,r) = Dynarray.get encodings (~-n-3) in
   add_encoding sb l ;
   add_encoding sb r ) in

 let buf = Buffer.create 24 in
 let functions =
  uniq |>
  Dynarray.map
   (fun n ->
    Buffer.clear buf;
    add_encoding buf n;
    Buffer.truncate buf (Buffer.length buf - 1) ;
    Buffer.add_char buf '\n' ;
    Buffer.contents buf) in
 Buffer.clear buf ;
 !compressed |>
 List.filter_map (fun n -> Dynarray.find_index ((=)n) uniq)  |>
 List.iter (fun n -> Buffer.add_char buf (Char.chr (n + Char.code 'A')) ; Buffer.add_char buf ',') ;
 Buffer.truncate buf (Buffer.length buf - 1) ;
 Buffer.add_char buf '\n' ;
 let main_routine = Buffer.contents buf in

 if debug then (
  print_string main_routine ;
  Dynarray.iter print_string functions) ;

 (* by hand calculation *)
(*
 let main_routine = "A,B,A,C,A,B,C,B,C,B\n" in
 let function_a = "L,10,R,8,L,6,R,6\n" in
 let function_b = "L,8,L,8,R,8\n" in
 let function_c = "R,8,L,6,L,10,L,10\n" in
*)
 let no_feed = "n\n" in

(* this gives you an answer! *)
(*
-14 -> -12,-7 -> -4,-8,-1,6 -> -2,10,-3,-6,-1,6 -> -2,10,-1,8,-2,6,-1,6 -> A:L,10,R,8,L,6,R,6
-11 -> -5,-10 -> -2,8,-5,-3 -> -2,8,-2,8,-1,8 -> B:L,8,L,8,R,8
-13 -> -8,-9 -> -3,-6,-4,-4 -> -1,8,-2,6,-2,10,-2,10 -> C:R,8,L,6,L,10,L,10
*)

 (* change modes *)
 program.(0) <- 2 ;
 Queue.clear inq ;
 Queue.clear outq ;

 String.iter (fun c -> Queue.push (Char.code c) inq) main_routine ;
(*
 String.iter (fun c -> Queue.push (Char.code c) inq) function_a ;
 String.iter (fun c -> Queue.push (Char.code c) inq) function_b ;
 String.iter (fun c -> Queue.push (Char.code c) inq) function_c ;
*)
 Dynarray.iter (fun fn -> String.iter (fun c -> Queue.push (Char.code c) inq) fn) functions ;
 String.iter (fun c -> Queue.push (Char.code c) inq) no_feed ;

 run_with step ;
 Queue.to_seq outq |> Seq.iter (fun n -> if n < 255 then print_char (Char.chr n) else (print_int n ; print_newline ()))

(* Problem 18 *)
(* all you really need to determine is the order of keys to select, *)
(* so find the shortest path to each available key, and then fork decisions on that *)
(* there are 26 keys (all letters) in the full input *)

let problem_18a () =
 let example = true in
 let debug = false in
 let input =
  In_channel.(with_open_bin (if example then "18e.txt" else "18.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let doors = Array.make 27 None in
 let keys = Array.make 27 None in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let h = Array.length input in
 let w = Bytes.length input.(0) in
 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   let c = Char.code (input.(y).%[x]) in
   if c >= 0x40 && c <= 0x5A then doors.(c-0x40) <- Some (y,x) else
   if c >= 0x60 && c <= 0x7A then keys.(c-0x60) <- Some (y,x) else ()
  done
 done ;
 (* note @ is 0x40, so it lives in doors.(0) ; make the start a key *)
 keys.(0) <- doors.(0) ;
 doors.(0) <- None ;

 let flood d0 path =
  (* setup *)
  let d = Array.make_matrix h w None in
  let (sy,sx) = keys.(List.hd path) |> Option.get in
  (* reset map *)
  Array.iter (function Some (y,x) -> input.(y).%[x] <- '#' | None -> ()) doors ;
  Array.iter (function Some (y,x) -> input.(y).%[x] <- '#' | None -> ()) keys ;
  (* open relevant doors and keys *)
  List.iter (function Some (y,x) -> input.(y).%[x] <- '.' | None -> ()) (List.map (Array.get doors) path);
  List.iter (function Some (y,x) -> input.(y).%[x] <- '.' | None -> ()) (List.map (Array.get keys) path);
  let q = Queue.create () in
  Queue.add (d0,sy,sx) q ;
  while not (Queue.is_empty q) do
   let (d0,y,x) = Queue.take q in
   if input.(y).%[x] = '#' then () else
   (match d.(y).(x) with
   | Some d' when d' <= d0 -> ()
   | _ ->
     d.(y).(x) <- Some d0 ;
     if debug then Printf.printf "(%d,%d,%d)\n" d0 y x ;
     Queue.add (d0+1,y-1,x) q ;
     Queue.add (d0+1,y+1,x) q ;
     Queue.add (d0+1,y,x-1) q ;
     Queue.add (d0+1,y,x+1) q )
  done ;
  let nearest = function
  | None -> None
  | Some (y,x) ->
    [ d.(y-1).(x) ;
      d.(y+1).(x) ;
      d.(y).(x-1) ;
      d.(y).(x+1) ] |>
    List.filter_map (Option.map ((+)1)) |>
    List.fold_left (fun a n -> match a with None -> Some n | Some a -> Some (min a n)) None in
  let res = Array.map nearest keys in
  (* return only keys we haven't picked up yet! *)
  List.iter (fun n -> res.(n) <- None) path ;
  res in

 let module ISet = Set.Make(struct type t = int let compare = compare end) in
 let memo = Hashtbl.create 4096 in
 let terminate = Array.make 27 None in
 let flood_memo d0 path =
  match Hashtbl.find_opt memo (List.hd path, ISet.of_list path) with
  | Some (ks,d) when d <= d0 -> terminate
  | Some (ks,d) ->
    Hashtbl.replace memo (List.hd path, ISet.of_list path) (ks, d0) ;
    Array.map (Option.map ((+)d0)) ks
  | None ->
    let ks = flood 0 path in
    Hashtbl.add memo (List.hd path, ISet.of_list path) (ks,d0) ;
    Array.map (Option.map ((+)d0)) ks in

 let solution = ref None in
 let total_keys = Array.fold_left (fun a el -> if Option.is_some el then succ a else a) 0 keys in
 let q = Queue.create () in
 Queue.push (0,[0]) q ;
 while not (Queue.is_empty q) do
  let (d0,path) = Queue.pop q in
  if List.length path = total_keys then solution := Some (min d0 (Option.value !solution ~default:d0)) ;
  if d0 < (Option.value !solution ~default:(d0+1)) then
   Array.iteri (fun i el -> match el with Some d0 -> Queue.push (d0,i::path) q | None -> ()) (flood_memo d0 path)
  else ()
 done ;
 !solution |> Option.get

(* modify the map and state for 4 positions instead of 1! *)
let problem_18b () =
 let example = false in
 let debug = false in
 let input =
  In_channel.(with_open_bin (if example then "18e.txt" else "18.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let doors = Array.make 30 None in (* 26 + 4 starting positions *)
 let keys = Array.make 30 None in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let h = Array.length input in
 let w = Bytes.length input.(0) in
 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   let c = Char.code (input.(y).%[x]) in
   if c >= 0x40 && c <= 0x5A then doors.(c-0x40+3) <- Some (y,x) else
   if c >= 0x60 && c <= 0x7A then keys.(c-0x60+3) <- Some (y,x) else ()
  done
 done ;

 (* note @ is 0x40, so it lives in doors.(3) ; use this to make the start "keys" *)
 let (sy,sx) = Option.get doors.(3) in
 keys.(0) <- Some (sy-1,sx-1) ;
 keys.(1) <- Some (sy-1,sx+1) ;
 keys.(2) <- Some (sy+1,sx+1) ;
 keys.(3) <- Some (sy+1,sx-1) ;
 doors.(3) <- None ;
 (* modify map *)
 input.(sy).%[sx] <- '#' ;
 input.(sy-1).%[sx] <- '#' ;
 input.(sy+1).%[sx] <- '#' ;
 input.(sy).%[sx-1] <- '#' ;
 input.(sy).%[sx+1] <- '#' ;

 (* Smarter Quandrant Logic Required for 1 Example, but Main Problem is OK! *)
 let quadrant (y,x) =
  if y < sy && x < sx then 0 else
  if y < sy && x > sx then 1 else
  if y > sy && x > sx then 2 else
  if y > sy && x < sx then 3 else
  raise (Invalid_argument "Invalid Key Position - Add Manual Quadrant Logic For Keys on Boundary!") in

 let robot_state path =
  let res = [|None;None;None;None|] in
  List.iter
  (fun idx ->
   let q = quadrant (Option.get keys.(idx)) in
   if Option.is_none res.(q) then res.(q) <- Some idx else ())
  path ;
  Array.map Option.get res in

 let flood d0 path =
  (* setup *)
  let d = Array.make_matrix h w None in
  let (sy,sx) = keys.(List.hd path) |> Option.get in
  (* reset map *)
  Array.iter (function Some (y,x) -> input.(y).%[x] <- '#' | None -> ()) doors ;
  Array.iter (function Some (y,x) -> input.(y).%[x] <- '#' | None -> ()) keys ;
  (* open relevant doors and keys *)
  List.iter (function Some (y,x) -> input.(y).%[x] <- '.' | None -> ()) (List.map (Array.get doors) path);
  List.iter (function Some (y,x) -> input.(y).%[x] <- '.' | None -> ()) (List.map (Array.get keys) path);
  let q = Queue.create () in
  Queue.add (d0,sy,sx) q ;
  while not (Queue.is_empty q) do
   let (d0,y,x) = Queue.take q in
   if input.(y).%[x] = '#' then () else
   (match d.(y).(x) with
   | Some d' when d' <= d0 -> ()
   | _ ->
     d.(y).(x) <- Some d0 ;
     if debug then Printf.printf "(%d,%d,%d)\n" d0 y x ;
     Queue.add (d0+1,y-1,x) q ;
     Queue.add (d0+1,y+1,x) q ;
     Queue.add (d0+1,y,x-1) q ;
     Queue.add (d0+1,y,x+1) q )
  done ;
  let nearest = function
  | None -> None
  | Some (y,x) ->
    [ d.(y-1).(x) ;
      d.(y+1).(x) ;
      d.(y).(x-1) ;
      d.(y).(x+1) ] |>
    List.filter_map (Option.map ((+)1)) |>
    List.fold_left (fun a n -> match a with None -> Some n | Some a -> Some (min a n)) None in
  let res = Array.map nearest keys in
  (* return only keys we haven't picked up yet! *)
  List.iter (fun n -> res.(n) <- None) path ;
  res in

 let module ISet = Set.Make(struct type t = int let compare = compare end) in
 let memo = Hashtbl.create 4096 in
 let terminate = Array.make 30 None in
 let flood_memo d0 path =
  let rstate = robot_state path in
  let keyset = ISet.of_list path in
  match Hashtbl.find_opt memo (rstate, keyset) with
  | Some (ks,d) when d <= d0 -> terminate
  | Some (ks,d) ->
    Hashtbl.replace memo (rstate, keyset) (ks, d0) ;
    Array.map (Option.map ((+)d0)) ks
  | None ->
    let ks =
     flood 0 (rstate.(0)::path) |>
     Array.map2 (fun a b -> match a,b with None, b -> b | a, None -> a | _ -> None) (flood 0 (rstate.(1)::path)) |>
     Array.map2 (fun a b -> match a,b with None, b -> b | a, None -> a | _ -> None) (flood 0 (rstate.(2)::path)) |>
     Array.map2 (fun a b -> match a,b with None, b -> b | a, None -> a | _ -> None) (flood 0 (rstate.(3)::path)) in
    Hashtbl.add memo (rstate, keyset) (ks,d0) ;
    Array.map (Option.map ((+)d0)) ks in

 let solution = ref None in
 let total_keys = Array.fold_left (fun a el -> if Option.is_some el then succ a else a) 0 keys in
 let q = Queue.create () in
 Queue.push (0,[0;1;2;3]) q ;
 while not (Queue.is_empty q) do
  let (d0,path) = Queue.pop q in
  if List.length path = total_keys then solution := Some (min d0 (Option.value !solution ~default:d0)) ;
  if d0 < (Option.value !solution ~default:(d0+1)) then
   Array.iteri (fun i el -> match el with Some d0 -> Queue.push (d0,i::path) q | None -> ()) (flood_memo d0 path)
  else ()
 done ;
 !solution |> Option.get

let problem_19a () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let step () = true in
 for y = 0 to 49 do
  for x = 0 to 49 do
   Queue.add x inq ;
   Queue.add y inq ;
   run_with step
  done
 done ;
 let res = Queue.to_seq outq |> Seq.fold_left (+) 0 in
 for y = 0 to 49 do
  for x = 0 to 49 do
   print_char (if Queue.take outq = 1 then '#' else '.')
  done ;
  print_newline ()
 done ;
 res

let problem_19b () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let step () = true in

 let find_yint x y is_lower =
  let y = ref y in
  while
   (if is_lower then incr else decr) y ;
   Queue.add x inq ;
   Queue.add !y inq ;
   run_with step ;
   Queue.take outq = 0
  do () done ;
  !y in

 let find_xint x y is_lower =
  let x = ref x in
  while
   (if is_lower then incr else decr) x ;
   Queue.add !x inq ;
   Queue.add y inq ;
   run_with step ;
   Queue.take outq = 0
  do () done ;
  !x in

 let slope_lo =
  let x = ref 1 in
  let y = ref 0 in
  while !x < 1_000_000 do
   x := 10 * !x ;
   y := 10 * !y ;
   y := max 0 (!y - 10) ;
   y := find_yint !x !y true ;
  done ;
  !y in

 if debug then Printf.printf "Slope LO: %d\n" slope_lo ;

 let slope_hi =
  let x = ref 1 in
  let y = ref (10 / (find_xint 0 10 true)) in
  while !x < 1_000_000 do
   x := 10 * !x ;
   y := 10 * !y ;
   y := !y + 10 ;
   y := find_yint !x !y false ;
  done ;
  !y in

 if debug then Printf.printf "Slope HI: %d\n" slope_hi ;

 (* look mom! no trig! *)
 (* use a system of 4 equations to solve x1 (x1, y1 = top right corner of square) ; box = 100 *)
 (* y2 = y1 + 100, x1 = x2 + 100  ; y1 - slope_hi / 1_000_000 * x1 = 0 ; y2 - slope_lo / 1_000_000 * x2 = 0 *)

 (* estimate x for top edge - try distance = 99.5 instead of 99 or 100 for more optimal guess *)
 let x1 = 995 * (1_000_000 + slope_hi) / (slope_hi - slope_lo) / 10 in
 let y1 = find_yint x1 (x1 * slope_lo / 1_000_000 - 2) true in
 if debug then Printf.printf "Estimate: X=%d, Y=%d, MD=%d\n" (x1-99) y1 (x1+y1-99) ;

 let res = ref 0 in
 (try
  for y = y1 - 5 to y1 + 5 do
   for x = x1 - 5 to x1 + 5 do
    Queue.add x inq ;
    Queue.add y inq ;
    run_with step ;
    Queue.add (x-99) inq ;
    Queue.add (y+99) inq ;
    run_with step ;
    Queue.add (x-99) inq ;
    Queue.add y inq ;
    run_with step ;
    let r1 = Queue.take outq in
    let r2 = Queue.take outq in
    let r3 = Queue.take outq in
    if r1 = 1 && r2 = 1 && r3 = 1 then (
     if debug then Printf.printf "Solution Found: (X=%d,Y=%d,D=%d)\n" (x-99) y (x+y-99);
     res := (x-99) * 10_000 + y ;
     raise_notrace Exit
    ) ;
   done
  done
 with Exit -> ()) ;
 !res

let problem_20a () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length (input.(0)) in (* luckily padding is included in the input *)
 let nodes = Array.make_matrix h w [] in
 let warps = Array.make (26*26*2) None in (* low bit, 0 = inner; low bit, 1 = outer *)

 (* determine exact shape *)
 let inner_w =
  let rec loop i =
   if input.(h/2).[i] <> '#' && input.(h/2).[i] <> '.'
   then i else loop (i+1) in
  loop 3 in
 let inner_e =
  let rec loop i =
   if input.(h/2).[i] <> '#' && input.(h/2).[i] <> '.'
   then i else loop (i-1) in
  loop (w-4) in
 let inner_n =
  let rec loop i =
   if input.(i).[w/2] <> '#' && input.(i).[w/2] <> '.'
   then i else loop (i+1) in
  loop 3 in
 let inner_s =
  let rec loop i =
   if input.(i).[w/2] <> '#' && input.(i).[w/2] <> '.'
   then i else loop (i-1) in
  loop (h-4) in

 (* register warps *)
 for x = 3 to w - 4 do
  if input.(0).[x] <> ' ' then (
   let idx = (((Char.code input.(0).[x] - 0x41) * 26 + (Char.code input.(1).[x] - 0x41)) lsl 1) lor 1 in
   warps.(idx) <- Some (2,x)
  ) ;
  if input.(h-1).[x] <> ' ' then (
   let idx = (((Char.code input.(h-2).[x] - 0x41) * 26 + (Char.code input.(h-1).[x] - 0x41)) lsl 1) lor 1 in
   warps.(idx) <- Some (h-3,x)
  )
 done ;
 for x = inner_w+1 to inner_e-1 do
  if input.(inner_s).[x] <> ' ' then (
   let idx = (((Char.code input.(inner_s-1).[x] - 0x41) * 26 + (Char.code input.(inner_s).[x] - 0x41)) lsl 1) in
   warps.(idx) <- Some (inner_s+1,x)
  ) ;
  if input.(inner_n).[x] <> ' ' then (
   let idx = (((Char.code input.(inner_n).[x] - 0x41) * 26 + (Char.code input.(inner_n+1).[x] - 0x41)) lsl 1) in
   warps.(idx) <- Some (inner_n-1,x)
  )
 done ;
 for y = 3 to h - 4 do
  if input.(y).[0] <> ' ' then (
   let idx = (((Char.code input.(y).[0] - 0x41) * 26 + (Char.code input.(y).[1] - 0x41)) lsl 1) lor 1 in
   warps.(idx) <- Some (y,2)
  ) ;
  if input.(y).[w-1] <> ' ' then (
   let idx = (((Char.code input.(y).[w-2] - 0x41) * 26 + (Char.code input.(y).[w-1] - 0x41)) lsl 1) lor 1 in
   warps.(idx) <- Some (y,w-3)
  )
 done ;
 for y = inner_n+1 to inner_s-1 do
  if input.(y).[inner_e] <> ' ' then (
   let idx = (((Char.code input.(y).[inner_e-1] - 0x41) * 26 + (Char.code input.(y).[inner_e] - 0x41)) lsl 1) in
   warps.(idx) <- Some (y,inner_e+1)
  ) ;
  if input.(y).[inner_w] <> ' ' then (
   let idx = (((Char.code input.(y).[inner_w] - 0x41) * 26 + (Char.code input.(y).[inner_w+1] - 0x41)) lsl 1) in
   warps.(idx) <- Some (y,inner_w-1)
  )
 done ;
  
 (* populate nodes w/o warps *)
 for y = 2 to h - 3 do
  for x = 2 to w - 3 do
   if input.(y).[x] = '.' then
    nodes.(y).(x) <-
     [y-1,x,input.(y-1).[x]
     ;y+1,x,input.(y+1).[x]
     ;y,x-1,input.(y).[x-1]
     ;y,x+1,input.(y).[x+1]] |>
     List.filter (fun (_,_,c) -> c = '.') |>
     List.map (fun (a,b,_) -> (a,b))
  done
 done ;

 (* add warp points to nodes *)
 for i = 0 to Array.length warps / 2 - 1 do
  match warps.(i lsl 1), warps.((i lsl 1) lor 1) with
  | Some (y1,x1), Some (y2,x2) ->
    nodes.(y1).(x1) <- (y2,x2) :: nodes.(y1).(x1) ;
    nodes.(y2).(x2) <- (y1,x1) :: nodes.(y2).(x2)
  | _ -> ()
 done ;

 let start =
  match warps.(0), warps.(1) with
  | None, Some b -> b | Some a, None -> a | _ -> assert false in

 let dest =
  match warps.(Array.length warps - 2), warps.(Array.length warps - 1) with
  | None, Some b -> b | Some a, None -> a | _ -> assert false in

 let q = Queue.create () in
 let map = Array.make_matrix h w (Int.max_int) in

 (
  let sy,sx = start in
  Queue.add (0,sy,sx) q
 ) ;

 while not (Queue.is_empty q) do
  let (d,y,x) = Queue.take q in
  if d < map.(y).(x) then (
   map.(y).(x) <- d ;
   List.to_seq nodes.(y).(x) |>
   Seq.map (fun (y,x) -> (d+1,y,x)) |>
   Queue.add_seq q
  ) else ()
 done ;

 map.(fst dest).(snd dest)

let problem_20b () =
 let example = false in
 let debug = false in
 let input = In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length (input.(0)) in (* luckily padding is included in the input *)
 let warps = Array.make (26*26*2) None in (* low bit, 0 = inner; low bit, 1 = outer *)

 (* determine exact shape *)
 let inner_w =
  let rec loop i =
   if input.(h/2).[i] <> '#' && input.(h/2).[i] <> '.'
   then i else loop (i+1) in
  loop 3 in
 let inner_e =
  let rec loop i =
   if input.(h/2).[i] <> '#' && input.(h/2).[i] <> '.'
   then i else loop (i-1) in
  loop (w-4) in
 let inner_n =
  let rec loop i =
   if input.(i).[w/2] <> '#' && input.(i).[w/2] <> '.'
   then i else loop (i+1) in
  loop 3 in
 let inner_s =
  let rec loop i =
   if input.(i).[w/2] <> '#' && input.(i).[w/2] <> '.'
   then i else loop (i-1) in
  loop (h-4) in

 (* register warps *)
 for x = 3 to w - 4 do
  if input.(0).[x] <> ' ' then (
   let idx = (((Char.code input.(0).[x] - 0x41) * 26 + (Char.code input.(1).[x] - 0x41)) lsl 1) lor 1 in
   warps.(idx) <- Some (2,x)
  ) ;
  if input.(h-1).[x] <> ' ' then (
   let idx = (((Char.code input.(h-2).[x] - 0x41) * 26 + (Char.code input.(h-1).[x] - 0x41)) lsl 1) lor 1 in
   warps.(idx) <- Some (h-3,x)
  )
 done ;
 for x = inner_w+1 to inner_e-1 do
  if input.(inner_s).[x] <> ' ' then (
   let idx = (((Char.code input.(inner_s-1).[x] - 0x41) * 26 + (Char.code input.(inner_s).[x] - 0x41)) lsl 1) in
   warps.(idx) <- Some (inner_s+1,x)
  ) ;
  if input.(inner_n).[x] <> ' ' then (
   let idx = (((Char.code input.(inner_n).[x] - 0x41) * 26 + (Char.code input.(inner_n+1).[x] - 0x41)) lsl 1) in
   warps.(idx) <- Some (inner_n-1,x)
  )
 done ;
 for y = 3 to h - 4 do
  if input.(y).[0] <> ' ' then (
   let idx = (((Char.code input.(y).[0] - 0x41) * 26 + (Char.code input.(y).[1] - 0x41)) lsl 1) lor 1 in
   warps.(idx) <- Some (y,2)
  ) ;
  if input.(y).[w-1] <> ' ' then (
   let idx = (((Char.code input.(y).[w-2] - 0x41) * 26 + (Char.code input.(y).[w-1] - 0x41)) lsl 1) lor 1 in
   warps.(idx) <- Some (y,w-3)
  )
 done ;
 for y = inner_n+1 to inner_s-1 do
  if input.(y).[inner_e] <> ' ' then (
   let idx = (((Char.code input.(y).[inner_e-1] - 0x41) * 26 + (Char.code input.(y).[inner_e] - 0x41)) lsl 1) in
   warps.(idx) <- Some (y,inner_e+1)
  ) ;
  if input.(y).[inner_w] <> ' ' then (
   let idx = (((Char.code input.(y).[inner_w] - 0x41) * 26 + (Char.code input.(y).[inner_w+1] - 0x41)) lsl 1) in
   warps.(idx) <- Some (y,inner_w-1)
  )
 done ;

 (* rebuild warp with minimum needed data *)
 let warps =
  let warp_table = Dynarray.create () in
  (* add start and destination to position 0 of the warp_table *)
  let start = Option.get warps.(1) in
  let dst   = Option.get warps.(Array.length warps - 1) in
  Dynarray.add_last warp_table (start,dst) ;
  for i = 1 to Array.length warps / 2 - 2 do
   match warps.(i lsl 1), warps.((i lsl 1) lor 1) with
   | Some (y1,x1), Some (y2,x2) ->
     Dynarray.add_last warp_table ((y1,x1),(y2,x2))
   | _ -> ()
  done ;
  Dynarray.to_array warp_table in
 
 let map = Array.make (h*w) None in
 let q = Queue.create () in
 let flood (y,x) =
  Array.fill map 0 (h*w) None ;
  Queue.clear q ;
  Queue.add (0,y,x) q ;
  while not (Queue.is_empty q) do
   let (d,y,x) = Queue.take q in
   if input.(y).[x] = '.' &&
      d < (Option.value ~default:(w*h) map.(y*w+x))
   then (
    map.(y*w+x) <- Some d ;
    Queue.add (d+1,y-1,x) q;
    Queue.add (d+1,y+1,x) q;
    Queue.add (d+1,y,x-1) q;
    Queue.add (d+1,y,x+1) q)
  done in

 let nodes =
  warps |>
  Array.map (fun (inner,outer) ->
   flood inner ;
   (let y,x = inner in
    map.(y*w+x) <- None) ;
   let inner_nodes =
    Array.mapi (fun i ((y1,x1),(y2,x2)) -> i, map.(y1*w+x1), map.(y2*w+x2)) warps in
   flood outer ;
   (let y,x = outer in
    map.(y*w+x) <- None) ;
   let outer_nodes =
    Array.mapi (fun i ((y1,x1),(y2,x2)) -> i, map.(y1*w+x1), map.(y2*w+x2)) warps in
   inner_nodes, outer_nodes) in
 
 let module Heap (Elem : Map.OrderedType) : sig
  type t
  val create : unit -> t
  val is_empty : t -> bool
  val clear : t -> unit
  val add : t -> Elem.t -> unit
  val pop_min : t -> Elem.t option
 end = struct
  type t = Elem.t Dynarray.t
  let create = Dynarray.create
  let ( .!() ) = Dynarray.get
  let ( .!()<- ) = Dynarray.set
  let is_empty = Dynarray.is_empty
  let clear = Dynarray.clear

  let left_child i = 2 * i + 1
  let right_child i = 2 * i + 2
  let parent_node i = (i - 1) / 2

  let order h i j =
   Elem.compare h.!(i) h.!(j)

  let swap h i j =
   let v = h.!(i) in
   h.!(i) <- h.!(j);
   h.!(j) <- v

  let rec heap_up h i =
    if i = 0 then () else
    let parent = parent_node i in
    if order h i parent < 0 then
      (swap h i parent; heap_up h parent)

  and heap_down h ~len i =
    let left, right = left_child i, right_child i in
    if left >= len then () else
    let smallest =
      if right >= len then left else
      if order h left right < 0 then left else right
    in
    if order h i smallest > 0 then
      (swap h i smallest; heap_down h ~len smallest)

  let add h s =
    let i = Dynarray.length h in
    Dynarray.add_last h s;
    heap_up h i

  let pop_min h =
    if Dynarray.is_empty h then None
    else begin
      let last = Dynarray.length h - 1 in
      swap h 0 last;
      let best = Dynarray.pop_last h in
      heap_down h ~len:last 0;
      Some best
    end
  
 end in

 (* type is d,z,node,is_inner (fst) *)
 let module PQ =
  Heap (struct type t = (int * int * int * bool) let compare (p1,_,_,_) (p2,_,_,_) = compare p1 p2 end) in

 let pq = PQ.create () in
 let seen = Hashtbl.create 4096 in
 
 let rec loop () =
  match PQ.pop_min pq with
  | None -> (-1)
  | Some (d,z,n,is_inner) when (z,n,is_inner) = (0,0,false) -> d
  | Some (d,z,n,is_inner) when z = 0 ->
    if debug then Printf.printf "%d, %d, %d, %s\n" d z n (if is_inner then "inner" else "outer") ;
    (match Hashtbl.find_opt seen (z,n,is_inner) with
    | Some d0 when d0 < d -> loop ()
    | _ ->
      Hashtbl.replace seen (z,n,is_inner) d ;
      let paths = (if is_inner then fst else snd) nodes.(n) in
      (match paths.(0) with (* destination test *)
      | _,_,Some d' -> PQ.add pq (d+d',0,0,false)
      | _ -> ()) ;
      paths |> Array.to_seq |> Seq.drop 1 |>
      Seq.iter
       (function
        | i, None, _ -> () (* ignore outers for z = 0 *)
        | i, Some d', _ -> (* warp from inner to outer z+1 *)
          PQ.add pq (d+d'+1,z+1,i,false)); loop ())
  | Some (d,z,n,is_inner) ->
    if debug then Printf.printf "%d, %d, %d, %s\n" d z n (if is_inner then "inner" else "outer") ;
    (match Hashtbl.find_opt seen (z,n,is_inner) with
    | Some d0 when d0 < d -> loop ()
    | _ ->
      Hashtbl.replace seen (z,n,is_inner) d ;
      let paths = (if is_inner then fst else snd) nodes.(n) in
      paths |> Array.to_seq |> Seq.drop 1 |>
      Seq.iter
       (function
        | i, None, None -> () (* skip *)
        | i, Some d', None -> (* warp from inner to outer z+1 *)
          PQ.add pq (d+d'+1,z+1,i,false)
        | i, None, Some d' -> (* warp from outer to inner z-1 *)
          PQ.add pq (d+d'+1,z-1,i,true)
        | i, Some d_in, Some d_out -> (* both *)
          PQ.add pq (d+d_in+1,z+1,i,false) ;
          PQ.add pq (d+d_out+1,z-1,i,true)
        ); loop ()) in

 PQ.add pq (0,0,0,true) ; (* NOTE: start point is stored as "inner" even though it is really "outer" *)
 loop ()

(* this covers both a & b, answers commented at end *)
let problem_21a () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "21e.txt" else "21.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 (* let step () = false in *)

 let step () =
  (* flush output *)
  Queue.to_seq outq |> Seq.iter (fun n -> if n < 255 then print_char (Char.chr n) else (print_int n ; print_newline ())) ;
  Queue.clear outq ;
  (* read input *)
  let s = read_line () in
  if s = "QUIT" then false
  else (
   String.iter (fun c -> Queue.add (Char.code c) inq) s ;
   Queue.add 10 inq ;
   true ) in

 (* change modes *)
 Queue.clear inq ;
 Queue.clear outq ;

(* answer: 21a *)
(* jmp if: (!A || !C) && D
   NOT A T
   NOT C J
   OR T J
   AND D J
*)
(* ALT: jmp if: !(A && C) && D
  OR C T
  AND A T
  NOT T J
  AND D J
*)

(* answer: 21b *)
(* jmp if: (!(E || H) && !C) || !(B || E) || !A && D
   OR E T
   OR H T
   NOT C J
   AND T J
   NOT A T
   AND A T
   OR B T
   OR E T
   NOT T T
   OR T J
   NOT A T
   OR T J
   AND D J
*)

 run_with step ;
 Queue.to_seq outq |> Seq.iter (fun n -> if n < 255 then print_char (Char.chr n) else (print_int n ; print_newline ()))

(*
 naive implementation:
 "deal into new stack" = Reverse List
 "cut N" = move top N (in order) to bottom. (just a rotation)
 "cut -N" = move bottom N (in order) to top.
 "deal with increment N" = deal cards to positions skipping (N-1) positions per card

 using cursors (cyclic buffer):
 "deal into new stack" = reverse cursor direction (multiply stride by (-1); move forward 1
 "cut N" move cursor forward N
 "cut -N" move cursor backward N
 "deal with increment N" = multiply stride by x, which satisifes (n(len)+1)/N = x, where n, x are integers.
*)

let problem_22a () =
 let example = false in
 let debug = true in
 let len = if example then 10 else 10_007 in

 let module Shuf = struct
  type t = (* Reverse == (Multiply (-1); Shift 1) *)
   | Shift of int
   | Multiply of int
   | Distribute of int

  let append_of_string q s =
   if s = "deal into new stack"
   then (
    Queue.add (Multiply (-1)) q ;
    Queue.add (Shift 1) q )
   else if String.sub s 0 3 = "cut"
   then (
    let n = int_of_string (String.sub s 4 (String.length s - 4)) in
    Queue.add (Shift n) q )
   else (
    let n = int_of_string (String.sub s 20 (String.length s - 20)) in
    Queue.add (Distribute n) q )

  let to_string = function
   | Shift n -> Printf.sprintf "Shift %d" n
   | Multiply n -> Printf.sprintf "Multiply %d" n
   | Distribute n -> Printf.sprintf "Distribute %d" n

  let simplify len = function
  | Shift n -> Shift n
  | Multiply n -> Multiply n
  | Distribute n ->
    let a = ref 1 in
    while (!a*len+1) mod n <> 0 do incr a done ;
    Multiply (((!a*len+1)/n) mod len)

  let pmod a b = if a mod b < 0 then a mod b + b else a mod b

  let apply len (cursor, stride) = function
  | Shift n -> (pmod (cursor+n*stride) len, stride)
  | Multiply n -> (cursor,(stride*n) mod len)
  | Distribute n -> raise_notrace (Invalid_argument "Distribute cannot be applied; Simplify before applying!")

 end in
 let q = Queue.create () in
 In_channel.(with_open_bin (if example then "22e.txt" else "22.txt") input_lines) |> List.iter (Shuf.append_of_string q) ;
 let shuf = Queue.to_seq q |> Seq.map (Shuf.simplify len) in
 if debug then Seq.iter (fun s -> s |> Shuf.to_string |> print_endline) shuf ;
 let (cursor, stride) =
  shuf |>
  Seq.fold_left (Shuf.apply len) (0,1) in
 Printf.printf "(%d,%d)\n" cursor stride ;
 let pos = ref 0 in
 if not example then (
 while
  Shuf.pmod (cursor + !pos * stride) len <> 2019
 do
  incr pos
 done ) ;
 !pos

let problem_22a2 () =
 let example = false in
 let debug = true in
 let len = if example then 10 else 10_007 in

 let module Shuf = struct
  type t = (* Reverse == (Multiply (-1); Shift 1) *)
   | Shift of int
   | Multiply of int
   | Distribute of int

  let append_of_string q s =
   if s = "deal into new stack"
   then (
    Queue.add (Multiply (-1)) q ;
    Queue.add (Shift 1) q )
   else if String.sub s 0 3 = "cut"
   then (
    let n = int_of_string (String.sub s 4 (String.length s - 4)) in
    Queue.add (Shift n) q )
   else (
    let n = int_of_string (String.sub s 20 (String.length s - 20)) in
    Queue.add (Distribute n) q )

  let to_string = function
   | Shift n -> Printf.sprintf "Shift %d" n
   | Multiply n -> Printf.sprintf "Multiply %d" n
   | Distribute n -> Printf.sprintf "Distribute %d" n

  let simplify len = function
  | Shift n -> Shift n
  | Multiply n -> Multiply n
  | Distribute n ->
    let a = ref 1 in
    while (!a*len+1) mod n <> 0 do incr a done ;
    Multiply (((!a*len+1)/n) mod len)

  let pmod a b = if a mod b < 0 then a mod b + b else a mod b

  let apply len (cursor, stride) = function
  | Shift n -> (pmod (cursor+n*stride) len, stride)
  | Multiply n -> (cursor,(stride*n) mod len)
  | Distribute n -> raise_notrace (Invalid_argument "Distribute cannot be applied; Simplify before applying!")

 end in
 let q = Queue.create () in
 In_channel.(with_open_bin (if example then "22e.txt" else "22.txt") input_lines) |> List.iter (Shuf.append_of_string q) ;
 let shuf = Queue.to_seq q |> Seq.map (Shuf.simplify len) in
 if debug then Seq.iter (fun s -> s |> Shuf.to_string |> print_endline) shuf ;
 let (cursor, stride) =
  shuf |>
  Seq.fold_left (Shuf.apply len) (0,1) in
 let stride = Shuf.pmod stride len in
 Printf.printf "(%d,%d)\n" cursor stride ;

 let diff = Shuf.pmod (2019 - cursor) len in

 let ans =
  if diff mod stride = 0
  then diff / stride else (
  let a = ref 1 in
  while
   (!a*len+1) mod stride <> 0
  do incr a done ;
  (((!a*len+1) / stride) * diff) mod len
 ) in
 ans

let problem_22b () =
 let debug = false in
 let part_a = false in
 let len = if part_a then 10007 else 119315717514047 in
 let iterations = if part_a then 1 else 101741582076661 in
 let target = if part_a then 2019 else 2020 in
 let solve_index = not part_a in

 let rec mulm a b c m =
  (* assume a and b are within -m < a,b > m to reduce divisions *)
  let a = if a < 0 then a + m else a in
  let b = if b < 0 then b + m else b in
  let (a, b) = (max a b, min a b) in
  if b = 0 then c mod m else
  if (b land 1) = 1 then
   mulm ((a lsl 1) mod m) (b lsr 1) (a+c) m
  else
   mulm ((a lsl 1) mod m) (b lsr 1) (c) m in

 let invert a m =
  let rec loop (t, t') (r, r') =
   if r' = 0
   then ( assert (r = 1); if t < 0 then t + m else t )
   else
    let q = r / r' in
    loop (t', t - q * t') (r', r - q * r') in
  loop (0,1) (m, if a < 0 then m + a else a) in

(*
 let invert_opt a m =
  let rec loop (t, t') (r, r') =
   if r' = 0
   then ( if r <> 1 then None else if t < 0 then Some (t + m) else Some t )
   else
    let q = r / r' in
    loop (t', t - q * t') (r', r - q * r') in
  loop (0,1) (m, if a < 0 then m + a else a)
*)

 let module Shuf = struct
  type t = (* Reverse == (Multiply (-1); Shift 1) *)
   | Shift of int
   | Multiply of int
   | Distribute of int

  let append_of_string q s =
   if s = "deal into new stack"
   then (
    Queue.add (Multiply (-1)) q ;
    Queue.add (Shift 1) q )
   else if String.sub s 0 3 = "cut"
   then (
    let n = int_of_string (String.sub s 4 (String.length s - 4)) in
    Queue.add (Shift n) q )
   else (
    let n = int_of_string (String.sub s 20 (String.length s - 20)) in
    Queue.add (Distribute n) q )

  let to_string = function
   | Shift n ->  "Shift " ^ (string_of_int n)
   | Multiply n -> "Multiply " ^ (string_of_int n)
   | Distribute n -> "Distribute " ^ (string_of_int n)

  let simplify len = function
  | Distribute n -> Multiply (invert n len)
  | x -> x

  let apply len (cursor, stride) = function
  | Shift n -> (mulm n stride cursor len, stride)
  | Multiply n -> (cursor, mulm n stride 0 len)
  | Distribute n -> raise_notrace (Invalid_argument "Distribute cannot be applied; Simplify before applying!")

 end in
 let q = Queue.create () in
 In_channel.(with_open_bin "22.txt" input_lines) |> List.iter (Shuf.append_of_string q) ;
 let shuf = Queue.to_seq q |> Seq.map (Shuf.simplify len) in
 if debug then Seq.iter (fun s -> s |> Shuf.to_string |> print_endline) shuf ;

 let (cursor, stride) =
  shuf |>
  Seq.fold_left (Shuf.apply len) (0, 1) in
 Printf.printf "(%d,%d)\n" cursor stride;

 let powers = Dynarray.create () in
 Dynarray.add_last powers (cursor, stride) ;

 for i = 1 to 64 do
  let cursor, stride = Dynarray.get_last powers in
  let (c,s) =
   Shuf.[Shift cursor; Multiply stride] |> List.to_seq |>
   Seq.cycle |> Seq.take 4 |>
   Seq.fold_left (Shuf.apply len) (0, 1) in
  if debug then Printf.printf "(%d,%d)\n" c s;
  Dynarray.add_last powers (c,s)
 done ;

 let cursor, stride =
  Seq.zip
  (Seq.unfold (fun n -> if n = 0 then None else Some (n land 1 = 1, n lsr 1)) iterations)
  (Dynarray.to_seq powers) |>
  Seq.fold_left
   (fun (c,s) (test,(c',s')) ->
    if test then (
     (c,s) |>
     Shuf.(fun cs -> apply len cs (Shift c')) |>
     Shuf.(fun cs -> apply len cs (Multiply s'))
    ) else (c,s)) (0,1) in

 Printf.printf "(%d,%d)\n" cursor stride;
 
 if solve_index
 then
  let ans, _ =
   Shuf.apply len (cursor, stride) (Shift target) in
  if ans < 0 then ans + len else ans
 else
  mulm (target - cursor) (invert stride len) 0 len

(* do we have to deal with race conditions here? *)
let problem_23a () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  (* for networking *)
  let buf = Queue.create () in
  let packet_size = 3 in

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   (* yield at every single step *)
   Effect.perform (Inter.Yield) ;
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Queue.add (-1) inq
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 buf ;
     (* network buffering *)
     if Queue.length buf = packet_size then (
      for i = 1 to packet_size do
       Queue.add (Queue.take buf) outq
      done
     ) ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inqs = Array.init 51 (fun _ -> Queue.create ()) in
 for i = 0 to 49 do
  Queue.add i inqs.(i)
 done ;
 let outq = Queue.create () in
 let runq : (unit -> unit) Queue.t = Queue.create () in

 let run_with id step =
  match run inqs.(id) outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then (Queue.add (Effect.Deep.continue k) runq ; (Queue.take runq) ()) else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let exit = ref false in
 let step () =
  if not (Queue.is_empty outq)
  then (
   let adr = Queue.take outq in
   let x   = Queue.take outq in
   let y   = Queue.take outq in
   let adr = if adr = 255 then (exit := true ; 50) else adr in
   Queue.add x inqs.(adr) ;
   Queue.add y inqs.(adr)) ;
  not !exit
 in

 for i = 0 to 49 do
  Queue.add (fun () -> run_with i step) runq
 done ;

 (Queue.take runq) () ;

 (*run_with step ;*)
 Queue.to_seq inqs.(50) |>
 Seq.iter (fun n -> print_int n ; print_newline ()) ;
 ()

(* do we have to deal with race conditions here? *)
let problem_23b () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  let pagesize_log2 = 12 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  (* for networking *)
  let buf = Queue.create () in
  let packet_size = 3 in

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   (* yield at every single step *)
   Effect.perform (Inter.Yield) ;
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Queue.add (-1) inq
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 buf ;
     (* network buffering *)
     if Queue.length buf = packet_size then (
      for i = 1 to packet_size do
       Queue.add (Queue.take buf) outq
      done
     ) ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inqs = Array.init 51 (fun _ -> Queue.create ()) in
 for i = 0 to 49 do
  Queue.add i inqs.(i)
 done ;
 let outq = Queue.create () in
 let runq : (unit -> unit) Queue.t = Queue.create () in

 let run_with id step =
  match run inqs.(id) outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then (Queue.add (Effect.Deep.continue k) runq; (Queue.take runq) ()) else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let exit = ref false in
 let last = ref None in
 let count = ref 0 in
 let step () =
  incr count ;
  if not (Queue.is_empty outq)
  then (
   let adr = Queue.take outq in
   let x   = Queue.take outq in
   let y   = Queue.take outq in
   let adr = if adr = 255 then (print_endline "NAT" ; Queue.clear inqs.(50) ; 50) else adr in
   Queue.add x inqs.(adr) ;
   Queue.add y inqs.(adr) ;
  ) ;
  (* this might work with an idle counter? sets idle if reads (-1) more than 5 times without writing, etc. *)
  (* if !idle_flags = (0x1 lsl 50) - 1 then ( *)
  (* if !count mod 500000 = 0 then ( *)
  if !count mod 200000 = 0 then (
    print_endline "IDLE!" ;
    if Queue.length inqs.(50) = 2 then
    (Queue.to_seq inqs.(50) |> Queue.add_seq inqs.(0)) ;
    match !last, (Queue.to_seq inqs.(50) |> Seq.drop 1 |> Seq.uncons) with
    | Some y, Some (y',_) when y = y' -> exit := true
    | _, Some (y',_) -> last := Some y' ; print_int y' ; print_newline ()
    | _ -> ()
  ) ;
  not !exit
 in

 for i = 0 to 49 do
  Queue.add (fun () -> run_with i step) runq
 done ;

 (Queue.take runq) () ;

 (*run_with step ;*)
 Queue.to_seq inqs.(50) |>
 Seq.iter (fun n -> print_int n ; print_newline ()) ;
 !last

(* 11248 is too low *)
(* 17286 is too low *)
(* 11249 is correct, using artificial sleep... *)

(* modified? game of life *)
let problem_24a () =
 let example = false in
 let size = 5 in
 let grid0 =
  In_channel.(with_open_bin (if example then "24e.txt" else "24.txt") input_all) |>
  String.to_seq |> Seq.filter ((<>)'\n') |>
  Seq.fold_left (fun a c -> (a lsr 1) lor (if c = '#' then (1 lsl (size * size - 1)) else 0)) 0 in

 let grids = [| grid0 ; grid0 |] in
 
 let get id (y,x) =
  if y < 0 || y >= size || x < 0 || x >= size then false else
  grids.(id) land (1 lsl (y*size+x)) <> 0 in

 let set id (y,x) bug =
  if y < 0 || y >= size || x < 0 || x >= size then () else
  if bug then grids.(id) <- grids.(id) lor (1 lsl (y*size+x)) else
  grids.(id) <- grids.(id) land (lnot (1 lsl (y*size+x))) in

 let print_grid id =
  for y = 0 to size - 1 do
   for x = 0 to size - 1 do
    print_char (if get id (y,x) then '#' else '.')
   done ;
   print_newline ()
  done in

 let iterate i =
  let in_id = i land 1 in
  let out_id = in_id lxor 1 in
  for y = 0 to size - 1 do
   for x = 0 to size - 1 do
    let adj = 
     [y-1,x;y+1,x;y,x-1;y,x+1] |>
     List.map (get in_id) |>
     List.fold_left (fun a bug -> if bug then succ a else a) 0 in
    if get in_id (y,x) then set out_id (y,x) (adj = 1)
    else set out_id (y,x) (adj = 1 || adj = 2)
   done
  done in

 let seen = Hashtbl.create 4096 in
 (*Hashtbl.add seen grids.(0) () ;*)

 let i = ref 1 in
 while
  iterate !i ;
  not (Hashtbl.mem seen grids.(!i land 1 lxor 1))
 do
  Hashtbl.add seen grids.(!i land 1 lxor 1) () ;
  incr i
 done ;
 
 print_grid (!i land 1 lxor 1) ;
 !i, grids.(!i land 1 lxor 1)

let problem_24b () =
 let example = false in
 let debug = false in
 let size = 5 in
 let grid0 =
  In_channel.(with_open_bin (if example then "24e.txt" else "24.txt") input_all) |>
  String.to_seq |> Seq.filter ((<>)'\n') |>
  Seq.fold_left (fun a c -> (a lsr 1) lor (if c = '#' then (1 lsl (size * size - 1)) else 0)) 0 in

 (* very specific adjacency addressing *)
 let adjacent = function
 | (z,0) -> [z-1,7;z,1;z,5;z-1,11]
 | (z,1) -> [z-1,7;z,2;z,6;z,0]
 | (z,2) -> [z-1,7;z,3;z,7;z,1]
 | (z,3) -> [z-1,7;z,4;z,8;z,2]
 | (z,4) -> [z-1,7;z-1,13;z,9;z,3]
 | (z,5) -> [z,0;z,6;z,10;z-1,11]
 | (z,6) -> [z,1;z,7;z,11;z,5]
 | (z,7) -> [z,2;z,8;z+1,0;z+1,1;z+1,2;z+1,3;z+1,4;z,6]
 | (z,8) -> [z,3;z,9;z,13;z,7]
 | (z,9) -> [z,4;z-1,13;z,14;z,8]
 | (z,10) -> [z,5;z,11;z,15;z-1,11]
 | (z,11) -> [z,6;z+1,0;z+1,5;z+1,10;z+1,15;z+1,20;z,16;z,10]
 | (z,12) -> assert false
 | (z,13) -> [z,8;z,14;z,18;z+1,4;z+1,9;z+1,14;z+1,19;z+1,24]
 | (z,14) -> [z,9;z-1,13;z,19;z,13]
 | (z,15) -> [z,10;z,16;z,20;z-1,11]
 | (z,16) -> [z,11;z,17;z,21;z,15]
 | (z,17) -> [z+1,20;z+1,21;z+1,22;z+1,23;z+1,24;z,18;z,22;z,16]
 | (z,18) -> [z,13;z,19;z,23;z,17]
 | (z,19) -> [z,14;z-1,13;z,24;z,18]
 | (z,20) -> [z,15;z,21;z-1,17;z-1,11]
 | (z,21) -> [z,16;z,22;z-1,17;z,20]
 | (z,22) -> [z,17;z,23;z-1,17;z,21]
 | (z,23) -> [z,18;z,24;z-1,17;z,22]
 | (z,24) -> [z,19;z-1,13;z-1,17;z,23]
 | _ -> assert false in

 let grids = Array.init 2 (fun _ -> Dynarray.create (), Dynarray.create ()) in
 Array.iter (fun (upper, lower) -> Dynarray.add_last upper grid0) grids ;

 let count_bits x =
  let rec loop a x =
   if x = 0 then a else
   loop (a + (x land 1)) (x lsr 1) in
  loop 0 x in

 let get_bits id z =
  let (upper, lower) = grids.(id) in
  if z > 0 then (
   while z - 1 >= Dynarray.length lower do Dynarray.add_last lower 0 done ;
   Dynarray.get lower (z - 1))
  else (
   while abs z >= Dynarray.length upper do Dynarray.add_last upper 0 done ;
   Dynarray.get upper (abs z)) in

 let get id (z,n) =
  let current_level = get_bits id z in
  current_level land (1 lsl n) <> 0 in
                     
 let set id (z,n) bug =
  let (upper, lower) = grids.(id) in
  let current_level = get_bits id z in
  Dynarray.set
   (if z > 0 then lower else upper)
   (if z > 0 then z - 1 else abs z)
   (if bug then
     current_level lor (1 lsl n)
    else
     current_level land (lnot (1 lsl n))) in

 (* based on iteration i, a space cannot be occupied in abs z > range *)
 let range i = i / 2 + 1 in

 let iterate i =
  let in_id = i land 1 in
  let out_id = in_id lxor 1 in
  let r = range i in
  for z = ~-r to r do
   for n = 0 to 24 do
    if n = 12 then ()
    else begin
     let adj =
      adjacent (z,n) |>
      List.map (get in_id) |>
      List.fold_left (fun a bug -> if bug then succ a else a) 0 in
     if get in_id (z,n) then set out_id (z,n) (adj = 1)
     else set out_id (z,n) (adj = 1 || adj = 2)
    end
   done
  done in

 let count_grid id =
  let upper, lower = grids.(id) in
  (+)
   (Dynarray.fold_left (fun a n -> a + count_bits n) 0 upper)
   (Dynarray.fold_left (fun a n -> a + count_bits n) 0 lower) in

 let print_level id z =
  for y = 0 to size - 1 do
   for x = 0 to size - 1 do
    print_char (if get id (z,y*size+x) then '#' else '.')
   done ;
   print_newline ()
  done in
 
 let iterations = 200 in

 for i = 1 to iterations do
  iterate i
 done ;

 if debug then (
 for i = (-5) to 5 do
  print_level (iterations land 1 lxor 1) i ;
  print_newline ()
 done ) ;

 count_grid (iterations land 1 lxor 1)

let problem_25a () =
 let example = false in
 let debug = true in

 (* Interrupt Module *)
 let module Inter = struct
  open Effect
  open Effect.Deep
  type _ Effect.t += Yield : unit t
  let rec go procs =
   if Queue.is_empty procs then () else
   match (Queue.take procs) () with
   | () -> go procs
   | effect Yield, k -> Queue.add (continue k) procs; go procs
 end in

 let program =
  In_channel.(with_open_bin (if example then "25e.txt" else "25.txt") input_line) |> Option.get |>
  String.split_on_char ',' |> List.map int_of_string |> Array.of_list in

 let run inq outq =
  (* set up program (mem:rwx) and virtual (vmem:rw) memory *)
  (* increase page size because this program is BIG *)
  let pagesize_log2 = 13 in
  let pagesize = 1 lsl pagesize_log2 (*4096*) in
  let mem = Array.make pagesize 0 in
  assert (Array.length program <= pagesize) ;
  Array.blit program 0 mem 0 (Array.length program) ;
  let vmem = Hashtbl.create 1024 in
  Hashtbl.add vmem 0 mem ;

  let (.%()) vmem ptr =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None -> 0
   | Some page -> page.(idx) in

  let (.%()<-) vmem ptr v =
   assert (ptr >= 0) ; (* no negative addresses, per spec *)
   let page_idx = ptr lsr pagesize_log2 in
   let idx = ptr land (pagesize - 1) in
   match Hashtbl.find_opt vmem page_idx with
   | None ->
     let new_page = Array.make pagesize 0 in
     new_page.(idx) <- v ;
     if debug then (
      Printf.printf "Allocating New Page @ %d!\n" page_idx
     ) ;
     Hashtbl.add vmem page_idx new_page
   | Some page -> page.(idx) <- v in

  let rbi = ref 0 in

  let get_param n i =
   (* Opcode Mask (Base 10): [Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    raise (Invalid_argument "Invalid Number of Parameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> vmem.%(mem.(i+n)) (* Position Mode *)
   | 1 -> mem.(i+n) (* Immediate Mode *)
   | 2 -> vmem.%(!rbi+mem.(i+n)) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal Parameter Mode") in

  let get_wparam_address n i =
   (* Opcode Mask (Base 10): [Mode3][Mode2][Mode1][OpHi][OpLo] *)
   let rshft =
    if n = 1 then 100 else
    if n = 2 then 1000 else
    if n = 3 then 10000 else
    raise (Invalid_argument "Invalid Number of WParameters") in
   match (mem.(i) / rshft) mod 10 with
   | 0 -> mem.(i+n) (* Position Mode *)
   | 1 -> i+n (* Immediate Mode : Never Used *)
   | 2 -> !rbi+mem.(i+n) (* Relative Mode *)
   | _ -> raise (Invalid_argument "Illegal WParameter Mode") in

  let rec run i =
   match mem.(i) mod 100 with
   | 99 -> (* HCF *)
     mem.(0)
   | 1 -> (* ADD *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 + param2 ;
     run (i+4)
   | 2 -> (* MUL *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- param1 * param2 ;
     run (i+4)
   | 3 -> (* READ *)
     while Queue.is_empty inq do
      Effect.perform (Inter.Yield)
     done ;
     vmem.%(get_wparam_address 1 i) <- Queue.take inq ;
     run (i+2)
   | 4 -> (* WRITE *)
     let param1 = get_param 1 i in
     Queue.add param1 outq ;
     run (i+2)
   | 5 -> (* JNZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 <> 0 then run (param2) else run (i+3)
   | 6 -> (* JZ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     if param1 = 0 then run (param2) else run (i+3)
   | 7 -> (* LT *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 < param2 then 1 else 0 ;
     run (i+4)
   | 8 -> (* EQ *)
     let param1 = get_param 1 i in
     let param2 = get_param 2 i in
     vmem.%(get_wparam_address 3 i) <- if param1 = param2 then 1 else 0 ;
     run (i+4)
   | 9 -> (* ARBI - ADJUST RELATIVE BASE INDEX *)
     let param1 = get_param 1 i in
     rbi := !rbi + param1 ;
     run (i+2)
   | _ -> raise (Invalid_argument "Illegal Instruction") in

  run 0 |> ignore in

 let inq = Queue.create () in
 let outq = Queue.create () in

 let run_with step =
  match run inq outq with
  | () -> ()
  | effect Inter.Yield, k ->
    if step () then Effect.Deep.continue k () else
    (try Effect.Deep.discontinue k Exit with Exit -> ()) in

 let step () =
  (* flush output *)
  Queue.to_seq outq |> Seq.iter (fun n -> if n < 255 then print_char (Char.chr n) else (print_int n ; print_newline ())) ;
  Queue.clear outq ;
  (* read input *)
  let s = read_line () in
  if s = "QUIT" then false
  else (
   String.iter (fun c -> Queue.add (Char.code c) inq) s ;
   Queue.add 10 inq ;
   true ) in

 (* change modes *)
 Queue.clear inq ;
 Queue.clear outq ;

 run_with step ;
 (* flush final output *)
 Queue.to_seq outq |> Seq.iter (fun n -> if n < 255 then print_char (Char.chr n) else (print_int n ; print_newline ())) ;
 (* mug + food ration + fuel cell + prime # works as a solution! *)
 ()
