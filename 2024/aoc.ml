(* aoc.ml *)
(* Advent of Code 2024 *)
(* stdlib: 5.2 *)

let problem_01a () =
 let inputs = In_channel.(with_open_bin "01.txt" input_lines) |>
  List.map (fun s -> Scanf.sscanf s "%d %d" (fun a b -> (a,b))) in
 let len = List.length inputs in
 let left = Array.make len 0 and right = Array.make len 0 in
 List.iteri (fun i (a,b) -> left.(i) <- a; right.(i) <- b) inputs ;
 Array.sort compare left; Array.sort compare right;
 Seq.zip (Array.to_seq left) (Array.to_seq right) |>
 Seq.fold_left (fun a (l,r) -> abs (l - r) + a) 0

let problem_01b () =
 let inputs = In_channel.(with_open_bin "01.txt" input_lines) |>
  List.map (fun s -> Scanf.sscanf s "%d %d" (fun a b -> (a,b))) in
 let len = List.length inputs in
 let left = Array.make len 0 and right = Array.make len 0 in
 List.iteri (fun i (a,b) -> left.(i) <- a; right.(i) <- b) inputs ;
 (* sorting is not necessary if you aren't short circuiting *)
 Array.sort compare left; Array.sort compare right;
 Array.fold_left (fun a x ->
  (Array.fold_left (fun a n -> if n = x then succ a else a) 0 right) * x + a
 ) 0 left

(* short-circuit the count when right > left *)
(* smarter search could help, but is it worth it in the long run? *)
(* the left side has no repeats, so memoization does not help *)
let problem_01b2 () =
 let exception BreakI of int in
 let inputs = In_channel.(with_open_bin "01.txt" input_lines) |>
  List.map (fun s -> Scanf.sscanf s "%d %d" (fun a b -> (a,b))) in
 let len = List.length inputs in
 let left = Array.make len 0 and right = Array.make len 0 in
 List.iteri (fun i (a,b) -> left.(i) <- a; right.(i) <- b) inputs ;
 Array.sort compare left; Array.sort compare right;
 Array.fold_left (fun a x ->
  (try
    Array.fold_left(fun a n -> if n > x then raise_notrace (BreakI a) else if n = x then succ a else a) 0 right
   with BreakI res -> res) * x + a) 0 left

let problem_02a () =
 let inputs =
   In_channel.(with_open_bin "02.txt" input_lines) |>
   List.map (fun s -> s |> String.split_on_char ' ' |> List.map int_of_string) in
 let is_safe ns =
  let diff =
   Seq.zip (List.to_seq ns) (List.to_seq ns |> Seq.drop 1) |>
   Seq.map (fun (a,b) -> a - b) in
  let min' = Seq.fold_left min 4 diff
  and max' = Seq.fold_left max (~-4) diff in
  if min' < 0 then min' >= (~-3) && max' <= (~-1) else
  min' >= 1 && max' <= 3 in
 inputs |>
 List.filter is_safe |>
 List.length

let problem_02b () =
 let inputs =
  In_channel.(with_open_bin "02.txt" input_lines) |>
  List.map (fun s -> s |> String.split_on_char ' ' |> List.map int_of_string) in
 let is_safe ns =
  let diff =
   Seq.zip (List.to_seq ns) (List.to_seq ns |> Seq.drop 1) |>
   Seq.map (fun (a,b) -> a - b) in
  let min' = Seq.fold_left min 4 diff
  and max' = Seq.fold_left max (~-4) diff in
  if min' < 0 then min' >= (~-3) && max' <= (~-1) else
  min' >= 1 && max' <= 3 in
 let is_safe_with_dampening ns =
   is_safe ns ||
   Seq.ints 0 |> Seq.take (List.length ns) |>
   Seq.map (fun idx -> List.filteri (fun i _ -> i <> idx) ns) |>
   Seq.exists is_safe in
 inputs |>
 List.filter is_safe_with_dampening |>
 List.length

(* parse without regex for more fun *)
(* find all format mul([0-9]\{1,3},[0-9]\{1,3}) *)
let problem_03a () =
 let (let*) = Option.bind in
 let is_digit = function '0'..'9' -> true | _ -> false in
 let module Instr = struct
  type t = Mul of int * int
  let parse_num (s,i) : (int * int) option =
   let digits = String.sub s i 3 |> String.to_seq |> Seq.take_while is_digit in
   let len = Seq.length digits in
   if len <> 0 then Some (int_of_string (String.of_seq digits),i+len) else None
  let rec parse (s,i) : (t * int) option =
   try
    if String.sub s i 4 <> "mul(" then parse (s,i+1) else
    match 
     let* (n1,i') = parse_num (s,i+4) in
     let* _ = if s.[i'] = ',' then Some () else None in
     let* (n2,i') = parse_num (s,i'+1) in
     if s.[i'] = ')' then Some (Mul (n1,n2), i'+1) else None
    with
    | None -> parse (s,i+1)
    | ret -> ret
   with Invalid_argument _ -> None (*EOF*)
 end in
 let input =
  In_channel.(with_open_bin "03.txt" input_all) in
 let instr_of_string s =
  let rec instr_of_string' acc (s,i) =
   match Instr.parse (s,i) with
   | Some (instr,next) -> instr_of_string' (instr::acc) (s,next)
   | None -> List.rev acc
  in
  instr_of_string' [] (s,0) in
 input |>
 instr_of_string |>
 List.map Instr.(function Mul (n1,n2) -> n1*n2) |>
 List.fold_left (+) 0

let problem_03b () =
 let (let*) = Option.bind in
 let is_digit = function '0'..'9' -> true | _ -> false in
 let module Instr = struct
  type t = Mul of int * int | Do | Dont
  let parse_num (s,i) : (int * int) option =
   let digits = String.sub s i 3 |> String.to_seq |> Seq.take_while is_digit in
   let len = Seq.length digits in
   if len <> 0 then Some (int_of_string (String.of_seq digits),i+len) else None
  let rec parse (s,i) : (t * int) option =
   try
    if String.sub s i 4 = "do()" then Some (Do, i+4) else
    if String.sub s i 7 = "don't()" then Some (Dont, i+7) else
    if String.sub s i 4 <> "mul(" then parse (s,i+1) else
    match 
     let* (n1,i') = parse_num (s,i+4) in
     let* _ = if s.[i'] = ',' then Some () else None in
     let* (n2,i') = parse_num (s,i'+1) in
     if s.[i'] = ')' then Some (Mul (n1,n2), i'+1) else None
    with
    | None -> parse (s,i+1)
    | ret -> ret
   with Invalid_argument _ -> None (*EOF*)
 end in
 let input =
  In_channel.(with_open_bin "03.txt" input_all) in
 let instr_of_string s =
  let rec instr_of_string' acc (s,i) =
   match Instr.parse (s,i) with
   | Some (instr,next) -> instr_of_string' (instr::acc) (s,next)
   | None -> List.rev acc
  in
  instr_of_string' [] (s,0) in
 let instrs = Instr.Do::(instr_of_string input) in
 List.to_seq instrs |>
 Seq.group Instr.(fun a b -> a = Do && b <> Dont) |>
 Seq.filter (fun s -> match Seq.uncons s with Some (Instr.Do,_) -> true | _ -> false) |>
 Seq.concat |>
 Seq.map Instr.(function Mul (n1,n2) -> n1*n2 | _ -> 0) |>
 Seq.fold_left (+) 0

(* 8 possible directions *)
let problem_04a () =
 let module XMAS = struct
  type dir = N | S | W | E | NW | NE | SW | SE
  type t = {y : int; x : int; dir : dir}
  let dirs = [ N ; S ; W ; E ; NW ; NE ; SW ; SE ]
  let it_of_dir = function
  | N  -> [0,0; ~-1,0; ~-2,0; ~-3,0]
  | S  -> [0,0; 1,0; 2,0; 3,0]
  | W  -> [0,0; 0,~-1; 0,~-2; 0,~-3]
  | E  -> [0,0; 0,1; 0,2; 0,3]
  | NE -> [0,0; ~-1,1; ~-2,2; ~-3,3]
  | NW -> [0,0; ~-1,~-1; ~-2,~-2; ~-3,~-3]
  | SE -> [0,0; 1,1; 2,2; 3,3]
  | SW -> [0,0; 1,~-1; 2,~-2; 3,~-3]
  let get map (y,x) = map.(y).[x]
  let int_of_dir d = List.find_index d dirs
  let is_xmas map y x =
   List.map (fun d -> (d,d |> it_of_dir |> List.map (fun (dy,dx) -> (y+dy,x+dx)))) dirs |>
   List.filter_map (fun (d,idxs) ->
    try
     let cs = List.map (get map) idxs in Some (d,cs)
    with _ -> None) |>
   List.filter_map (fun (d,cs) -> if cs |> List.to_seq |> String.of_seq |> (=) "XMAS" then Some {y = y; x = x; dir = d} else None)
 end in
 let inputs = 
  In_channel.(with_open_bin "04.txt" input_lines) |> Array.of_list in
 let table = Hashtbl.create 4096 in
 for y = 0 to Array.length inputs - 1 do
  for x = 0 to String.length inputs.(y) - 1 do
   if inputs.(y).[x] = 'X' then
    let matches = XMAS.is_xmas inputs y x |> List.to_seq |> Seq.map (fun x -> (x,())) in
    Hashtbl.add_seq table matches ;
   else ()
 done done;
 Hashtbl.length table

(* fewer directions to test this time, only orientation of the MMs *)
(* like part 1, we track matches in a hashset, but we do not have to, a counter will suffice *)
let problem_04b () =
 let module XMAS = struct
  type dir = N | S | W | E
  type t = {y : int; x : int; dir : dir}
  let dirs = [ N ; S ; W ; E ]
  (* it should spell "MASMS" *)
  let it_of_dir = function
  | N -> [~-1,~-1;0,0; 1,1; ~-1,1; 1,~-1]
  | S -> [1,1;0,0; ~-1,~-1; 1,~-1; ~-1,1]
  | W -> [~-1,~-1;0,0; 1,1; 1,~-1; ~-1,1]
  | E -> [~-1,1;0,0; 1,~-1; 1,1; ~-1,~-1]
  let get map (y,x) = map.(y).[x]
  let int_of_dir d = List.find_index d dirs
  let is_xmas map y x =
   List.map (fun d -> (d,d |> it_of_dir |> List.map (fun (dy,dx) -> (y+dy,x+dx)))) dirs |>
   List.filter_map (fun (d,idxs) ->
    try
     let cs = List.map (get map) idxs in Some (d,cs)
    with _ -> None) |>
   List.filter_map (fun (d,cs) -> if cs |> List.to_seq |> String.of_seq |> (=) "MASMS" then Some {y = y; x = x; dir = d} else None)
 end in
 let inputs = 
  In_channel.(with_open_bin "04.txt" input_lines) |> Array.of_list in
 let table = Hashtbl.create 4096 in
 for y = 0 to Array.length inputs - 1 do
  for x = 0 to String.length inputs.(y) - 1 do
   if inputs.(y).[x] = 'A' then
    let matches = XMAS.is_xmas inputs y x |> List.to_seq |> Seq.map (fun x -> (x,())) in
    Hashtbl.add_seq table matches ;
   else ()
 done done;
 Hashtbl.length table

(* X|Y means X must be printed before Y *)
let problem_05a () =
 let inputs = 
  In_channel.(with_open_bin "05.txt" input_lines) |> Array.of_list in
 let break =
  Array.find_index ((=)"") inputs |> Option.get in
 let rules =
  Array.sub inputs 0 break |>
  Array.map (fun s -> s |> String.split_on_char '|' |> (function [l;r] -> (int_of_string l, int_of_string r) | _ -> assert false)) in
 let updates =
  Array.sub inputs (break+1) (Array.length inputs - break - 1) |>
  Array.map (fun s -> s |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list) in
 let is_valid_1 update (l,r) =
  match Array.find_index ((=)l) update, Array.find_index ((=)r) update with | Some x, Some y when y < x -> false | _ -> true in
 let is_valid update = rules |> Array.to_seq |> Seq.map (is_valid_1 update) |> Seq.for_all (Fun.id) in
 updates |> Array.to_seq |>
 Seq.filter is_valid |>
 Seq.map (fun update -> update.(Array.length update / 2)) |>
 Seq.fold_left (+) 0

let problem_05b () =
 let inputs = 
  In_channel.(with_open_bin "05.txt" input_lines) |> Array.of_list in
 let break =
  Array.find_index ((=)"") inputs |> Option.get in
 let rules =
  Array.sub inputs 0 break |>
  Array.map (fun s -> s |> String.split_on_char '|' |> (function [l;r] -> (int_of_string l, int_of_string r) | _ -> assert false)) in
 let updates =
  Array.sub inputs (break+1) (Array.length inputs - break - 1) |>
  Array.map (fun s -> s |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list) in
 let is_valid_1 update (l,r) =
  match Array.find_index ((=)l) update, Array.find_index ((=)r) update with | Some x, Some y when y < x -> false | _ -> true in
 let is_valid update = rules |> Array.map (is_valid_1 update) |> Array.for_all (Fun.id) in
 let is_applicable_1 update (l,r) =
  match Array.find_index ((=)l) update, Array.find_index ((=)r) update with | Some _, Some _ -> true | _ -> false in
 let reorder update =
  let applicable = Array.to_seq rules |> Seq.filter (is_applicable_1 update) |> Array.of_seq in
  Array.fast_sort (fun x y -> if Array.exists ((=)(x,y)) applicable then ~-1 else if Array.exists ((=)(y,x)) applicable then 1 else 0) update ; update in
 updates |>
 Array.to_seq |>
 Seq.filter (Fun.negate is_valid) |>
 Seq.map reorder |>
 Seq.map (fun update -> update.(Array.length update / 2)) |>
 Seq.fold_left (+) 0

(* guard always starts facing up *)
let problem_06a () =
 let exception Break of unit in
 let (.%[]<-) = Bytes.set in
 let (.%[]) = Bytes.get in
 let module Guard = struct
  type dir = N | E | S | W
  type t = {y : int ; x : int ; d : dir}
  let turn = function N -> E | E -> S | S -> W | W -> N
  (* if we hit an invalid arg, we would be stepping out, so we reraise Break () *)
  let move map g =
   try
    map.(g.y).%[g.x] <- 'X';
    match g.d with
    | N when map.(g.y - 1).%[g.x] = '#' -> {g with d = turn (g.d)}
    | N -> {g with y = g.y - 1}
    | E when map.(g.y).%[g.x + 1] = '#' -> {g with d = turn (g.d)}
    | E -> {g with x = g.x + 1}
    | S when map.(g.y + 1).%[g.x] = '#' -> {g with d = turn (g.d)}
    | S -> {g with y = g.y + 1}
    | W when map.(g.y).%[g.x - 1] = '#' -> {g with d = turn (g.d)}
    | W -> {g with x = g.x - 1}
   with _ -> raise_notrace (Break ())
 end in
 let example = false in
 let debug = false in
 let inputs = 
  In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let count_visited map =
  Array.fold_left
  (fun a bs ->
   Bytes.fold_left (fun a c -> if c = 'X' then succ a else a)
   0 bs + a)
  0 map in
 let guard =
  inputs |>
  Array.to_seq |>
  Seq.fold_lefti
   (fun a y xs ->
    match a with
    | None -> Option.bind (Bytes.index_opt xs '^') (fun x -> Some Guard.{y; x; d = Guard.N})
    | Some _ -> a) None |>
  Option.get |> ref in
 (try
  while true do
   guard := Guard.move inputs !guard
  done
 with Break _ -> ()) ;
 if debug then
  for y = 0 to Array.length inputs - 1 do
   for x = 0 to Bytes.length inputs.(y) - 1 do
    print_char inputs.(y).%[x]
   done ; print_newline ()
  done ;
 count_visited inputs

(* guard always starts facing up *)
(* time: ~19s unopt *)
let problem_06b () =
 let exception BreakB of bool in
 let (.%[]<-) = Bytes.set in
 let (.%[]) = Bytes.get in
 let module Guard = struct
  type dir = N | E | S | W
  type t = {y : int ; x : int ; d : dir}
  let turn = function N -> E | E -> S | S -> W | W -> N
  (* if we hit an invalid arg, we would be stepping out, so we reraise Break () *)
  let move hist map g =
   if Hashtbl.mem hist g then raise_notrace (BreakB false) ;
   try
    Hashtbl.add hist g ();
    match g.d with
    | N when map.(g.y - 1).%[g.x] = '#' -> {g with d = turn (g.d)}
    | N -> {g with y = g.y - 1}
    | E when map.(g.y).%[g.x + 1] = '#' -> {g with d = turn (g.d)}
    | E -> {g with x = g.x + 1}
    | S when map.(g.y + 1).%[g.x] = '#' -> {g with d = turn (g.d)}
    | S -> {g with y = g.y + 1}
    | W when map.(g.y).%[g.x - 1] = '#' -> {g with d = turn (g.d)}
    | W -> {g with x = g.x - 1}
   with _ -> raise_notrace (BreakB true)
 end in
 let example = false in
 let inputs = 
  In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let history = Hashtbl.create (Array.length inputs * Bytes.length inputs.(0) * 4) in
 let guard0 =
  inputs |>
  Array.to_seq |>
  Seq.fold_lefti
   (fun a y xs ->
    match a with
    | None -> Option.bind (Bytes.index_opt xs '^') (fun x -> Some Guard.{y; x; d = Guard.N})
    | Some _ -> a) None |>
  Option.get in
 let guard = ref guard0 in
 let count = ref 0 in
 for y = 0 to Array.length inputs - 1 do
  for x = 0 to Bytes.length inputs.(y) - 1 do
   if inputs.(y).%[x] = '.' then (
    inputs.(y).%[x] <- '#' ;
    (try
     while true do guard := Guard.move history inputs !guard done
    with BreakB oob -> if not oob then incr count else ()) ;
    (* clean up *)
    Hashtbl.clear history ;
    guard := guard0 ;
    inputs.(y).%[x] <- '.' ;
   ) else ()
  done
 done ;
 !count

(* guard always starts facing up *)
(* time: ~12s unopt ; 1s opt *)
let problem_06b2 () =
 let exception BreakB of bool in
 let (.%[]<-) = Bytes.set in
 let (.%[]) = Bytes.get in
 let module Guard = struct
  type dir = N | E | S | W
  type t = {y : int ; x : int ; d : dir}
  type hist = {arr : int array ; h : int ; w : int}
  let turn = function N -> E | E -> S | S -> W | W -> N
  let flag_of_dir = function N -> 1 | E -> 2 | S -> 4 | W -> 8
  (* use 1D state arrays (type hist(ory)) intead of hashtbls for efficiency! *)
  let hist_create h w = {arr = Array.make (h*w) 0; h ; w}
  let hist_clear hist = Array.fill hist.arr 0 (hist.h * hist.w) 0 
  let hist_set hist g = try hist.arr.(g.y * hist.w + g.x) <- hist.arr.(g.y * hist.w + g.x) lor (flag_of_dir g.d) with _ -> ()
  let hist_mem hist g = try (hist.arr.(g.y * hist.w + g.x) land flag_of_dir g.d) <> 0 with _ -> false
  (* if we hit an invalid arg, we would be stepping out, so we reraise our breakpoint *)
  let move hist map g =
   if hist_mem hist g then raise_notrace (BreakB false) ;
   try
    hist_set hist g;
    match g.d with
    | N when map.(g.y - 1).%[g.x] = '#' -> {g with d = turn (g.d)}
    | N -> {g with y = g.y - 1}
    | E when map.(g.y).%[g.x + 1] = '#' -> {g with d = turn (g.d)}
    | E -> {g with x = g.x + 1}
    | S when map.(g.y + 1).%[g.x] = '#' -> {g with d = turn (g.d)}
    | S -> {g with y = g.y + 1}
    | W when map.(g.y).%[g.x - 1] = '#' -> {g with d = turn (g.d)}
    | W -> {g with x = g.x - 1}
   with _ -> raise_notrace (BreakB true)
 end in
 let example = false in
 let inputs = 
  In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let guard0 =
  inputs |>
  Array.to_seq |>
  Seq.fold_lefti
   (fun a y xs ->
    match a with
    | None -> Option.bind (Bytes.index_opt xs '^') (fun x -> Some Guard.{y; x; d = Guard.N})
    | Some _ -> a) None |>
  Option.get in
 let guard = ref guard0 in
 let count = ref 0 in
 let history = Guard.hist_create (Array.length inputs) (Bytes.length inputs.(0)) in
 for y = 0 to Array.length inputs - 1 do
  for x = 0 to Bytes.length inputs.(y) - 1 do
   if inputs.(y).%[x] = '.' then (
    inputs.(y).%[x] <- '#' ;
    (try
     while true do guard := Guard.move history inputs !guard done
    with BreakB oob -> if not oob then incr count else ()) ;
    (* clean up *)
    Guard.hist_clear history ;
    guard := guard0 ;
    inputs.(y).%[x] <- '.' ;
   ) else ()
  done
 done ;
 !count

(* guard always starts facing up *)
(* use results instead of exceptions *)
(* time: ~14.5s unopt ; 1s opt *)
let problem_06b3 () =
 let (.%[]<-) = Bytes.set in
 let (.%[]) = Bytes.get in
 let module Guard = struct
  type dir = N | E | S | W
  type t = {y : int ; x : int ; d : dir}
  type hist = {arr : int array ; h : int ; w : int}
  let turn = function N -> E | E -> S | S -> W | W -> N
  let flag_of_dir = function N -> 1 | E -> 2 | S -> 4 | W -> 8
  (* use 1D state arrays (type hist(ory)) intead of hashtbls for efficiency! *)
  let hist_create h w = {arr = Array.make (h*w) 0; h ; w}
  let hist_clear hist = Array.fill hist.arr 0 (hist.h * hist.w) 0 
  let hist_set hist g = try hist.arr.(g.y * hist.w + g.x) <- hist.arr.(g.y * hist.w + g.x) lor (flag_of_dir g.d) with _ -> ()
  let hist_mem hist g = try (hist.arr.(g.y * hist.w + g.x) land flag_of_dir g.d) <> 0 with _ -> false
  (* if we hit an invalid arg, we would be stepping out, so we reraise our breakpoint *)
  let move hist map g =
   if hist_mem hist g then Result.error false else
   try
    hist_set hist g;
    (match g.d with
    | N when map.(g.y - 1).%[g.x] = '#' -> {g with d = turn (g.d)}
    | N -> {g with y = g.y - 1}
    | E when map.(g.y).%[g.x + 1] = '#' -> {g with d = turn (g.d)}
    | E -> {g with x = g.x + 1}
    | S when map.(g.y + 1).%[g.x] = '#' -> {g with d = turn (g.d)}
    | S -> {g with y = g.y + 1}
    | W when map.(g.y).%[g.x - 1] = '#' -> {g with d = turn (g.d)}
    | W -> {g with x = g.x - 1}) |>
    Result.ok
   with _ -> Result.error true
 end in
 let example = false in
 let inputs = 
  In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let guard0 =
  inputs |>
  Array.to_seq |>
  Seq.fold_lefti
   (fun a y xs ->
    match a with
    | None -> Option.bind (Bytes.index_opt xs '^') (fun x -> Some Guard.{y; x; d = Guard.N})
    | Some _ -> a) None |>
  Option.get in
 let count = ref 0 in
 let history = Guard.hist_create (Array.length inputs) (Bytes.length inputs.(0)) in
 for y = 0 to Array.length inputs - 1 do
  for x = 0 to Bytes.length inputs.(y) - 1 do
   if inputs.(y).%[x] = '.' then (
    inputs.(y).%[x] <- '#' ;
    let guard = ref @@ Ok guard0 in
    while Result.is_ok !guard do
     guard := Result.bind !guard (Guard.move history inputs)
    done ; !guard |>
    Result.iter_error (fun oob -> if not oob then incr count);
    (* clean up *)
    Guard.hist_clear history ;
    inputs.(y).%[x] <- '.' ;
   ) else ()
  done
 done ;
 !count

let problem_07a () =
 let example = false in
 let parse_input s =
  match s |> String.split_on_char ':' with
  | [s1; s2]  ->
    let ns = s2 |> String.split_on_char ' ' |> List.filter ((<>)"") |> List.map int_of_string in
    (int_of_string s1, ns)
  | _ -> failwith "Invalid Input Line" in

 let rec test a (res, ns) =
  match ns with
  | [] -> a = res
  | hd::tl ->
    a <= res &&
    test (hd+a) (res,tl) ||
    test (hd*a) (res,tl) in

 let test_wrapper (res, ns) =
  test (List.hd ns) (res, List.tl ns) in

 let inputs = 
  In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_lines) |>
  List.map parse_input in
 inputs |>
 List.filter test_wrapper |>
 List.fold_left (fun a (res,_) -> res + a) 0

(* forward recursive version : slow *)
let problem_07b () =
 let example = false in
 let parse_input s =
  match s |> String.split_on_char ':' with
  | [s1; s2]  ->
    let ns = s2 |> String.split_on_char ' ' |> List.filter ((<>)"") |> List.map int_of_string in
    (int_of_string s1, ns)
  | _ -> failwith "Invalid Input Line" in

 let concat n1 n2 = Printf.sprintf "%d%d" n1 n2 |> int_of_string in
  
 let rec test a (res, ns) =
  match ns with
  | [] -> a = res
  | hd::tl ->
    a <= res &&
    test (hd+a) (res,tl) ||
    test (hd*a) (res,tl) ||
    test (concat a hd) (res,tl) in

 let test_wrapper (res, ns) =
  test (List.hd ns) (res, List.tl ns) in

 let inputs = 
  In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_lines) |>
  List.map parse_input in
 inputs |>
 List.filter test_wrapper |>
 List.fold_left (fun a (res,_) -> res + a) 0

(* reversed recursive version : extremely fast *)
let problem_07b2 () =
 let example = false in
 let parse_input s =
  match s |> String.split_on_char ':' with
  | [s1; s2]  ->
    let ns = s2 |> String.split_on_char ' ' |> List.filter ((<>)"") |> List.map int_of_string in
    (int_of_string s1, ns)
  | _ -> failwith "Invalid Input Line" in

 let rec is_suffix_of suf n =
  if suf = 0 then true else
  if suf mod 10 = n mod 10 then is_suffix_of (suf / 10) (n / 10) else
  false in

 let rec strip_suffix suf n =
  if suf = 0 then n else strip_suffix (suf / 10) (n / 10) in
  
 let rec test (res, ns) =
  match ns with
  | [hd] -> hd = res
  | hd::tl ->
    res >= 0 &&
    test (res - hd,tl) ||
    (if hd = 0 || res mod hd <> 0  then false else test (res / hd,tl)) ||
    (if is_suffix_of hd res |> not then false else test (strip_suffix hd res,tl))
  | _ -> false in

 let test_wrapper (res, ns) =
  test (res, List.rev ns) in

 let inputs = 
  In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_lines) |>
  List.map parse_input in

 inputs |>
 List.filter test_wrapper |>
 List.fold_left (fun a (res,_) -> res + a) 0

(* possible antenna frequency values: [0-9A-Za-z] *)
(* antinodes occur when a point is 1d from one antenna and 2d from another of the same frequency *)
(* count only unique locations; map is not infinite *)
let problem_08a () =
 let example = false in
 let module CMap = Map.Make (struct type t = char let compare = compare end) in
 let map =
  In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_lines) |>
  Array.of_list in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in
 let antennae =
  (let res = ref CMap.empty in
   for y = 0 to map_h - 1 do for x = 0 to map_w - 1 do
    if map.(y).[x] <> '.' then
     let v = Option.value (CMap.find_opt map.(y).[x] !res) ~default:[] in
     res := CMap.add (map.(y).[x]) ((y,x)::v) !res else ()
   done done; !res) |> CMap.map (Array.of_list) in
 let table = Array.make (map_h * map_w) false in
 let in_bounds (y,x) = 0 <= y && y < map_h && 0 <=x && x < map_w in
 let mark_point (y,x) = if in_bounds (y,x) then table.(y*map_h+x) <- true in
 let mark_antinodes ants =
  for i = 0 to Array.length ants - 2 do
   for j = i+1 to Array.length ants - 1 do
    let (y1,x1) = ants.(i) in
    let (y2,x2) = ants.(j) in
    let (dy,dx) = y2 - y1, x2 - x1 in
    mark_point (y2+dy,x2+dx) ; mark_point (y1-dy,x1-dx)
   done
  done in
 CMap.iter (fun _ ants -> mark_antinodes ants) antennae ;
 table |> Array.to_seq |> Seq.filter (Fun.id) |> Seq.length

let problem_08b () =
 let example = false in
 (* you can use an indexer and array instead (probably faster), but this is easier *)
 let module CMap = Map.Make (struct type t = char let compare = compare end) in
 let map =
  In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_lines) |>
  Array.of_list in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in
 let antennae =
  (let res = ref CMap.empty in
   for y = 0 to map_h - 1 do for x = 0 to map_w - 1 do
    if map.(y).[x] <> '.' then
     let v = Option.value (CMap.find_opt map.(y).[x] !res) ~default:[] in
     res := CMap.add (map.(y).[x]) ((y,x)::v) !res else ()
   done done; !res) |> CMap.map (Array.of_list) in
 let table = Array.make (map_h * map_w) false in
 let in_bounds (y,x) = 0 <= y && y < map_h && 0 <=x && x < map_w in
 let mark_point (y,x) = if in_bounds (y,x) then table.(y*map_h+x) <- true in
 let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
 let mark_antinodes ants =
  for i = 0 to Array.length ants - 2 do
   for j = i+1 to Array.length ants - 1 do
    let (y1,x1) = ants.(i) and (y2,x2) = ants.(j) in
    let (dy,dx) = y2 - y1, x2 - x1 in
    let d = gcd (abs dy) (abs dx) in
    let (dy,dx) = dy / d, dx / d in
    let set1 = Seq.iterate (fun (y,x) -> (y+dy,x+dx)) ants.(i) in
    let set2 = Seq.iterate (fun (y,x) -> (y-dy,x-dx)) ants.(i) in
    set1 |> Seq.take_while in_bounds |> Seq.iter mark_point ;
    set2 |> Seq.take_while in_bounds |> Seq.iter mark_point
   done
  done in
 CMap.iter (fun _ ants -> mark_antinodes ants) antennae ;
 table |> Array.to_seq |> Seq.filter (Fun.id) |> Seq.length

(* uses an indexer to avoid using an ordered set *)
let problem_08b2 () =
 let example = false in
 (* max index = 61 *)
 let index_of_char = function
 | '0'..'9' as c -> Char.code c - 0x30
 | 'A'..'Z' as c -> Char.code c - 0x41 + 10
 | 'a'..'z' as c -> Char.code c - 0x61 + 36
 | _ -> failwith "Invalid Char" in
 let antennae = Array.make 62 [] in
 let map =
  In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_lines) |>
  Array.of_list in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in
 for y = 0 to map_h - 1 do for x = 0 to map_w - 1 do
  if map.(y).[x] <> '.' then
   antennae.(index_of_char map.(y).[x]) <- (y,x)::antennae.(index_of_char map.(y).[x])
 done done;
 let table = Array.make (map_h * map_w) false in
 let in_bounds (y,x) = 0 <= y && y < map_h && 0 <=x && x < map_w in
 let mark_point (y,x) = if in_bounds (y,x) then table.(y*map_h+x) <- true in
 let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
 let mark_antinodes ants =
  for i = 0 to Array.length ants - 2 do
   for j = i+1 to Array.length ants - 1 do
    let (y1,x1) = ants.(i) and (y2,x2) = ants.(j) in
    let (dy,dx) = y2 - y1, x2 - x1 in
    (* d may be < 0, but that is okay *)
    let d = gcd dy dx in
    let (dy,dx) = dy / d, dx / d in
    let set1 = Seq.iterate (fun (y,x) -> (y+dy,x+dx)) ants.(i) in
    let set2 = Seq.iterate (fun (y,x) -> (y-dy,x-dx)) ants.(i) in
    set1 |> Seq.take_while in_bounds |> Seq.iter mark_point ;
    set2 |> Seq.take_while in_bounds |> Seq.iter mark_point
   done
  done in
 Array.iter (function [] | [_] -> () | xs -> mark_antinodes (Array.of_list xs)) antennae ;
 table |> Array.to_seq |> Seq.filter (Fun.id) |> Seq.length

(* disk fragmentation *)
(* file,free,file,free *)
let problem_09a () =
 let example = false in
 let (.%[]) = (fun bs i -> (Bytes.get bs i |> Char.code) - 0x30) in
 let (.%[]<-) = (fun bs i n -> Bytes.set bs i (Char.chr (n+0x30))) in
 let input = In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_lines) |> List.hd |> Bytes.unsafe_of_string in
 let input_len = Bytes.length input in
 let disk_size =
  (let count = ref 0 in
   for i = 0 to input_len - 1 do
    if i land 1 = 0 then count := !count + input.%[i]
   done ; !count) in
 let disk_cur = ref 0 in
 let input_cur = ref 0 in
 let input_end_cur = ref (input_len - 1) in
 let disk = Array.make (disk_size) ~-1 in
 (* will throw an exception when done *)
 let step () =
  while input.%[!input_cur] <> 0 do
   disk.(!disk_cur) <- !input_cur / 2 ;
   input.%[!input_cur] <- input.%[!input_cur] - 1 ;
   incr disk_cur
  done ;
  while input.%[!input_cur + 1] <> 0 do
   while input.%[!input_end_cur] = 0 do input_end_cur := !input_end_cur - 2 done ;
   disk.(!disk_cur) <- !input_end_cur / 2 ;
   input.%[!input_end_cur] <- input.%[!input_end_cur] - 1 ;
   input.%[!input_cur + 1] <- input.%[!input_cur + 1] - 1 ;
   incr disk_cur
  done ;
  input_cur := !input_cur + 2 in
 (try
  while true do step () done
 with _ -> ()) ;
 disk |>
 Array.fold_left (fun (a,i) n -> (a+i*n, succ i)) (0,0) |> fst

(* although it works, this solution is very ham-handed; 09b2 is much better *)
let problem_09b () =
 let example = false in
 let (.%[]) = (fun bs i -> (Bytes.get bs i |> Char.code) - 0x30) in
 let (.%[]<-) = (fun bs i n -> Bytes.set bs i (Char.chr (n+0x30))) in
 let input = In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_lines) |> List.hd |> Bytes.unsafe_of_string in
 (* this is an oversight: added to keep track of the size of the holes *)
 let input_orig = Bytes.copy input in
 let input_len = Bytes.length input in
 let free_chunks = Array.init (input_len / 2) (fun i -> (input.%[i*2+1], Array.make (input.%[i*2+1]) 0)) in
 let input_end_cur = ref (input_len - 1) in

 (* will throw an exception when done *)
 (* try to move only once per step! *)
 let step () =
  while input.%[!input_end_cur] = 0 do input_end_cur := !input_end_cur - 2 done ;
  match Array.find_index (fun (free, _) -> free >= input.%[!input_end_cur]) free_chunks with
  (* would have gotten it first try if I remembered this guard!!! *)
  | Some i when i < (!input_end_cur / 2) ->
    let (free, chunk) = free_chunks.(i) in
    Array.fill chunk (Array.length chunk - free) (input.%[!input_end_cur]) (!input_end_cur / 2) ;
    free_chunks.(i) <- (free - input.%[!input_end_cur], chunk) ;
    input.%[!input_end_cur] <- 0 ;
    true
  | _ -> input_end_cur := !input_end_cur - 2 ; false in

 (try
  while true do step () |> ignore done
 with _ -> ()) ;

 let count = ref 0 in
 let cur = ref 0 in
 let count_unmoved id n =
  let rec count_unmoved' id n =
   if n = 0 then () else
   (count := !count + !cur * id ;
    incr cur ;
    count_unmoved' id (n-1)) in
   if n <> 0 then count_unmoved' id n
   else cur := !cur + input_orig.%[id*2] in
   
 let count_chunk i =
  let (_, chunk) = free_chunks.(i) in
  let (count',cur') = Array.fold_left (fun (a,i) n -> a + i * n, succ i) (!count,!cur) chunk in
  count := count' ;
  cur := cur' in
 for i = 0 to input_len / 2 - 1 do
  count_unmoved i (input.%[i*2]) ;
  count_chunk i
 done;
 (* don't forget last unmoved count *)
 count_unmoved (input_len / 2) (input.%[input_len - 1]) ;
 !count
 (*free_chunks, input*)

let problem_09b2 () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_lines) |> List.hd in
 let (info,_,_,_) =
  String.fold_left
   (fun (a,id,cur,free) c ->
    let n = Char.code c - 0x30 in
    if not free then
     ((id,cur,n)::a, id+1, cur+n, true)
    else
     ((~-id,cur,n)::a, id, cur+n, false)) ([],0,0,false) input in
 let chunk_info_rev = info |> List.filter (fun (id,_,_) -> id >= 0) in
 let free_info = info |> List.filter (fun (id,_,_) -> id < 0) |> List.rev |> Array.of_list in
 let chunk_info_rev =
  chunk_info_rev |>
  List.map
  (fun (id,cur,n) ->
   match Array.find_index (fun (_,cur', n') -> n' >= n && cur' <= cur) free_info with
   | None -> (id,cur,n)
   | Some i ->
     let (id', cur', n') = free_info.(i) in
     free_info.(i) <- (id', cur'+n, n'-n) ;
     (id, cur',n)) in
 let checksum_chunk (id,cur,n) =
  Seq.ints cur |> Seq.take n |>
  Seq.fold_left (fun a cur -> a+id*cur) 0 in
  
 chunk_info_rev |> List.fold_left (fun a record -> a + checksum_chunk record) 0

(* topographic lava map *)
(* 0 - lowest, 9 - highest *)
(* as long as possible, gradual uphill slope, no diagonals *)
(* any path that begins at 0 and ends at 9 with each increase being exactly 1 *)
(* score is the number of destinations available from a single trailhead (0) *)
let problem_10a () =
 let (.%[]) s i = Char.code s.[i] - 0x30 in
 let example = false in
 let map = In_channel.(with_open_bin (if example then "10e.txt" else "10.txt") input_lines) |> Array.of_list in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let get_score p =
  let set = ref YXSet.empty in
  let rec get_score' last (y,x) =
   match map.(y).%[x] with
   | 9 when last = 8 -> set := YXSet.add (y,x) !set
   | n when last = n - 1 -> 
     get_score' n (y-1,x) ;
     get_score' n (y+1,x) ;
     get_score' n (y,x+1) ;
     get_score' n (y,x-1)
   | _ -> ()
   | exception Invalid_argument _ -> () in
  get_score' (~-1) p ;
  YXSet.cardinal !set in
 let trailheads =
  Array.fold_left
  (fun (a,y) xs ->
   String.fold_left
   (fun (a,x) c -> (if c = '0' then (y,x)::a else a), succ x)
   (a,0) xs |> fst, succ y)
  ([],0) map |> fst in
 trailheads |>
 List.map get_score |>
 List.fold_left (+) 0

let problem_10b () =
 let (.%[]) s i = Char.code s.[i] - 0x30 in
 let example = false in
 let map = In_channel.(with_open_bin (if example then "10e.txt" else "10.txt") input_lines) |> Array.of_list in
 let rec get_score last (y,x) =
  match map.(y).%[x] with
  | 9 when last = 8 -> 1
  | n when last = n - 1 -> 
    get_score n (y-1,x) +
    get_score n (y+1,x) +
    get_score n (y,x+1) +
    get_score n (y,x-1)
  | _ -> 0
  | exception Invalid_argument _ -> 0 in
 let trailheads =
  Array.fold_left
  (fun (a,y) xs ->
   String.fold_left
   (fun (a,x) c -> (if c = '0' then (y,x)::a else a), succ x)
   (a,0) xs |> fst, succ y)
  ([],0) map |> fst in
 trailheads |>
 List.map (get_score (~-1)) |>
 List.fold_left (+) 0

(* update rules *)
(* if 0 -> 1 *)
(* if even number of digits -> split (left digits, right digits) *)
(* otherwise: n -> n * 2024 *)
(* order is preserved *)

let problem_11a () =
 let steps = 25 in
 let example = false in
 let stones =
  In_channel.(with_open_bin (if example then "11e.txt" else "11.txt") input_lines) |>
  List.hd |> String.split_on_char ' ' |> List.map int_of_string |> List.to_seq |> Queue.of_seq in
 let rec digit_len n =
  if n = 0 then 0 else 1 + digit_len (n/10) in
 let rec lsr10 n shift =
  if shift = 0 then n else lsr10 (n/10) (shift - 1) in
 let rec pow10 n =
  if n = 0 then 1 else 10 * pow10 (n - 1) in
 let update1 () =
  match Queue.take stones with
  | 0 -> Queue.add 1 stones ;
  | n when digit_len n land 1 = 0 ->
    let brkpt = digit_len n / 2 in
    Queue.add (lsr10 n brkpt) stones ;
    Queue.add (n mod (pow10 brkpt)) stones
  | n -> Queue.add (n * 2024) stones in
 let blink () =
  let len = Queue.length stones in
  for i = 0 to len - 1 do update1 () done in
 for i = 1 to steps do blink () done ;
 Queue.length stones
 (*Queue.to_seq stones |> Seq.iter (fun n -> print_int n ; print_newline ())*)

(* 11b with 25 steps instead of 75 *)
let problem_11a2 () =
 let steps = 25 in
 let example = false in
 let debug = false in
 let stones = Hashtbl.create 4096 in

 let register (n,c) = 
  if not (Hashtbl.mem stones n)
  then Hashtbl.add stones n c
  else Hashtbl.replace stones n (Hashtbl.find stones n + c) in

 let rec digit_len n =
  if n = 0 then 0 else 1 + digit_len (n/10) in
 let rec lsr10 n shift =
  if shift = 0 then n else lsr10 (n/10) (shift - 1) in
 let rec pow10 n =
  if n = 0 then 1 else 10 * pow10 (n - 1) in

 let transform (n,c) =
  let c0 = Hashtbl.find stones n in
  if c = c0 then Hashtbl.remove stones n else Hashtbl.replace stones n (c0 - c) ;
  (match n with
  | 0 -> register (1,c)
  | n when digit_len n land 1 = 1 -> register (n*2024,c)
  | n ->
    let brkpt = digit_len n / 2 in
    register (lsr10 n brkpt,c) ;
    register (n mod (pow10 brkpt),c)) in
  
 (* initialize *)
 In_channel.(with_open_bin (if example then "11e.txt" else "11.txt") input_lines) |>
 List.hd |> String.split_on_char ' ' |> List.map int_of_string |>
 List.iter (fun n -> register (n,1)) ;
 
 let blink () =
  (* force eager memoization by converting to a list *)
  stones |>
  Hashtbl.to_seq |> List.of_seq |>
  List.iter transform in

 for i = 1 to steps do blink () done ;
 if debug then Hashtbl.to_seq stones |> Seq.iter (fun (n,c) -> Printf.printf "%d,%d\n" n c);
 (*Hashtbl.length stones*)
 Hashtbl.fold (fun n c a -> a + c) stones 0

(*
multiples of 2024:
2024 -> 2* 24 -> 2* 4*
4048 -> 4* 48 -> 4* 8*
6072 -> 6* 72 -> 7* 2*
8096 -> 8* 96 -> 9* 6*
10120 -> 20482880 -> 2048 2880 -> 20 48 28 80 -> all seen
12144 -> 24579456 -> 2457 9456 -> 24 57 94 56 -> all seen
14168 -> 28676032 -> 2867 6032 -> 28 67 60 32 -> all seen
16192 -> 32772608 -> 3277 2608 -> 32 77 26 08* -> all seen
18216 -> 36869184 -> 3686 9184 -> 36 86 91 84 -> all seen
*)

(* memoization puzzle *)
(* order does not really matter, track only unique numbers *)
let problem_11b () =
 let steps = 75 in
 let example = false in
 let debug = false in
 let stones = Hashtbl.create 4096 in

 let register (n,c) = 
  if not (Hashtbl.mem stones n)
  then Hashtbl.add stones n c
  else Hashtbl.replace stones n (Hashtbl.find stones n + c) in

 let rec digit_len n =
  if n = 0 then 0 else 1 + digit_len (n/10) in
 let rec lsr10 n shift =
  if shift = 0 then n else lsr10 (n/10) (shift - 1) in
 let rec pow10 n =
  if n = 0 then 1 else 10 * pow10 (n - 1) in

 let transform (n,c) =
  let c0 = Hashtbl.find stones n in
  if c = c0 then Hashtbl.remove stones n else Hashtbl.replace stones n (c0 - c) ;
  (match n with
  | 0 -> register (1,c)
  | n when digit_len n land 1 = 1 -> register (n*2024,c)
  | n ->
    let brkpt = digit_len n / 2 in
    register (lsr10 n brkpt,c) ;
    register (n mod (pow10 brkpt),c)) in
  
 (* initialize *)
 In_channel.(with_open_bin (if example then "11e.txt" else "11.txt") input_lines) |>
 List.hd |> String.split_on_char ' ' |> List.map int_of_string |>
 List.iter (fun n -> register (n,1)) ;
 
 let blink () =
  (* force eager memoization by converting to a list *)
  stones |>
  Hashtbl.to_seq |> List.of_seq |>
  List.iter transform in

 for i = 1 to steps do blink () done ;
 if debug then Hashtbl.to_seq stones |> Seq.iter (fun (n,c) -> Printf.printf "%d,%d\n" n c);
 (*Hashtbl.length stones*)
 Hashtbl.fold (fun n c a -> a + c) stones 0

(* fences *)
let problem_12a () =
 (* use as Plot type, uniquely identify by min_elt, area = cardinal, perimeter needs to be calculated *)
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let example = false in

 let map = In_channel.(with_open_bin (if example then "12e.txt" else "12.txt") input_lines) |> Array.of_list in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in

 let seen = Array.make (map_h * map_w) false in
 let rec fill map crop acc (y,x) =
  if YXSet.mem (y,x) acc || y < 0 || x < 0 || y >= map_h || x >= map_w then acc else
  if map.(y).[x] = crop then 
    [y-1,x;y+1,x;y,x+1;y,x-1] |>
    List.fold_left (fill map crop) (YXSet.add (y,x) acc)
  else acc in

 let plots =
  Array.fold_left (fun (a,y) xs ->
   String.fold_left (fun (a,x) c ->
    if not seen.(y*map_w + x) then
     let points = fill map c YXSet.empty (y,x) in
     (YXSet.iter (fun (y,x) -> seen.(y*map_w + x) <- true) points ;
     (points::a, succ x))
    else (a, succ x)) (a,0) xs |> fst, succ y) ([],0) map |> fst in

 let perimeter_of_plot points =
  YXSet.fold (fun (y,x) p ->
   let adj =
    [y-1,x;y+1,x;y,x+1;y,x-1] |>
    List.fold_left (fun a pt -> if YXSet.mem pt points then succ a else a) 0 in
    p + 4 - adj) points 0 in
 plots |>
 List.map (fun pts -> YXSet.cardinal pts, perimeter_of_plot pts) |>
 List.fold_left (fun a (area, peri) -> a + area*peri) 0

let problem_12b () =
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let example = false in
 
 let map = In_channel.(with_open_bin (if example then "12e.txt" else "12.txt") input_lines) |> Array.of_list in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in

 let seen = Array.make (map_h * map_w) false in
 let rec fill map crop acc (y,x) =
  if YXSet.mem (y,x) acc || y < 0 || x < 0 || y >= map_h || x >= map_w then acc else
  if map.(y).[x] = crop then 
    [y-1,x;y+1,x;y,x+1;y,x-1] |>
    List.fold_left (fill map crop) (YXSet.add (y,x) acc)
  else acc in

 let plots =
  Array.fold_left (fun (a,y) xs ->
   String.fold_left (fun (a,x) c ->
    if not seen.(y*map_w + x) then
     let points = fill map c YXSet.empty (y,x) in
     (YXSet.iter (fun (y,x) -> seen.(y*map_w + x) <- true) points ;
     (points::a, succ x))
    else (a, succ x)) (a,0) xs |> fst, succ y) ([],0) map |> fst in

 let outer_perimeter (dy,dx) points =
  YXSet.diff
   (YXSet.map (fun (y,x) -> y+dy,x+dx) points)
   points in

 let sides_of_plot points =
  (* consider each orientation separately to ensure no overlaps *)
  let op_n = outer_perimeter (~-1,0) points in
  let op_s = outer_perimeter (1,0) points in
  let op_w = outer_perimeter (0,~-1) points in
  let op_e = outer_perimeter (0,1) points in
  (* take outer perimeters of outer perimeters to identify endpoints ; each line will have exactly 2 *)
  let endpoints =
   (outer_perimeter (0,~-1) op_n |> YXSet.cardinal) + (outer_perimeter (0,1) op_n |> YXSet.cardinal) +
   (outer_perimeter (0,~-1) op_s |> YXSet.cardinal) + (outer_perimeter (0,1) op_s |> YXSet.cardinal) +
   (outer_perimeter (~-1,0) op_w |> YXSet.cardinal) + (outer_perimeter (1,0) op_w |> YXSet.cardinal) +
   (outer_perimeter (~-1,0) op_e |> YXSet.cardinal) + (outer_perimeter (1,0) op_e |> YXSet.cardinal) in
  endpoints / 2 in

 plots |>
 List.map (fun pts -> YXSet.cardinal pts, sides_of_plot pts) |>
 List.fold_left (fun a (area, sides) -> a + area*sides) 0

(* coordinate transformation *)
(* local module is overkill, just use a tuple *)
let problem_13a () =
 let example = false in

 (* solve by examining p in terms of bases b and b' (90 deg rotation) *)
 let solve (ax,ay,bx,by,px,py) =
  let p_crs_b = px * by - py * bx in
  let a_crs_b = ax * by - ay * bx in
  let a = if a_crs_b = 0 || p_crs_b mod a_crs_b <> 0 then 0 else p_crs_b / a_crs_b in
  let px' = px - ax * a in
  let py' = py - ay * a in
  let b = if px' mod bx = 0 && py' mod by = 0 && px' / bx = py' / by then px' / bx else (~-1) in
  if b = (~-1) then None else Some (a,b) in
 
 let input = In_channel.(with_open_bin (if example then "13e.txt" else "13.txt") input_lines) |> Array.of_list in
 let group_idxs = Array.fold_left (fun (a,i) s -> (if s = "" then i::a else a), succ i) ([0],1) input |> fst |> List.rev in
 let parse idx =
  let (ax,ay) = Scanf.sscanf input.(idx)   "Button A: X+%d, Y+%d" (fun a b -> a,b) in
  let (bx,by) = Scanf.sscanf input.(idx+1) "Button B: X+%d, Y+%d" (fun a b -> a,b) in
  let (px,py) = Scanf.sscanf input.(idx+2) "Prize: X=%d, Y=%d" (fun a b -> a,b) in
  (ax,ay,bx,by,px,py) in

 List.map parse group_idxs |>
 List.filter_map solve |>
 List.fold_left (fun acc (a,b) -> acc + 3*a + b) 0

(* coordinate transformation *)
(* local module is overkill, just use a tuple *)
let problem_13b () =
 let example = false in

 (* solve by examining p in terms of bases b and b' (90 deg rotation) *)
 let solve (ax,ay,bx,by,px,py) =
  let p_crs_b = px * by - py * bx in
  let a_crs_b = ax * by - ay * bx in
  let a = if a_crs_b = 0 || p_crs_b mod a_crs_b <> 0 then 0 else p_crs_b / a_crs_b in
  let px' = px - ax * a in
  let py' = py - ay * a in
  let b = if px' mod bx = 0 && py' mod by = 0 && px' / bx = py' / by then px' / bx else (~-1) in
  if b = (~-1) then None else Some (a,b) in
 
 let input = In_channel.(with_open_bin (if example then "13e.txt" else "13.txt") input_lines) |> Array.of_list in
 let group_idxs = Array.fold_left (fun (a,i) s -> (if s = "" then i::a else a), succ i) ([0],1) input |> fst |> List.rev in
 let parse idx =
  let (ax,ay) = Scanf.sscanf input.(idx)   "Button A: X+%d, Y+%d" (fun a b -> a,b) in
  let (bx,by) = Scanf.sscanf input.(idx+1) "Button B: X+%d, Y+%d" (fun a b -> a,b) in
  let (px,py) = Scanf.sscanf input.(idx+2) "Prize: X=%d, Y=%d" (fun a b -> a+10_000_000_000_000,b+10_000_000_000_000) in
  (*let (px,py) = Scanf.sscanf input.(idx+2) "Prize: X=%d, Y=%d" (fun a b -> a,b) in*)
  (ax,ay,bx,by,px,py) in

 List.map parse group_idxs |>
 List.filter_map solve |>
 List.fold_left (fun acc (a,b) -> acc + 3*a + b) 0

(* wrappable robots *)
(* very much like the ice storm problem! *)
(* where will the robots be after 100 seconds? *)
(* answer, number of robots in each quadrant *)
let problem_14a () =
 let example = false in
 let steps = 100 in
 let pmod a b = let m = a mod b in if m < 0 then m + b else m in
 (* always odd *)
 let (map_w, map_h) = if example then (11,7) else (101,103) in
 let parse s = Scanf.sscanf s "p=%d,%d v=%d,%d" (fun a b c d -> a,b,c,d) in
 let quadrant_counters = Array.make 5 0 in
 let quadrant_of_robot (x,y,dx,dy) =
  let x' = pmod (steps * dx + x) map_w in 
  let y' = pmod (steps * dy + y) map_h in 
  if x' < map_w / 2 then
   if y' < map_h / 2 then 2 else
   if y' > map_h / 2 then 3 else 0
  else if x' > map_w / 2 then
   if y' < map_h / 2 then 1 else
   if y' > map_h / 2 then 4 else 0
  else 0 in
 let robots = In_channel.(with_open_bin (if example then "14e.txt" else "14.txt") input_lines) |> List.map parse in
 robots |>
 List.iter (fun r ->
  let q = quadrant_of_robot r in
  quadrant_counters.(q) <- quadrant_counters.(q) + 1) ;
 quadrant_counters.(1) *
 quadrant_counters.(2) *
 quadrant_counters.(3) *
 quadrant_counters.(4)

(* what is a christmas tree? *)
(* important point : wraparound value = map_h * map_w *)
(*
let problem_14b () =
 let (.%[]<-) = Bytes.set in
 let pmod a b = let m = a mod b in if m < 0 then m + b else m in
 let (map_w, map_h) = (101,103) in
 let parse s = Scanf.sscanf s "p=%d,%d v=%d,%d" (fun a b c d -> a,b,c,d) in
 let dim_string = Printf.sprintf "%d %d\n" map_w map_h in
 let write_pbm fname map =
  let oc = Out_channel.open_bin fname in
  output_string oc "P1\n" ;
  output_string oc dim_string ;
  Array.iter (fun bs -> output_bytes oc bs ; output_char oc '\n') map ;
  Out_channel.close oc in
 let robots = In_channel.(with_open_bin "14.txt" input_lines) |> List.map parse in
 let map = Array.init map_h (fun _ -> Bytes.make map_w '0') in
 let clear () = Array.iter (fun bs -> Bytes.fill bs 0 map_w '0') map in
 let yx_of_robot steps (x,y,dx,dy) =
  let x' = pmod (steps * dx + x) map_w in 
  let y' = pmod (steps * dy + y) map_h in 
  (y',x') in
 for i = 0 to map_h * map_w - 1 do 
  List.map (yx_of_robot i) robots |>
  List.iter (fun (y,x) -> map.(y).%[x] <- '1') ;
  write_pbm (Printf.sprintf "pbm/%05d.pbm" i) map ;
  clear ();
 done ;
 ()
*)

(* now that we KNOW the tree is solid, find by minimum of (horizontal) edges *)
let problem_14b () =
 let debug = false in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let pmod a b = let m = a mod b in if m < 0 then m + b else m in
 let (map_w, map_h) = (101,103) in
 let parse s = Scanf.sscanf s "p=%d,%d v=%d,%d" (fun a b c d -> a,b,c,d) in
 let dim_string = Printf.sprintf "%d %d\n" map_w map_h in
 let write_pbm fname map =
  let oc = Out_channel.open_bin fname in
  output_string oc "P1\n" ;
  output_string oc dim_string ;
  Array.iter (fun bs -> output_bytes oc bs ; output_char oc '\n') map ;
  Out_channel.close oc in
 let robots = In_channel.(with_open_bin "14.txt" input_lines) |> List.map parse in
 let map = Array.init map_h (fun _ -> Bytes.make map_w '0') in
 let clear () = Array.iter (fun bs -> Bytes.fill bs 0 map_w '0') map in
 let yx_of_robot steps (x,y,dx,dy) =
  let x' = pmod (steps * dx + x) map_w in 
  let y' = pmod (steps * dy + y) map_h in 
  (y',x') in
 let hedges_of_map map =
  let count = ref 0 in
  for y = 0 to map_h - 1 do
   for x = 0 to map_w - 2 do
    if map.(y).%[x] <> map.(y).%[x+1] then incr count else ()
   done
  done; !count in
 let lowest_entropy_i = ref 0 in
 let lowest_entropy = ref (map_h * map_w * 2) in
 for i = 0 to map_h * map_w - 1 do 
  List.map (yx_of_robot i) robots |>
  List.iter (fun (y,x) -> map.(y).%[x] <- '1') ;
  let hedges = hedges_of_map map in
  if hedges < !lowest_entropy then
  (lowest_entropy := hedges;
   lowest_entropy_i := i);
  clear ();
 done ;
 if debug then
 (List.map (yx_of_robot !lowest_entropy_i) robots |>
  List.iter (fun (y,x) -> map.(y).%[x] <- '1') ;
  write_pbm (Printf.sprintf "p14-%05d.pbm" !lowest_entropy_i) map) ;
 !lowest_entropy_i

(* "safety factor" is a rough measure of entropy, *)
(* so check for minimum "entropy" *)
(* TODO: try other metrics *)
let problem_14b2 () =
 let example = false in
 let pmod a b = let m = a mod b in if m < 0 then m + b else m in
 (* always odd *)
 let (map_w, map_h) = if example then (11,7) else (101,103) in
 let parse s = Scanf.sscanf s "p=%d,%d v=%d,%d" (fun a b c d -> a,b,c,d) in
 let quadrant_counters = Array.make 5 0 in
 let quadrant_of_robot steps (x,y,dx,dy) =
  let x' = pmod (steps * dx + x) map_w in 
  let y' = pmod (steps * dy + y) map_h in 
  if x' < map_w / 2 then
   if y' < map_h / 2 then 2 else
   if y' > map_h / 2 then 3 else 0
  else if x' > map_w / 2 then
   if y' < map_h / 2 then 1 else
   if y' > map_h / 2 then 4 else 0
  else 0 in
 let robots = In_channel.(with_open_bin (if example then "14e.txt" else "14.txt") input_lines) |> List.map parse in
 let len = List.length robots in
 let min_i = ref 0 in
 let min_ent = ref (len * len * len * len) in
 (* to ensure no issues with 0s, add 1 *)
 let entropy () =
  (quadrant_counters.(1) + 1) * 
  (quadrant_counters.(2) + 1) * 
  (quadrant_counters.(3) + 1) * 
  (quadrant_counters.(4) + 1) in
 for i = 0 to map_h * map_w - 1 do
  List.iter (fun r ->
   let q = quadrant_of_robot i r in
   quadrant_counters.(q) <- quadrant_counters.(q) + 1)
  robots ;
  let ent = entropy () in
  if ent < !min_ent then (min_ent := ent ; min_i := i) ;
  Array.fill quadrant_counters 0 5 0 ;
 done ;
 !min_i

let problem_15a () =
 let debug = true in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let example = false in
 let input = In_channel.(with_open_bin (if example then "15e.txt" else "15.txt") input_lines) |> Array.of_list in
 let map_h = input |> Array.fold_left (fun (a,i) s -> (if s = "" then i else a), succ i) (0,0) |> fst in
 (*let map_w = String.length input.(0) in*)
 let move_w = String.length input.(map_h+1) in
 let move_h = Array.length input - map_h - 1 in
 let move_len = move_w * move_h in
 let get_move i = input.(map_h + 1 + i / move_w).[i mod move_w] in
 let map = Array.init map_h (fun i -> Bytes.unsafe_of_string input.(i)) in
 let rec can_move (y,x) = function
 | '^' when map.(y-1).%[x] = '.' -> true
 | '^' when map.(y-1).%[x] = '#' -> false
 | '^' when map.(y-1).%[x] = 'O' -> can_move (y-1,x) '^'
 | '>' when map.(y).%[x+1] = '.' -> true
 | '>' when map.(y).%[x+1] = '#' -> false
 | '>' when map.(y).%[x+1] = 'O' -> can_move (y,x+1) '>'
 | 'v' when map.(y+1).%[x] = '.' -> true
 | 'v' when map.(y+1).%[x] = '#' -> false
 | 'v' when map.(y+1).%[x] = 'O' -> can_move (y+1,x) 'v'
 | '<' when map.(y).%[x-1] = '.' -> true
 | '<' when map.(y).%[x-1] = '#' -> false
 | '<' when map.(y).%[x-1] = 'O' -> can_move (y,x-1) '<'
 | _ -> assert false in
 (* move requires you to "pick up" your @ piece before running *)
 let rec move c (y,x) = function
 | '^' when map.(y-1).%[x] = '.' -> map.(y-1).%[x] <- c
 | '^' when map.(y-1).%[x] = 'O' -> map.(y-1).%[x] <- c ; move 'O' (y-1,x) '^'
 | '>' when map.(y).%[x+1] = '.' -> map.(y).%[x+1] <- c
 | '>' when map.(y).%[x+1] = 'O' -> map.(y).%[x+1] <- c ; move 'O' (y,x+1) '>'
 | 'v' when map.(y+1).%[x] = '.' -> map.(y+1).%[x] <- c
 | 'v' when map.(y+1).%[x] = 'O' -> map.(y+1).%[x] <- c ; move 'O' (y+1,x) 'v'
 | '<' when map.(y).%[x-1] = '.' -> map.(y).%[x-1] <- c
 | '<' when map.(y).%[x-1] = 'O' -> map.(y).%[x-1] <- c ; move 'O' (y,x-1) '<'
 | _ -> assert false in
 let update_robot r = function
 | '^' -> let (y,x) = !r in r := (y-1,x)
 | '>' -> let (y,x) = !r in r := (y,x+1)
 | 'v' -> let (y,x) = !r in r := (y+1,x)
 | '<' -> let (y,x) = !r in r := (y,x-1)
 | _ -> assert false in
 let print_map () =
  for y = 0 to map_h - 1 do
   print_bytes map.(y) ;
   print_newline ()
  done in
 let sum_gps () =
  Array.fold_left (fun (a,y) bs ->
   Bytes.fold_left (fun (a,x) c ->
    (if c = 'O' then a+x+100*y else a), succ x)
    (a,0) bs |> fst, succ y)
   (0,0) map |> fst in
 let robot =
  Array.fold_left (fun (yx,y) bs ->
   if Option.is_none yx then
    let yx' =
     Bytes.index_opt bs '@' |>
     Option.map (fun x -> (y,x)) in
    (yx', succ y)
   else (yx,succ y)) (None, 0) map |>
  fst |>
  Option.get |>
  ref in
 for i = 0 to move_len - 1 do
  let m = get_move i in
  if can_move !robot m then
  ( let (y,x) = !robot in
    map.(y).%[x] <- '.' ;
    move '@' !robot m ;
    update_robot robot m ) ;
 done ;
 if debug then print_map () ;
 sum_gps ()

let problem_15b () =
 let debug = true in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let example = false in
 let input = In_channel.(with_open_bin (if example then "15e.txt" else "15.txt") input_lines) |> Array.of_list in
 let map_h = input |> Array.fold_left (fun (a,i) s -> (if s = "" then i else a), succ i) (0,0) |> fst in
 let map_w = String.length input.(0) in
 let move_w = String.length input.(map_h+1) in
 let move_h = Array.length input - map_h - 1 in
 let move_len = move_w * move_h in
 let get_move i = input.(map_h + 1 + i / move_w).[i mod move_w] in
 let map =
  Array.init map_h (fun y ->
   Bytes.init (map_w * 2) (fun x ->
    if x land 1 = 0 then
     (match input.(y).[x/2] with
     | 'O' -> '['
     | c -> c)
    else
     (match input.(y).[x/2] with
     | 'O' -> ']'
     | '@' -> '.'
     | c -> c))) in
 let rec can_move (y,x) = function
 | '^' when map.(y-1).%[x] = '.' -> true
 | '^' when map.(y-1).%[x] = '#' -> false
 | '^' when map.(y-1).%[x] = '[' -> can_move (y-1,x) '^' && can_move (y-1,x+1) '^'
 | '^' when map.(y-1).%[x] = ']' -> can_move (y-1,x) '^' && can_move (y-1,x-1) '^'
 | '>' when map.(y).%[x+1] = '.' -> true
 | '>' when map.(y).%[x+1] = '#' -> false
 | '>' when map.(y).%[x+1] = '[' -> can_move (y,x+1) '>'
 | '>' when map.(y).%[x+1] = ']' -> can_move (y,x+1) '>'
 | 'v' when map.(y+1).%[x] = '.' -> true
 | 'v' when map.(y+1).%[x] = '#' -> false
 | 'v' when map.(y+1).%[x] = '[' -> can_move (y+1,x) 'v' && can_move (y+1,x+1) 'v'
 | 'v' when map.(y+1).%[x] = ']' -> can_move (y+1,x) 'v' && can_move (y+1,x-1) 'v'
 | '<' when map.(y).%[x-1] = '.' -> true
 | '<' when map.(y).%[x-1] = '#' -> false
 | '<' when map.(y).%[x-1] = '[' -> can_move (y,x-1) '<'
 | '<' when map.(y).%[x-1] = ']' -> can_move (y,x-1) '<'
 | _ -> assert false in
 (* move requires you to "pick up" your @ piece before running *)
 let rec move c (y,x) = function
 | '^' when map.(y-1).%[x] = '.' -> map.(y-1).%[x] <- c
 | '^' when map.(y-1).%[x] = '[' -> map.(y-1).%[x] <- c ; move '[' (y-1,x) '^' ; map.(y-1).%[x+1] <- '.' ; move ']' (y-1,x+1) '^'
 | '^' when map.(y-1).%[x] = ']' -> map.(y-1).%[x] <- c ; move ']' (y-1,x) '^' ; map.(y-1).%[x-1] <- '.' ; move '[' (y-1,x-1) '^'
 | '>' when map.(y).%[x+1] = '.' -> map.(y).%[x+1] <- c
 | '>' when map.(y).%[x+1] = '[' -> map.(y).%[x+1] <- c ; move '[' (y,x+1) '>'
 | '>' when map.(y).%[x+1] = ']' -> map.(y).%[x+1] <- c ; move ']' (y,x+1) '>'
 | 'v' when map.(y+1).%[x] = '.' -> map.(y+1).%[x] <- c
 | 'v' when map.(y+1).%[x] = '[' -> map.(y+1).%[x] <- c ; move '[' (y+1,x) 'v'; map.(y+1).%[x+1] <- '.' ; move ']' (y+1,x+1) 'v'
 | 'v' when map.(y+1).%[x] = ']' -> map.(y+1).%[x] <- c ; move ']' (y+1,x) 'v'; map.(y+1).%[x-1] <- '.' ; move '[' (y+1,x-1) 'v'
 | '<' when map.(y).%[x-1] = '.' -> map.(y).%[x-1] <- c
 | '<' when map.(y).%[x-1] = '[' -> map.(y).%[x-1] <- c ; move '[' (y,x-1) '<'
 | '<' when map.(y).%[x-1] = ']' -> map.(y).%[x-1] <- c ; move ']' (y,x-1) '<'
 | _ -> assert false in
 let update_robot r = function
 | '^' -> let (y,x) = !r in r := (y-1,x)
 | '>' -> let (y,x) = !r in r := (y,x+1)
 | 'v' -> let (y,x) = !r in r := (y+1,x)
 | '<' -> let (y,x) = !r in r := (y,x-1)
 | _ -> assert false in
 let print_map () =
  for y = 0 to map_h - 1 do
   print_bytes map.(y) ;
   print_newline ()
  done in
 let sum_gps () =
  Array.fold_left (fun (a,y) bs ->
   Bytes.fold_left (fun (a,x) c ->
    (if c = '[' then a+x+100*y else a), succ x)
    (a,0) bs |> fst, succ y)
   (0,0) map |> fst in
 let robot =
  Array.fold_left (fun (yx,y) bs ->
   if Option.is_none yx then
    let yx' =
     Bytes.index_opt bs '@' |>
     Option.map (fun x -> (y,x)) in
    (yx', succ y)
   else (yx,succ y)) (None, 0) map |>
  fst |>
  Option.get |>
  ref in
 for i = 0 to move_len - 1 do
  let m = get_move i in
  if can_move !robot m then
  ( let (y,x) = !robot in
    map.(y).%[x] <- '.' ;
    move '@' !robot m ;
    update_robot robot m ) ;
 done ;
 if debug then print_map () ;
 sum_gps ()

(* maze walk, compete for lowest score *)
(* S = start, E = end *)
(* moving increases score by 1 point, rotating by 1000 *)
(* all paths start facing East *)
(* use djikstra's algorithm *)
let problem_16a () =
 let example = false in
 let module YXD = struct
  type dir = N | S | W | E
  type t = {y : int ; x : int ; d : dir; cost : int}
  let rotate_cw c =
   match c.d with
   | N -> {c with d = E; cost = c.cost + 1000}
   | E -> {c with d = S; cost = c.cost + 1000}
   | S -> {c with d = W; cost = c.cost + 1000}
   | W -> {c with d = N; cost = c.cost + 1000}
  let rotate_ccw c =
   match c.d with
   | N -> {c with d = W; cost = c.cost + 1000}
   | W -> {c with d = S; cost = c.cost + 1000}
   | S -> {c with d = E; cost = c.cost + 1000}
   | E -> {c with d = N; cost = c.cost + 1000}
  let move c =
   match c.d with
   | N -> {c with y = c.y - 1; cost = c.cost + 1}
   | S -> {c with y = c.y + 1; cost = c.cost + 1}
   | W -> {c with x = c.x - 1; cost = c.cost + 1}
   | E -> {c with x = c.x + 1; cost = c.cost + 1}
  let can_move map c =
   match c.d with
   | N -> map.(c.y-1).[c.x] <> '#'
   | S -> map.(c.y+1).[c.x] <> '#'
   | W -> map.(c.y).[c.x-1] <> '#'
   | E -> map.(c.y).[c.x+1] <> '#'
 end in
 let map = In_channel.(with_open_bin (if example then "16e.txt" else "16.txt") input_lines) |> Array.of_list in
 let seen = Hashtbl.create 2048 in
 let moves = Queue.create () in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in
 let start_yx = (map_h - 2,1) in
 let end_yx = (1, map_w - 2) in
 Queue.add YXD.{y = fst start_yx; x = snd start_yx ; d = E; cost = 0} moves ;
 while not @@ Queue.is_empty moves do
  let yxd = Queue.take moves in
  match Hashtbl.find_opt seen (yxd.y,yxd.x,yxd.d) with
  | None ->
    Hashtbl.add seen (yxd.y,yxd.x,yxd.d) yxd.cost;
    if YXD.can_move map yxd then Queue.add YXD.(move yxd) moves;
    Queue.add YXD.(rotate_cw yxd) moves ;
    Queue.add YXD.(rotate_ccw yxd) moves
  | Some c when c > yxd.cost ->
    Hashtbl.replace seen (yxd.y,yxd.x,yxd.d) yxd.cost ;
    if YXD.can_move map yxd then Queue.add YXD.(move yxd) moves;
    Queue.add YXD.(rotate_cw yxd) moves ;
    Queue.add YXD.(rotate_ccw yxd) moves
  | Some c -> ()
 done ;
 YXD.[N;S;E;W] |>
 List.map (fun d -> (fst end_yx, snd end_yx, d)) |>
 List.filter_map (Hashtbl.find_opt seen) |>
 List.fold_left min Int.max_int

let problem_16b () =
 let example = false in
 let module Dir = struct type t = N | S | W | E end in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module YXDSet = Set.Make(struct type t = int * int * Dir.t let compare = compare end) in
 let module YXD = struct
  include Dir
  type t = {y : int ; x : int ; d : Dir.t; cost : int; history : YXDSet.t}
  let rotate_cw c =
   match c.d with
   | N -> {c with d = E; cost = c.cost + 1000}
   | E -> {c with d = S; cost = c.cost + 1000}
   | S -> {c with d = W; cost = c.cost + 1000}
   | W -> {c with d = N; cost = c.cost + 1000}
  let rotate_ccw c =
   match c.d with
   | N -> {c with d = W; cost = c.cost + 1000}
   | W -> {c with d = S; cost = c.cost + 1000}
   | S -> {c with d = E; cost = c.cost + 1000}
   | E -> {c with d = N; cost = c.cost + 1000}
  let move c =
   match c.d with
   | N -> {c with y = c.y - 1; cost = c.cost + 1; history = YXDSet.add (c.y-1,c.x,c.d) c.history}
   | S -> {c with y = c.y + 1; cost = c.cost + 1; history = YXDSet.add (c.y+1,c.x,c.d) c.history}
   | W -> {c with x = c.x - 1; cost = c.cost + 1; history = YXDSet.add (c.y,c.x-1,c.d) c.history}
   | E -> {c with x = c.x + 1; cost = c.cost + 1; history = YXDSet.add (c.y,c.x+1,c.d) c.history}
  let can_move map c =
   match c.d with
   | N -> map.(c.y-1).[c.x] <> '#'
   | S -> map.(c.y+1).[c.x] <> '#'
   | W -> map.(c.y).[c.x-1] <> '#'
   | E -> map.(c.y).[c.x+1] <> '#'
 end in
 let map = In_channel.(with_open_bin (if example then "16e.txt" else "16.txt") input_lines) |> Array.of_list in
 let seen = Hashtbl.create 2048 in
 let moves = Queue.create () in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in
 let start_yx = (map_h - 2,1) in
 let end_yx = (1, map_w - 2) in
 let optimal = ref Int.max_int in
 Queue.add YXD.{y = fst start_yx; x = snd start_yx ; d = E; cost = 0; history = YXDSet.singleton (map_h - 2,1,Dir.E)} moves ;
 while not @@ Queue.is_empty moves do
  let yxd = Queue.take moves in
  if (yxd.y,yxd.x) = end_yx then optimal := min !optimal yxd.cost ;
  if yxd.cost > !optimal then () else
  (match Hashtbl.find_opt seen (yxd.y,yxd.x,yxd.d) with
  | None ->
    Hashtbl.add seen (yxd.y,yxd.x,yxd.d) (yxd.cost, yxd.history);
    if YXD.can_move map yxd then Queue.add YXD.(move yxd) moves;
    Queue.add YXD.(rotate_cw yxd) moves ;
    Queue.add YXD.(rotate_ccw yxd) moves
  | Some (c,_) when c > yxd.cost ->
    Hashtbl.replace seen (yxd.y,yxd.x,yxd.d) (yxd.cost, yxd.history) ;
    if YXD.can_move map yxd then Queue.add YXD.(move yxd) moves;
    Queue.add YXD.(rotate_cw yxd) moves ;
    Queue.add YXD.(rotate_ccw yxd) moves
  | Some (c,h) when c = yxd.cost ->
    Hashtbl.replace seen (yxd.y,yxd.x,yxd.d) (yxd.cost, YXDSet.union h yxd.history) ;
    (* force at least one move to update the history (and merge), because rotating doesn't *)
    (* the logic here is odd, but sound *)
    if YXD.can_move map yxd then Queue.add YXD.(move yxd) moves; ()
    (* removing these two lines saves a *lot* of time *)
    (* Queue.add YXD.(rotate_cw yxd) moves ; Queue.add YXD.(rotate_ccw yxd) moves *)
  | Some _ -> ())
 done ;
 let lowest_score =
  Dir.[N;S;E;W] |>
  List.map (fun d -> (fst end_yx, snd end_yx, d)) |>
  List.filter_map (Hashtbl.find_opt seen) |>
  List.fold_left (fun a (c,_) -> min a c) Int.max_int in
 (* all paths that made it to the end *)
 let set0 =
  Dir.[N;S;E;W] |>
  List.map (fun d -> (fst end_yx, snd end_yx, d)) |>
  List.filter_map (Hashtbl.find_opt seen) |>
  List.filter (fun (cost,_) -> cost = lowest_score) |>
  List.fold_left (fun a (_,history) -> YXDSet.union a history) YXDSet.empty in
 (* walk back over the path to add in all equal sub-paths *)
 set0 |>
 YXDSet.to_seq |>
 Seq.map (fun yxd -> Hashtbl.find seen yxd |> snd) |>
 Seq.fold_left YXDSet.union YXDSet.empty |>
 YXDSet.to_seq |>
 Seq.map (fun (y,x,d) -> (y,x)) |>
 YXSet.of_seq |> YXSet.cardinal

(* TODO: use priority queue for a (slight) speed up *)
let problem_16b2 () =
 let example = false in
 let module Dir = struct
  type t = N | S | W | E
  let int_of_dir : (t -> int) = Obj.magic
 end in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module YXDSet = Set.Make(struct type t = int * int * Dir.t let compare = compare end) in
 let module YXD = struct
  include Dir
  type t = {y : int ; x : int ; d : Dir.t; cost : int; history : YXDSet.t}
  let rotate_cw c =
   match c.d with
   | N -> {c with d = E; cost = c.cost + 1000}
   | E -> {c with d = S; cost = c.cost + 1000}
   | S -> {c with d = W; cost = c.cost + 1000}
   | W -> {c with d = N; cost = c.cost + 1000}
  let rotate_ccw c =
   match c.d with
   | N -> {c with d = W; cost = c.cost + 1000}
   | W -> {c with d = S; cost = c.cost + 1000}
   | S -> {c with d = E; cost = c.cost + 1000}
   | E -> {c with d = N; cost = c.cost + 1000}
  let move c =
   match c.d with
   | N -> {c with y = c.y - 1; cost = c.cost + 1; history = YXDSet.add (c.y-1,c.x,c.d) c.history}
   | S -> {c with y = c.y + 1; cost = c.cost + 1; history = YXDSet.add (c.y+1,c.x,c.d) c.history}
   | W -> {c with x = c.x - 1; cost = c.cost + 1; history = YXDSet.add (c.y,c.x-1,c.d) c.history}
   | E -> {c with x = c.x + 1; cost = c.cost + 1; history = YXDSet.add (c.y,c.x+1,c.d) c.history}
  let can_move map c =
   match c.d with
   | N -> map.(c.y-1).[c.x] <> '#'
   | S -> map.(c.y+1).[c.x] <> '#'
   | W -> map.(c.y).[c.x-1] <> '#'
   | E -> map.(c.y).[c.x+1] <> '#'
 end in
 let map = In_channel.(with_open_bin (if example then "16e.txt" else "16.txt") input_lines) |> Array.of_list in
 let moves = Queue.create () in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in
 let seen = Array.make (map_h * map_w * 4) (Int.max_int, YXDSet.empty) in
 let seen_get_tup (y,x,d) = seen.(y*map_w*4+x*4+(Dir.int_of_dir d)) in
 let seen_get (yxd : YXD.t) = seen.(yxd.y*map_w*4+yxd.x*4+(Dir.int_of_dir yxd.d)) in
 let seen_set (yxd : YXD.t) = seen.(yxd.y*map_w*4+yxd.x*4+(Dir.int_of_dir yxd.d)) <- (yxd.cost, yxd.history) in
 let seen_merge (yxd : YXD.t) =
  let (c,h) = seen.(yxd.y*map_w*4+yxd.x*4+(Dir.int_of_dir yxd.d)) in
  seen.(yxd.y*map_w*4+yxd.x*4+(Dir.int_of_dir yxd.d)) <- (c, YXDSet.union h yxd.history) in
 let start_yx = (map_h - 2,1) in
 let end_yx = (1, map_w - 2) in
 let optimal = ref Int.max_int in
 Queue.add YXD.{y = fst start_yx; x = snd start_yx ; d = E; cost = 0; history = YXDSet.singleton (map_h - 2,1,Dir.E)} moves ;
 while not @@ Queue.is_empty moves do
  let yxd = Queue.take moves in
  if (yxd.y,yxd.x) = end_yx then optimal := min !optimal yxd.cost ;
  if yxd.cost > !optimal then () else
  (match seen_get yxd with
  | (c,_) when c > yxd.cost ->
    seen_set yxd;
    if YXD.can_move map yxd then Queue.add YXD.(move yxd) moves;
    Queue.add YXD.(rotate_cw yxd) moves ;
    Queue.add YXD.(rotate_ccw yxd) moves
  | (c,h) when c = yxd.cost ->
    seen_merge yxd;
    (* force at least one move to update the history (and merge), because rotating doesn't *)
    (* the logic here is odd, but sound *)
    if YXD.can_move map yxd then Queue.add YXD.(move yxd) moves; ()
    (* removing these two lines saves a *lot* of time *)
    (* Queue.add YXD.(rotate_cw yxd) moves ; Queue.add YXD.(rotate_ccw yxd) moves *)
  | _ -> ())
 done ;
 let lowest_score =
  Dir.[N;S;E;W] |>
  List.map (fun d -> (fst end_yx, snd end_yx, d)) |>
  List.map seen_get_tup |>
  List.fold_left (fun a (c,_) -> min a c) Int.max_int in
 (* all paths that made it to the end *)
 let set0 =
  Dir.[N;S;E;W] |>
  List.map (fun d -> (fst end_yx, snd end_yx, d)) |>
  List.map seen_get_tup |>
  List.filter (fun (cost,_) -> cost = lowest_score) |>
  List.fold_left (fun a (_,history) -> YXDSet.union a history) YXDSet.empty in
 (* walk back over the path to add in all equal sub-paths *)
 set0 |>
 YXDSet.to_seq |>
 Seq.map (fun yxd_tup -> seen_get_tup yxd_tup |> snd) |>
 Seq.fold_left YXDSet.union YXDSet.empty |>
 YXDSet.to_seq |>
 Seq.map (fun (y,x,d) -> (y,x)) |>
 YXSet.of_seq |> YXSet.cardinal

(* 3-bit computer *)
(* instr len = 6-bits *)
(* ip increases by 2 per instruction *)
(* halts at invalid address *)
(* both literal and combo operands; lit : 0-7, combo 0-3 (lit, 4-6 = reg(a,b,c)) *)
(* OPCODES
 * 0 - ADV (Division) A / 2^(combo) -> A
 * 1 - BXL (Bitwise XOR) B lxor (lit) -> B
 * 2 - BST (Store B) (combo) mod 8 -> B
 * 3 - JNZ (Jump NZ) (lit) -> ip
 * 4 - BXC (B XOR C) B lxor C -> B
 * 5 - OUT (Output) (combo) -> stdout
 * 6 - BDV (Division) A / 2^(combo) -> B
 * 7 - CDV (Division) A / 2^(combo) -> C
 *)
let problem_17a () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "17e.txt" else "17.txt") input_lines) |> Array.of_list in
 let registers = Array.make 3 0 in
 for i = 0 to 2 do
  (* "Register %c: %d" *)
  String.sub input.(i) 12 (String.length input.(i) - 12) |> int_of_string |> Array.set registers i
 done ;
  (* "Program: %d,%d,..." *)
 let program = String.split_on_char ',' (String.sub input.(4) 9 (String.length input.(4) - 9)) |> List.map int_of_string |> Array.of_list in
 let ip = ref 0 in
 let literal_of_combo = function
 | n when n >= 0 && n <= 3 -> n
 | idx when idx >= 4 && idx <= 6 -> registers.(idx-4)
 | _ -> failwith "Invalid Combo Operand!" in
 let out = Queue.create () in
 let run () =
  try
   while true do
    match program.(!ip), program.(!ip+1) with
    | 0, c -> let n = literal_of_combo c in registers.(0) <- registers.(0) / (1 lsl n) ; ip := !ip + 2
    | 1, n -> registers.(1) <- registers.(1) lxor n ; ip := !ip + 2
    | 2, c -> registers.(1) <- (literal_of_combo c) land 7 ; ip := !ip + 2
    | 3, n -> if registers.(0) <> 0 then ip := n else ip := !ip + 2
    | 4, _ -> registers.(1) <- registers.(1) lxor registers.(2) ; ip := !ip + 2
    | 5, c -> Queue.add (literal_of_combo c land 7) out; ip := !ip + 2
    | 6, c -> let n = literal_of_combo c in registers.(1) <- registers.(0) / (1 lsl n); ip := !ip + 2
    | 7, c -> let n = literal_of_combo c in registers.(2) <- registers.(0) / (1 lsl n); ip := !ip + 2
    | _ -> failwith "Invalid Opcode!"
   done
  with Invalid_argument _ -> () in
 run () ;
 let outbuf = Buffer.create (Queue.length out * 4) in
 while not @@ Queue.is_empty out do
  Buffer.add_string outbuf (string_of_int @@ Queue.take out);
  Buffer.add_char outbuf ','
 done ;
 Buffer.truncate outbuf (Buffer.length outbuf - 1) ;
 print_endline (Buffer.contents outbuf)

let problem_17b () =
 let example = false in
 let debug = true in
 let input = In_channel.(with_open_bin (if example then "17e2.txt" else "17.txt") input_lines) |> Array.of_list in
 let registers = Array.make 3 0 in
 for i = 0 to 2 do
  (* "Register %c: %d" *)
  String.sub input.(i) 12 (String.length input.(i) - 12) |> int_of_string |> Array.set registers i
 done ;
 let b0 = registers.(1) in
 let c0 = registers.(2) in
  (* "Program: %d,%d,..." *)
 let program = String.split_on_char ',' (String.sub input.(4) 9 (String.length input.(4) - 9)) |> List.map int_of_string |> Array.of_list in
 let ip = ref 0 in
 let literal_of_combo = function
 | n when n >= 0 && n <= 3 -> n
 | idx when idx >= 4 && idx <= 6 -> registers.(idx-4)
 | _ -> failwith "Invalid Combo Operand!" in
 let out = Queue.create () in
 let counter = ref 0 in
 let inf_protect = 1000 in
 let run_with a0 =
  registers.(0) <- a0 ;
  registers.(1) <- b0 ;
  registers.(2) <- c0 ;
  Queue.clear out ;
  ip := 0 ;
  counter := 0 ;
  try
   while true do
    incr counter;
    if !counter > inf_protect then (Queue.clear out ; raise_notrace (Invalid_argument "INFLOOP")) ;
    if Queue.length out > Array.length program then (Queue.clear out ; raise_notrace (Invalid_argument "KILL")) ;
    match program.(!ip), program.(!ip+1) with
    | 0, c -> let n = literal_of_combo c in registers.(0) <- registers.(0) / (1 lsl n) ; ip := !ip + 2
    | 1, n -> registers.(1) <- registers.(1) lxor n ; ip := !ip + 2
    | 2, c -> registers.(1) <- (literal_of_combo c) land 7 ; ip := !ip + 2
    | 3, n -> if registers.(0) <> 0 then ip := n else ip := !ip + 2
    | 4, _ -> registers.(1) <- registers.(1) lxor registers.(2) ; ip := !ip + 2
    | 5, c -> Queue.add (literal_of_combo c land 7) out; ip := !ip + 2
    | 6, c -> let n = literal_of_combo c in registers.(1) <- registers.(0) / (1 lsl n); ip := !ip + 2
    | 7, c -> let n = literal_of_combo c in registers.(2) <- registers.(0) / (1 lsl n); ip := !ip + 2
    | _ -> failwith "Invalid Opcode!"
   done
  with Invalid_argument _ -> () in
 
 let print_out () =
  let outbuf = Buffer.create (Queue.length out * 4) in
  while not @@ Queue.is_empty out do
   Buffer.add_string outbuf (string_of_int @@ Queue.take out);
   Buffer.add_char outbuf ','
  done ;
  Buffer.truncate outbuf (Buffer.length outbuf - 1) ;
  print_endline (Buffer.contents outbuf) in

 let rec find matching prefix i =
  run_with (prefix+i) ;
  if matching = (Queue.to_seq out |> Array.of_seq) then prefix+i else
  find matching prefix (i+1) in
  
  (* find solutions in 3-bit chunks via right folding *)
  let rec find_a start =
   if start = Array.length program then 0 else
   find (Array.sub program start (Array.length program - start)) (find_a (start+1) lsl 3) 0 in

  let a0 = find_a 0 in
  if debug then (run_with a0 ; print_out ()) else () ;
  a0

(* pathfinding : simulate start at first 1024 bytes *)
(* bfs works because path cost is the same *)
let problem_18a () =
 let example = false in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let input =
  In_channel.(with_open_bin (if example then "18e.txt" else "18.txt") input_lines) |>
  List.map
   (fun s -> s |> String.split_on_char ',' |>
    (function
     | n1::n2::[] -> int_of_string n2, int_of_string n1 (* swap x,y to y,x for consistency *)
     | _ -> raise_notrace (Invalid_argument "Cannot Parse Tuple!"))) |>
  Array.of_list in
 let (map_h,map_w) = if example then (7,7) else (71,71) in
 let map = Array.init map_h (fun _ -> Bytes.make map_w '.') in
 let cost = Array.init map_h (fun _ -> Array.make map_w ~-1) in
 let start_yx = (0,0) in
 let end_yx = (map_h-1,map_w-1) in
 let byte_len = if example then 12 else 1024 in
 for i = 0 to (min (Array.length input) byte_len) - 1 do
  let (y,x) = input.(i) in
  map.(y).%[x] <- '#' ;
  cost.(y).(x) <- 0
 done ;
 let queue = Queue.create () in
 Queue.add (fst start_yx, snd start_yx,0) queue ;
 let rec loop () =
  if Queue.is_empty queue then None else
  let (y,x,c) = Queue.take queue in
  if (y,x) = end_yx then Some c else
  match map.(y).%[x], cost.(y).(x) with
  | '.', n when n < 0 ->
    cost.(y).(x) <- c;
    Queue.add (y-1,x,c+1) queue;
    Queue.add (y+1,x,c+1) queue;
    Queue.add (y,x-1,c+1) queue;
    Queue.add (y,x+1,c+1) queue;
    loop ()
  | _ -> loop ()
  | exception Invalid_argument _ -> loop ()
 in
 loop () |> Option.get

let problem_18b () =
 let example = false in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let input =
  In_channel.(with_open_bin (if example then "18e.txt" else "18.txt") input_lines) |>
  List.map
   (fun s -> s |> String.split_on_char ',' |>
    (function
     | n1::n2::[] -> int_of_string n2, int_of_string n1 (* swap x,y to y,x for consistency *)
     | _ -> raise_notrace (Invalid_argument "Cannot Parse Tuple!"))) |>
  Array.of_list in
 let (map_h,map_w) = if example then (7,7) else (71,71) in
 let map = Array.init map_h (fun _ -> Bytes.make map_w '.') in
 (* could just use bool/visited instead, but this is extra information at no cost *)
 let cost = Array.init map_h (fun _ -> Array.make map_w ~-1) in
 let start_yx = (0,0) in
 let end_yx = (map_h-1,map_w-1) in
 let queue = Queue.create () in
 (* we know we can start here, because it was valid in 18a *)
 let byte_len0 = if example then 12 else 1024 in

 let clear () =
  for i = 0 to map_h - 1 do
   Bytes.fill map.(i) 0 map_w '.';
   Array.fill cost.(i) 0 map_w ~-1 ;
   Queue.clear queue 
  done in

 let run byte_len =
  clear () ;
  for i = 0 to (min (Array.length input) byte_len) - 1 do
   let (y,x) = input.(i) in
   map.(y).%[x] <- '#' ;
   cost.(y).(x) <- 0
  done ;
  Queue.add (fst start_yx, snd start_yx,0) queue ;
  let rec loop () =
   if Queue.is_empty queue then None else
   let (y,x,c) = Queue.take queue in
   if (y,x) = end_yx then Some c else
   match map.(y).%[x], cost.(y).(x) with
   | '.', n when n < 0 ->
     cost.(y).(x) <- c;
     Queue.add (y-1,x,c+1) queue;
     Queue.add (y+1,x,c+1) queue;
     Queue.add (y,x-1,c+1) queue;
     Queue.add (y,x+1,c+1) queue;
     loop ()
   | _ -> loop ()
   | exception Invalid_argument _ -> loop () in
  loop () in

 (* for the final case (after start_idx = end_idx), start_idx will always point at the first None! *)
 let rec loop_bin_search start_idx end_idx =
 if end_idx < start_idx then start_idx else
 let idx = start_idx + (end_idx - start_idx) lsr 1 in
 if Option.is_none (run idx)
 then loop_bin_search start_idx (idx - 1)
 else loop_bin_search (idx + 1) end_idx in

 let byte_len = loop_bin_search (byte_len0+1) (Array.length input - 1) in

(*
 let rec loop idx = if Option.is_none (run idx) then idx else loop (idx+1) in
 let byte_len = loop (byte_len0+1) in
*)

 input.(byte_len-1) |>
 (fun (y,x) -> (x,y))

(* colors w (white), u (blue), b (black), r (red), g (green) *)
let problem_19a () =
 let module ColorTree = struct
  type node = Leaf | Stem of char * node list
  (*type root = node list*)
  let rec of_string_idx s i =
   if i = String.length s then Leaf else
   Stem (s.[i], [of_string_idx s (i+1)])
  let of_string s = of_string_idx s 0
  let rec merge (root : node list) s i =
   if i > String.length s then root else
   let next =
    if i = String.length s then List.find_opt (function Leaf -> true | _ -> false) root
    else List.find_opt (function Stem (c,_) when c = s.[i] -> true | _ -> false) root in
   match next with
   | None -> (of_string_idx s i) :: root
   | Some (Stem (c,tl)) -> Stem(c,(merge tl s (i+1))) :: (List.filter (function Stem (c',_) when c = c' -> false | _ -> true) root)
   | Some (Leaf) -> root
  let rec mem_idx (top : node list) (root : node list) s i =
   let term =
    List.find_opt (function Leaf -> true | _ -> false) root in
   let next =
    if i = String.length s then term
    else List.find_opt (function Stem (c,_) when c = s.[i] -> true | _ -> false) root in
   match next, term with
   | Some (Leaf), _ -> true
   | None, None -> false
   | None, Some (Leaf) -> mem_idx top top s i
   | Some (Stem (c,tl)), None -> mem_idx top tl s (i+1)
   | Some (Stem (c,tl)), Some (Leaf) -> (mem_idx top tl s (i+1)) || mem_idx top top s i
   | _ -> assert false
  let mem top s = mem_idx top top s 0
 end in
 let example = false in
 let input = In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_lines) |> Array.of_list in
 let towel_set =
  input.(0) |>
  String.split_on_char ',' |>
  List.map (String.trim) |>
  List.fold_left (fun a s -> ColorTree.merge a s 0) [] in
 Array.to_seq input |>
 Seq.drop 2 |>
 Seq.filter (ColorTree.mem towel_set) |>
 Seq.length

(* colors w (white), u (blue), b (black), r (red), g (green) *)
let problem_19b () =
 let module ColorTree = struct
  type node = Leaf | Stem of char * node list
  (*type root = node list*)
  let memo = Hashtbl.create 4096
  let rec of_string_idx s i =
   if i = String.length s then Leaf else
   Stem (s.[i], [of_string_idx s (i+1)])
  let of_string s = of_string_idx s 0
  let rec merge (root : node list) s i =
   if i > String.length s then root else
   let next =
    if i = String.length s then List.find_opt (function Leaf -> true | _ -> false) root
    else List.find_opt (function Stem (c,_) when c = s.[i] -> true | _ -> false) root in
   match next with
   | None -> (of_string_idx s i) :: root
   | Some (Stem (c,tl)) -> Stem(c,(merge tl s (i+1))) :: (List.filter (function Stem (c',_) when c = c' -> false | _ -> true) root)
   | Some (Leaf) -> root
  let rec mem_idx (top : node list) (root : node list) s i =
   let term =
    List.find_opt (function Leaf -> true | _ -> false) root in
   let next =
    if i = String.length s then term
    else List.find_opt (function Stem (c,_) when c = s.[i] -> true | _ -> false) root in
   match next, term with
   | Some (Leaf), _ -> true
   | None, None -> false
   | None, Some (Leaf) -> mem_idx top top s i
   | Some (Stem (c,tl)), None -> mem_idx top tl s (i+1)
   | Some (Stem (c,tl)), Some (Leaf) -> (mem_idx top tl s (i+1)) || mem_idx top top s i
   | _ -> assert false
  let mem top s = mem_idx top top s 0
  let rec mem_n_idx (top : node list) (root : node list) s i =
   match Hashtbl.find_opt memo (root,(String.sub s i (String.length s - i))) with
   | Some n -> n
   | None ->
     let res =
      begin
      let term =
       List.find_opt (function Leaf -> true | _ -> false) root in
      let next =
       if i = String.length s then term
       else List.find_opt (function Stem (c,_) when c = s.[i] -> true | _ -> false) root in
      match next, term with
      | Some (Leaf), _ -> 1
      | None, None -> 0 
      | None, Some (Leaf) -> mem_n_idx top top s i
      | Some (Stem (c,tl)), None -> mem_n_idx top tl s (i+1)
      | Some (Stem (c,tl)), Some (Leaf) -> (mem_n_idx top tl s (i+1)) + (mem_n_idx top top s i)
      | _ -> assert false
      end in
      Hashtbl.add memo (root,String.sub s i (String.length s - i)) res ; res
  let mem_n top s = mem_n_idx top top s 0
 end in
 let example = false in
 let input = In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_lines) |> Array.of_list in
 let towel_set =
  input.(0) |>
  String.split_on_char ',' |>
  List.map (String.trim) |>
  List.fold_left (fun a s -> ColorTree.merge a s 0) [] in
 Array.to_seq input |>
 Seq.drop 2 |>
 Seq.map (ColorTree.mem_n towel_set) |>
 Seq.fold_left ( + ) 0

(* slightly slower (in bytecode), but a MUCH simpler solution *)
(* faster than 19b if compiled by ~10 ms *)
let problem_19b2 () =
 let memo = Hashtbl.create 4096 in
 let example = false in
 let input = In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_lines) |> Array.of_list in
 let subtowels =
  input.(0) |>
  String.split_on_char ',' |> List.to_seq |>
  Seq.map (fun s -> (String.trim s, ())) |>
  Hashtbl.of_seq in
 Hashtbl.add memo "" 1 ;
 let rec possibilities s =
  match Hashtbl.find_opt memo s with
  | Some n -> n
  | None ->
    let res =
    Seq.ints 1 |> Seq.take (String.length s) |>
    Seq.map (fun i -> String.sub s 0 i, if i = String.length s then "" else String.sub s i (String.length s - i)) |>
    Seq.filter (fun (s1,_) -> Hashtbl.mem subtowels s1) |>
    Seq.fold_left (fun a (_,s2) -> a + possibilities s2) 0 in
    Hashtbl.add memo s res ;
    res in
 Array.to_seq input |>
 Seq.drop 2 |>
 Seq.fold_left (fun a s -> a + possibilities s) 0

(* no branching paths! *)
let problem_20a () =
 let example = false in
 let map = In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_lines) |> Array.of_list in
 let (start_yx, end_yx, _) =
  map |>
  Array.fold_left
   (fun (a,b,y) bs ->
    (match String.index_opt bs 'S' with None -> a | Some x -> (y,x)),
    (match String.index_opt bs 'E' with None -> b | Some x -> (y,x)),
    (y+1))
   ((0,0), (0,0), 0) in
 let map_h = Array.length map in
 let map_w = String.length map.(0) in
 let paths = Array.init map_h (fun y -> Array.init map_w (fun x -> if map.(y).[x] = '#' then ~-2 else ~-1)) in
 paths.(fst start_yx).(snd start_yx) <- 0;
 let radius_1 (y,x) =
  [        y-1,x;        
   y  ,x-1;      y  ,x+1;
           y+1,x        ] in 
 let radius_2 (y,x) =
  [                y-2,x;
           y-1,x-1;      y-1,x+1;
   y  ,x-2;                      y  ,x+2;
           y+1,x+1;      y+1,x+1;
                   y+2,x                ] in
 let rec fill (y,x) =
  match
   radius_1 (y,x) |>
   List.find_opt
   (fun (y,x) -> match paths.(y).(x) with -1 -> true | _ -> false | exception _ -> false)
  with
  | None -> ()
  | Some (y',x') -> paths.(y').(x') <- paths.(y).(x) + 1 ; fill (y',x') in
 fill (fst start_yx, snd start_yx) ;
 let cutoff = 100 in
 let counter = ref 0 in
 let test (y,x) =
  radius_2 (y,x) |>
  List.iter
  (fun (y',x') ->
   match paths.(y').(x') with
   | n when n >= 0 -> if n - paths.(y).(x) - 2 >= cutoff then incr counter else ()
   | _ -> ()
   | exception _ -> ()) in
  for y = 0 to map_h - 1 do 
   for x = 0 to map_w - 1 do
    if paths.(y).(x) >= 0 then test (y,x) else ()
   done
  done ;
  !counter

let problem_20b () =
 let example = false in
 let (.%()) = Dynarray.get in
 let map =
  In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_lines) |>
  Array.of_list in
 let ((sy,sx),_) =
  map |> Array.fold_left
   (fun (a,y) bs ->
    (match String.index_opt bs 'S' with None -> a | Some x -> (y,x)),
    (y+1))
   ((0,0), 0) in
 let path_map =
  Array.init (Array.length map) (fun y ->
   Array.init (String.length map.(0)) (fun x ->
    if map.(y).[x] = '#' then ~-2 else ~-1)) in

 let radius_1 (y,x) =
  [|y-1,x;y,x-1;y,x+1;y+1,x|] in 

 let path = Dynarray.create () in

 path_map.(sy).(sx) <- 0;
 Dynarray.add_last path (sy, sx) ;

 let rec fill (y,x) =
  match
   radius_1 (y,x) |>
   Array.find_opt (fun (y,x) -> match path_map.(y).(x) with -1 -> true | _ -> false | exception _ -> false)
  with
  | None -> ()
  | Some (y',x') -> Dynarray.add_last path (y',x'); path_map.(y').(x') <- path_map.(y).(x) + 1 ; fill (y',x') in

 fill (sy,sx) ;

 let cutoff = 100 in
 let limit = 20 in
 let path_len = Dynarray.length path in
 let counter = ref 0 in

 for dst = path_len - 1 downto cutoff do
  for src = 0 to dst - cutoff - 1 do
   let (y',x') = path.%(dst) in
   let (y, x ) = path.%(src) in
   let r = abs (y' - y) + abs (x' - x) in
   if r <= limit && r >= 2 && (dst - src - r >= cutoff) then
   incr counter
  done
 done ;

 !counter

(*
789
456
123
 0A
---
 ^A
<v>
*)

let problem_21a () =

 let min_dir = function
 | 'A','A' -> "A"
 | 'A','<' -> "v<<A"
 | 'A','v' -> "<vA"
 | 'A','>' -> "vA"
 | 'A','^' -> "<A"
 | '<','A' -> ">>^A"
 | '<','<' -> "A"
 | '<','v' -> ">A"
 | '<','>' -> ">>A"
 | '<','^' -> ">^A"
 | 'v','A' -> "^>A"
 | 'v','<' -> "<A"
 | 'v','v' -> "A"
 | 'v','>' -> ">A"
 | 'v','^' -> "^A"
 | '>','A' -> "^A"
 | '>','<' -> "<<A"
 | '>','v' -> "<A"
 | '>','>' -> "A"
 | '>','^' -> "<^A"
 | '^','A' -> ">A"
 | '^','<' -> "v<A"
 | '^','v' -> "vA"
 | '^','>' -> "v>A"
 | '^','^' -> "A"
 | _ -> assert false in

 (* hand optimized *)
 (* rule 1: group all motions *)
 (* rule 2: unless it would result in an illegal motion, always do < first and > last *)

 let min_num = function
 | 'A','A' -> "A" | 'A','0' -> "<A" | 'A','1' -> "^<<A" | 'A','2' -> "<^A" | 'A','3' -> "^A" | 'A','4' -> "^^<<A"
 | 'A','5' -> "<^^A" | 'A','6' -> "^^A" | 'A','7' -> "^^^<<A" | 'A','8' -> "<^^^A" | 'A','9' -> "^^^A"
 | '0','A' -> ">A" | '0','0' -> "A" | '0','1' -> "^<A" | '0','2' -> "^A" | '0','3' -> ">^A" | '0','4' -> "^^<A"
 | '0','5' -> "^^A" | '0','6' -> "^^>A" | '0','7' -> "^^^<A" | '0','8' -> "^^^A" | '0','9' -> ">^^^A"
 | '1','A' -> ">>vA" | '1','0' -> ">vA" | '1','1' -> "A" | '1','2' -> ">A" | '1','3' -> ">>A" | '1','4' -> "^A"
 | '1','5' -> "^>A" | '1','6' -> "^>>A" | '1','7' -> "^^A" | '1','8' -> "^^>A" | '1','9' -> "^^>>A"
 | '2','A' -> "v>A" | '2','0' -> "vA" | '2','1' -> "<A" | '2','2' -> "A" | '2','3' -> ">A" | '2','4' -> "<^A"
 | '2','5' -> "^A" | '2','6' -> "^>A" | '2','7' -> "<^^A" | '2','8' -> "^^A" | '2','9' -> "^^>A"
 | '3','A' -> "vA" | '3','0' -> "<vA" | '3','1' -> "<<A" | '3','2' -> "<A" | '3','3' -> "A" | '3','4' -> "<<^A"
 | '3','5' -> "<^A" | '3','6' -> "^A" | '3','7' -> "<<^^A" | '3','8' -> "<^^A" | '3','9' -> "^^A"
 | '4','A' -> ">>vvA" | '4','0' -> ">vvA" | '4','1' -> "vA" | '4','2' -> "v>A" | '4','3' -> "v>>A" | '4','4' -> "A"
 | '4','5' -> ">A" | '4','6' -> ">>A" | '4','7' -> "^A" | '4','8' -> "^>A" | '4','9' -> "^>>A"
 | '5','A' -> "vv>A" | '5','0' -> "vvA" | '5','1' -> "<vA" | '5','2' -> "vA" | '5','3' -> "v>A" | '5','4' -> "<A"
 | '5','5' -> "A" | '5','6' -> ">A" | '5','7' -> "<^A" | '5','8' -> "^A" | '5','9' -> "^>A"
 | '6','A' -> "vvA" | '6','0' -> "<vvA" | '6','1' -> "<<vA" | '6','2' -> "<vA" | '6','3' -> "vA" | '6','4' -> "<<A"
 | '6','5' -> "<A" | '6','6' -> "A" | '6','7' -> "<<^A" | '6','8' -> "<^A" | '6','9' -> "^A"
 | '7','A' -> ">>vvvA" | '7','0' -> ">vvvA" | '7','1' -> "vvA" | '7','2' -> "vv>A" | '7','3' -> "vv>>A" | '7','4' -> "vA"
 | '7','5' -> "v>A" | '7','6' -> "v>>A" | '7','7' -> "A" | '7','8' -> ">A" | '7','9' -> ">>A"
 | '8','A' -> "vvv>A" | '8','0' -> "vvvA" | '8','1' -> "<vvA" | '8','2' -> "vvA" | '8','3' -> "vv>A" | '8','4' -> "<vA"
 | '8','5' -> "vA" | '8','6' -> "v>A" | '8','7' -> "<A" | '8','8' -> "A" | '8','9' -> ">A"
 | '9','A' -> "vvvA" | '9','0' -> "<vvvA" | '9','1' -> "<<vvA" | '9','2' -> "<vvA" | '9','3' -> "vvA" | '9','4' -> "<<vA"
 | '9','5' -> "<vA" | '9','6' -> "vA" | '9','7' -> "<<A" | '9','8' -> "<A" | '9','9' -> "A"
 | _ -> assert false in

 (* right-recursive (bottom-up) descent, w/ min_num buried at bottom *)
 let robot_1 (src,dst) =
  let s = min_num (src,dst) |> String.to_seq |> Seq.cons 'A' in
  Seq.zip s (Seq.drop 1 s) |>
  Seq.map min_dir |> 
  Seq.fold_left ( ^ ) "" in
 let robot_2 (src,dst) =
  let s = robot_1 (src,dst) |> String.to_seq |> Seq.cons 'A' in
  Seq.zip s (Seq.drop 1 s) |>
  Seq.map min_dir |> 
  Seq.fold_left ( ^ ) "" in
(*
 let translate_0 input =
  let s = input |> String.to_seq |> Seq.cons 'A' in
  Seq.zip s (Seq.drop 1 s) |>
  Seq.map min_num |> 
  Seq.fold_left ( ^ ) "" in
 let translate_1 input =
  let s = input |> String.to_seq |> Seq.cons 'A' in
  Seq.zip s (Seq.drop 1 s) |>
  Seq.map robot_1 |> 
  Seq.fold_left ( ^ ) "" in
*)
 let translate_2 input =
  let s = input |> String.to_seq |> Seq.cons 'A' in
  Seq.zip s (Seq.drop 1 s) |>
  Seq.map robot_2 |> 
  Seq.fold_left ( ^ ) "" in

(*
 (* left-recursive (top-down), memoized solution - too slow for b *)

 let hash = function
 | 'A','A' -> 0 | 'A','<' -> 1 | 'A','v' -> 2 | 'A','>' -> 3 | 'A','^' -> 4
 | '<','A' -> 5 | '<','<' -> 6 | '<','v' -> 7 | '<','>' -> 8 | '<','^' -> 9
 | 'v','A' -> 10 | 'v','<' -> 11 | 'v','v' -> 12 | 'v','>' -> 13 | 'v','^' -> 14
 | '>','A' -> 15 | '>','<' -> 16 | '>','v' -> 17 | '>','>' -> 18 | '>','^' -> 19
 | '^','A' -> 20 | '^','<' -> 21 | '^','v' -> 22 | '^','>' -> 23 | '^','^' -> 24
 | _ -> assert false in

 (* for max depth 25 *)
 let memo = Array.make 625 None in
 let memo_get (n,src,dst) = memo.(25*(n-1)+(hash(src,dst))) in
 let memo_set (n,src,dst) v = memo.(25*(n-1)+(hash(src,dst))) <- Some v in

 (* this version returns sequences *)
 let rec robot_n n (src,dst) =
  if n = 0 then Seq.return dst else
  match memo_get (n,src,dst) with
  | Some res -> res
  | None ->
    if n = 1 then min_dir (src,dst) |> String.to_seq else
    let s = robot_n 1 (src,dst) |> Seq.cons 'A' in
    let res =
     Seq.zip s (Seq.drop 1 s) |>
     Seq.map (robot_n (n-1)) |> 
     Seq.concat in
    (memo_set (n,src,dst) res; res) in

 let translate_n n input =
  let s0 = input |> String.to_seq |> Seq.cons 'A' in
  let s1 =
   Seq.zip s0 (Seq.drop 1 s0) |>
   Seq.map min_num |>
   Seq.map (String.to_seq) |>
   Seq.concat |>
   Seq.cons 'A' in
  Seq.zip s1 (Seq.drop 1 s1) |>
  Seq.map (robot_n n) |> 
  Seq.concat in

*)

 let example = false in
 In_channel.(with_open_bin (if example then "21e.txt" else "21.txt") input_lines) |>
 List.map (fun s -> (translate_2 s |> String.length) * (String.sub s 0 3 |> int_of_string)) |>
 List.fold_left ( + ) 0

let problem_21b () =

 let min_dir = function
 | 'A','A' -> "A"
 | 'A','<' -> "v<<A"
 | 'A','v' -> "<vA"
 | 'A','>' -> "vA"
 | 'A','^' -> "<A"
 | '<','A' -> ">>^A"
 | '<','<' -> "A"
 | '<','v' -> ">A"
 | '<','>' -> ">>A"
 | '<','^' -> ">^A"
 | 'v','A' -> "^>A"
 | 'v','<' -> "<A"
 | 'v','v' -> "A"
 | 'v','>' -> ">A"
 | 'v','^' -> "^A"
 | '>','A' -> "^A"
 | '>','<' -> "<<A"
 | '>','v' -> "<A"
 | '>','>' -> "A"
 | '>','^' -> "<^A"
 | '^','A' -> ">A"
 | '^','<' -> "v<A"
 | '^','v' -> "vA"
 | '^','>' -> "v>A"
 | '^','^' -> "A"
 | _ -> assert false in

 (* hand optimized *)
 (* rule 1: group all motions *)
 (* rule 2: unless it would result in an illegal motion, always do < first and > last *)

 let min_num = function
 | 'A','A' -> "A" | 'A','0' -> "<A" | 'A','1' -> "^<<A" | 'A','2' -> "<^A" | 'A','3' -> "^A" | 'A','4' -> "^^<<A"
 | 'A','5' -> "<^^A" | 'A','6' -> "^^A" | 'A','7' -> "^^^<<A" | 'A','8' -> "<^^^A" | 'A','9' -> "^^^A"
 | '0','A' -> ">A" | '0','0' -> "A" | '0','1' -> "^<A" | '0','2' -> "^A" | '0','3' -> ">^A" | '0','4' -> "^^<A"
 | '0','5' -> "^^A" | '0','6' -> "^^>A" | '0','7' -> "^^^<A" | '0','8' -> "^^^A" | '0','9' -> ">^^^A"
 | '1','A' -> ">>vA" | '1','0' -> ">vA" | '1','1' -> "A" | '1','2' -> ">A" | '1','3' -> ">>A" | '1','4' -> "^A"
 | '1','5' -> "^>A" | '1','6' -> "^>>A" | '1','7' -> "^^A" | '1','8' -> "^^>A" | '1','9' -> "^^>>A"
 | '2','A' -> "v>A" | '2','0' -> "vA" | '2','1' -> "<A" | '2','2' -> "A" | '2','3' -> ">A" | '2','4' -> "<^A"
 | '2','5' -> "^A" | '2','6' -> "^>A" | '2','7' -> "<^^A" | '2','8' -> "^^A" | '2','9' -> "^^>A"
 | '3','A' -> "vA" | '3','0' -> "<vA" | '3','1' -> "<<A" | '3','2' -> "<A" | '3','3' -> "A" | '3','4' -> "<<^A"
 | '3','5' -> "<^A" | '3','6' -> "^A" | '3','7' -> "<<^^A" | '3','8' -> "<^^A" | '3','9' -> "^^A"
 | '4','A' -> ">>vvA" | '4','0' -> ">vvA" | '4','1' -> "vA" | '4','2' -> "v>A" | '4','3' -> "v>>A" | '4','4' -> "A"
 | '4','5' -> ">A" | '4','6' -> ">>A" | '4','7' -> "^A" | '4','8' -> "^>A" | '4','9' -> "^>>A"
 | '5','A' -> "vv>A" | '5','0' -> "vvA" | '5','1' -> "<vA" | '5','2' -> "vA" | '5','3' -> "v>A" | '5','4' -> "<A"
 | '5','5' -> "A" | '5','6' -> ">A" | '5','7' -> "<^A" | '5','8' -> "^A" | '5','9' -> "^>A"
 | '6','A' -> "vvA" | '6','0' -> "<vvA" | '6','1' -> "<<vA" | '6','2' -> "<vA" | '6','3' -> "vA" | '6','4' -> "<<A"
 | '6','5' -> "<A" | '6','6' -> "A" | '6','7' -> "<<^A" | '6','8' -> "<^A" | '6','9' -> "^A"
 | '7','A' -> ">>vvvA" | '7','0' -> ">vvvA" | '7','1' -> "vvA" | '7','2' -> "vv>A" | '7','3' -> "vv>>A" | '7','4' -> "vA"
 | '7','5' -> "v>A" | '7','6' -> "v>>A" | '7','7' -> "A" | '7','8' -> ">A" | '7','9' -> ">>A"
 | '8','A' -> "vvv>A" | '8','0' -> "vvvA" | '8','1' -> "<vvA" | '8','2' -> "vvA" | '8','3' -> "vv>A" | '8','4' -> "<vA"
 | '8','5' -> "vA" | '8','6' -> "v>A" | '8','7' -> "<A" | '8','8' -> "A" | '8','9' -> ">A"
 | '9','A' -> "vvvA" | '9','0' -> "<vvvA" | '9','1' -> "<<vvA" | '9','2' -> "<vvA" | '9','3' -> "vvA" | '9','4' -> "<<vA"
 | '9','5' -> "<vA" | '9','6' -> "vA" | '9','7' -> "<<A" | '9','8' -> "<A" | '9','9' -> "A"
 | _ -> assert false in

 (* max memo size is 25 * depth, so this can be converted into an array for a speed up *)
 let memo = Hashtbl.create 4096 in

 (* perform step 1 outside; memoize only directional keypad *)
 let rec robot_n_len n (src,dst) =
  match Hashtbl.find_opt memo (n,src,dst) with
  | Some res -> res
  | None ->
    if n = 0 then 1 else
    if n = 1 then String.length (min_dir (src,dst)) else
    let res =
     let s = min_dir (src,dst) |> String.to_seq |> Seq.cons 'A' in
     Seq.zip s (Seq.drop 1 s) |>
     Seq.map (robot_n_len (n-1)) |>
     Seq.fold_left ( + ) 0 in
    (Hashtbl.add memo (n,src,dst) res; res) in

 let translate_n n input =
  let s0 = input |> String.to_seq |> Seq.cons 'A' in
  let s1 =
   Seq.zip s0 (Seq.drop 1 s0) |>
   Seq.map min_num |>
   Seq.map (String.to_seq) |>
   Seq.concat |>
   Seq.cons 'A' in
  Seq.zip s1 (Seq.drop 1 s1) |>
  Seq.map (robot_n_len n) |>
  Seq.fold_left ( + ) 0 in

 let depth = 25 in
 let example = false in
 In_channel.(with_open_bin (if example then "21e.txt" else "21.txt") input_lines) |>
 List.map (fun s -> (s |> translate_n depth) * (String.sub s 0 3 |> int_of_string)) |>
 List.fold_left ( + ) 0

let problem_22a () =
 let example = false in
 let prng secret =
  let secret = ((secret lsl 6 ) lxor secret) land (1 lsl 24 - 1) in
  let secret = ((secret lsr 5 ) lxor secret) land (1 lsl 24 - 1) in
  let secret = ((secret lsl 11) lxor secret) land (1 lsl 24 - 1) in
  secret in

 let rec generate n seed =
  if n = 0 then seed else
  generate (n-1) (prng seed) in
 
 let input = In_channel.(with_open_bin (if example then "22e.txt" else "22.txt") input_lines) |> List.map (int_of_string) in
 input |> List.to_seq |>
 Seq.map (generate 2000) |>
 Seq.fold_left ( + ) 0
 
let problem_22b () =
 let prng secret =
  let secret = ((secret lsl 6 ) lxor secret) land (1 lsl 24 - 1) in
  let secret = ((secret lsr 5 ) lxor secret) land (1 lsl 24 - 1) in
  let secret = ((secret lsl 11) lxor secret) land (1 lsl 24 - 1) in
  secret in

 let rec generate_list acc n seed =
  if n = -1 then List.rev acc else
  generate_list (seed::acc) (n-1) (prng seed) in

 let diffs ns =
  let s = List.to_seq ns |> Seq.map (fun n -> n mod 10) in
  Seq.zip s (Seq.drop 1 s) |>
  Seq.map (fun (last, next) -> (next, next-last)) in

 (* may make sense to use (.%{;..}) indexing *)
 let seen = Array.make (20*20*20*20) false in
 let seen_get (n1,n2,n3,n4) = seen.((n1+10)*20*20*20+(n2+10)*20*20+(n3+10)*20+(n4+10)) in
 let seen_set (n1,n2,n3,n4) = seen.((n1+10)*20*20*20+(n2+10)*20*20+(n3+10)*20+(n4+10)) <- true in
 let seen_clear () = Array.fill seen 0 (20*20*20*20) false in
 let sums = Array.make (20*20*20*20) 0 in
 let sums_update (n1,n2,n3,n4) v =
  sums.((n1+10)*20*20*20+(n2+10)*20*20+(n3+10)*20+(n4+10)) <- 
   sums.((n1+10)*20*20*20+(n2+10)*20*20+(n3+10)*20+(n4+10)) + v in

 let tabulate_sums ds =
  for i = 3 to Array.length ds - 1 do
   let (_,n1) = ds.(i-3) in
   let (_,n2) = ds.(i-2) in
   let (_,n3) = ds.(i-1) in
   let (v,n4) = ds.(i) in
   if not (seen_get (n1,n2,n3,n4)) then
    (sums_update (n1,n2,n3,n4) v ; seen_set (n1,n2,n3,n4)) ;
  done in

 let example = false in
 let input = In_channel.(with_open_bin (if example then "22e.txt" else "22.txt") input_lines) |> List.map (int_of_string) in
 input |> List.to_seq |>
 Seq.map (generate_list [] 2000) |>
 Seq.map (fun ns -> ns |> diffs |> Array.of_seq) |>
 Seq.iter (fun ds -> tabulate_sums ds ; seen_clear ()) ;
 Array.fold_left max 0 sums

(* adjacency matrix *)
(* input : network map, non-directional *)
(* historian's computer starts with the letter 't' *)
let problem_23a () =
 (* can optimize with indexer, but there are 520 unique nodes anyway *)
 let module AddressSet = Set.Make(Int) in
 let dim = 676 in
 let adj = Array.make (dim*dim) 0 in
 let adr_of_s s = (Char.code s.[0] - 0x61) * 26 + (Char.code s.[1] - 0x61) in
(*
 let adr_to_s adr =
  let c0 = adr / 26 + 0x61 in
  let c1 = adr mod 26 + 0x61 in
  Printf.sprintf "%c%c" (Char.chr c0) (Char.chr c1) in
*)
 let add_edge_nm s1 s2 =
  (* 0x61 = 'a' *)
  let adr1 = adr_of_s s1 in
  let adr2 = adr_of_s s2 in
  adj.(dim*adr1+adr2) <- 1 ;
  adj.(dim*adr2+adr1) <- 1 in
 let rm_node adr =
  for y = 0 to dim - 1 do adj.(y*dim+adr) <- 0 done;
  Array.fill adj (adr*dim) dim 0 in
 let connected_nodes adr =
  let nodes = ref AddressSet.empty in
  for i = adr*dim to adr*dim+(dim-1) do
   if adj.(i) = 1 then nodes := AddressSet.add (i-adr*dim) !nodes
  done ;
  !nodes in
 let count_and_purge adr =
  let nodes = connected_nodes adr in
  let loop_count =
   AddressSet.fold
    (fun adr_edge a ->
     let loop_nodes = AddressSet.inter nodes (connected_nodes adr_edge) in
     a + AddressSet.cardinal loop_nodes) nodes 0 in
  (* avoid double counting other tx nodes *)
  rm_node adr ;
  (* each loop should be counted twice *)
  loop_count lsr 1 in

 let example = false in
  
 In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_lines) |>
 List.iter (fun s -> s |> String.split_on_char '-' |> (function [s1;s2] -> add_edge_nm s1 s2 | _ -> ())) ;
 let count = ref 0 in
 for adr = adr_of_s "ta" to adr_of_s "ua" - 1 do
  count := !count + count_and_purge adr
 done;
 !count

(* adjacency matrix *)
(* find largest fully-connected group *)
let problem_23b () =
 (* can optimize with indexer, but there are 520 unique nodes anyway *)
 let module AddressSet = Set.Make(Int) in
 (* add three special address rows for intersection ops *)
 let dim = 676 in
 let adj = Array.make ((dim+1)*dim) 0 in
 let adr_buf = dim in

 let adr_of_s s = (Char.code s.[0] - 0x61) * 26 + (Char.code s.[1] - 0x61) in
 let adr_to_s adr =
  let c0 = adr / 26 + 0x61 in
  let c1 = adr mod 26 + 0x61 in
  Printf.sprintf "%c%c" (Char.chr c0) (Char.chr c1) in

 let add_edge_nm s1 s2 =
  (* 0x61 = 'a' *)
  let adr1 = adr_of_s s1 in
  let adr2 = adr_of_s s2 in
  (* edges *)
  adj.(dim*adr1+adr2) <- 1 ;
  adj.(dim*adr2+adr1) <- 1 ;
  (* self-edges *)
  adj.(dim*adr1+adr1) <- 1 ;
  adj.(dim*adr2+adr2) <- 1 in

 let connected_nodes adr =
  let nodes = ref AddressSet.empty in
  for i = adr*dim to adr*dim+(dim-1) do
   if adj.(i) = 1 then nodes := AddressSet.add (i-adr*dim) !nodes
  done ;
  !nodes in

 let reset_buf () = Array.fill adj (adr_buf*dim) dim 1 in

 let inter_and adr =
  let row = adr*dim in
  let buf_row = adr_buf*dim in
  for i = 0 to dim - 1 do
   adj.(buf_row+i) <- adj.(buf_row+i) land adj.(row+i)
  done in

(*
 let print_row adr =
  connected_nodes adr |>
  AddressSet.iter (fun adr -> Printf.printf "%s," (adr_to_s adr)) ;
  print_newline () in
*)

 let merge_set adr_set =
  reset_buf ();
  AddressSet.iter inter_and adr_set ;
  connected_nodes adr_buf in

 let string_of_set adr_set =
  let buf = Buffer.create 64 in
  adr_set |>
  AddressSet.iter
  (fun adr ->
   Buffer.add_string buf (adr_to_s adr) ;
   Buffer.add_char buf ',') ;
  Buffer.truncate buf (Buffer.length buf - 1);
  Buffer.contents buf in

 let example = false in

 In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_lines) |>
 List.iter (fun s -> s |> String.split_on_char '-' |> (function [s1;s2] -> add_edge_nm s1 s2 | _ -> ())) ;

 let uniq = Hashtbl.create 1024 in
 
 (* search *)
 let cutoff = 13 in
 for i = 0 to dim - 2 do
  for j = i+1 to dim - 1 do
   reset_buf () ;
   inter_and i ;
   inter_and j ;
   let nodes = connected_nodes adr_buf in
   if AddressSet.cardinal nodes >= cutoff
   then
    begin
     match Hashtbl.find_opt uniq nodes with
     | None -> Hashtbl.add uniq nodes 1
     | Some n -> Hashtbl.replace uniq nodes (n+1)
    end
  (*print_row adr_buf*) else ()
  done
 done ;
 (* extra optimization - remove if no output *)
 let cutoff =
  Hashtbl.to_seq uniq |> Seq.fold_left (fun a (_,v) -> max a v) 0 in
 Hashtbl.to_seq uniq |>
 Seq.filter (fun (_,v) -> v >= cutoff) |>
 Seq.map (fun (k,_) -> merge_set k) |>
 Seq.iter (fun k -> print_endline @@ string_of_set k)

 (* verification *)
(*
 reset_buf ();
 inter_and (adr_of_s "aq") ;
 inter_and (adr_of_s "cc") ;
 inter_and (adr_of_s "ea") ;
 inter_and (adr_of_s "gc") ;
 inter_and (adr_of_s "jo") ;
 inter_and (adr_of_s "od") ;
 inter_and (adr_of_s "pa") ;
 inter_and (adr_of_s "rg") ;
 inter_and (adr_of_s "rv") ;
 inter_and (adr_of_s "ub") ;
 inter_and (adr_of_s "ul") ;
 inter_and (adr_of_s "vr") ;
 inter_and (adr_of_s "yy") ;
 print_set adr_buf
*)

(* this is incredibly slow in ocaml if you use default Hashtbl.mem *)
(* Set.compare exists for sets of sets, but we just use Dynarray here (it is fast enough) *)
let problem_23b2 () =
 let module ISet = Set.Make(Int) in
 let example = false in
 let dim = 676 in
 let graph = Array.make dim ISet.empty in
 let adr_of_s s = (Char.code s.[0] - 0x61) * 26 + (Char.code s.[1] - 0x61) in
 let adr_to_s adr =
  let c0 = adr / 26 + 0x61 in
  let c1 = adr mod 26 + 0x61 in
  Printf.sprintf "%c%c" (Char.chr c0) (Char.chr c1) in
 In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_lines) |>
 List.iter (fun s ->
  s |> String.split_on_char '-' |>
  (function [s1;s2] ->
   let adr1 = adr_of_s s1 in
   let adr2 = adr_of_s s2 in
   graph.(adr1) <- ISet.add adr2 graph.(adr1) ;
   graph.(adr2) <- ISet.add adr1 graph.(adr2)
   | _ -> ())) ;
 let saturated = Dynarray.create () in
 let rec log_saturated key required =
  (* use Dynarray.exists instead of Hashtbl.mem! *)
  (* you may be able to optimize with index_of superset replacement *)
  if Dynarray.exists (ISet.subset required) saturated then () else
  ( Dynarray.add_last saturated required;
    graph.(key) |>
    ISet.iter (fun adr ->
    if ISet.mem adr required then () else
    if ISet.subset required graph.(adr)
    then log_saturated adr (ISet.add adr required)
    else ())) in
 for key = 0 to dim - 1 do
  if graph.(key) <> ISet.empty
  then log_saturated key (ISet.singleton key)
  else ()
 done ;
 let buf = Buffer.create 64 in
 Dynarray.fold_left (fun a k -> if ISet.cardinal a < ISet.cardinal k then k else a) ISet.empty saturated |>
 ISet.iter (fun adr -> Buffer.add_string buf (adr_to_s adr) ; Buffer.add_char buf ',') ;
 Buffer.truncate buf (Buffer.length buf - 1) ;
 Buffer.contents buf

(* logic gates *)
(* z00-z45 *)
let problem_24a () =
 let adr_of_c c =
  match c with
  | '0' .. '9' -> Char.code c - 0x30
  | 'a' .. 'z' -> Char.code c - 0x61 + 10
  | _ -> failwith "Invalid Index Character" in
 let adr_to_c adr =
  match adr mod 36 with
  | n when n >= 0 && n <= 9 -> Char.chr (n + 0x30)
  | n -> Char.chr (n - 10 + 0x61) in
 let adr_of_s s =
  adr_of_c s.[0] * 36 * 36 +
  adr_of_c s.[1] * 36 +
  adr_of_c s.[2] in
 let adr_to_s n =
  let res = Bytes.create 3 in
  Bytes.set res 2 (adr_to_c n) ;
  Bytes.set res 1 (adr_to_c (n/36)) ;
  Bytes.set res 0 (adr_to_c (n/(36*36))) ;
  Bytes.to_string res in
 let module Gate = struct
  type opcode = AND | OR | XOR
  type t = {in_adrs: int * int; out_adr : int ; op: opcode }
  let of_string s =
   match String.split_on_char ' ' s with
   | [in1; opcode; in2; _; out] ->
     {in_adrs = (adr_of_s in1, adr_of_s in2); 
      out_adr = adr_of_s out;
      op = (match opcode with "AND" -> AND | "OR" -> OR | "XOR" -> XOR | _ -> failwith "Invalid Opcode") }
   | _ -> failwith "Invalid Gate String"
 end in
 let example = false in
 let debug = false in
 let max_z = if example then 12 else 45 in
 let input =
  In_channel.(with_open_bin (if example then "24e.txt" else "24.txt") input_lines) |>
  Array.of_list in
 let brk = Array.find_index ((=)"") input |> Option.get in
 let nodes = Hashtbl.create 1024 in 
 (* once a gate has been used, we remove it *)
 let gates = Queue.create () in
 (* process input wires (xs and ys) *)
 for i = 0 to brk - 1 do
  let key = adr_of_s (String.sub input.(i) 0 3) in
  Hashtbl.add nodes key (Char.code input.(i).[5] - 0x30)
 done ;
 for i = brk + 1 to Array.length input - 1 do
  Queue.add (Gate.of_string input.(i)) gates ;
 done ;
 let z_adrs =
  Seq.ints 0 |> Seq.take (max_z + 1) |>
  Seq.map (Printf.sprintf "z%02d") |>
  Seq.map (adr_of_s) |>
  Seq.memoize in
 let get_z () =
  z_adrs |>
  Seq.map (fun adr -> Hashtbl.find nodes adr) |>
  Seq.fold_left (fun (a,bit) n -> (a+n*bit,bit lsl 1)) (0,0x01) |>
  fst in
 let get_z_force () =
  z_adrs |>
  Seq.map (fun adr -> Hashtbl.find_opt nodes adr) |>
  Seq.map (Option.value ~default:0) |>
  Seq.fold_left (fun (a,bit) n -> print_int n ; print_newline () ; (a+n*bit,bit lsl 1)) (0,0x01) |>
  fst in
 while not @@ Queue.is_empty gates do
  let g = Queue.take gates in
   let (l_adr,r_adr) = g.in_adrs in
   match Hashtbl.find_opt nodes l_adr, Hashtbl.find_opt nodes r_adr, g.op with
   | Some l, Some r, AND -> Hashtbl.replace nodes g.out_adr (l land r)
   | Some l, Some r, OR  -> Hashtbl.replace nodes g.out_adr (l lor  r)
   | Some l, Some r, XOR -> Hashtbl.replace nodes g.out_adr (l lxor r)
   | _ -> Queue.add g gates
 done ;
 if false then print_endline @@ adr_to_s 0 ;
 (if debug then get_z_force () else get_z ())

(* four pairs of output wires have been swapped *)
let problem_24b () =
 let adr_of_c c =
  match c with
  | '0' .. '9' -> Char.code c - 0x30
  | 'a' .. 'z' -> Char.code c - 0x61 + 10
  | _ -> failwith "Invalid Index Character" in
 let adr_to_c adr =
  match adr mod 36 with
  | n when n >= 0 && n <= 9 -> Char.chr (n + 0x30)
  | n -> Char.chr (n - 10 + 0x61) in
 let adr_of_s s =
  adr_of_c s.[0] * 36 * 36 +
  adr_of_c s.[1] * 36 +
  adr_of_c s.[2] in
 let adr_to_s n =
  let res = Bytes.create 3 in
  Bytes.set res 2 (adr_to_c n) ;
  Bytes.set res 1 (adr_to_c (n/36)) ;
  Bytes.set res 0 (adr_to_c (n/(36*36))) ;
  Bytes.to_string res in
 let module Gate = struct
  type opcode = AND | OR | XOR
  type t = {in_adrs: int * int; out_adr : int ; op: opcode }
  let of_string s =
   match String.split_on_char ' ' s with
   | [in1; opcode; in2; _; out] ->
     {in_adrs = (adr_of_s in1, adr_of_s in2); 
      out_adr = adr_of_s out;
      op = (match opcode with "AND" -> AND | "OR" -> OR | "XOR" -> XOR | _ -> failwith "Invalid Opcode") }
   | _ -> failwith "Invalid Gate String"
  let to_string g =
   let (l,r) = g.in_adrs in
   let opstring = (match g.op with AND -> "AND" | OR -> "OR" | XOR -> "XOR") in
   Printf.sprintf "%s %s %s -> %s" (adr_to_s l) (adr_to_s r) opstring (adr_to_s g.out_adr)
 end in
 let example = false in
 let debug = false in
 let max_z = if example then 12 else 45 in
 let input =
  In_channel.(with_open_bin (if example then "24e.txt" else "24.txt") input_lines) |>
  Array.of_list in
 let brk = Array.find_index ((=)"") input |> Option.get in
 let nodes = Hashtbl.create 1024 in 
 (* once a gate has been used, we remove it *)
 let gates = Queue.create () in
 (* process input wires (xs and ys) *)
 for i = 0 to brk - 1 do
  let key = adr_of_s (String.sub input.(i) 0 3) in
  Hashtbl.add nodes key (Char.code input.(i).[5] - 0x30)
 done ;
 for i = brk + 1 to Array.length input - 1 do
  Queue.add (Gate.of_string input.(i)) gates ;
 done ;
 let cache = Queue.to_seq gates |> Array.of_seq in
 let x_adrs =
  Seq.ints 0 |> Seq.take (max_z + 1) |>
  Seq.map (Printf.sprintf "x%02d") |>
  Seq.map (adr_of_s) |>
  Seq.memoize in
 let y_adrs =
  Seq.ints 0 |> Seq.take (max_z + 1) |>
  Seq.map (Printf.sprintf "y%02d") |>
  Seq.map (adr_of_s) |>
  Seq.memoize in
 let z_adrs =
  Seq.ints 0 |> Seq.take (max_z + 1) |>
  Seq.map (Printf.sprintf "z%02d") |>
  Seq.map (adr_of_s) |>
  Seq.memoize in
 let set_x x =
  Seq.unfold (fun a -> Some (a land 1, a lsr 1)) x |>
  Seq.zip x_adrs |>
  Seq.iter (fun (adr, v) -> Hashtbl.replace nodes adr v) in
 let set_y y =
  Seq.unfold (fun a -> Some (a land 1, a lsr 1)) y |>
  Seq.zip y_adrs |>
  Seq.iter (fun (adr, v) -> Hashtbl.replace nodes adr v) in
 let get_z () =
  z_adrs |>
  Seq.map (fun adr -> Hashtbl.find nodes adr) |>
  Seq.fold_left (fun (a,bit) n -> (a+n*bit,bit lsl 1)) (0,0x01) |>
  fst in
 let get_z_force () =
  z_adrs |>
  Seq.map (fun adr -> Hashtbl.find_opt nodes adr) |>
  Seq.map (Option.value ~default:0) |>
  Seq.fold_left (fun (a,bit) n -> (a+n*bit,bit lsl 1)) (0,0x01) |>
  fst in
(*
 let run () =
  while not @@ Queue.is_empty gates do
   let g = Queue.take gates in
    let (l_adr,r_adr) = g.in_adrs in
    match Hashtbl.find_opt nodes l_adr, Hashtbl.find_opt nodes r_adr, g.op with
    | Some l, Some r, AND -> Hashtbl.replace nodes g.out_adr (l land r)
    | Some l, Some r, OR  -> Hashtbl.replace nodes g.out_adr (l lor  r)
    | Some l, Some r, XOR -> Hashtbl.replace nodes g.out_adr (l lxor r)
    | _ -> Queue.add g gates
  done in
*)
 let run_with x y =
  Hashtbl.clear nodes;
  Queue.clear gates;
  set_x x ;
  set_y y ;
  Queue.add_seq gates (Array.to_seq cache) ;
  let count = ref 0 in
  while !count < 20000 && (not @@ Queue.is_empty gates) do
   count := !count + 1 ;
   let g = Queue.take gates in
    let (l_adr,r_adr) = g.in_adrs in
    match Hashtbl.find_opt nodes l_adr, Hashtbl.find_opt nodes r_adr, g.op with
    | Some l, Some r, AND -> Hashtbl.replace nodes g.out_adr (l land r)
    | Some l, Some r, OR  -> Hashtbl.replace nodes g.out_adr (l lor  r)
    | Some l, Some r, XOR -> Hashtbl.replace nodes g.out_adr (l lxor r)
    | _ -> Queue.add g gates
  done in
 let swap1 adr1 adr2 v = if v = adr1 then adr2 else if v = adr2 then adr1 else v in
 let swap adr1 adr2 =
  cache |>
  Array.map_inplace Gate.(fun g ->
   {g with out_adr  = swap1 adr1 adr2 g.out_adr }) in
 (* swaps *)
(*
 let non_special = 
[|"bcq";"bgj";"bkp";"bmv";"bqm";"bss";"btq";"btv";"cbh";"ccs";"ccw";"cdb";"cdt";"chb";"cjt";"ckw";"cpr";"crk";"crp";"crt";"dbd";"dbg";"dfn";"dgr";"dgw";"dnf";"drd";"dwd";"dwg";"fbb";"fjc";"fjn";"fjs";"fjt";"fkb";"fmr";"fpv";"ftq";"fvc";"fvv";"gcg";"ggg";"gkk";"grr";"gtv";"hbc";"hbk";"hdk";"hmh";"hnk";"hnq";"hnv";"hqr";"hrd";"hvv";"hwp";"jck";"jdk";"jdq";"jfq";"jhd";"jmt";"jnj";"jnr";"jrf";"jrg";"jrk";"jsd";"jtc";"jtm";"kcc";"kcm";"kcp";"kgn";"kgt";"kmf";"knb";"knv";"ksh";"ktn";"kwh";"mcj";"mcv";"mfc";"mgr";"mtb";"nbm";"nbr";"ncp";"njb";"nkn";"nnq";"nnr";"ntw";"nwn";"pct";"pdb";"pfb";"pgq";"phc";"pkv";"pnv";"ppp";"pps";"pqk";"prt";"pss";"pwb";"qcm";"qft";"qfv";"qkk";"qmm";"qmr";"qnf";"qpd";"qph";"qpk";"qpn";"qqr";"qsj";"qst";"qtq";"rbk";"rcc";"rcm";"rdg";"rdn";"rhk";"rhw";"rjw";"rmv";"rqf";"rqw";"rrn";"rsj";"scw";"sfs";"sfw";"shb";"shp";"shr";"smh";"smj";"spp";"sqr";"ssf";"svk";"swq";"tbr";"tdb";"thm";"thq";"tjk";"tkt";"tnn";"tph";"tsw";"ttn";"twj";"vhj";"vhv";"vmf";"vnc";"vnm";"vnw";"vrr";"vsm";"vsq";"vtb";"wdv";"wgk";"wqg";"wqt";"wrt";"wsm";"z00";"z01";"z02";"z03";"z04";"z05";"z06";"z07";"z08";"z09";"z10";"z11";"z12";"z13";"z14";"z15";"z16";"z17";"z18";"z19";"z20";"z21";"z22";"z23";"z24";"z25";"z26";"z27";"z28";"z29";"z30";"z31";"z32";"z33";"z34";"z35";"z36";"z37";"z38";"z39";"z40";"z41";"z42";"z43";"z44";"z45"|] in
*)

 swap (adr_of_s "z16") (adr_of_s "fkb") ;
 swap (adr_of_s "nnr") (adr_of_s "rqf") ;
 swap (adr_of_s "z31") (adr_of_s "rdn") ;
 swap (adr_of_s "z37") (adr_of_s "rrn") ;

(*
 for i = 0 to Array.length non_special - 2 do
  for j = i+1 to Array.length non_special - 1 do
   (*print_int i ; print_char ',' ; print_int j ; print_newline () ;*)
   swap (adr_of_s non_special.(i)) (adr_of_s non_special.(j)) ;
   run_with (1 lsl 31) (0) ;
   if get_z_force () = (1 lsl 31) then Printf.printf "MATCH %s, %s\n" non_special.(i) non_special.(j) ;
   swap (adr_of_s non_special.(i)) (adr_of_s non_special.(j)) ;
  done
 done ;
*)

 Random.self_init () ;
 for i = 1 to 1556 do
  let a = Random.bits () in
  let b = Random.bits () in
  run_with a b ;
  if get_z () <> a+b then print_endline "FAIL!" else ()
 done ;
 print_endline "-----" ;
 for i = 0 to max_z do
  run_with 0 (1 lsl i) ;
  if get_z () <> (1 lsl i) then (print_int i ; print_newline ()) else ()
 done ;
 print_endline "-----" ;
 for i = 0 to max_z do
  run_with (1 lsl i) 0 ;
  if get_z () <> (1 lsl i) then (print_int i ; print_newline ()) else ()
 done ;
 print_endline "-----" ;
 for i = 0 to max_z - 1 do
  run_with (1 lsl i) (1 lsl i) ;
  if get_z () <> (1 lsl (i+1)) then (print_int i ; print_newline ()) else ()
 done ;
 print_endline "-----" ;
 run_with 0 (2024) ;
 (if debug then get_z_force () else get_z ())

(* each lock combo is 7x5 (hxw) with a line in-between/after *)
(* # - filled, . - empty *)
(* sum must < 6 for all columns *)
let problem_25a () =
 let debug = false in
 let example = false in
 let module KLSet = Set.Make(struct type t = int * int * int * int *int let compare = compare end) in
 let input =
  In_channel.(with_open_bin (if example then "25e.txt" else "25.txt") input_lines) |>
  Array.of_list in
 let num_slots = (Array.length input + 1) lsr 3 in
 let is_lock slot = input.(slot*8) = "#####" in
 (* let is_key slot = input.(slot*8+6) = "#####" in *)
 let parse slot =
  let res = Array.make 5 0 in
  for y = 1 to 5 do
   for x = 0 to 4 do
    if input.(slot*8+y).[x] = '#' then res.(x) <- res.(x) + 1
  done done ;
  (res.(0), res.(1), res.(2), res.(3), res.(4)) in
 let valid (a,b,c,d,e) (a',b',c',d',e') =
  a + a' < 6 && b + b' < 6 && c + c' < 6 && d + d' < 6 && e + e' < 6 in
 let kl_to_string (a,b,c,d,e) = Printf.sprintf "%d,%d,%d,%d,%d" a b c d e in
 let (keys,locks) =
  Seq.ints 0 |> Seq.take num_slots |>
  Seq.fold_left
   (fun (keys,locks) slot ->
    if is_lock slot
    then (keys, KLSet.add (parse slot) locks)
    else (KLSet.add (parse slot) keys, locks))
  (KLSet.empty, KLSet.empty) in
 let count = ref 0 in
 KLSet.iter (fun lock ->
  KLSet.iter (fun key ->
   if valid lock key then incr count)
  keys)
 locks ;
 if debug then (
  print_endline "KEYS" ;
  KLSet.iter (fun kl -> kl_to_string kl |> print_endline) keys;
  print_endline "LOCKS" ;
  KLSet.iter (fun kl -> kl_to_string kl |> print_endline) locks);
 !count
