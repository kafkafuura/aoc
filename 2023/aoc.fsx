(* aoc.fsx *)
(* f-sharp is weird! *)

(* Helper Modules *)

(* Extension Module for Seq *)
(* exists and forall may be used instead *)
module SeqExt =
 let all (sequence : bool seq) =
  let iter = sequence.GetEnumerator() in
  let rec loop acc = match iter.Current with false -> false | true when iter.MoveNext() -> loop () | _ -> true in
  if iter.MoveNext() then loop () else false

 let map_all (f: 'T -> bool) (sequence : 'T seq) =
  let iter = sequence.GetEnumerator() in
  let rec loop acc = match f iter.Current with false -> false | true when iter.MoveNext() -> loop () | _ -> true in
  if iter.MoveNext() then loop () else false
 
 let any (sequence : bool seq) =
  let iter = sequence.GetEnumerator() in
  let rec loop acc = match iter.Current with true -> true | false when iter.MoveNext() -> loop () | _ -> false in
  if iter.MoveNext() then loop () else false

 let map_any (f: 'T -> bool) (sequence : 'T seq) =
  let iter = sequence.GetEnumerator() in
  let rec loop acc = match f iter.Current with false -> false | true when iter.MoveNext() -> loop () | _ -> true in
  if iter.MoveNext() then loop () else false

(* find the first and last digit in each line and sum *)
let problem_01a () =
 System.IO.File.ReadLines("01.txt") |>
 Seq.map (fun s ->
  s |> Seq.fold (fun (a,b) c ->
   if (int c) >= (int '0') && (int c) <= (int '9') then
    let n = (int c) - (int '0') in (Option.orElse (Some n) a, Some n)
   else (a,b)
  ) (None, None) |>
  (function (Some a, Some b) -> a * 10 + b | _ -> 0)
 ) |> Seq.sum

let problem_01b () =
 (* extract number from remaining substring *)
 let n_of_sub (s:string) =
  let numbers = [| "one" ; "two" ; "three" ; "four" ; "five" ; "six" ; "seven" ; "eight" ; "nine" |] in
  (* early exit equality test through numbers loop *)
  let rec loop i =
   if i > 9 then None else
   if s.Length >= numbers[i-1].Length && (Seq.zip s numbers[i-1] |> Seq.fold (fun a (x, y) -> a && (x = y)) true) then Some i
   else loop (i+1) in
  if s.Length >= 1 && ((int s[0]) >= (int '0')) && ((int s[0]) <= (int '9')) then Some ((int s[0]) - (int '0'))
  else loop 1
 in
 System.IO.File.ReadLines("01.txt") |>
 Seq.map (fun s ->
  seq { 0..(s.Length-1) } |>
  Seq.fold (fun a i ->
   match n_of_sub s[i..] with Some n -> n :: a | None -> a
  ) List.empty |>
  Seq.fold (fun (a,b) n -> (Option.orElse (Some n) a, Some n)) (None, None) |>
  (* because list was built in reverse, reverse the digits! *)
  (function (Some a, Some b) -> b * 10 + a | _ -> 0)
 ) |> Seq.sum


(* no local modules in f-sharp; used for problem 02 *)
module Cube =
 type t = Red | Green | Blue
 let of_string = function
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | s -> (eprintfn $"failed to parse: {s}"; failwith "cube_of_sub: invalid cube type")
 let to_string = function
  | Red   -> "red"
  | Green -> "green"
  | Blue  -> "blue"

(* sidenote: asserts do nothing in fsi *)
let problem_02a () =
 let valid rnd =
  rnd |>
  Seq.forall (function
   | (Cube.Red,   n) when n <= 12 -> true
   | (Cube.Green, n) when n <= 13 -> true
   | (Cube.Blue,  n) when n <= 14 -> true
   | _ -> false )
 in
 let parse (s:string) =
  let (lhs,rhs) = s.Split(": ") |> (fun a -> assert (a.Length >= 2) ; (a[0], a[1])) in
  let g_num = int lhs[5..] in (* "Game %i" *)
  let rounds =
   rhs.Trim().Split("; ") |>
   Seq.map (fun s ->
    s.Trim().Split(", ") |>
    Seq.map (fun s ->
     let (l,r) = s.Split(' ') |> (fun a -> assert (a.Length >= 2) ; (a[0], a[1])) in
     (Cube.of_string r, int l))) in
  (g_num, rounds) in
 System.IO.File.ReadLines("02.txt") |>
 Seq.map parse |>
 Seq.fold (fun a (id,rounds) -> if rounds |> Seq.forall valid then a + id else a) 0

let problem_02b () =
 let max_int x y = if x < y then y else x in
 let game_power (id, rounds) =
  let update_min (r,g,b) = function
   | (Cube.Red,   n) -> (max_int r n,g,b)
   | (Cube.Green, n) -> (r,max_int g n,b)
   | (Cube.Blue,  n) -> (r,g,max_int b n)
  in
  rounds |>
  Seq.concat |>
  Seq.fold update_min (0,0,0) |>
  (fun (r,g,b) -> r * g * b)
 in
 let parse (s:string) =
  let (lhs,rhs) = s.Split(": ") |> (fun a -> assert (a.Length >= 2) ; (a[0], a[1])) in
  let g_num = int lhs[5..] in (* "Game %i" *)
  let rounds =
   rhs.Trim().Split("; ") |>
   Seq.map (fun s ->
    s.Trim().Split(", ") |>
    Seq.map (fun s ->
     let (l,r) = s.Split(' ') |> (fun a -> assert (a.Length >= 2) ; (a[0], a[1])) in
     (Cube.of_string r, int l))) in
  (g_num, rounds) in
 System.IO.File.ReadLines("02.txt") |>
(* fsharp is eager evaluation, so this may not be super efficient compared to function composition *)
(*
 Seq.map parse |>
 Seq.map game_power |>
*)
 (* fake >> monad *)
 Seq.map (parse >> game_power) |>
 Seq.sum

module Vec2D =
 [<Struct>]
 type t =
   { x : int32; y : int32 }
   static member (+) (v1 : t, v2: t) = {x = v1.x + v2.x; y = v1.y + v2.y}
   static member (-) (v1 : t, v2: t) = {x = v1.x - v2.x; y = v1.y - v2.y}
   static member (~-) (v : t) = {x = 0 - v.x; y = 0 - v.y}
   static member op_Equality (v1 : t, v2: t) = v1.x = v2.x && v1.y = v2.y
 let inline add (v1: t) (v2: t) = v1 + v2
 let inline sub (v1: t) (v2: t) = v1 - v2
 let inline of_tup_xy t = {x = fst t; y = snd t}
 let inline of_tup_yx t = {x = snd t; y = fst t}

let problem_03a () =
 let is_digit c = (int c) >= (int '0') && (int c) <= (int '9') in
 let input = Seq.toArray (System.IO.File.ReadLines("03.txt")) in
 let find_nums (pt:Vec2D.t) = 
  let rec find_origin (p: Vec2D.t) : (Vec2D.t) =
   if p.x < 0 then {x = 0; y = p.y} else
   if not << is_digit <| input[p.y][p.x] then {x = p.x + 1; y = p.y} else
   find_origin {x = p.x - 1; y = p.y } in
  [ (-1,-1)  ; (0 ,-1)  ; (1 ,-1)
  ; (-1, 0)             ; (1 , 0)
  ; (-1, 1)  ; (0 , 1)  ; (1 , 1) ] |>
  List.map (Vec2D.of_tup_xy) |>
  List.map (fun p -> p + pt) |>
  List.filter (fun p -> p.x >= 0 && p.y >= 0 && p.x < input[0].Length && p.y < input.Length) |>
  List.filter (fun p -> is_digit (input[p.y][p.x])) |>
  List.map find_origin |> Set.ofList in

 let num_of_origin (p : Vec2D.t) =
  let rec fold_num a s = if Seq.isEmpty s || not << is_digit <| Seq.head s then a else fold_num (Seq.head s :: a) (Seq.tail s) in
  input[p.y][p.x..] |> fold_num [] |>
  List.fold (fun (a, factor) c -> (a + ((int c) - (int '0')) * factor, 10 * factor)) (0,1) |> fst in

 let mutable origin_set = Set.empty in
 for i = 0 to input.Length - 1 do
  for j = 0 to input[i].Length - 1 do
   if (not << is_digit <| input[i][j]) && (input[i][j] <> '.')
   then origin_set <- Set.union origin_set (find_nums {y = i; x = j}) else () ;
 Set.toList origin_set |>
 List.map num_of_origin |> List.sum

let problem_03b () =
 let is_digit c = (int c) >= (int '0') && (int c) <= (int '9') in
 let input = Seq.toArray (System.IO.File.ReadLines("03.txt")) in
 let find_nums (pt:Vec2D.t) = 
  let rec find_origin (p: Vec2D.t) : (Vec2D.t) =
   if p.x < 0 then {x = 0; y = p.y} else
   if (not << is_digit) <| input[p.y][p.x] then {x = p.x + 1; y = p.y} else
   find_origin {x = p.x - 1; y = p.y } in
  [ (-1,-1)  ; (0 ,-1)  ; (1 ,-1)
  ; (-1, 0)             ; (1 , 0)
  ; (-1, 1)  ; (0 , 1)  ; (1 , 1) ] |>
  List.map (Vec2D.of_tup_xy) |>
  List.map (fun p -> p + pt) |>
  List.filter (fun p -> p.x >= 0 && p.y >= 0 && p.x < input[0].Length && p.y < input.Length) |>
  List.filter (fun p -> is_digit <| input[p.y][p.x]) |>
  List.map find_origin |> Set.ofList in

 let num_of_origin (p : Vec2D.t) =
  let rec fold_num a s = if Seq.isEmpty s || not << is_digit <| Seq.head s then a else fold_num (Seq.head s :: a) (Seq.tail s) in
  input[p.y][p.x..] |> fold_num [] |>
  List.fold (fun (a, factor) c -> (a + ((int c) - (int '0')) * factor, 10 * factor)) (0,1) |> fst in
  
 let mutable res = 0 in
 for i = 0 to input.Length - 1 do
  for j = 0 to input[i].Length - 1 do
   if input[i][j] = '*' then
    match find_nums {y = i; x = j} with
    | s when Set.count s = 2 ->
      Set.toList s |>
      List.map num_of_origin |>
      List.fold ( * ) 1 |>
      (fun n -> res <- res + n)
    | _ -> () ;
 res

let problem_04a () =
 System.IO.File.ReadLines("04.txt") |>
 Seq.map
  (fun line ->
    let (s1,s2) =
     (line.Split(':') |> Seq.tail |> Seq.head).Split('|') |>
     (fun s -> (Seq.head s, Seq.head <| Seq.tail s)) in
    let n1 = s1.Split(' ') |> Seq.filter (fun s -> s <> "") |> Set.ofSeq in
    let n2 = s2.Split(' ') |> Seq.filter (fun s -> s <> "") |> Set.ofSeq in
    Set.intersect n1 n2 |> Set.count
  ) |>
 List.ofSeq |>
 List.filter (fun n -> n <> 0) |>
 List.map (fun n -> 1 <<< (n-1)) |>
 List.sum
 
let problem_04b () =
 let wins = 
  System.IO.File.ReadLines("04.txt") |>
  Seq.map
   (fun line ->
     let (s1,s2) =
      (line.Split(':') |> Seq.tail |> Seq.head).Split('|') |>
      (fun s -> (Seq.head s, Seq.head <| Seq.tail s)) in
     let n1 = s1.Split(' ') |> Seq.filter (fun s -> s <> "") |> Set.ofSeq in
     let n2 = s2.Split(' ') |> Seq.filter (fun s -> s <> "") |> Set.ofSeq in
     Set.intersect n1 n2 |> Set.count
   ) |> Array.ofSeq
 (* the following is "converted" from ocaml code *)
 let len = Array.length wins in
 let cards = Array.replicate len 1 in
 for i = 0 to len - 2 do
  for j = i + 1 to min (len - 1) (i + wins.[i]) do
   cards.[j] <- cards.[j] + cards.[i]
  done
 done ;
 Array.sum cards

(* successfully converted from ocaml without understanding the logic *)
(* f-sharp's default int is int32, we need int64 *)
let problem_05a () =
 let rec map_value_with_book input book =
  match book with
  | [] -> input
  | (dst,src,len)::book' when input >= src && input < src+len -> input-src+dst
  | _::book' -> map_value_with_book input book' in
 (* parse with fold_left *)
 let parse acc (line:string) =
  match acc with
  | ([], _, _) ->
    let seeds =
     line.Split(':')|>
     Seq.tail |> Seq.head |> (fun s -> s.Split(' ')) |>
     Seq.filter (fun s -> s <> "") |> Seq.map (fun s -> int64 s) |> Seq.toList in
     (seeds, [], [])
  | _ when line = "" -> acc
  | (seeds, [], almanac) when String.exists (fun c -> c = ':') line -> (seeds, [], almanac)
  | (seeds, book, almanac) when String.exists (fun c -> c = ':') line -> (seeds, [], book::almanac)
  | (seeds, book, almanac) ->
    let entry =
     (match line.Split(' ') |> Seq.filter(fun s -> s <> "") |> Seq.map (fun s -> int64 s) |> Seq.toList with
      [a;b;c] -> (a,b,c) | _ -> failwith "invalid input: expected 3 numbers per map entry") in
    (seeds, (entry::book), almanac) in
 let lines = Seq.toArray <| System.IO.File.ReadLines("05.txt") in
 let (seeds, last_book, almanac) = Array.fold parse ([],[],[]) lines in
 let almanac = last_book::almanac |> List.rev in
 List.map (fun seed -> List.fold map_value_with_book seed almanac) seeds |>
 List.fold (min) System.Int64.MaxValue

let problem_05b () =
 let rec map_value_with_book input book =
  match book with
  | [] -> input
  | (dst,src,len)::book' when input >= src && input < src+len -> input-src+dst
  | _::book' -> map_value_with_book input book' in
 let rec map_value_with_book_inverted input book =
  match book with
  | [] -> input
  | (src,dst,len)::book' when input >= src && input < src+len -> input-src+dst
  | _::book' -> map_value_with_book_inverted input book' in
 (* range folding algorithm : assuming continuity *)
 let get_new_layer layer1 layer2 =
  let top_bounds = List.fold (fun acc (_,src,len) -> src::src+len::acc) [] layer1 |> List.sort |> List.distinct in
  let mid_bounds = List.fold (fun acc (_,src,len) -> src::src+len::acc) [] layer2 |> List.sort |> List.distinct in
  let new_mid_bounds = List.map (fun pt -> map_value_with_book_inverted pt layer1) mid_bounds in
  let new_srcs = new_mid_bounds @ top_bounds |> List.sort |> List.distinct in
  let new_dsts = Seq.map (fun pt -> List.fold map_value_with_book pt [layer1;layer2]) new_srcs in
  let new_lens = Seq.map2 (fun x y -> x - y) (Seq.skip 1 new_srcs) new_srcs in
  let new_layer = Seq.map2 (fun (a,b) c -> (a,b,c)) (Seq.zip new_dsts new_srcs) (new_lens) in
  List.ofSeq new_layer in
 let seeds_to_seed_ranges acc seed =
  match acc with
  | (Some seed_start, ranges) -> (None, (seed_start, seed)::ranges)
  | (None, ranges) -> (Some seed, ranges) in
 let rec is_valid_seed s seed_ranges =
  match seed_ranges with
  | [] -> false
  | (src,len)::srs when s >= src && s < src+len -> true
  | _::srs -> is_valid_seed s srs in
 (* parse with fold_left *)
 let parse acc (line:string) =
  match acc with
  | ([], _, _) ->
    let seeds =
     line.Split(':' )|>
     Seq.tail |> Seq.head |> (fun s -> s.Split(' ')) |>
     Seq.filter (fun s -> s <> "") |> Seq.map (fun s -> int64 s) |> Seq.toList in
     (seeds, [], [])
  | _ when line = "" -> acc
  | (seeds, [], almanac) when String.exists (fun c -> c = ':') line -> (seeds, [], almanac)
  | (seeds, book, almanac) when String.exists (fun c -> c = ':') line -> (seeds, [], book::almanac)
  | (seeds, book, almanac) ->
    let entry =
     (match line.Split(' ') |> Seq.filter(fun s -> s <> "") |> Seq.map (fun s -> int64 s) |> Seq.toList with
      [a;b;c] -> (a,b,c) | _ -> failwith "invalid input: expected 3 numbers per map entry") in
    (seeds, (entry::book), almanac) in
 let lines = Seq.toArray <| System.IO.File.ReadLines("05.txt") in
 let (seeds, last_book, almanac) = Array.fold parse ([],[],[]) lines in
 let almanac = last_book::almanac |> List.rev in
 let (_,seed_ranges) = List.fold seeds_to_seed_ranges (None,[]) seeds in
 let almanac_opt = List.fold (get_new_layer) (List.head almanac) (List.tail almanac) in
 (* minimum can only exist on lower bounds of inflection points! *)
 let candidates_type1 = List.map (fun (start,len) -> map_value_with_book start almanac_opt) seed_ranges in
 let candidates_type2 =
  List.map (fun (dst,src,len) -> if is_valid_seed src seed_ranges then Some dst else None) almanac_opt |>
  List.filter Option.isSome |> List.map (Option.get) in
 let min_type1 = List.fold (min) System.Int64.MaxValue candidates_type1 in 
 let min_type2 = List.fold (min) System.Int64.MaxValue candidates_type2 in 
 min min_type1 min_type2

let problem_06a () =
 let wins t d =
  let tf = float t in
  let df = float d in
  let det = System.Math.Sqrt(0.25 * tf * tf - df) in
  (* logic : if det is a whole number, exclude edges *)
  let low  = int (  System.Double.Floor(0.5 * tf - det)) + 1 in
  let high = int (System.Double.Ceiling(0.5 * tf + det)) - 1 in
  high - low + 1
 System.IO.File.ReadLines("06.txt") |>
 Seq.map (fun s ->
  s.Split(':').[1].Split(' ') |> Seq.filter (fun s -> s <> "") |>
  Seq.map (fun s -> int s)) |>
 (fun res -> Seq.zip (Seq.head res) (Seq.head <| Seq.tail res)) |>
 Seq.map (fun tup -> tup ||> wins) |>
 Seq.fold ( * ) 1

let problem_06b () =
 let wins t d =
  let tf = float t in
  let df = float d in
  let det = System.Math.Sqrt(0.25 * tf * tf - df) in
  (* logic : if det is a whole number, exclude edges *)
  let low  = int (  System.Double.Floor(0.5 * tf - det)) + 1 in
  let high = int (System.Double.Ceiling(0.5 * tf + det)) - 1 in
  high - low + 1
 System.IO.File.ReadLines("06.txt") |>
 Seq.map (fun s ->
  s.Split(':').[1].Split(' ') |> Seq.filter (fun s -> s <> "") |>
  (* add single line for part b : everything else is identical *)
  String.concat "" |> Seq.singleton |>
  Seq.map (fun s -> int64 s)) |>
 (fun res -> Seq.zip (Seq.head res) (Seq.head <| Seq.tail res)) |>
 Seq.map (fun tup -> tup ||> wins) |>
 Seq.fold ( * ) 1

(* ocaml to fs conversion *)
let problem_07a () =
 let card_to_number card =
  match card with
  | 'T' -> 8 | 'J' -> 9 | 'Q' -> 10 | 'K' -> 11 | 'A' -> 12
  | card when (int card) >= (int '2') && (int card) <= (int '9') -> (int card) - 0x32
  | _ -> failwith "invalid card" in
 let hand_rank hand =
  let set = Array.replicate 13 0 in
  String.iter (fun c -> let n = card_to_number c in set.[n] <- set.[n] + 1) hand;
  Array.sortInPlaceWith (fun x y -> compare y x) set;
  if set.[0] = 5 then 6 else
  if set.[0] = 4 then 5 else
  if set.[0] = 3 && set.[1] = 2 then 4 else
  if set.[0] = 3 then 3 else
  if set.[0] = 2 && set.[1] = 2 then 2 else
  if set.[0] = 2 then 1 else 0 in
 let compare_hand hand1 hand2 =
  match compare (hand_rank hand1) (hand_rank hand2) with
  | 0 -> Seq.compareWith (fun x y -> compare (card_to_number x) (card_to_number y)) hand1 hand2
  | res -> res in
 let parse_line (line:string) =
  match line.Split(' ') |> Seq.filter (fun s -> s <> "") |> Seq.toList with
  | [hand; bid] -> (hand, int bid)
  | _ -> failwith "invalid [hand;bid]"  in
 System.IO.File.ReadLines("07.txt") |> Seq.map parse_line |> Array.ofSeq |>
 Array.sortWith (fun (h1,_) (h2,_) -> compare_hand h1 h2) |>
 Array.fold (fun (acc,i) (_,x) -> (acc + i * x, i + 1)) (0,1) |> fst

(* ocaml to fs conversion *)
let problem_07b () =
 let card_to_number card =
  match card with
  | 'T' -> 9 | 'J' -> 0 | 'Q' -> 10 | 'K' -> 11 | 'A' -> 12
  | card when (int card) >= (int '2') && (int card) <= (int '9') -> (int card) - 0x31
  | _ -> failwith "invalid card" in
 let hand_rank hand =
  let set = Array.replicate 13 0 in
  String.iter (fun c -> let n = card_to_number c in set.[n] <- set.[n] + 1) hand;
  let jokers = set.[0] in
  set.[0] <- 0; (* reset joker count *)
  Array.sortInPlaceWith (fun x y -> compare y x) set;
  if set.[0] + jokers = 5 then 6 else
  if set.[0] + jokers = 4 then 5 else
  if set.[0] + jokers = 3 && set.[1] = 2 then 4 else
  if set.[0] + jokers = 3 then 3 else
  if set.[0] + jokers = 2 && set.[1] = 2 then 2 else
  if set.[0] + jokers = 2 then 1 else 0 in
 let compare_hand hand1 hand2 =
  match compare (hand_rank hand1) (hand_rank hand2) with
  | 0 -> Seq.compareWith (fun x y -> compare (card_to_number x) (card_to_number y)) hand1 hand2
  | res -> res in
 let parse_line (line:string) =
  match line.Split(' ') |> Seq.filter (fun s -> s <> "") |> Seq.toList with
  | [hand; bid] -> (hand, int bid)
  | _ -> failwith "invalid [hand;bid]"  in
 System.IO.File.ReadLines("07.txt") |> Seq.map parse_line |> Array.ofSeq |>
 Array.sortWith (fun (h1,_) (h2,_) -> compare_hand h1 h2) |>
 Array.fold (fun (acc,i) (_,x) -> (acc + i * x, i + 1)) (0,1) |> fst

let problem_08a () =
 let mutable d_ptr = 0 in
 let mutable map_addr = "AAA" in
 let mutable step = 0 in
 let (instr, map) =
  System.IO.File.ReadLines("08.txt") |> Seq.toArray |>
  (fun arr -> (arr[0], arr[2..] |> Array.map (fun s -> (s[0..2],s[7..9],s[12..14])))) in
 while map_addr <> "ZZZ" do
  step <- step + 1 ;
  let (_,l,r) = Array.find (fun (s,_,_) -> s = map_addr) map in
  map_addr <- (match instr[d_ptr] with 'L' -> l | 'R' -> r | _ -> failwith "invalid input: [LR]") ;
  d_ptr <- (d_ptr + 1) % instr.Length
 done;
 step

let problem_08b () =
 let mutable d_ptr = 0 in
 let (instr, map) =
  System.IO.File.ReadLines("08.txt") |> Seq.toArray |>
  (fun arr -> (arr[0], arr[2..] |> Array.map (fun s -> (s[0..2],s[7..9],s[12..14])))) in
 let addrs = map |> Array.map (fun (s:string,_,_) -> if s[2] = 'Z' then Some s else None) |> Array.filter Option.isSome |> Array.map Option.get in
 let cycles = Array.replicate (addrs.Length) 0 in
 (* do not stop until cycles collide on a factor of instr.Length, @ offset = 0 *)
 (* cycles happen to be relative primes - factor of instr.Length so just multiply after removing common factor *)
 (* with a less well-behaved problem set, offsets may need to be considered *)
 let mutable ptr = "" in
 for i = 0 to cycles.Length - 1 do
  d_ptr <- 0;
  cycles[i] <- cycles[i] + 1 ;
  let (_,l,r) = Array.find (fun (s,_,_) -> s = addrs[i]) map in
  ptr <- (match instr[d_ptr] with 'L' -> l | 'R' -> r | _ -> failwith "invalid input: [LR]") ;
  d_ptr <- (d_ptr + 1) % instr.Length
  while addrs[i] <> ptr || d_ptr <> 0 do
   cycles[i] <- cycles[i] + 1 ;
   let (_,l,r) = Array.find (fun (s,_,_) -> s = ptr) map in
   ptr <- (match instr[d_ptr] with 'L' -> l | 'R' -> r | _ -> failwith "invalid input: [LR]") ;
   d_ptr <- (d_ptr + 1) % instr.Length
  done
 done ;
 cycles |>
 Array.map (fun n -> n / instr.Length) |>
 Array.fold (fun a i -> int64 i * a) (int64 instr.Length)

(* value interpolation *)
let problem_09a () =
 let interpolate (line: int array) =
  let diff = Array2D.create (line.Length) (line.Length) 0 in
  (* blit line *)
  diff[0,0..] <- line ;
  (* calculate differences *)
  for i = 1 to line.Length - 1 do
   for j = 0 to line.Length - i - 1 do
    diff[i,j] <- diff[i-1,j+1] - diff[i-1,j] done done ;
  (* find first non-zero row fron the bottom == constant row *)
  let const_row =
   ( let rec loop i = if diff.[i,0..] |> Seq.forall (fun n -> n = 0) then loop (i-1) else i in loop (line.Length - 1) ) in
  (* extend const_row by 1 *)
  diff[const_row, line.Length - const_row] <- diff[const_row, line.Length - const_row - 1] ;
  (* construct diagonal *)
  for i = const_row - 1 downto 1 do diff[i,line.Length - i] <- diff[i,line.Length - 1 - i] + diff[i+1,line.Length - 1 - i] done ;
  diff[0,line.Length - 1] + diff[1,line.Length - 1] in

 System.IO.File.ReadLines("09.txt") |>
 Seq.map (fun s -> s.Split(' ') |> Seq.map (int) |> Seq.toArray) |> Seq.toArray |>
 Array.map interpolate |> Array.sum

let problem_09b () =
 let interpolate (line: int array) =
  let diff = Array2D.create (line.Length) (line.Length) 0 in
  (* blit line *)
  diff[0,0..] <- line ;
  (* calculate differences *)
  for i = 1 to line.Length - 1 do
   for j = 0 to line.Length - i - 1 do
    diff[i,j] <- diff[i-1,j+1] - diff[i-1,j] done done ;
  (* find first non-zero row fron the bottom == constant row *)
  let const_row =
   ( let rec loop i = if diff.[i,0..] |> Seq.forall (fun n -> n = 0) then loop (i-1) else i in loop (line.Length - 1) ) in
  (* extend const_row by 1 *)
  diff[const_row, line.Length - const_row] <- diff[const_row, line.Length - const_row - 1] ;
  (* construct diagonal *)
  for i = const_row - 1 downto 1 do diff[i,line.Length - i] <- diff[i,line.Length - 1 - i] + diff[i+1,line.Length - 1 - i] done ;
  diff[0,line.Length - 1] + diff[1,line.Length - 1] in

 System.IO.File.ReadLines("09.txt") |>
 (* all you need to do is reverse the input lines *)
 Seq.map (fun s -> s.Split(' ') |> Seq.map (int) |> Seq.rev |> Seq.toArray) |> Seq.toArray |>
 Array.map interpolate |> Array.sum

(* pipe map w/o T sections *)
let problem_10a () =
 let list_filteri (f : int -> 'T -> bool) (lst:list<'T>) : (list<'T>) =
   Seq.zip (seq {0 .. (lst.Length - 1)}) lst |>
   Seq.filter (fun iel -> iel ||> f) |> Seq.map (snd) |> Seq.toList in
 let debug = false in
 let map = System.IO.File.ReadLines("10.txt") |> Seq.toArray in
 let (sy,sx) =
  (let rec loop y x = if map.[y].[x] = 'S' then (y,x) else if (x+1) >= String.length map.[y] then loop (y+1) 0 else loop y (x+1) in loop 0 0) in
 let get_opt (y,x) = if y >= 0 && y < Array.length map && x >= 0 && x < String.length map.[y] then Some map.[y].[x] else None in
 let next_of_S (y,x) =
  let valids = [|"|F7";"-LF";"-7J";"|LJ"|] in
  [y-1,x;y,x-1;y,x+1;y+1,x] |>
  List.map (fun (y,x) -> (y, x, get_opt (y,x))) |>
  list_filteri (fun i (y,x,v) -> Option.isSome v && String.exists (fun c -> Option.get v = c) valids.[i]) |>
  List.head |> (fun (y',x',v) -> (y,x,y',x',v)) in
 let next_of_loc (y,x,y',x',v) =
  let dy = y'-y in
  let dx = x'-x in
  match v with
  | Some '|' | Some '-' -> (y',x',y'+dy,x'+dx,get_opt (y'+dy,x'+dx))
  | Some '7' | Some 'L' -> (y',x',y'+dx,x'+dy,get_opt (y'+dx,x'+dy))
  | Some 'J' | Some 'F' -> (y',x',y'-dx,x'-dy,get_opt (y'-dx,x'-dy))
  | Some 'S' -> (y',x',y',x',get_opt (y',x'))
  | _ -> failwith "Assertion Error" in
 let print_ptr (y,x,y',x',v) =
  printf "(%d,%d) -> (%d,%d): %c\n" y x y' x' (Option.get v) in
 let is_S (_,_,_,_,v) = Option.get v = 'S' in
 let ptr = ref (next_of_S (sy,sx)) in
 let acc = ref 0 in
 while is_S ptr.Value |> not do
  acc.Value <- acc.Value + 1;
  if debug then print_ptr ptr.Value;
  ptr.Value <- next_of_loc ptr.Value;
 done;
 if debug then printf "acc: %d\n" acc.Value;
 (acc.Value+1)/2

(* pipe map w/o T sections *)
let problem_10b () =
 let list_filteri (f : int -> 'T -> bool) (lst:list<'T>) : (list<'T>) =
   Seq.zip (seq {0 .. (lst.Length - 1)}) lst |>
   Seq.filter (fun iel -> iel ||> f) |> Seq.map (snd) |> Seq.toList in
 let debug = false in
 let map = System.IO.File.ReadLines("10.txt") |> Seq.toArray in
 let (sy,sx) =
  (let rec loop y x = if map.[y].[x] = 'S' then (y,x) else if (x+1) >= String.length map.[y] then loop (y+1) 0 else loop y (x+1) in loop 0 0) in
 let get_opt (y,x) = if y >= 0 && y < Array.length map && x >= 0 && x < String.length map.[y] then Some map.[y].[x] else None in
 let next_of_S (y,x) =
  let valids = [|"|F7";"-LF";"-7J";"|LJ"|] in
  [y-1,x;y,x-1;y,x+1;y+1,x] |>
  List.map (fun (y,x) -> (y, x, get_opt (y,x))) |>
  list_filteri (fun i (y,x,v) -> Option.isSome v && String.exists (fun c -> Option.get v = c) valids.[i]) |>
  List.head |> (fun (y',x',v) -> (y,x,y',x',v)) in
 let next_of_loc (y,x,y',x',v) =
  let dy = y'-y in
  let dx = x'-x in
  match v with
  | Some '|' | Some '-' -> (y',x',y'+dy,x'+dx,get_opt (y'+dy,x'+dx))
  | Some '7' | Some 'L' -> (y',x',y'+dx,x'+dy,get_opt (y'+dx,x'+dy))
  | Some 'J' | Some 'F' -> (y',x',y'-dx,x'-dy,get_opt (y'-dx,x'-dy))
  | Some 'S' -> (y',x',y',x',get_opt (y',x'))
  | _ -> failwith "Assertion Error" in
 let print_ptr (y,x,y',x',v) =
  printf "(%d,%d) -> (%d,%d): %c\n" y x y' x' (Option.get v) in
 let is_S (_,_,_,_,v) = Option.get v = 'S' in

 (* redo part 1 but mark pipes without counting steps *)
 let is_main_loop = Array2D.create (Array.length map) (String.length map.[0]) false in
 is_main_loop.[sy,sx] <- true;
 let mark_ptr (_,_,y',x',_) = is_main_loop.[y',x'] <- true in
 let mutable ptr = next_of_S (sy,sx) in
 while is_S ptr |> not do
  mark_ptr ptr;
  ptr <- next_of_loc ptr;
 done;
 (* determine what shape S should be *)
 let shape_of_S =
  match get_opt (sy-1,sx), get_opt (sy,sx+1), get_opt (sy+1,sx), get_opt (sy,sx-1) with
  | Some n, _, Some s, _ when  (n = '|' || n = 'F' || n = '7') && (s = '|' || s = 'L' || s = 'J') -> '|'
  | Some n, Some e, _, _ when  (n = '|' || n = 'F' || n = '7') && (e = '-' || e = 'J' || e = '7') -> 'L'
  | Some n, _, _, Some w when  (n = '|' || n = 'F' || n = '7') && (w = '-' || w = 'L' || w = 'F') -> 'J'
  | _, Some e, _, Some w when  (w = '-' || w = 'L' || w = 'F') && (e = '-' || e = 'J' || e = '7') -> '-'
  | _, _, Some s, Some w when  (w = '-' || w = 'L' || w = 'F') && (s = '|' || s = 'L' || s = 'J') -> '7'
  | _, Some e, Some s, _ when  (e = '-' || e = 'J' || e = '7') && (s = '|' || s = 'L' || s = 'J') -> 'F'
  | _ -> failwith "Assertion Failure" in
 (* generate sub-pixel map, ignoring sections of pipe not in main loop *)
 let sub_map = Array2D.create (Array.length map * 2 + 1) (String.length map.[0] * 2 + 1) '.' in
 let get_opt_sub (y,x) = if y >= 0 && y < Array2D.length1 sub_map && x >= 0 && x < Array2D.length2 sub_map then Some sub_map.[y,x] else None in
 if debug then Printf.printf "sub_map: h = %d, w = %d\n" (Array2D.length1 sub_map) (Array2D.length2 sub_map);
 for y = 1 to Array2D.length1 sub_map - 2 do
  for x = 1 to Array2D.length2 sub_map - 2 do
   if y &&& 1 = 1 && x &&& 1 = 1 && is_main_loop.[y/2,x/2] then sub_map.[y,x] <- map.[y/2].[x/2] else
   if y &&& 1 = 1 then (
    match get_opt (y/2,(x-1)/2), get_opt (y/2,(x+1)/2), is_main_loop.[y/2,(x-1)/2] && is_main_loop.[y/2,(x+1)/2] with
    | Some w, Some e, true ->
     if (w = 'F' || w = 'L' || w = '-') && (e = '7' || e = 'J' || e = '-') then sub_map.[y,x] <- '-'
    | _ -> ()
   ) else
   if x &&& 1 = 1 then (
    match get_opt ((y-1)/2,x/2), get_opt ((y+1)/2,x/2), is_main_loop.[(y-1)/2,x/2] && is_main_loop.[(y+1)/2,x/2] with
    | Some n, Some s, true ->
     if (n = 'F' || n = '7' || n = '|') && (s = 'L' || s = 'J' || s = '|') then sub_map.[y,x] <- '|'
    | _ -> ()
   ) done done;
 (* fill in S + surrounding pipe on sub-pixel map *)
 sub_map.[2*sy+1,2*sx+1] <- shape_of_S;
 (* starting point may differ depending on input: guess on S's acute angle *)
 (* iy,ix *can* be (0,0), but if we guess right, the recursion should be more efficient *)
 let (iy,ix) =
  if shape_of_S = '|' then ( sub_map.[2*sy+0,2*sx+1] <- '|'; sub_map.[2*sy+2,2*sx+1] <- '|' ; (2*sy+1,2*sx+0) ) else
  if shape_of_S = 'L' then ( sub_map.[2*sy+0,2*sx+1] <- '|'; sub_map.[2*sy+1,2*sx+2] <- '-' ; (2*sy+0,2*sx+2) ) else
  if shape_of_S = 'J' then ( sub_map.[2*sy+0,2*sx+1] <- '|'; sub_map.[2*sy+1,2*sx+0] <- '-' ; (2*sy+0,2*sx+0) ) else
  if shape_of_S = '-' then ( sub_map.[2*sy+1,2*sx+0] <- '-'; sub_map.[2*sy+1,2*sx+2] <- '-' ; (2*sy+0,2*sx+1) ) else
  if shape_of_S = '7' then ( sub_map.[2*sy+1,2*sx+0] <- '-'; sub_map.[2*sy+2,2*sx+1] <- '|' ; (2*sy+2,2*sx+0) ) else
  if shape_of_S = 'F' then ( sub_map.[2*sy+1,2*sx+2] <- '-'; sub_map.[2*sy+2,2*sx+1] <- '|' ; (2*sy+2,2*sx+2) ) else failwith "Assertion Error" in
 if debug then for i = 0 to Array2D.length1 sub_map - 1 do for j = 0 to Array2D.length2 sub_map - 1 do printf "%c" sub_map.[i,j] done; printfn "" done;
 if debug then printfn "##########" ;
 (* recursive fill *)
 let rec mark_I_sub (y,x) =
  match get_opt_sub (y,x) with
  | Some '.' ->
   sub_map.[y,x] <- 'I';
   mark_I_sub (y-1,x);
   mark_I_sub (y,x+1);
   mark_I_sub (y+1,x);
   mark_I_sub (y,x-1)
  | _ -> () in
 mark_I_sub (iy,ix);
 if debug then for i = 0 to Array2D.length1 sub_map - 1 do for j = 0 to Array2D.length2 sub_map - 1 do printf "%c" sub_map.[i,j] done; printfn "" done;
 let mutable acc = 0 in
 (* if our choice of iy,ix was wrong, check for empty spaces instead! *)
 let inner_marker = if sub_map.[0,0] = 'I' then '.' else 'I' in
 for i = 0 to Array.length map - 1 do
  for j = 0 to String.length map.[0] - 1 do
   if sub_map[2*i+1,2*j+1] = inner_marker then acc <- acc + 1 done done;
 acc

let problem_11a () =
 let debug = true in
 let map = System.IO.File.ReadLines("11.txt") |> Seq.toArray in
 let dim1 = map.Length in
 let dim2 = map.[0].Length in
 let is_empty_row y = map.[y] |> Seq.forall (fun c -> c = '.') in
 let is_empty_col x = map |> Seq.map (fun s -> s.[x]) |> Seq.forall (fun c -> c = '.') in
 if debug then 
  printfn "Empty Rows:"
  for y = 0 to dim1 - 1 do
   if is_empty_row y then printfn $"{y}" else () done
  printfn "Empty Cols:"
  for x = 0 to dim2 - 1 do
   if is_empty_col x then printfn $"{x}" else () done ;
 let mutable rx = 0 in
 let mutable ry = 0 in
 let mutable galaxy_list = [] in
 let expansion_factor = 1 in
 for y = 0 to dim1 - 1 do
  if is_empty_row y then ry <- ry + expansion_factor else
   rx <- 0 ;
   for x = 0 to dim2 - 1 do
    if is_empty_col x then rx <- rx + expansion_factor else
     if map.[y].[x] = '#' then galaxy_list <- (ry,rx) :: galaxy_list
    rx <- rx + 1
   done
  ry <- ry + 1
 done
 let rec fold_manhattan a = function
  | h :: t -> fold_manhattan (t |> Seq.fold (fun a (y,x) -> a + abs ((fst h) - y) + abs ((snd h) - x)) a) t
  | _ -> a
 in
 galaxy_list |> fold_manhattan 0

let problem_11b () =
 let debug = true in
 let map = System.IO.File.ReadLines("11.txt") |> Seq.toArray in
 let dim1 = map.Length in
 let dim2 = map.[0].Length in
 let is_empty_row y = map.[y] |> Seq.forall (fun c -> c = '.') in
 let is_empty_col x = map |> Seq.map (fun s -> s.[x]) |> Seq.forall (fun c -> c = '.') in
 if debug then 
  printfn "Empty Rows:"
  for y = 0 to dim1 - 1 do
   if is_empty_row y then printfn $"{y}" else () done
  printfn "Empty Cols:"
  for x = 0 to dim2 - 1 do
   if is_empty_col x then printfn $"{x}" else () done ;
 let mutable rx = 0 in
 let mutable ry = 0 in
 let mutable galaxy_list = [] in
 let expansion_factor = 1000000 - 1 in
 for y = 0 to dim1 - 1 do
  if is_empty_row y then ry <- ry + expansion_factor else
   rx <- 0 ;
   for x = 0 to dim2 - 1 do
    if is_empty_col x then rx <- rx + expansion_factor else
     if map.[y].[x] = '#' then galaxy_list <- (ry,rx) :: galaxy_list
    rx <- rx + 1
   done
  ry <- ry + 1
 done
 let rec fold_manhattan a = function
  | h :: t -> fold_manhattan (t |> Seq.fold (fun a (y,x) -> a + abs ((fst h) - y) + abs ((snd h) - x)) a) t | _ -> a in
 (* requires int64 *)
 galaxy_list |> List.map (fun (y,x) -> (int64 y,int64 x)) |> fold_manhattan 0L

(* dynamic programming problem *)
let problem_12a () =
 let solve springs param =
  let springs = springs + "." in
  let param = Array.append param [|0|] in
  let dyn = Array3D.create springs.Length param.Length (Seq.max param + 1) 0 in
  if springs.[0] <> '.' then dyn.[0,0,1] <- 1 ; (* cur = 1 is valid at start *)
  if springs.[0] <> '#' then dyn.[0,0,0] <- 1 ; (* cur = 0 is valid at start *)
  for i = 1 to Array3D.length1 dyn - 1 do
   for j = 0 to Array3D.length2 dyn - 1 do
    for k = 0 to Array3D.length3 dyn - 1 do
     let if_dot  = if k =  0 then (if j <> 0 then dyn[i-1,j-1,param.[j-1]] else 0) + dyn[i-1,j,k] else 0 in
     let if_hash = if k <> 0 then dyn[i-1,j,k-1] else 0 in
     if springs.[i] = '.' then dyn[i,j,k] <- if_dot  else
     if springs.[i] = '#' then dyn[i,j,k] <- if_hash else
     if springs.[i] = '?' then dyn[i,j,k] <- if_dot + if_hash done done done ;
  dyn.[Array3D.length1 dyn - 1, Array3D.length2 dyn - 1, 0] in
 System.IO.File.ReadLines("12.txt") |>
 Seq.map (fun s -> s.Split(' ') |> (fun a -> (a.[0], a.[1].Split(',') |> Array.map (int)))) |>
 Seq.map (fun el -> el ||> solve) |> Seq.sum

(* dynamic programming problem *)
(* requires uint64 *)
let problem_12b () =
 let solve springs param =
  let springs = springs + "." in
  let param = Array.append param [|0|] in
  let dyn = Array3D.create springs.Length param.Length (Seq.max param + 1) 0UL in
  if springs.[0] <> '.' then dyn.[0,0,1] <- 1UL ; (* cur = 1 is valid at start *)
  if springs.[0] <> '#' then dyn.[0,0,0] <- 1UL ; (* cur = 0 is valid at start *)
  for i = 1 to Array3D.length1 dyn - 1 do
   for j = 0 to Array3D.length2 dyn - 1 do
    for k = 0 to Array3D.length3 dyn - 1 do
     let if_dot  = if k =  0 then (if j <> 0 then dyn[i-1,j-1,param.[j-1]] else 0UL) + dyn[i-1,j,k] else 0UL in
     let if_hash = if k <> 0 then dyn[i-1,j,k-1] else 0UL in
     if springs.[i] = '.' then dyn[i,j,k] <- if_dot  else
     if springs.[i] = '#' then dyn[i,j,k] <- if_hash else
     if springs.[i] = '?' then dyn[i,j,k] <- if_dot + if_hash done done done ;
  dyn.[Array3D.length1 dyn - 1, Array3D.length2 dyn - 1, 0] in
 let extend_s s = s + "?" + s + "?" + s + "?" + s + "?" + s in
 let extend_p p =
  let len = Array.length p in
  seq {0..(len * 5 - 1)} |> Seq.map (fun i -> p.[i % len] ) |> Seq.toArray in
 System.IO.File.ReadLines("12.txt") |>
 Seq.map (fun s -> s.Split(' ') |> (fun a -> (extend_s a.[0], a.[1].Split(',') |> Array.map (int) |> extend_p))) |>
 Seq.map (fun el -> el ||> solve) |> Seq.sum

let problem_13a () =
 let is_reflection_y (map : string array) y =
  let len = min y (map.Length - y) in
  seq { for i = y-len to y-1 do map.[i] } |>
  Seq.zip (seq { for i = y+len-1 downto y do map.[i] }) |>
  Seq.forall (fun el -> el ||> (=))
 let is_reflection_x (map : string array) x =
  let len = min x (map.[0].Length - x) in
  map |>
  Seq.forall
   (fun line ->
    line.[(x-len)..(x-1)] |> (* left *)
    Seq.zip (Seq.rev line.[x..(x+len-1)]) |> (* right reversed *)
    Seq.forall (fun (a,b) -> a = b)) in
 let map_to_num (map : string array) =
  let mutable acc = 0 in
  (* 1 means "between 1 and 2", (1-indexed) *)
  for i = 1 to map.Length - 1 do if is_reflection_y map i then acc <- acc + 100 * i ; done
  for i = 1 to map.[0].Length - 1 do if is_reflection_x map i then acc <- acc + i ; done
  acc
 System.IO.File.ReadLines("13.txt") |>
 Seq.fold (fun (maps, map) line -> if line = "" then ((map |> List.rev |> List.toArray)::maps,[]) else (maps,line::map)) ([],[]) ||>
 (fun maps last -> (Seq.rev last |> Seq.toArray) :: maps) |>
 Seq.map map_to_num |> Seq.sum

(* differences = 1 not 0 *)
let problem_13b () =
 let is_reflection_y (map : string array) y =
  let len = min y (map.Length - y) in
  seq { for i = y-len to y-1 do map.[i] } |>
  Seq.zip (seq { for i = y+len-1 downto y do map.[i] }) |>
  Seq.map (fun lines -> lines ||> Seq.zip |> Seq.map (fun cs -> cs ||> compare |> abs) |> Seq.sum) |>
  Seq.sum |> (=) 1 in
 let is_reflection_x (map : string array) x =
  let len = min x (map.[0].Length - x) in
  map |>
  Seq.map
   (fun line ->
    line.[(x-len)..(x-1)] |> (* left *)
    Seq.zip (Seq.rev line.[x..(x+len-1)]) |> (* right reversed *)
    Seq.map (fun el -> el ||> (compare) |> abs) |> Seq.sum) |> Seq.sum |>
  (=) 1 in
 let map_to_num (map : string array) =
  let mutable acc = 0 in
  (* 1 means "between 1 and 2", (1-indexed) *)
  for i = 1 to map.Length - 1 do if is_reflection_y map i then acc <- acc + 100 * i ; done
  for i = 1 to map.[0].Length - 1 do if is_reflection_x map i then acc <- acc + i ; done
  acc
 System.IO.File.ReadLines("13.txt") |>
 Seq.fold (fun (maps, map) line -> if line = "" then ((map |> List.rev |> List.toArray)::maps,[]) else (maps,line::map)) ([],[]) ||>
 (fun maps last -> (Seq.rev last |> Seq.toArray) :: maps) |>
 Seq.map map_to_num |> Seq.sum

let problem_14a () =
 (* incredibly annoying to implement this via "keys" like python rather than a compare function *)
 let group (it:char seq) : char seq seq =
  Seq.zip it
   (* key generation by windows *)
   (it |> Seq.windowed 2 |>
   Seq.map (fun w -> w.[0] <> '#' && w.[1] <> '#') |>
   Seq.fold (fun (a,i) b -> if b then (i::a,i) else (i::a,i+1)) ([],0) |>
   (fun (a,b) -> b::a |> Seq.rev)) |>
  Seq.groupBy (snd) |> Seq.map (snd) |> Seq.map (Seq.map (fst)) in
 let reorder (forward:bool) (it:char seq) : char seq =
  let l_char = if forward then '.' else 'O' in
  let r_char = if forward then 'O' else '.' in
  it |> group |>
  Seq.collect
   (fun grp ->
    if Seq.head grp = '#' then grp else
     let count = Seq.fold (fun a c -> if c = l_char then a + 1 else a) 0 grp in
     Seq.append (Seq.init count (fun _ -> l_char)) (Seq.init (Seq.length grp - count) (fun _ -> r_char))) in

 let roll_we (forward:bool) (map:char[,]): unit =
  for i in 0..(Array2D.length1 map - 1) do
   map[i, 0..(Array2D.length2 map - 1)] <- Seq.toArray (map[i,0..(Array2D.length2 map - 1)] |> reorder forward)
  done

 (* vertical slicing is convenient *)
 let roll_ns (forward:bool) (map:char[,]): unit =
  for j in 0..(Array2D.length2 map - 1) do
   map[0..(Array2D.length1 map - 1),j] <- Seq.toArray (map[0..(Array2D.length1 map - 1),j] |> reorder forward)
  done

 let weight (map:char[,]) : int =
  let mutable acc = 0 in
  for i = 0 to Array2D.length1 map - 1 do
   for j = 0 to Array2D.length2 map - 1 do
    if map[i,j] = 'O' then acc <- acc + Array2D.length1 map - i ;
  acc in

 let input = System.IO.File.ReadLines("14.txt") |> Seq.toArray in
 let map = Array2D.init input.Length input.[0].Length (fun i j -> input.[i].[j]) in
 roll_ns false map;
 weight map

(* slow, non-smart version *)
let problem_14b () =
 (* incredibly annoying to implement this via "keys" like python rather than a compare function *)
 (* furthermore, this groupBy works like "partition" assuming *every key is unique* to a group *)
 let group (it:char seq) : char seq seq =
  Seq.zip it
   (* key generation by windows *)
   (it |> Seq.windowed 2 |>
   Seq.map (fun w -> w.[0] <> '#' && w.[1] <> '#') |>
   Seq.fold (fun (a,i) b -> if b then (i::a,i) else (i::a,i+1)) ([],0) |>
   (fun (a,b) -> b::a |> Seq.rev)) |>
  Seq.groupBy (snd) |> Seq.map (snd) |> Seq.map (Seq.map (fst)) in
 let reorder (forward:bool) (it:char seq) : char seq =
  let l_char = if forward then '.' else 'O' in
  let r_char = if forward then 'O' else '.' in
  it |> group |>
  Seq.collect
   (fun grp ->
    if Seq.head grp = '#' then grp else
     let count = Seq.fold (fun a c -> if c = l_char then a + 1 else a) 0 grp in
     Seq.append (Seq.init count (fun _ -> l_char)) (Seq.init (Seq.length grp - count) (fun _ -> r_char))) in

 let roll_we (forward:bool) (map:char[,]): unit =
  for i in 0..(Array2D.length1 map - 1) do
   map[i, 0..(Array2D.length2 map - 1)] <- Seq.toArray (map[i,0..(Array2D.length2 map - 1)] |> reorder forward)
  done

 (* vertical slicing is convenient *)
 let roll_ns (forward:bool) (map:char[,]): unit =
  for j in 0..(Array2D.length2 map - 1) do
   map[0..(Array2D.length1 map - 1),j] <- Seq.toArray (map[0..(Array2D.length1 map - 1),j] |> reorder forward)
  done

 let roll_full (map:char[,]): unit =
  roll_ns false map;
  roll_we false map;
  roll_ns true  map;
  roll_we true  map;

 let weight (map:char[,]) : int =
  let mutable acc = 0 in
  for i = 0 to Array2D.length1 map - 1 do
   for j = 0 to Array2D.length2 map - 1 do
    if map[i,j] = 'O' then acc <- acc + Array2D.length1 map - i ;
  acc in
 let input = System.IO.File.ReadLines("14.txt") |> Seq.toArray in
 let map = Array2D.init input.Length input.[0].Length (fun i j -> input.[i].[j]) in
 roll_ns false map;
 weight map

let problem_14b_smart () =
 (* incredibly annoying to implement this via "keys" like python rather than a compare function *)
 (* furthermore, this groupBy works like "partition" assuming *every key is unique* to a group *)
 let group (it:char seq) : char seq seq =
  Seq.zip it
   (* key generation by windows *)
   (it |> Seq.windowed 2 |>
   Seq.map (fun w -> w.[0] <> '#' && w.[1] <> '#') |>
   Seq.fold (fun (a,i) b -> if b then (i::a,i) else (i::a,i+1)) ([],0) |>
   (fun (a,b) -> b::a |> Seq.rev)) |>
  Seq.groupBy (snd) |> Seq.map (snd) |> Seq.map (Seq.map (fst)) in
 let reorder (forward:bool) (it:char seq) : char seq =
  let l_char = if forward then '.' else 'O' in
  let r_char = if forward then 'O' else '.' in
  it |> group |>
  Seq.collect
   (fun grp ->
    if Seq.head grp = '#' then grp else
     let count = Seq.fold (fun a c -> if c = l_char then a + 1 else a) 0 grp in
     Seq.append (Seq.init count (fun _ -> l_char)) (Seq.init (Seq.length grp - count) (fun _ -> r_char))) in

 let roll_we (forward:bool) (map:char[,]): unit =
  for i in 0..(Array2D.length1 map - 1) do
   map[i, 0..(Array2D.length2 map - 1)] <- Seq.toArray (map[i,0..(Array2D.length2 map - 1)] |> reorder forward)
  done

 (* vertical slicing is convenient *)
 let roll_ns (forward:bool) (map:char[,]): unit =
  for j in 0..(Array2D.length2 map - 1) do
   map[0..(Array2D.length1 map - 1),j] <- Seq.toArray (map[0..(Array2D.length1 map - 1),j] |> reorder forward)
  done

 let roll_full (map:char[,]): unit =
  roll_ns false map;
  roll_we false map;
  roll_ns true  map;
  roll_we true  map;

 let weight (map:char[,]) : int =
  let mutable acc = 0 in
  for i = 0 to Array2D.length1 map - 1 do
   for j = 0 to Array2D.length2 map - 1 do
    if map[i,j] = 'O' then acc <- acc + Array2D.length1 map - i ;
  acc in

 let input = System.IO.File.ReadLines("14.txt") |> Seq.toArray in
 let map = Array2D.init input.Length input.[0].Length (fun i j -> input.[i].[j]) in
 let rec roll_and_test weight_stack =
  roll_full map;
  let top = weight map in
  match List.tryFindIndex ((=)top) weight_stack with
  | Some n
     when n > 5
       && weight_stack.Length > ((n+1) * 2)
       && weight_stack.[0..n] = weight_stack[(n+1)..(2*n+1)] ->
         (List.rev weight_stack[0..n], weight_stack.Length + 1)
  | _ -> roll_and_test (top::weight_stack) in
 let (weight_stack,offset) = roll_and_test [] in
 (*weight_stack*)
 printfn $"cycle length = {weight_stack.Length}"
 printfn $"offset = {offset}"
 weight_stack.[(1_000_000_000 - offset) % (weight_stack.Length)]

let problem_15a () =
 let digest s = Seq.fold (fun a c -> ((int c + a) * 17) &&& 255) 0 s in
 let input = Seq.head (System.IO.File.ReadLines("15.txt")) in
 Seq.fold (fun a s -> a + digest s) 0 (input.Split(','))

let problem_15b () =
 let digest s = Seq.fold (fun a c -> ((int c + a) * 17) &&& 255) 0 s in
 let input = Seq.head (System.IO.File.ReadLines("15.txt")) in
 let boxes = Array.create 256 (List.empty) in
 input.Split(',') |>
 Seq.iter
  (fun s ->
   if s[s.Length-1] = '-' then
    let label = s[..(s.Length - 2)] in
    let id = digest label in
    boxes[id] <- List.filter (fun (lbl,_) -> lbl <> label) boxes[id]
   else
    let label = s[..(s.Length - 3)] in
    let fnum = (int s[s.Length - 1]) - (int '0') in
    let id = digest label in
    let mutable found = false in
    boxes[id] <- List.map (fun (lbl,n) -> if lbl = label then found <- true ; (label,fnum) else (lbl,n)) boxes[id] ;
    if not found then boxes[id] <- (label,fnum) :: boxes[id] else ()
  );
 boxes |>
 Array.mapi
  (fun i lst ->
   lst |>
   List.mapi (fun j (lbl,fnum) -> (i+1) * (lst.Length-j) * fnum) |> List.sum) |>
 Array.sum
 
(*
extern void dsyevr_(
    char const* jobz, char const* range, char const* uplo,
    int32_t const* n,
    double* A, int32_t const* lda,
    double const* vl,
    double const* vu, int32_t const* il, int32_t const* iu,
    double const* abstol, int32_t* m,
    double* W,
    double* Z, int32_t const* ldz, int32_t* isuppz,
    double* work, int32_t const* lwork,
    int32_t* iwork, int32_t const* liwork,
    int32_t* info
    //, size_t, size_t, size_t
);
*)

(* suppress native pointer warnings *)
#nowarn "51"
(* suppress null pointer warnings *)
#nowarn "9"

(* float == double *)
open System.Runtime.InteropServices
module Aocl =
 [<DllImport(@"AOCL-LibFlame-Win-dll.dll",
             EntryPoint = "dsyevr_",
             CallingConvention = CallingConvention.Cdecl,
             CharSet = CharSet.None,
             ExactSpelling = true)>]
 extern void dsyevr(char* jobz, char* range, char* uplo,
                    int32* n,
                    float* A, int32* lda,
                    float* vl,
                    float* vu, int32* il, int32* iu,
                    float* abstol, int32* m,
                    float* W,
                    float* Z, int32* ldz, int32* isuppz,
                    float* work, int32* lwork,
                    int32* iwork, int32* liwork,
                    int32* info)
 [<DllImport(@"AOCL-LibFlame-Win-dll.dll",
             EntryPoint = "dgesv_",
             CallingConvention = CallingConvention.Cdecl,
             CharSet = CharSet.None,
             ExactSpelling = true)>]
 extern void dgesv(int32* n, int32* nrhs,
                   float* A, int32* lda,
                   int32* ipiv,
                   float* B, int32* ldb,
                   int32* info)

type dgesv_params =
 { mutable n : int32;    (* order of A *)
   mutable nrhs : int32; (* number of right hand sides *)
   a : float array;      (* dim (LDA,N); float64 *)
   mutable lda : int32;  (* leading dim of A *)
   ipiv : int32 array;   (* [out] dim (N); int32 ; pivot indices*)
   b : float array;      (* dim (LDB,NRHS); float64 *)
   mutable ldb : int32;  (* leading dim of A *)
   mutable info : int32; (* info ; 0 = success *)
 }
 static member Default =
  { n = 0; nrhs = 1;
    a = [||]; lda = 0;
    ipiv = [||];
    b = [||]; ldb = 0;
    info = 0;
  }
 static member OfArray2D (a : float array2d, b : float array) =
  let a' = Array.create (Array2D.length1 a * Array2D.length2 a) 0.0 in
  let lda = Array2D.length2 a in
  let n = Array2D.length1 a in
  let ldb = Array.length b in
  for i = 0 to n - 1 do
   for j = 0 to lda - 1 do
    a'.[i*n+j] <- a[j,i]
  { n = n; nrhs = 1;
    a = a'; lda = lda; ipiv = Array.create n 0; 
    b = b; ldb = ldb; info = 0 }
 member this.Dgesv () =
  Aocl.dgesv(&&this.n,&&this.nrhs,
             &&this.a[0],&&this.lda,&&this.ipiv[0],
             &&this.b[0],&&this.ldb,&&this.info)

(*
   Vector Equations Used for Hail Mary Matrix:
   t[i]*(rv-h[i]v) = (h[i]p-rp)
   0 = (hp-rp) x (rv-hv) ; for all i
   0 = (h[1]p - rp) x (rv - h[1]v) - (h[2]p - rp) x (rv - h[2]v)
   (h[1]p - h[2]p) x rv + (h[2]v - h[1]v) x rp = h[1]p x h[1]v - h[2]p x h[2]v
   where 1 & 2 can be *any* subscript
   using 1 & 2 and 1 & 3 creates the 6 necessary equations
*)

module HailXYZ =
 type t = {x : float; y: float; z : float; vx : float; vy : float; vz: float}
 let of_string (s:string) =
  let (pos,v) = s.Split(" @ ") |> (fun s -> (s[0].Split(", "), s[1].Split(", "))) in
  {x = float pos[0];y = float pos[1];z = float pos[2];vx = float v[0];vy = float v[1];vz = float v[2]}
 let to_string (h : t) = sprintf "%f, %f, %f @ %f, %f, %f" h.x h.y h.z h.vx h.vy h.vz
 let hail_mary_matrix h1 h2 h3 =
  let lhs = Array2D.create 6 6 0.0 in
  lhs.[0,*] <- [| h1.vy - h2.vy; h2.vx - h1.vx; 0.; h2.y - h1.y; h1.x - h2.x; 0. |];
  lhs.[1,*] <- [| h1.vy - h3.vy; h3.vx - h1.vx; 0.; h3.y - h1.y; h1.x - h3.x; 0. |];
  lhs.[2,*] <- [| 0.; h1.vz - h2.vz; h2.vy - h1.vy; 0.; h2.z - h1.z; h1.y - h2.y |];
  lhs.[3,*] <- [| 0.; h1.vz - h3.vz; h3.vy - h1.vy; 0.; h3.z - h1.z; h1.y - h3.y |];
  lhs.[4,*] <- [| h1.vz - h2.vz; 0.; h2.vx - h1.vx; h2.z - h1.z; 0.; h1.x - h2.x |];
  lhs.[5,*] <- [| h1.vz - h3.vz; 0.; h3.vx - h1.vx; h3.z - h1.z; 0.; h1.x - h3.x |];
  let rhs =
    [| (h1.x * h1.vy - h2.x * h2.vy) - (h1.y * h1.vx - h2.y * h2.vx)
     ; (h1.x * h1.vy - h3.x * h3.vy) - (h1.y * h1.vx - h3.y * h3.vx)
     ; (h1.y * h1.vz - h2.y * h2.vz) - (h1.z * h1.vy - h2.z * h2.vy)
     ; (h1.y * h1.vz - h3.y * h3.vz) - (h1.z * h1.vy - h3.z * h3.vy)
     ; (h1.x * h1.vz - h2.x * h2.vz) - (h1.z * h1.vx - h2.z * h2.vx)
     ; (h1.x * h1.vz - h3.x * h3.vz) - (h1.z * h1.vx - h3.z * h3.vx) |] in
   (lhs, rhs)

let problem_24b () =
 let input = System.IO.File.ReadLines("24.txt") |> Seq.map HailXYZ.of_string |> Seq.toArray in
 let (a,b) = HailXYZ.hail_mary_matrix input.[0] input.[1] input.[2] in
 let solver = dgesv_params.OfArray2D(a,b) in
 solver.Dgesv ();
 printfn "[%20.0f; %20.0f; %20.0f]" solver.b.[0] solver.b.[1] solver.b.[2] ;
 printfn "Solution: %20.0f" (solver.b.[0] + solver.b.[1] + solver.b.[2])

(* even const values must be declared mutable if passing a ptr *)
type dsyevr_params =
 { mutable jobz : char;
   mutable range : char;
   mutable uplo: char;
   mutable n: int32;
   a: float array;
   mutable lda: int32;
   mutable vl: float;
   mutable vu: float;
   mutable il: int32;
   mutable iu: int32;
   mutable abstol: float;
   mutable m: int32;
   w: float array;
   z: float array;
   mutable ldz: int32;
   isuppz: int32 array;
   work: float array;
   mutable lwork: int32;
   iwork: int32 array;
   mutable liwork: int32;
   mutable info: int32; }
 static member Default =
  {jobz = 'V'; range = 'I'; uplo = 'U'; n = 0;
   a = [||]; lda = 0;
   vl = 0.0; vu = 0.0; il = 0; iu = 0; abstol = 0.0; m = 0;
   w = [||]; z = [||];
   ldz = 0; isuppz = [||];
   work = [||]; lwork = 0;
   iwork = [||]; liwork = 0;
   info = 0;}
 member this.Dsyevr () =
  Aocl.dsyevr(&&this.jobz,&&this.range,&&this.uplo,&&this.n,
              &&this.a[0],&&this.lda,&&this.vl,&&this.vu,&&this.il,&&this.iu,&&this.abstol,&&this.m,
              &&this.w[0],&&this.z[0],&&this.ldz,&&this.isuppz[0],
              &&this.work[0],&&this.lwork,&&this.iwork[0],&&this.liwork,&&this.info)

let problem_25a () =
 let incr (container:int ref) = container.Value <- container.Value + 1 in
 let idxer = Array.create (26 * 26 * 26) (-1) in
 let mat_size = ref 0 in
 let set_idxer (s:string) = idxer.[26 * 26 * (int s.[0] - 97) + 26 * (int s.[1] - 97) + (int s.[2] - 97)] <- 0 in
 let get_idx (s:string) = idxer.[26 * 26 * (int s.[0] - 97) + 26 * (int s.[1] - 97) + (int s.[2] - 97)] in
 let finalize_idxer () =
  for i = 0 to 26 * 26 * 26 - 1 do
   if idxer.[i] = 0 then (incr mat_size ; idxer.[i] <- mat_size.Value)
  done

 let input = System.IO.File.ReadLines("25.txt") |> Seq.toArray in
 input |>
 Seq.iter (fun s -> s.Split(' ') |> Array.iter set_idxer) ;
 finalize_idxer ();

 let mat_size = mat_size.Value in (* make immutable *)
 let adj_mat = (* laplacian/adjacency matrix *)
  Array.create (mat_size * mat_size) 0.0 in
 (* fortran style helper functions *)
 let f_get (mat : float array) i j = mat.[(j-1)*mat_size+(i-1)] in
 let f_set (mat : float array) i j v = mat.[(j-1)*mat_size+(i-1)] <- v in

 input |>
 Array.iter (fun s ->
  let nodes = s.Split(' ') |> Seq.map get_idx in
  let a =  Seq.head nodes in
  let bs = Seq.tail nodes in
  Seq.iter (fun b -> f_set adj_mat a b (-1.0) ; f_set adj_mat b a (-1.0)) bs
 )

 let reset_degree () = for i = 1 to mat_size do f_set adj_mat i i 0.0 done
 let calculate_degree () =
  let mutable deg = 0.0 in
  for i = 1 to mat_size do
   deg <- 0.0 ;
   for j = 1 to mat_size do
    if f_get adj_mat i j <> 0.0 then deg <- deg + 1.0
   done ;
   f_set adj_mat i i deg
  done ;

 calculate_degree ();

 let restore_adj () =
  reset_degree ();
  for i = 1 to mat_size - 1 do
   for j = i + 1 to mat_size do
    f_set adj_mat i j (f_get adj_mat j i) done done;
  calculate_degree ();

 let best_cut z =
  let search_neg = ref (0, System.Double.MinValue) in
  let search_pos = ref (0, System.Double.MaxValue) in
  (* find lowest abs +/- *)
  for i = 1 to mat_size do
   if System.Double.Sign(f_get z i 2) < 0 then (if compare (f_get z i 2) (snd search_neg.Value) > 0 then search_neg.Value <- (i,f_get z i 2) else ())
   else (if compare (f_get z i 2) (snd search_pos.Value) < 0 then search_pos.Value <- (i,f_get z i 2) else ())
  done ;
  let lowest = if compare (System.Double.Abs(snd search_neg.Value)) (snd search_pos.Value) < 0 then search_neg.Value else search_pos.Value in
  printf "using border node: %i\n" (fst lowest) ;
  (* be careful, the upper triangle and diagonal of adj_mat is destroyed during syevr! so use lower only! *)
  let neighbors = Array.init mat_size (fun i -> if f_get adj_mat (max (fst lowest) (i+1)) (min (fst lowest) (i+1)) = 0.0 then 0.0 else f_get z (i+1) (2)) in
  (* for i = 1 to !mat_size do print_float neighbors.{i} ; print_newline () done; *)
  (* highest abs flipped sign of lowest *)
  let best_neighbor =
   if System.Double.Sign(snd lowest) < 0 then (
    search_pos.Value <- (0, System.Double.MinValue) ;
    for i = 1 to mat_size do if compare neighbors.[i-1] (snd search_pos.Value) > 0 then search_pos.Value <- (i,neighbors.[i-1]) else () done ;
    search_pos.Value
   ) else (
    search_neg.Value <- (0, System.Double.MaxValue) ;
    for i = 1 to mat_size do if compare neighbors.[i-1] (snd search_neg.Value) < 0 then search_neg.Value <- (i,neighbors.[i-1]) else () done ;
    search_neg.Value
   ) in
  printf "making cut @ %i-%i :: (%f,%f)\n" (fst lowest) (fst best_neighbor) (snd lowest) (snd best_neighbor);
  (* make cut at lower triangle anticipating a call to restore_adj *)
  f_set adj_mat (max (fst lowest) (fst best_neighbor)) (min (fst lowest) (fst best_neighbor)) 0.0
 in

 (* first set values to find optimal work sizes *)
 let p = {
  dsyevr_params.Default with
   a = adj_mat;
   n = mat_size; lda = mat_size; ldz = mat_size;
   il = 1; iu = 2; lwork = -1; liwork = -1;
   isuppz = Array.create (mat_size * 2) 0;
   work = Array.create 1 0.0; (* size value will appear here @ first address *)
   iwork = Array.create 1 0;  (* size value will appear here @ first address *)
   w = Array.create 2 0.0;
   z = Array.create (mat_size * 2) 0.0} in
 p.Dsyevr ();
 printfn "lwork : %i\nliwork: %i" (int p.work[0]) p.iwork[0] ;
 let p = {
  p with
   work   = Array.create (int p.work[0])  0.0;
   lwork  = (int p.work[0]);
   iwork  = Array.create (p.iwork[0]) 0;
   liwork = p.iwork[0] } in
 for i = 1 to 3 do
  p.Dsyevr ();
  best_cut p.z ;
  restore_adj () done;

 p.Dsyevr ();

 let abstol = 1e-5 in
 let counter = ref 0 in
 for j = 1 to mat_size do if System.Double.Abs(f_get (p.z) j 1 - f_get (p.z) 1 1) < abstol then incr counter done ;
 Printf.printf "sizes : %i,%i\nans: %i\n" counter.Value (mat_size - counter.Value) ((mat_size - counter.Value) * counter.Value)
 
let _ = ()
 (*printfn "Problem 01: (%i, %i)" (problem_01a ()) (problem_01b ())*)
 (*printfn "Problem 02: (%i, %i)" (problem_02a ()) (problem_02b ())*)
 (*printfn "Problem 15: (%i, %i)" (problem_15a ()) (problem_15b ())*)
