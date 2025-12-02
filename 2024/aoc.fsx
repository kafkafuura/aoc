(* aoc.fsx *)
(* Advent of Code 2024 *)

exception BreakI of int

let problem_01 () =
 let (left, right) =
  System.IO.File.ReadLines("01.txt") |>
  Seq.map (fun s -> let lr = s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) in int lr.[0], int lr.[1]) |>
  Seq.toArray |> Array.unzip in
 Array.sortInPlace left ; Array.sortInPlace right ;
 let res1 =
  Array.zip left right |>
  Array.fold (fun a (l,r) -> abs (l - r) + a) 0 in
 let res2 =
  Array.fold
   (fun a x ->
    (try 
     (Array.fold (fun a n -> if n > x then raise (BreakI a) else if n = x then a + 1 else a) 0 right)
     with BreakI res -> res) * x + a)
   0 left in
 (res1, res2)

(* alternatively use right |> Seq.countBy id |> Map.ofSeq to create a searchable map ; probably slower *)

let problem_02 () =
 let inputs =
  System.IO.File.ReadLines("02.txt") |>
  Seq.map (fun s -> s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int) |>
  Seq.toArray in
 let is_safe ns =
  let (min', max') =
   Array.pairwise ns |>
   Seq.map (fun (a,b) -> a - b) |>
   Seq.fold (fun (min',max') n -> min min' n, max max' n) (4,-4) in
  if min' < 0 then min' >= (-3) && max' <= (-1) else
  min' >= 1 && max' <= 3 in
 let is_safe_with_dampening ns =
   is_safe ns ||
   seq { 0 .. (Array.length ns) } |>
   Seq.map (fun idx -> Array.choose (fun (i,n) -> if i = idx then None else Some(n)) (Array.indexed ns)) |>
   Seq.exists is_safe in
  inputs |>
  Seq.fold
   (fun (r1,r2) ns ->
    (if is_safe ns then r1+1 else r1),
    (if is_safe_with_dampening ns then r2+1 else r2))
   (0,0)

(* note: unlike rust, ranges are inclusive! *)
module P03_Instr =
 type t = Mul of int * int | Do | Dont
 type OptionExprBuilder () =
  member inline _.Bind(comp,[<InlineIfLambda>]func) = Option.bind func comp
  member inline _.Return(value) = Some(value)
  member inline _.MergeSources(v1,v2) = Option.map2 (fun a b -> (a,b)) v1 v2
  member inline _.Zero() = Some () (* use do! None to fail *)
 let opt = new OptionExprBuilder ()
 let parse_num (s : string, i) =
  let len =
   s.[i..(i+3)-1] |>
   Seq.takeWhile (fun c -> System.Char.IsAsciiDigit(c)) |>
   Seq.length in
  if len = 0 then None else Some (int s.[i..(i+len-1)], i+len)
 let rec next_instr (s : string, i) =
  if s.[i..] = "" then None else
  if s.[i..(i+4-1)] = "do()" then Some (Do, i+4) else
  if s.[i..(i+7-1)] = "don't()" then Some (Dont, i+7) else
  if s.[i..(i+4-1)] <> "mul(" then next_instr (s,i+1) else
   try
    opt { 
     let! (n1,i') = parse_num (s,i+4) in
     let! i' = if s.[i'] = ',' then Some (i'+1) else None in
     let! (n2,i') = parse_num (s,i') in
     let! i' = if s.[i'] = ')' then Some (i'+1) else None in
     return (Mul (n1,n2), i') } |>
    Option.orElseWith (fun () -> next_instr (s,i+1))
   with _ -> None

let problem_03 () =
 let input = System.IO.File.ReadAllText("03.txt") in
 let rec token_folder a (s,i) =
  match P03_Instr.next_instr (s,i) with
  | None -> List.rev a
  | Some (instr, next) -> token_folder (instr::a) (s,next) in
 let tokens = P03_Instr.Do :: (token_folder [] (input,0)) in
 let res1 =
  tokens |>
  Seq.map (function P03_Instr.Mul (n1,n2) -> n1*n2 | _ -> 0) |>
  Seq.fold (+) 0 in
 let res2 =
  Seq.fold
   (fun (sum,on) instr ->
     match on, instr with
     | false, P03_Instr.Do -> (sum, true)
     | false, _ -> (sum, false)
     | _, P03_Instr.Dont -> (sum, false)
     | _, P03_Instr.Mul (n1,n2) -> (sum + n1*n2, true)
     | _, _ -> (sum, true))
   (0, true)
   tokens |>
  fst in
 res1, res2

(* use a counter instead of tracking matches in a set *)
let problem_04 () =
 (* type dir = N | S | W | E | NW | NE | SW | SE *)
 let xmas_its =
  seq {
   [0,0; -1,0; -2,0; -3,0]
   [0,0; 1,0; 2,0; 3,0]
   [0,0; 0,-1; 0,-2; 0,-3]
   [0,0; 0,1; 0,2; 0,3]
   [0,0; -1,1; -2,2; -3,3]
   [0,0; -1,-1; -2,-2; -3,-3]
   [0,0; 1,1; 2,2; 3,3]
   [0,0; 1,-1; 2,-2; 3,-3]
  } in
 (* type dir = N | S | W | E *)
 let x_mas_its =
  seq {
   [-1,-1;0,0; 1,1; -1,1; 1,-1]
   [1,1;0,0; -1,-1; 1,-1; -1,1]
   [-1,-1;0,0; 1,1; 1,-1; -1,1]
   [-1,1;0,0; 1,-1; 1,1; -1,-1]
  } in
 let mutable res1 = 0 in
 let mutable res2 = 0 in
 let test_xmas (map : string array) y x =
  xmas_its |>
  Seq.iter
   (fun it ->
    try
     if
      it |>
      Seq.map (fun (dy,dx) -> map.[y+dy].[x+dx]) |>
      Seq.zip "XMAS" |>
      Seq.forall ((<||)(=))
     then res1 <- res1 + 1
    with _ -> ()) in
 let test_x_mas (map : string array) y x =
  x_mas_its |>
  Seq.iter
   (fun it ->
    try
     if
      it |>
      Seq.map (fun (dy,dx) -> map.[y+dy].[x+dx]) |>
      Seq.zip "MASMS" |>
      Seq.forall ((<||)(=))
     then res2 <- res2 + 1
    with _ -> ()) in
 let inputs =
  System.IO.File.ReadLines("04.txt") |>
  Seq.toArray in
 for y = 0 to Array.length inputs - 1 do
  for x = 0 to String.length inputs.[y] - 1 do
   if inputs.[y].[x] = 'X' then test_xmas inputs y x ;
   if inputs.[y].[x] = 'A' then test_x_mas inputs y x ;
  done
 done;
 res1, res2

let problem_05 () =
 let inputs =
  System.IO.File.ReadLines("05.txt") |>
  Seq.toArray in
 let brkpt =
  Array.findIndex ((=)"") inputs in
 let rules =
  inputs.[0 .. brkpt-1] |>
  Array.map (fun s -> let ns = s.Split('|', System.StringSplitOptions.RemoveEmptyEntries) in (int ns.[0], int ns.[1])) in
 let updates =
  inputs.[brkpt+1 ..] |>
  Array.map (fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int) in
 let is_valid_1 update (l,r) =
  match Seq.tryFindIndex ((=)l) update, Seq.tryFindIndex ((=)r) update with Some x, Some y when y < x -> false | _ -> true in
 let is_valid update =
  rules |> Seq.map (is_valid_1 update) |> Seq.forall (id) in
 let reorder update =
  Array.sortInPlaceWith (fun a b -> if Array.exists ((=)(a,b)) rules then (-1) else if Array.exists ((=)(b,a)) rules then 1 else 0) update ; update in
 let res1 = 
  updates |>
  Seq.filter (is_valid) |>
  Seq.map (fun update -> update.[Array.length update / 2]) |>
  Seq.fold (+) 0 in
 let res2 =
  updates |>
  Seq.filter (not << is_valid) |>
  Seq.map reorder |>
  Seq.map (fun update -> update.[Array.length update / 2]) |>
  Seq.fold (+) 0 in
 res1, res2

let problem_06 () =
 let inputs =
  System.IO.File.ReadLines("06.txt") |>
  Seq.toArray in
 let ascii = new System.Text.ASCIIEncoding () in
 (* Guard psuedo module startA *)
 (* Guard type t = int (y) * int (x) * int (dir_flag) *)
 let flag_n = 0x01 in
 let flag_e = 0x02 in
 let flag_s = 0x04 in
 let flag_w = 0x08 in
 let turn dir = if dir <<< 1 > flag_w then flag_n else dir <<< 1 in
 (* Guard psuedo module endA *)
 let map_h = Array.length inputs in
 let map_w = String.length inputs.[0] in
 let history = Array.create (map_h * map_w) 0 in
 let map =
  inputs |>
  Array.map (ascii.GetBytes) in
 let guard0 =
  Seq.zip (seq { 0 .. map_h-1 }) map |>
  Seq.fold
   (fun a (y,line) ->
    Seq.zip (seq { 0 .. map_w-1 }) line |>
    Seq.fold (fun a (x,b) -> if b = '^'B then Option.defaultValue (y,x,flag_n) a |> Some else a) a)
   None |>
  Option.get in
 (* Guard psuedo module startB *)
 (* capture environment and simply use guard tuple as input *)
 (* reuse BreakI exception *)
 let move (y,x,d) =
  if history.[y*map_h+x] &&& d <> 0 then raise (BreakI 0) ;
  history.[y*map_h+x] <- history.[y*map_h+x] ||| d ;
  try
   if d = flag_n then (if map.[y-1].[x] = '#'B then (y,x, turn d) else (y-1,x,d)) else
   if d = flag_e then (if map.[y].[x+1] = '#'B then (y,x, turn d) else (y,x+1,d)) else
   if d = flag_s then (if map.[y+1].[x] = '#'B then (y,x, turn d) else (y+1,x,d)) else
   if d = flag_w then (if map.[y].[x-1] = '#'B then (y,x, turn d) else (y,x-1,d)) else
   failwith "Invalid Direction!"
  with _ -> raise (BreakI 1) in
 (* Guard psuedo module endB *)
 let count_visited () = Array.fold (fun a loc -> if loc <> 0 then a + 1 else a) 0 history in
 let mutable guard = guard0 in
 let res1 =
  try
   while true do guard <- move guard done
  with BreakI _ -> () ;
  count_visited () in
 let res2 =
  let mutable count = 0 in
  for y = 0 to map_h - 1 do
   for x = 0 to map_w - 1 do
    if map.[y].[x] = '.'B then
     map.[y].[x] <- '#'B ;
     guard <- guard0 ; 
     Array.fill history 0 (history.Length) 0 ;
     try
      while true do guard <- move guard done
     with BreakI code -> if code = 0 then count <- count + 1 else () ;
     map.[y].[x] <- '.'B
   done
  done ;
  count in
 res1, res2

(* requires int64 *)
let problem_07 () =
 let example = false in
 let parse_input (s : string) =
  let res = s.Split([|':';' '|], System.StringSplitOptions.RemoveEmptyEntries) in
  int64 res.[0], (res.[1 .. ] |> Array.map int64) in
 let inputs =
  System.IO.File.ReadLines(if example then "07e.txt" else "07.txt") |> Seq.map parse_input |> Seq.toArray in

 let rec is_suffix_of (suf : int64) (n : int64) =
  if suf = 0L then true else
  if suf % 10L = n % 10L then is_suffix_of (suf / 10L) (n / 10L) else
  false in

 let rec strip_suffix (suf : int64) (n : int64) =
  if suf = 0L then n else strip_suffix (suf / 10L) (n / 10L) in
  
 (* expects reversed ns *)
 let rec test (res, ns) =
  match ns with
  | [hd] -> hd = res
  | hd::tl ->
    res >= 0L &&
    test (res - hd,tl) ||
    (if hd = 0L || res % hd <> 0L  then false else test (res / hd,tl))
  | _ -> false in

 (* expects reversed ns *)
 let rec test3 (res, ns) =
  match ns with
  | [hd] -> hd = res
  | hd::tl ->
    res >= 0L &&
    test3 (res - hd,tl) ||
    (if hd = 0L || res % hd <> 0L  then false else test3 (res / hd,tl)) ||
    (if is_suffix_of hd res |> not then false else test3 (strip_suffix hd res,tl))
  | _ -> false in

 let res1 =
  inputs |>
  Seq.filter (fun (res,ns) -> test (res, ns |> Array.rev |> Array.toList)) |>
  Seq.fold (fun a (res,_) -> a + res) 0L in

 let res2 =
  inputs |>
  Seq.filter (fun (res,ns) -> test3 (res, ns |> Array.rev |> Array.toList)) |>
  Seq.fold (fun a (res,_) -> a + res) 0L in

 res1, res2

(* Note: in the future, it may make sense to make an 'a Vec module that uses System.Collections.Generic.List<'a> *)
let problem_08 () =
 let example = false in
 let map =
  System.IO.File.ReadLines(if example then "08e.txt" else "08.txt") |> Seq.toArray in
 (* use .NET mutable lists, a.k.a. vectors *)
 (* (hashed)dictionary would work just fine but sorted may be faster than hashing here *)
 let antennae = new System.Collections.Generic.SortedDictionary<char,System.Collections.Generic.List<int * int>>()
 let map_h = Array.length map in
 let map_w = String.length map.[0] in
 for y = 0 to map_h - 1 do
  for x = 0 to map_w - 1 do
   if map.[y].[x] <> '.' then
    let v =
     if antennae.ContainsKey(map.[y].[x]) then
       antennae.[map.[y].[x]]
     else
       let res = new System.Collections.Generic.List<int * int>() in
       antennae.Add (map.[y].[x], res); res in
    v.Add ((y,x))
  done
 done;
 let table = new System.Collections.Generic.SortedSet<int * int>() in
 let table2 = new System.Collections.Generic.SortedSet<int * int>() in
 let in_bounds (y,x) = 0 <= y && y < map_h && 0 <=x && x < map_w in
 let mark_point (y,x) = if in_bounds (y,x) then table.Add(y,x) |> ignore in
 (* you can remove the bounds checking from this one *)
 let mark_point2 (y,x) = if in_bounds (y,x) then table2.Add(y,x) |> ignore in
 (* necessary to hint container here ; you can use IList to be more generic, e.g., mutability not required *)
 let mark_antinodes (ants : System.Collections.Generic.List<_>)  =
  for i = 0 to ants.Count - 2 do
   for j = i+1 to ants.Count - 1 do
    let (y1,x1) = ants.[i] in
    let (y2,x2) = ants.[j] in
    let (dy,dx) = y2 - y1, x2 - x1 in
    mark_point (y2+dy,x2+dx) ; mark_point (y1-dy,x1-dx)
   done
  done
 let rec gcd a b = if b = 0 then a else gcd b (a % b) in
 let mark_antinodes2 (ants : System.Collections.Generic.List<_>)  =
  for i = 0 to ants.Count - 2 do
   for j = i+1 to ants.Count - 1 do
    let (y1,x1) = ants.[i] in
    let (y2,x2) = ants.[j] in
    let (dy,dx) = y2 - y1, x2 - x1 in
    (* d may be < 0, but that is okay *)
    let d = gcd dy dx in
    let (dy,dx) = dy / d, dx / d in
    (* non-inclusive version *)
    let set1 = Seq.unfold (fun (y,x) -> Some ((y+dy,x+dx),(y+dy,x+dx))) ants.[i] in
    let set2 = Seq.unfold (fun (y,x) -> Some ((y-dy,x-dx),(y-dy,x-dx))) ants.[i] in
    mark_point2 ants.[i] ;
    set1 |> Seq.takeWhile in_bounds |> Seq.iter mark_point2 ;
    set2 |> Seq.takeWhile in_bounds |> Seq.iter mark_point2
   done
  done
 antennae |> Seq.iter (fun kvp -> mark_antinodes kvp.Value ; mark_antinodes2 kvp.Value) ;
 table.Count, table2.Count

(* disk defragmentation *)
(* requires int64 *)
let problem_09 () =
 let example = false in
 let ascii = new System.Text.ASCIIEncoding () in
 let input =
  System.IO.File.ReadLines(if example then "09e.txt" else "09.txt") |>
  Seq.head |> ascii.GetBytes |> Array.map (fun c -> c - 0x30uy) in

 (* part 1 mutables *)
 let mutable res1 = 0L in
 let mutable read_head = 0 in
 let mutable read_tail = Array.length input - 1 in
 let mutable write_head = 0 in

 (* part 2 execution *)
 let (info,_,_,_) =
  Array.fold
   (fun (a,id,cur,free) n (* : byte *) ->
    if not free then
     ((id,cur,n)::a, id+1, cur+(int n), true)
    else
     ((-id,cur,n)::a, id, cur+(int n), false))
   ([],0,0,false) input in
 let chunk_info_rev = List.filter (fun (id,_,_) -> id >= 0) info in
 let free_info = List.filter (fun (id,_,_) -> id < 0) info |> List.rev |> Array.ofList in
 let chunk_info_rev =
  chunk_info_rev |>
  List.map
   (fun (id,cur,n) ->
    match Array.tryFindIndex (fun (_,cur', n') -> n' >= n && cur' <= cur) free_info with
    | None -> (id,cur,n)
    | Some i ->
      let (id', cur', n') = free_info.[i] in
      free_info.[i] <- (id', cur'+(int n), n'-n) ;
      (id, cur',n)) in
 (* requires int64 here too, not just in the counter *)
 let checksum_chunk (id,cur,n) =
  seq {(int64 cur) .. int64 cur + (int64 n - 1L)} |>
  Seq.fold (fun a cur -> a+(int64 id)*cur) 0L in

 let res2 = chunk_info_rev |> Seq.fold (fun a record -> a + (checksum_chunk record |> int64)) 0L in

 (* part 1 execution *)
 (* this destroys the input data, so we do this last *)
 (* simulate writing to disk, which each count1 udpate but do not actually allocate *)
 (* this will throw an exception when done (out of bounds) *)
 let step () =
  while input.[read_head] <> 0uy do
   res1 <- res1 + (int64 <| write_head * (read_head / 2)) ;
   input.[read_head] <- input.[read_head] - 1uy ;
   write_head <- write_head + 1 ;
  done ;
  while input.[read_head+1] <> 0uy do
   while input.[read_tail] = 0uy do read_tail <- read_tail - 2 done ;
   res1 <- res1 + (int64 <| write_head * (read_tail / 2)) ;
   input.[read_tail] <- input.[read_tail] - 1uy ;
   input.[read_head+1] <- input.[read_head+1] - 1uy ;
   write_head <- write_head + 1 ;
  done ;
  read_head <- read_head + 2

 try while true do step () done with _ -> () ;

 res1, res2

let problem_10 () =
 let example = false in
 let ascii = new System.Text.ASCIIEncoding () in
 let map =
  System.IO.File.ReadLines(if example then "10e.txt" else "10.txt") |>
  Seq.map (fun s -> s |> ascii.GetBytes |> Array.map (fun c -> sbyte c - 0x30y)) |>
  Seq.toArray in
 (* for part 2 *)
 let rec get_paths last (y,x) =
  try
   match map.[y].[x] with
   | 9y when last = 8y -> 1
   | n when last = n - 1y ->
     get_paths n (y-1,x) +
     get_paths n (y+1,x) +
     get_paths n (y,x-1) +
     get_paths n (y,x+1)
   | _ -> 0
  with _ -> 0 in

 (* for part 1 *)
 let collision_set = new System.Collections.Generic.HashSet<int*int>() in
 let get_score (y,x) =
  collision_set.Clear () ;
  let rec get_score' last (y,x) =
   try
    match map.[y].[x] with
    | 9y when last = 8y -> collision_set.Add (y,x) |> ignore
    | n when last = n - 1y ->
      get_score' n (y-1,x) ;
      get_score' n (y+1,x) ;
      get_score' n (y,x-1) ;
      get_score' n (y,x+1)
    | _ -> ()
   with _ -> () in
  get_score' (-1y) (y,x) ;
  collision_set.Count in

 let trailheads =
  map |>
  Array.fold
   (fun (a,y) xs ->
    xs |>
    Array.fold
     (fun (a,x) b ->
      (if b = 0y then (y,x)::a else a), x+1)
     (a,0) |> fst, y+1)
   ([],0) |> fst in
 
 trailheads |>
 List.fold (fun (res1, res2) (y,x) ->
   res1 + get_score (y,x),
   res2 + get_paths (-1y) (y,x))
  (0,0)

let problem_11 () =
 let example = false in
 let steps1 = 25 in
 let steps2 = 75 in
 let stones = new System.Collections.Generic.Dictionary<int64,int64>() in

 let register (n : int64, c : int64) =
  if stones.ContainsKey n then stones.[n] <- stones.[n] + c else stones.[n] <- c in

 let rec digit_len n =
  if n = 0L then 0L else 1L + digit_len (n/10L) in
 let rec lsr10 n shift =
  if shift = 0L then n else lsr10 (n/10L) (shift - 1L) in
 let rec pow10 n =
  if n = 0L then 1L else 10L * pow10 (n - 1L) in

 let transform (n,c) =
  let c0 = stones.[n] in
  if c = c0 then stones.Remove n |> ignore else stones.[n] <- (c0 - c) ;
  (match n with
  | 0L -> register (1L,c)
  | n when digit_len n &&& 1L = 1L -> register (n*2024L,c)
  | n ->
    let brkpt = digit_len n >>> 1 in
    register (lsr10 n brkpt,c) ;
    register (n % (pow10 brkpt),c)) in

 (* initialize *)
 System.IO.File.ReadLines(if example then "11e.txt" else "11.txt") |>
 Seq.head |> (fun s -> s.Split(' ',System.StringSplitOptions.RemoveEmptyEntries)) |>
 Seq.map int64 |> Seq.iter (fun n -> register(n,1L)) ;

 let blink () =
  (* force eager memoization by caching into an array *)
  stones |> Seq.toArray |>
  Array.iter (fun kvp -> transform (kvp.Key, kvp.Value)) in

 for i = 1 to steps1 do blink () done ;
 let res1 = stones |> Seq.fold (fun a kvp -> a + kvp.Value) 0L in
 for i = steps1 + 1 to steps2 do blink () done ;
 let res2 = stones |> Seq.fold (fun a kvp -> a + kvp.Value) 0L in
 res1, res2

let problem_12 () =
 let example = false in
 let map =
  System.IO.File.ReadLines(if example then "12e.txt" else "12.txt") |>
  Seq.toArray in
 let map_h = Array.length map in
 let map_w = String.length map.[0] in
 let seen = Array.create (map_h * map_w) false in
 
 let rec fill (map : string array) crop acc (y,x) =
  if Set.contains (y,x) acc || y < 0 || x < 0 || y >= map_h || x >= map_w then acc else
  if map.[y].[x] = crop then 
    [y-1,x;y+1,x;y,x+1;y,x-1] |>
    List.fold (fill map crop) (Set.add (y,x) acc)
  else acc in

 let plots =
  Array.fold (fun (a,y) xs ->
   Seq.fold (fun (a,x) c ->
    if not seen.[y*map_w + x] then
     let points = fill map c Set.empty (y,x) in
     (Set.iter (fun (y,x) -> seen.[y*map_w + x] <- true) points ;
     (points::a, x+1))
    else (a, x+1)) (a,0) xs |> fst, y+1) ([],0) map |> fst in

 let outer_perimeter (dy,dx) points =
  Set.difference
   (Set.map (fun (y,x) -> y+dy,x+dx) points)
   points in

 let perimeter_of_plot points =
  (* cardinal directions *)
  seq {-1,0;1,0;0,-1;0,1} |>
  Seq.fold (fun a shift -> (outer_perimeter shift points).Count + a) 0 in

 let sides_of_plot points =
  (* consider each orientation separately to ensure no overlaps *)
  let op_n = outer_perimeter (-1,0) points in
  let op_s = outer_perimeter (1,0) points in
  let op_w = outer_perimeter (0,-1) points in
  let op_e = outer_perimeter (0,1) points in
  (* take outer perimeters of outer perimeters to identify endpoints ; each line will have exactly 2 *)
  let endpoints =
   (outer_perimeter (0,-1) op_n |> Set.count) + (outer_perimeter (0,1) op_n |> Set.count) +
   (outer_perimeter (0,-1) op_s |> Set.count) + (outer_perimeter (0,1) op_s |> Set.count) +
   (outer_perimeter (-1,0) op_w |> Set.count) + (outer_perimeter (1,0) op_w |> Set.count) +
   (outer_perimeter (-1,0) op_e |> Set.count) + (outer_perimeter (1,0) op_e |> Set.count) in
  endpoints / 2 in

 plots |>
 List.map (fun pts -> Set.count pts, perimeter_of_plot pts, sides_of_plot pts) |>
 List.fold (fun (a1,a2) (area, peri, sides) -> a1 + area*peri, a2 + area*sides) (0,0)

let problem_13 () =
 let example = false in

 (* solve by examining p in terms of bases b and b' (90 deg rotation) *)
 let solve (ax,ay,bx,by,px,py) =
  let p_crs_b = px * by - py * bx in
  let a_crs_b = ax * by - ay * bx in
  let a = if a_crs_b = 0L || p_crs_b % a_crs_b <> 0L then 0L else p_crs_b / a_crs_b in
  let px' = px - ax * a in
  let py' = py - ay * a in
  let b = if px' % bx = 0L && py' % by = 0L && px' / bx = py' / by then px' / bx else (-1L) in
  if b = (-1L) then None else Some (a,b) in

 let solve2 (ax,ay,bx,by,px,py) = solve (ax,ay,bx,by,px+1_000_0000_000_000L,py+1_000_0000_000_000L) in
 
 let input =
  System.IO.File.ReadLines(if example then "13e.txt" else "13.txt") |>
  Seq.toArray in
 let group_idxs = Array.fold (fun (a,i) s -> (if s = "" then i::a else a), i+1) ([0],1) input |> fst |> List.rev in
 (* let's try regexes in F# b/c no scanf *)
 let scan_num = new System.Text.RegularExpressions.Regex(@"\d+") in
 let parse idx =
  let (ax,ay) = scan_num.Matches(input.[idx])   |> (fun mc -> int64 mc.[0].Value, int64 mc.[1].Value) (*"Button A: X+%d, Y+%d"*)
  let (bx,by) = scan_num.Matches(input.[idx+1]) |> (fun mc -> int64 mc.[0].Value, int64 mc.[1].Value) (*"Button B: X+%d, Y+%d"*)
  let (px,py) = scan_num.Matches(input.[idx+2]) |> (fun mc -> int64 mc.[0].Value, int64 mc.[1].Value) (*"Prize: X=%d, Y=%d"*)
  (ax,ay,bx,by,px,py) in
 let games = List.map parse group_idxs in
 let res1 =
  games |>
  List.choose solve |>
  List.fold (fun acc (a,b) -> acc + 3L*a + b) 0L in
 let res2 =
  games |>
  List.choose solve2 |>
  List.fold (fun acc (a,b) -> acc + 3L*a + b) 0L in
 res1, res2

(* quadrant metrics act like entropy; finer subdivisions may be more accurate if this fails *)
(* counting edges is another reliable metric *)
let problem_14 () =
 let example = false in
 let steps = 100 in
 let scan_num = new System.Text.RegularExpressions.Regex(@"-?\d+") in
 (* "p=%d,%d v=%d,%d" *)
 let parse s =
  scan_num.Matches(s) |>
  (fun mc -> int mc.[0].Value, 
             int mc.[1].Value,
             int mc.[2].Value,
             int mc.[3].Value) in
 let robots =
  System.IO.File.ReadLines(if example then "14e.txt" else "14.txt") |> Seq.map parse |>
  Seq.toArray in
 let pmod a b = let m = a % b in if m < 0 then m + b else m in
 let (map_w, map_h) = if example then (11,7) else (101,103) in
 let quadrant_counters = Array.create 5 0 in
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
 let entropy steps =
  Array.fill quadrant_counters 0 5 0 ;
  Array.iter (fun r ->
   let q = quadrant_of_robot steps r in
   quadrant_counters.[q] <- quadrant_counters.[q] + 1)
   robots ;
  quadrant_counters.[0] <- 1 ;
  Array.fold ( * ) 1 quadrant_counters in
 let (min_ent, min_i) =
  seq { 0 .. map_w * map_h - 1 } |>
  Seq.fold
   (fun (min_ent,min_i) i ->
    let ent = entropy i in
    if ent < min_ent then (ent,i) else (min_ent,min_i))
   (System.Int32.MaxValue, 0) in
 entropy steps, min_i

let problem_15 () =
 let debug = true in
 let example = false in
 let input =
  System.IO.File.ReadLines(if example then "15e.txt" else "15.txt") |> Seq.toArray in
 let map_h = Array.findIndex ((=)"") input in
 let map_w = String.length input.[0] in
 let move_w = String.length input.[map_h+1] in
 let move_h = Array.length input - map_h - 1 in
 let move_len = move_w * move_h in
 let get_move i = input.[map_h + 1 + i / move_w].[i % move_w] in
 let map1 =
  Array.init map_h (fun y ->
   Array.init map_w (fun x ->
    input.[y].[x])) in
 let map2 =
  Array.init map_h (fun y ->
   Array.init (map_w * 2) (fun x ->
    if x &&& 1 = 0 then
     (match input.[y].[x/2] with
     | 'O' -> '['
     | c -> c)
    else
     (match input.[y].[x/2] with
     | 'O' -> ']'
     | '@' -> '.'
     | c -> c))) in
 let rec can_move (map : char array array) (y,x) = function
 | '^' when map.[y-1].[x] = '.' -> true
 | '^' when map.[y-1].[x] = '#' -> false
 | '^' when map.[y-1].[x] = 'O' -> can_move map (y-1,x) '^'
 | '^' when map.[y-1].[x] = '[' -> can_move map (y-1,x) '^' && can_move map (y-1,x+1) '^'
 | '^' when map.[y-1].[x] = ']' -> can_move map (y-1,x) '^' && can_move map (y-1,x-1) '^'
 | '>' when map.[y].[x+1] = '.' -> true
 | '>' when map.[y].[x+1] = '#' -> false
 | '>' when map.[y].[x+1] = 'O' -> can_move map (y,x+1) '>'
 | '>' when map.[y].[x+1] = '[' -> can_move map (y,x+1) '>'
 | '>' when map.[y].[x+1] = ']' -> can_move map (y,x+1) '>'
 | 'v' when map.[y+1].[x] = '.' -> true
 | 'v' when map.[y+1].[x] = '#' -> false
 | 'v' when map.[y+1].[x] = 'O' -> can_move map (y+1,x) 'v'
 | 'v' when map.[y+1].[x] = '[' -> can_move map (y+1,x) 'v' && can_move map (y+1,x+1) 'v'
 | 'v' when map.[y+1].[x] = ']' -> can_move map (y+1,x) 'v' && can_move map (y+1,x-1) 'v'
 | '<' when map.[y].[x-1] = '.' -> true
 | '<' when map.[y].[x-1] = '#' -> false
 | '<' when map.[y].[x-1] = 'O' -> can_move map (y,x-1) '<'
 | '<' when map.[y].[x-1] = '[' -> can_move map (y,x-1) '<'
 | '<' when map.[y].[x-1] = ']' -> can_move map (y,x-1) '<'
 | _ -> failwith "Unreachable (can_move)" in
 (* move requires you to "pick up" your @ piece before running *)
 let rec move (map : char array array) c (y,x) = function
 | '^' when map.[y-1].[x] = '.' -> map.[y-1].[x] <- c
 | '^' when map.[y-1].[x] = 'O' -> map.[y-1].[x] <- c ; move map 'O' (y-1,x) '^'
 | '^' when map.[y-1].[x] = '[' -> map.[y-1].[x] <- c ; move map '[' (y-1,x) '^' ; map.[y-1].[x+1] <- '.' ; move map ']' (y-1,x+1) '^'
 | '^' when map.[y-1].[x] = ']' -> map.[y-1].[x] <- c ; move map ']' (y-1,x) '^' ; map.[y-1].[x-1] <- '.' ; move map '[' (y-1,x-1) '^'
 | '>' when map.[y].[x+1] = '.' -> map.[y].[x+1] <- c
 | '>' when map.[y].[x+1] = 'O' -> map.[y].[x+1] <- c ; move map 'O' (y,x+1) '>'
 | '>' when map.[y].[x+1] = '[' -> map.[y].[x+1] <- c ; move map '[' (y,x+1) '>'
 | '>' when map.[y].[x+1] = ']' -> map.[y].[x+1] <- c ; move map ']' (y,x+1) '>'
 | 'v' when map.[y+1].[x] = '.' -> map.[y+1].[x] <- c
 | 'v' when map.[y+1].[x] = 'O' -> map.[y+1].[x] <- c ; move map 'O' (y+1,x) 'v'
 | 'v' when map.[y+1].[x] = '[' -> map.[y+1].[x] <- c ; move map '[' (y+1,x) 'v'; map.[y+1].[x+1] <- '.' ; move map ']' (y+1,x+1) 'v'
 | 'v' when map.[y+1].[x] = ']' -> map.[y+1].[x] <- c ; move map ']' (y+1,x) 'v'; map.[y+1].[x-1] <- '.' ; move map '[' (y+1,x-1) 'v'
 | '<' when map.[y].[x-1] = '.' -> map.[y].[x-1] <- c
 | '<' when map.[y].[x-1] = 'O' -> map.[y].[x-1] <- c ; move map 'O' (y,x-1) '<'
 | '<' when map.[y].[x-1] = '[' -> map.[y].[x-1] <- c ; move map '[' (y,x-1) '<'
 | '<' when map.[y].[x-1] = ']' -> map.[y].[x-1] <- c ; move map ']' (y,x-1) '<'
 | _ -> failwith "Unreachable (move)" in
 let update_robot (r : (int * int) ref) = function
 | '^' -> let (y,x) = r.Value in r.Value <- (y-1,x)
 | '>' -> let (y,x) = r.Value in r.Value <- (y,x+1)
 | 'v' -> let (y,x) = r.Value in r.Value <- (y+1,x)
 | '<' -> let (y,x) = r.Value in r.Value <- (y,x-1)
 | _ -> failwith "Unreachable (update_robot)" in
 let print_map (map : char array array) =
  for y = 0 to map.Length - 1 do
   for x = 0 to map.[y].Length - 1 do
    printf "%c" map.[y].[x]
   done ;
   printfn ""
  done
 let sum_gps map =
  Array.fold (fun (a,y) bs ->
   Array.fold (fun (a,x) c ->
    (if c = '[' || c = 'O' then a+x+100*y else a), x+1)
    (a,0) bs |> fst, y+1)
   (0,0) map |> fst in
 let get_robot map =
  Array.fold (fun (yx,y) bs ->
   if Option.isNone yx then
    let yx' =
     Array.tryFindIndex ((=)'@') bs |>
     Option.map (fun x -> (y,x)) in
    (yx', y+1)
   else (yx,y+1)) (None, 0) map |>
  fst |>
  Option.get |>
  ref in
 let run (map : char array array) (robot : (int * int) ref) =
  for i = 0 to move_len - 1 do
   let m = get_move i in
   if can_move map robot.Value m then
    (let (y,x) = robot.Value in
     map.[y].[x] <- '.' ;
     move map '@' robot.Value m ;
     update_robot robot m) ;
  done
 run map1 (get_robot map1) ;
 run map2 (get_robot map2) ;
 if debug then (print_map map1 ; print_map map2 ) else () ;
 sum_gps map1, sum_gps map2

module Dir =
 type t = N | E | S | W
 let rotate_cw = function
  | N -> E | E -> S | S -> W | W -> N
 let rotate_ccw = function
  | N -> W | W -> S | S -> E | E -> N
 let int_of_dir = function
  | N -> 0 | E -> 1 | S -> 2 | W -> 3

module YXD =
 open Dir
 type t = {y : int; x : int; d : Dir.t; cost : int; history : Set<int * int * Dir.t>}
 let rotate_cw yxd  = {yxd with d = Dir.rotate_cw yxd.d ; cost = yxd.cost + 1000}
 let rotate_ccw yxd = {yxd with d = Dir.rotate_ccw yxd.d; cost = yxd.cost + 1000}
 let singleton (y : int, x : int, d : Dir.t) =
  {y=y; x=x; d=d; cost = 0; history = Set.singleton (y,x,d)}
 (* only log moves, not rotations *)
 let move yxd =
  match yxd.d with
  | N -> {yxd with y = yxd.y - 1; cost = yxd.cost + 1; history = Set.add (yxd.y-1,yxd.x,yxd.d) yxd.history}
  | S -> {yxd with y = yxd.y + 1; cost = yxd.cost + 1; history = Set.add (yxd.y+1,yxd.x,yxd.d) yxd.history}
  | W -> {yxd with x = yxd.x - 1; cost = yxd.cost + 1; history = Set.add (yxd.y,yxd.x-1,yxd.d) yxd.history}
  | E -> {yxd with x = yxd.x + 1; cost = yxd.cost + 1; history = Set.add (yxd.y,yxd.x+1,yxd.d) yxd.history}
 let can_move (map : string array) yxd =
  match yxd.d with
  | N -> map.[yxd.y-1].[yxd.x] <> '#'
  | S -> map.[yxd.y+1].[yxd.x] <> '#'
  | W -> map.[yxd.y].[yxd.x-1] <> '#'
  | E -> map.[yxd.y].[yxd.x+1] <> '#'

let problem_16 () =
 let example = false in
 let map =
  System.IO.File.ReadLines(if example then "16e.txt" else "16.txt") |> Seq.toArray in
 let map_h = Array.length map in
 let map_w = String.length map.[0] in
 let moves = System.Collections.Generic.PriorityQueue<YXD.t,int>() in
 let seen = Array3D.create map_h map_w 4 (System.Int32.MaxValue, Set.empty) in
 let start_yx = (map_h - 2, 1) in
 let end_yx = (1, map_w - 2) in
 let lowest = ref System.Int32.MaxValue in
 moves.Enqueue(YXD.singleton (fst start_yx, snd start_yx, Dir.E), 0);
 while moves.Count <> 0 do
  let yxd = moves.Dequeue () in
  (* track lowest continually to prune costly paths at the end *)
  if (yxd.y, yxd.x) = end_yx then lowest.Value <- min lowest.Value yxd.cost else ()
  let (c,h) = seen.[yxd.y, yxd.x, Dir.int_of_dir yxd.d] in
  if yxd.cost > lowest.Value then () else
  if yxd.cost > c then () else
  if yxd.cost < c then
   seen.[yxd.y, yxd.x, Dir.int_of_dir yxd.d] <- (yxd.cost, yxd.history)
   if YXD.can_move map yxd then let yxd_m = YXD.move yxd in moves.Enqueue(yxd_m, yxd_m.cost) else () ;
   (let yxd_cw = YXD.rotate_cw yxd in moves.Enqueue(yxd_cw, yxd_cw.cost));
   (let yxd_ccw = YXD.rotate_ccw yxd in moves.Enqueue(yxd_ccw, yxd_ccw.cost));
  else
   seen.[yxd.y, yxd.x, Dir.int_of_dir yxd.d] <- (yxd.cost, Set.union yxd.history h)
   (* required because we do not log rotations : trust me, it works *)
   if YXD.can_move map yxd then let yxd_m = YXD.move yxd in moves.Enqueue(yxd_m, yxd_m.cost) else () ;
 done
(*
 let lowest_score =
  seen.[fst end_yx, snd end_yx, 0 .. 3] |>
  Seq.fold (fun a (c,_) -> min a c) System.Int32.MaxValue in
*)
 let lowest_score = lowest.Value in
 let optimal_paths_at_end =
  seen.[fst end_yx, snd end_yx, 0 .. 3] |>
  Seq.filter (fun (c,_) -> c = lowest_score) |>
  Seq.fold (fun a (_,h) -> Set.union a h) Set.empty in
 (* strip Dir.t to remove duplicate directions at S and E *)
 let optimal_paths =
  optimal_paths_at_end |>
  Seq.fold (fun a (y,x,d) -> Set.union a (seen.[y,x,Dir.int_of_dir d] |> snd)) optimal_paths_at_end |>
  Set.map (fun (y,x,_) -> (y,x)) in
 lowest_score, optimal_paths.Count

let problem_17 () =
 let input =
  System.IO.File.ReadLines("17.txt") |> Seq.toArray in
 let a0 = input.[0].[12 ..] |> int64 in
 let b0 = input.[1].[12 ..] |> int64 in
 let c0 = input.[2].[12 ..] |> int64 in
 let program = input.[4].[9 ..].Split(',') |> Array.map int in
 let registers = Array.create 3 0L in
 let out = new System.Collections.Generic.List<int>() in
 let mutable ip = 0 in

 let literal_of_combo = function
 | n when n >= 0 && n <= 3 -> int64 n
 | idx when idx >= 4 && idx <= 6 -> registers.[idx-4]
 | _ -> failwith "Invalid Combo Operand!" in
 
 let run_with a0 =
  registers.[0] <- a0 ;
  registers.[1] <- b0 ;
  registers.[2] <- c0 ;
  out.Clear () ;
  let rec loop ip =
   if ip >= Array.length program - 1 then () else
    match program.[ip], program[ip+1] with
    | (*adv*) 0, c -> let n = literal_of_combo c in registers.[0] <- registers.[0] / (1L <<< (int n)) ; loop (ip + 2)
    | (*bxl*) 1, n -> registers.[1] <- registers.[1] ^^^ (int64 n) ; loop (ip + 2)
    | (*bst*) 2, c -> registers.[1] <- (literal_of_combo c) &&& 7L ; loop (ip + 2)
    | (*jnz*) 3, n -> if registers.[0] <> 0L then loop n else loop (ip + 2)
    | (*bxc*) 4, _ -> registers.[1] <- registers.[1] ^^^ registers.[2] ; loop (ip + 2)
    | (*out*) 5, c -> out.Add (int (literal_of_combo c &&& 7L)) ; loop (ip + 2)
    | (*bdv*) 6, c -> let n = literal_of_combo c in registers.[1] <- registers.[0] / (1L <<< (int n)); loop (ip + 2)
    | (*cdv*) 7, c -> let n = literal_of_combo c in registers.[2] <- registers.[0] / (1L <<< (int n)); loop (ip + 2)
    | _ -> failwith "Invalid Opcode!" in
  loop 0 in

 let out_to_string () =
  let outbuf = new System.Text.StringBuilder(out.Count * 2) in
  out |> Seq.iter (fun n -> outbuf.Append(n).Append(',') |> ignore) ;
  outbuf.Remove(outbuf.Length - 1,1).ToString()

 run_with a0 ;
 let res1 = out_to_string () in

 let rec find matching prefix i =
  run_with (prefix+i) ;
  if matching = (Seq.toArray out) then prefix+i else
  find matching prefix (i+1L) in
  
  (* find solutions in 3-bit chunks via right folding *)
  let rec find_a start =
   if start = Array.length program then 0L else
   find (program.[start..]) (find_a (start+1) <<< 3) 0L in

 let res2 = find_a 0 in
 res1,res2

let problem_18 () =
 let example = false in
 let input =
  System.IO.File.ReadLines(if example then "18e.txt" else "18.txt") |>
  Seq.map (fun s -> s.Split(',') |> (fun ss -> int ss.[1], int ss.[0])) |>
  Seq.toArray in
 let dim = if example then 7 else 71 in
 let map = Array2D.create dim dim '.'B in
 let cost = Array2D.create dim dim (-1) in
 let start_yx = (0,0) in
 let end_yx = (dim-1,dim-1) in
 let queue = System.Collections.Generic.Queue<int*int*int>() in
 let byte_len0 = if example then 12 else 1024 in

 let clear () =
  for y = 0 to dim - 1 do
   for x = 0 to dim - 1 do
    map.[y,x] <- '.'B ;
    cost.[y,x] <- (-1) ;
   done
  done
  queue.Clear () in

 let run byte_len =
  clear () ;
  (* unfortunately there is no fill function for Array2D *)
  for i = 0 to (min (Array.length input) byte_len) - 1 do
   let (y,x) = input.[i] in
   map.[y,x] <- '#'B ;
   cost.[y,x] <- 0
  done ;
  queue.Enqueue (fst start_yx, snd start_yx, 0) ;

  // CAUTION: because exeption paths are not considered alongsize match/with,
  // you must pull "loop ()" outside the match for tail-call optimization to work!
  let rec loop () =
   if queue.Count = 0 then None else
   let (y,x,c) = queue.Dequeue () in
   if (y,x) = end_yx then Some c else
   try
    match map.[y,x], cost.[y,x] with
    | '.'B, n when n < 0 ->
      cost.[y,x] <- c
      queue.Enqueue (y-1,x,c+1)
      queue.Enqueue (y+1,x,c+1)
      queue.Enqueue (y,x-1,c+1)
      queue.Enqueue (y,x+1,c+1)
    | _ -> ()
   with :? System.IndexOutOfRangeException -> ()
   loop () in
  loop ()

 let res1 = run byte_len0 |> Option.get in

(*
 let rec loop idx = if (run idx) = None then idx else loop (idx+1) in
 let byte_len = loop (byte_len0+1) in
*)

 let rec loop_bin_search start_idx end_idx =
  if end_idx < start_idx then start_idx else
  let idx = start_idx + ((end_idx - start_idx) >>> 1) in
  if (run idx) = None
  then loop_bin_search start_idx (idx - 1)
  else loop_bin_search (idx + 1) end_idx in

 let byte_len = loop_bin_search (byte_len0+1) (Array.length input - 1) in

 let res2 =
  input.[byte_len-1] |>
  (fun (y,x) -> (x,y)) in
 res1, res2

(* solution without trees *)
let problem_19 () =
 let example = false in
 let memo = new System.Collections.Generic.Dictionary<string,int64>() in
 let input = System.IO.File.ReadLines(if example then "19e.txt" else "19.txt") |> Seq.toArray in
 let subtowels =
  new System.Collections.Generic.HashSet<string> (input.[0].Split([|',';' '|],System.StringSplitOptions.RemoveEmptyEntries)) in
 let towels = input.[2 ..] in
 memo.[""] <- 1 ;
 let rec possibilities (s : string) =
  if memo.ContainsKey(s) then memo.[s] else
  let res =
   seq { 0 .. s.Length - 1 } |>
   Seq.filter (fun i -> subtowels.Contains(s.[..i])) |>
   Seq.fold (fun a i -> a + (possibilities (s.[i+1..]))) 0L in
  memo.[s] <- res ; res in
 let res_arr =
  towels |>
  Seq.map possibilities |>
  Seq.filter ((<>) 0L) |>
  Seq.toArray in
 (Array.length res_arr, Array.fold ( + ) 0L res_arr)

let problem_20 () =
 let example = false in
 let ascii = new System.Text.ASCIIEncoding () in
 let map =
  System.IO.File.ReadLines(if example then "20e.txt" else "20.txt") |>
  Seq.map ascii.GetBytes |> Seq.toArray in
 let (sy,sx) =
  map |>
  Array.fold (fun (yx,y) xs ->
   (yx |> Option.orElseWith (fun () -> xs |> Array.tryFindIndex ((=)'S'B) |> Option.map (fun x -> y,x))), y + 1)
   (None, 0) |> fst |> Option.get

 let radius_1 (y,x) =
  seq {y-1,x;y,x-1;y,x+1;y+1,x} in 

 let path = new System.Collections.Generic.List<int*int>() in
 path.Add ((sy,sx)) ;

 map.[sy].[sx] <- '#'B;

 let rec fill (y,x) =
  match
   radius_1 (y,x) |>
   Seq.tryFind (fun (y,x) -> try map.[y].[x] <> '#'B with _ -> false)
  with
  | None -> ()
  | Some (y',x') -> path.Add (y',x'); map.[y'].[x'] <- '#'B ; fill (y',x') in

 fill (path.[0]) ;
 
 let cutoff = 100 in
 let limit_a = 2 in
 let limit_b = 20 in
 let path_len = path.Count in

 let run limit =
  let mutable counter = 0 in
  for dst = path_len - 1 downto cutoff do
   for src = 0 to dst - cutoff - 1 do
    let (y',x') = path.[dst] in
    let (y, x ) = path.[src] in
    let r = abs (y' - y) + abs (x' - x) in
    if r <= limit && r >= 2 && (dst - src - r >= cutoff) then
     counter <- counter + 1
   done
  done ;
  counter in

 run limit_a, run limit_b

let problem_21 () =

 let min_dir =
  function
  | 'A','A' -> "A" | 'A','<' -> "v<<A" | 'A','v' -> "<vA" | 'A','>' -> "vA" | 'A','^' -> "<A"
  | '<','A' -> ">>^A" | '<','<' -> "A" | '<','v' -> ">A" | '<','>' -> ">>A" | '<','^' -> ">^A"
  | 'v','A' -> "^>A" | 'v','<' -> "<A" | 'v','v' -> "A" | 'v','>' -> ">A" | 'v','^' -> "^A"
  | '>','A' -> "^A" | '>','<' -> "<<A" | '>','v' -> "<A" | '>','>' -> "A" | '>','^' -> "<^A"
  | '^','A' -> ">A" | '^','<' -> "v<A" | '^','v' -> "vA" | '^','>' -> "v>A" | '^','^' -> "A"
  | _ -> "" in

 let min_num =
  function
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
  | _ -> "" in

 let hash_dir =
  function
  | 'A','A' -> 0 | 'A','<' -> 1 | 'A','v' -> 2 | 'A','>' -> 3 | 'A','^' -> 4
  | '<','A' -> 5 | '<','<' -> 6 | '<','v' -> 7 | '<','>' -> 8 | '<','^' -> 9
  | 'v','A' -> 10 | 'v','<' -> 11 | 'v','v' -> 12 | 'v','>' -> 13 | 'v','^' -> 14
  | '>','A' -> 15 | '>','<' -> 16 | '>','v' -> 17 | '>','>' -> 18 | '>','^' -> 19
  | '^','A' -> 20 | '^','<' -> 21 | '^','v' -> 22 | '^','>' -> 23 | '^','^' -> 24
  | _ -> System.Int32.MinValue in

 let depth = 25 in
 let memo = Array.create (25*depth) None in
 let memo_get (n,src,dst) = memo.[25*(n-1)+(hash_dir(src,dst))] in
 let memo_set (n,src,dst) v = memo.[25*(n-1)+(hash_dir(src,dst))] <- Some v in

 (* perform step 1 outside; memoize only directional keypad *)
 let rec robot_n_len n (src,dst) =
  if n = 0 then 1L else
  match memo_get (n,src,dst) with
  | Some res -> res
  | None ->
    if n = 1 then String.length (min_dir (src,dst)) |> int64 else
    let res =
     //let s = Seq.append (Seq.singleton 'A') (min_dir (src,dst)) in
     let s = seq { 'A' ; yield! (min_dir (src,dst))} in
     Seq.zip s (Seq.skip 1 s) |>
     Seq.map (robot_n_len (n-1)) |>
     Seq.fold ( + ) 0L in
    (memo_set (n,src,dst) res; res) in

 let translate_n n input =
  let s0 = seq { 'A' ; yield! input } in
  let s1 =
   seq {
    'A' ;
     yield!
      (Seq.zip s0 (Seq.skip 1 s0) |>
       Seq.map min_num |>
       Seq.concat) } in
  Seq.zip s1 (Seq.skip 1 s1) |>
  Seq.map (robot_n_len n) |>
  Seq.fold ( + ) 0L in

 let example = false in
 let input = System.IO.File.ReadLines(if example then "21e.txt" else "21.txt") |> Seq.toArray in
 let res1 =
  input |>
  Seq.map (fun s -> (s |> translate_n 2) * (s.[0 .. 2] |> int64)) |>
  Seq.fold ( + ) 0L in
 let res2 = 
  input |>
  Seq.map (fun s -> (s |> translate_n depth) * (s.[0 .. 2] |> int64)) |>
  Seq.fold ( + ) 0L in
 res1, res2

let problem_22 () =
 let prng secret =
  let secret = ((secret <<< 6 ) ^^^ secret) &&& ((1 <<< 24) - 1) in
  let secret = ((secret >>> 5 ) ^^^ secret) &&& ((1 <<< 24) - 1) in
  let secret = ((secret <<< 11) ^^^ secret) &&& ((1 <<< 24) - 1) in
  secret in

 let generate_all n seed =
  let res = Array.create (n+1) 0 in
  res.[0] <- seed ;
  let mutable seed = seed in
  for i = 1 to n do
   seed <- prng seed ;
   res.[i] <- seed
  done ;
  res in

 let diffs (ns : int array) =
  let s = ns |> Seq.map (fun n -> n % 10) in
  Seq.zip s (Seq.skip 1 s) |>
  Seq.map (fun (last, next) -> (next, next-last)) in

 let seen = Array.create (20*20*20*20) false in
 let seen_get (n1,n2,n3,n4) = seen.[(n1+10)*20*20*20+(n2+10)*20*20+(n3+10)*20+(n4+10)] in
 let seen_set (n1,n2,n3,n4) = seen.[(n1+10)*20*20*20+(n2+10)*20*20+(n3+10)*20+(n4+10)] <- true in
 let seen_clear () = Array.fill seen 0 (20*20*20*20) false in
 let sums = Array.create (20*20*20*20) 0 in
 let sums_update (n1,n2,n3,n4) v =
  sums.[(n1+10)*20*20*20+(n2+10)*20*20+(n3+10)*20+(n4+10)] <- 
   sums.[(n1+10)*20*20*20+(n2+10)*20*20+(n3+10)*20+(n4+10)] + v in

 let tabulate_sums ds =
  for i = 3 to Array.length ds - 1 do
   let (_,n1) = ds.[i-3] in
   let (_,n2) = ds.[i-2] in
   let (_,n3) = ds.[i-1] in
   let (v,n4) = ds.[i] in
   if not (seen_get (n1,n2,n3,n4)) then
    (sums_update (n1,n2,n3,n4) v ; seen_set (n1,n2,n3,n4)) ;
  done

 let example = false in
 let generated =
  System.IO.File.ReadLines (if example then "22e.txt" else "22.txt") |>
  Seq.map int |>
  Seq.map (generate_all 2000) |>
  Seq.toArray in
 let res1 =
  generated |>
  Seq.map (fun row -> int64 row.[2000]) |>
  Seq.fold ( + ) 0L in
 let res2 =
  generated |>
  Seq.map (fun ns -> ns |> diffs |> Seq.toArray) |>
  Seq.iter (fun ds -> tabulate_sums ds ; seen_clear ()) ;
  Array.fold max 0 sums in
 res1, res2

let problem_23 () =
 let adr_of_s (s : string) = (int s.[0] - 0x61)*26+(int s.[1] - 0x61) in
 let adr_to_s (adr : int) = new System.String([|char ((adr/26)+0x61);char ((adr%26)+0x61)|]) in
 let graph = System.Collections.Generic.Dictionary<int,System.Collections.Generic.SortedSet<int>>() in
 let example = false in
 System.IO.File.ReadLines(if example then "23e.txt" else "23.txt") |>
 Seq.iter (fun s ->
  let pair = s.Split('-',System.StringSplitOptions.RemoveEmptyEntries) in
  if not <| graph.ContainsKey(adr_of_s pair.[0]) then graph.[adr_of_s pair.[0]] <- new System.Collections.Generic.SortedSet<int>() ;
  if not <| graph.ContainsKey(adr_of_s pair.[1]) then graph.[adr_of_s pair.[1]] <- new System.Collections.Generic.SortedSet<int>() ;
  graph.[adr_of_s pair.[0]].Add(adr_of_s pair.[1]) |> ignore ;
  graph.[adr_of_s pair.[1]].Add(adr_of_s pair.[0]) |> ignore ) ;
 let loops = ref 0 in
 let starts_with_t adr = adr >= adr_of_s "ta" && adr < adr_of_s "ua" in
 // will count 3 times cw and 3 times ccw, so divide by 6
 // note: when making a set of a mutable set, you need a comparer : System.Collections.Generic.SortedSet<int>.CreateSetComparer()
 // Set<int> does not need an explicit comparer
 let log_loop (key: int) =
  graph.[key] |>
  Seq.iter (fun adr1 ->
   (graph.[adr1] |> Set.ofSeq).Remove(key) |>
   Seq.iter (fun adr2 ->
    if graph.[adr2].Contains(key) && (starts_with_t key || starts_with_t adr1 || starts_with_t adr2)
    then loops.Value <- loops.Value + 1)) in
 graph.Keys |> Seq.iter log_loop ; 
 let res1 = loops.Value / 6 in
 let saturated = new System.Collections.Generic.HashSet<Set<int>>() in
 (* the "smart" way of doing things *)
 let rec log_saturated key (required : Set<int>) =
  if saturated.Contains(required) then () else
  saturated.Add(required) |> ignore;
  graph.[key] |>
  Seq.iter (fun adr ->
   if Set.contains adr required then () else
   if graph.[adr].IsSupersetOf(required) then log_saturated adr (Set.add adr required))
 graph.Keys |>
 Seq.iter (fun key -> log_saturated key (Set.singleton key)) ;
 let res2 =
  saturated |>
  Seq.fold (fun a set -> if Set.count set > Set.count a then set else a) Set.empty |>
  Set.map adr_to_s |>
  Seq.fold (fun (a : System.Text.StringBuilder) s -> a.Append(s).Append(',')) (new System.Text.StringBuilder(15*3)) |>
  ( fun sb -> sb.Remove(sb.Length-1,1) ) |>
  string in
 printfn "%d" saturated.Count ;
 res1, res2

(* very slow, but fully automated *)
let problem_24 () =
 let example = false in
 let debug = true in
 let input =
  System.IO.File.ReadLines(if example then "24e.txt" else "24.txt") |>
  Seq.toArray in
 let brk = input |> Array.tryFindIndex ((=)"") |> Option.get in
 let nodes = new System.Collections.Generic.Dictionary<string,int>() in
 (* opcodes : 0uy - XOR, 1uy - AND, 2uy = OR *)
 let op_of_string = function
  | "XOR" -> 0uy
  | "AND" -> 1uy
  | "OR" -> 2uy
  | _ -> 255uy
 let op_to_string = function
  | 0uy -> "XOR"
  | 1uy -> "AND"
  | 2uy -> "OR"
  | _ -> "INVALID"
 (* key = output *)
 let gates = new System.Collections.Generic.Dictionary<string,string * string * byte>()
 let scores = new System.Collections.Generic.Dictionary<string * string,int>()
 let in_len = if example then 5 else 45 in
 let z_len = if example then 13 else 46 in
 let set_xy (x,y) =
  x |>
  Seq.unfold (fun n -> Some (n &&& 1L, n >>> 1)) |>
  Seq.take in_len |>
  Seq.indexed |>
  Seq.iter (fun (i,bit) -> nodes[sprintf "x%02d" i] <- int bit) ;
  y |>
  Seq.unfold (fun n -> Some (n &&& 1L,n >>> 1)) |>
  Seq.take in_len |>
  Seq.indexed |>
  Seq.iter (fun (i,bit) -> nodes[sprintf "y%02d" i] <- int bit) in
 let seen = System.Collections.Generic.HashSet<string>() in
 let rec get_value key =
  if nodes.ContainsKey(key) then nodes.[key] else
   let (l,r,op) = gates.[key] in
    seen.Add (key) |> ignore;
    if seen.Contains(l) || seen.Contains(r) then failwith "Invalid Loop" else
    if op = 0uy then (let res = (get_value l) ^^^ (get_value r) in nodes.[key] <- res; seen.Remove(key) |> ignore ; res) else
    if op = 1uy then (let res = (get_value l) &&& (get_value r) in nodes.[key] <- res; seen.Remove(key) |> ignore ; res) else
    if op = 2uy then (let res = (get_value l) ||| (get_value r) in nodes.[key] <- res; seen.Remove(key) |> ignore ; res) else
    failwith "Invalid Opcode" in
 let get_z () =
  seq { 0 .. z_len - 1 } |>
  Seq.map (sprintf "z%02d") |>
  Seq.map get_value |>
  Seq.indexed |>
  Seq.fold (fun a (i,n) -> a ||| (int64 n <<< i)) 0L in
 let reset_with x y =
  seen.Clear () ;
  nodes.Clear () ;
  set_xy (x,y) in
 let set_initials () =
  for i = 0 to brk - 1 do
   nodes.[input.[i].[0 .. 2]] <- int (input.[i].[5 .. 5])
  done
 let load_gates () =
  for i = brk+1 to Array.length input - 1 do
   input.[i].Split(' ') |>
   (fun split -> gates.[split.[4]] <- (split.[0], split.[2], op_of_string (split.[1])))
  done
 set_initials () ;
 load_gates () ;
 let res1 = get_z () in
 let test_bit n =
  let mutable res = 0 in
  (* either-or *)
  if n < in_len then
   reset_with (1L <<< n) 0L 
   if get_z () <> (1L <<< n) then res <- res ||| 0x1 ;
   reset_with 0L (1L <<< n)
   if get_z () <> (1L <<< n) then res <- res ||| 0x2 ;
  (* simple carry *)
  if n > 0 then
   reset_with (1L <<< (n-1)) (1L <<< (n-1))
   if get_z () <> (1L <<< n) then res <- res ||| 0x4 ;
  (* multiple carry *)
  if n > 1 then
   reset_with ((1L <<< (n-1)) ||| (1L <<< (n-2))) ((1L <<< (n-1)) ||| (1L <<< (n-2)))
   if get_z () <> ((1L <<< n) ||| (1L <<< (n-1))) then res <- res ||| 0x8 ;
   reset_with (1L <<< (n-2)) ((1L <<< (n-1)) ||| (1L <<< (n-2)))
   if get_z () <> (1L <<< n) then res <- res ||| 0x10 ;
  res
 let rec next_failure n =
  let test =
   try
    test_bit n
   with _ -> 0x80 in
  if test <> 0
  then n
  else next_failure (n+1) in
 let swap key1 key2 =
  let gate = gates.[key1] in
  gates.[key1] <- gates.[key2];
  gates.[key2] <- gate in

 let score_swap key1 key2 =
  swap key1 key2 ;
  try scores.[(key1,key2)] <- next_failure 0 with _ -> ();
  swap key1 key2 in
 let best_swap () =
  scores.Clear () ;
  let baseline = next_failure 0 in
  assert (baseline < z_len) ;
  let (_,_,zop) = gates.[sprintf "z%02d" baseline] in
  (* any gate pointing to z__ must be an XOR... except the last carry *)
  if zop <> 0uy (* XOR *) && baseline <> (z_len - 1) then
   let zkey = sprintf "z%02d" baseline in
   let keys = gates.Keys |> Seq.toArray in
   for i = 0 to Array.length keys - 1 do
    let (_,_,op) = gates.[keys.[i]] in
    if op = 0uy then score_swap (zkey) (keys.[i]) else ()
   done ;
  else
   let keys = gates.Keys |> Seq.toArray in
   for i = 0 to Array.length keys - 2 do
    for j = i+1 to Array.length keys - 1 do
     score_swap (keys.[i]) (keys.[j])
    done
   done
  let best =
   scores |>
   Seq.fold (fun a kv -> if kv.Value > a then kv.Value else a) (-1) in
  scores |>
  Seq.filter (fun kv -> kv.Value = best) |>
  Seq.toArray in

(*
 swap "z16" "fkb"
 swap "nnr" "rqf"
 swap "z31" "rdn"
 swap "z37" "rrn"
 reset_with 2024L 48L
*)

 let res2 = ref [] in
 for i = 1 to 4 do
  let candidates = best_swap () |> Array.map (fun kv-> kv.Key) in
  if Array.length candidates > 1 then
   printfn "Manual Check Required!"
   candidates |>
   Seq.iter (fun (s1,s2) -> printfn "%s,%s" s1 s2)
  else
   candidates.[0] ||> swap
   if debug then 
    candidates |>
    Seq.iter (fun (s1,s2) -> printfn "%s,%s" s1 s2)
   res2.Value <- (snd candidates.[0])::(fst candidates.[0])::res2.Value
 done
 let res2 =
  List.sort (res2.Value) |>
  Seq.fold
   (fun (sb : System.Text.StringBuilder) key -> sb.Append(key).Append(','))
   (new System.Text.StringBuilder (4*8)) |>
  (fun sb -> sb.Remove(sb.Length-1,1)) |>
  string in

 res1, res2

let problem_25 () =
 let example = false in
 let input =
  System.IO.File.ReadLines(if example then "25e.txt" else "25.txt") |>
  Seq.toArray in
 let num_slots = (Array.length input + 1) >>> 3 in
 let is_lock slot = input.[slot<<<3] = "#####" in
 let parse slot =
  let res = Array.create 5 0 in
  for y = 1 to 5 do
   for x = 0 to 4 do
    if input.[(slot<<<3)+y].[x] = '#' then res.[x] <- res.[x] + 1 ;
   done
  done ;
  (res.[0],res.[1],res.[2],res.[3],res.[4]) in
 let valid (a,b,c,d,e) (a',b',c',d',e') =
  a + a' < 6 && b + b' < 6 && c + c' < 6 && d + d' < 6 && e + e' < 6 in
 let (keys, locks) =
  seq { 0 .. num_slots - 1 } |>
  Seq.fold
   (fun (keys, locks) slot ->
    if is_lock slot
    then (keys, Set.add (parse slot) locks)
    else (Set.add (parse slot) keys, locks))
   (Set.empty, Set.empty) in
 let mutable count = 0 in
 keys |>
 Set.iter (fun key ->
  locks |>
  Set.iter (fun lock ->
   if valid key lock then count <- count + 1)) ;
 count
