(* Advent of Code 2025 *)

let problem_01a () =
 let example = false in
 let pmod a b = let m = a mod b in if m < 0 then m + b else m in
 let start = 50 in
 let decode s =
  String.sub s 1 (String.length s - 1) |>
  int_of_string |>
  (if s.[0] = 'L' then (~-) else (~+)) in
 In_channel.(with_open_bin (if example then "01e.txt" else "01.txt") input_lines) |> List.map decode |>
 List.fold_left
  (fun (a,i) x ->
   let i' = pmod (i + x) 100 in
   (* test for landing on 0 *)
   let a' = if i' = 0 then a + 1 else a in
   (a',i')) (0,start) |> fst

(* 0x434C49434B CLICK *)
let problem_01b () =
 let example = false in
 let pmod a b = let m = a mod b in if m < 0 then m + b else m in
 let start = 50 in
 let decode s =
  String.sub s 1 (String.length s - 1) |>
  int_of_string |>
  (if s.[0] = 'L' then (~-) else (~+)) in
 In_channel.(with_open_bin (if example then "01e.txt" else "01.txt") input_lines) |> List.map decode |>
 (* extra caution: filter 0 to prevent counting without clicking *)
 List.filter ((<>)0) |>
 List.fold_left
  (fun (a,i) x ->
   let i' = pmod (i + x) 100 in
   (* test for crossing but not landing, including x = 100 *)
   let a' = if i <> 0 && i' <> 0 && (x < 0 && i' >= i || x > 0 && i' <= i) then a + 1 else a in
   (* test for full loops (x > 100)  *)
   let a' = a' + ((abs x) - 1) / 100 in
   (* test for landing on 0 *)
   let a' = if i' = 0 then a' + 1 else a' in
   (a',i')) (0,start) |> fst

let problem_02a () =
 let example = false in
 let pow10 n =
  let rec loop a p10 = if a = 0 then p10 else loop (a-1) (p10 * 10) in
  if n < 0 then 0 else loop n 1 in
 let digits n =
  let rec loop a p10 = if n < p10 then a else loop (a+1) (p10 * 10) in
  loop 1 10 in
 let find_invalid (low, high) =
  let dlow = digits low and dhigh = digits high in
  let (dlow,low) =
   if dlow land 1 = 1
   then (dlow + 1, pow10 (dlow))
   else (dlow,low) in
  let (dhigh,high) =
   if dhigh land 1 = 1
   then (dhigh - 1, pow10 (dhigh - 1) - 1)
   else (dhigh,high) in
  let ilow  = low / (pow10 (dlow lsr 1)) in
  let ihigh = high / (pow10 (dhigh lsr 1)) in
  let ilow = if ilow * (pow10 (dlow lsr 1)) + ilow < low then ilow + 1 else ilow in
  let ihigh = if ihigh * (pow10 (dhigh lsr 1)) + ihigh > high then ihigh - 1 else ihigh in
  if ilow > ihigh then None else Some (ilow, ihigh) in
 let sum_invalid = function
  | None -> 0
  | Some (ilow, ihigh) ->
    let res = ref 0 in
    for i = ilow to ihigh do
     res := !res + i * (pow10 (digits i)) + i
    done ;
    !res in
 let input =
  In_channel.(with_open_bin (if example then "02e.txt" else "02.txt") input_line) |> Option.get |>
  String.split_on_char ',' |>
  List.map
  (fun s ->
   let dash = String.index s '-' in
   (int_of_string @@ String.sub s 0 dash), (int_of_string @@ String.sub s (dash+1) (String.length s - dash - 1))) in
  input |>
  List.map (fun x -> x |> find_invalid |> sum_invalid) |>
  List.fold_left (+) 0

let problem_02b () =
 let example = false in

 let pow10 n =
  let rec loop a p10 = if a = 0 then p10 else loop (a-1) (p10 * 10) in
  if n < 0 then 0 else loop n 1 in

 let digits n =
  let rec loop a p10 = if n < p10 then a else loop (a+1) (p10 * 10) in
  loop 1 10 in

 (* generalize from 2 to n *)
 let repeat x n =
  let d = digits x in
  let rec loop a n =
   if n < 1 then 0 else
   if n = 1 then a else
   loop (a * pow10 d + x) (n-1)
  in loop x n in

 (* generalize from 2 to n *)
 let find_invalid_n n (low, high) =
  let dlow = digits low and dhigh = digits high in
  let (dlow,low) =
   if dlow mod n <> 0
   then (dlow + (n - (dlow mod n)), pow10 (dlow + (n - (dlow mod n)) - 1))
   else (dlow,low) in
  let (dhigh,high) =
   if dhigh mod n <> 0
   then (dhigh - (dlow mod n), pow10 (dhigh - (dlow mod n)) - 1)
   else (dhigh,high) in
  let ilow  = low / (pow10 (dlow - dlow / n)) in
  let ihigh = high / (pow10 (dhigh - dhigh / n)) in
  let ilow = if repeat ilow n < low then ilow + 1 else ilow in
  let ihigh = if repeat ihigh n > high then ihigh - 1 else ihigh in
  if ilow > ihigh then None else Some (ilow, ihigh) in

 (* use sets because multiple n searches can hit the same number (e.g., 222222) *)
 let module ISet = Set.Make(Int) in

 let collect_invalid_n iset (n,ilow,ihigh) =
   Seq.ints ilow |> Seq.take_while ((>=)ihigh) |>
   Seq.fold_left (fun a i -> ISet.add (repeat i n) a) iset in

 let find_invalid_all (low, high) =
  let dhigh = digits high in
  let ranges =
   Seq.ints 2 |> Seq.take_while ((>=)dhigh) |>
   Seq.filter_map (fun n -> find_invalid_n n (low, high) |> Option.map (fun (ilow,ihigh) -> (n,ilow,ihigh))) in
  Seq.fold_left collect_invalid_n ISet.empty ranges in

 In_channel.(with_open_bin (if example then "02e.txt" else "02.txt") input_line) |> Option.get |>
 String.split_on_char ',' |>
 List.map
 (fun s ->
  let dash = String.index s '-' in
  (int_of_string @@ String.sub s 0 dash), (int_of_string @@ String.sub s (dash+1) (String.length s - dash - 1))) |>
 List.map find_invalid_all |>
 List.fold_left ISet.union ISet.empty |>
 Fun.flip (ISet.fold (+)) 0
(*
  (* print rather than sum, to check against example *)
  ISet.iter (Printf.printf "%d\n")
*)

(* find left-most highest number and scan right for highest second number *)
let problem_03a () =
 let example = false in
 let joltage line =
  let (lmax, imax, _) =
   String.sub line 0 (String.length line - 1) |>
   String.fold_left
   (fun (cmax,imax,i) c -> if c > cmax then (c,i,i+1) else (cmax,imax,i+1)) ('\x00', (-1), 0) in
  let rmax =
   String.sub line (imax+1) (String.length line - imax - 1) |>
   String.fold_left max '\x00' in
  (Char.code lmax - 0x30) * 10 + (Char.code rmax - 0x30) in
 In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
 List.map joltage |>
 List.fold_left (+) 0

(* 2 -> 12 ; try generalizing to n *)
let problem_03b () =
 let example = false in
 let limit = 12 in

 let rec joltage_n n a line =
  if n = 0 then a else
  let (cmax, imax, _) =
   String.sub line 0 (String.length line - (n-1)) |>
   String.fold_left
   (fun (cmax,imax,i) c -> if c > cmax then (c,i,i+1) else (cmax,imax,i+1)) ('\x00', (-1), 0) in
  let a' = (a * 10 + (Char.code cmax - 0x30)) in
  if n = 1 then a' else
  joltage_n (n-1) a' (String.sub line (imax+1) (String.length line - imax - 1)) in

 In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
 List.map (joltage_n limit 0) |>
 List.fold_left (+) 0

(* use slices to avoid allocating substrings *)
(* no faster than problem_03b when compiled *)
let problem_03b2 () =
 let example = false in
 let limit = 12 in

 let rec fold_left_slice f a (s,i,len) =
  if len = 0 then a else fold_left_slice f (f a s.[i]) (s,i+1,len-1) in

 let rec joltage_n n a (line,i,len) =
  if n <= 0 then a else
  let (cmax, imax, _) =
   fold_left_slice
    (fun (cmax,imax,i) c -> if c > cmax then (c,i,i+1) else (cmax,imax,i+1))
    ('\x00', (-1), 0)
    (line, i, len - (n-1)) in
  let a' = (a * 10 + (Char.code cmax - 0x30)) in
  if n = 1 then a' else
  joltage_n (n-1) a' (line,i+imax+1,len-imax-1) in

 In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
 List.map (fun s -> joltage_n limit 0 (s,0,String.length s)) |>
 List.fold_left (+) 0

let problem_04a () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "04e.txt" else "04.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length input.(0) in
 let adj y x =
  [y-1,x-1; y-1,x; y-1,x+1
  ;y  ,x-1;        y  ,x+1
  ;y+1,x-1; y+1,x; y+1,x+1] |>
  List.fold_left
  (fun a (y,x) -> if y >= 0 && y < h && x >= 0 && x < w && input.(y).[x] = '@' then succ a else a) 0 in
 let res = ref 0 in
 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   if input.(y).[x] = '@' && adj y x < 4
   then incr res
  done
 done ;
 !res

let problem_04b () =
 let example = false in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let input = In_channel.(with_open_bin (if example then "04e.txt" else "04.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let h = Array.length input in
 let w = Bytes.length input.(0) in
 let adj y x =
  [y-1,x-1; y-1,x; y-1,x+1
  ;y  ,x-1;        y  ,x+1
  ;y+1,x-1; y+1,x; y+1,x+1] |>
  List.fold_left
  (fun a (y,x) -> if y >= 0 && y < h && x >= 0 && x < w && input.(y).%[x] = '@' then succ a else a) 0 in

 (* it is more efficient to 'change as we go' instead of buffer swapping in this instance *)
 let rec loop a =
  let res = ref 0 in
  for y = 0 to h - 1 do
   for x = 0 to w - 1 do
    if input.(y).%[x] = '@' && adj y x < 4
    then (input.(y).%[x] <- '.' ; incr res)
   done
  done ;
  if !res = 0 then a else (loop [@tailcall]) (!res+a) in
 loop 0

(* optimized version *)
(* faster in bytecode, same optimized *)
let problem_04b2 () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "04e.txt" else "04.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length input.(0) in

 let adj y x =
  [y-1,x-1; y-1,x; y-1,x+1
  ;y  ,x-1;        y  ,x+1
  ;y+1,x-1; y+1,x; y+1,x+1] |>
  List.fold_left
  (fun a (y,x) -> if y >= 0 && y < h && x >= 0 && x < w && input.(y).[x] = '@' then succ a else a) 0 in

 let adj_cache = Array.make_matrix h w 0 in
 let q = Queue.create () in

 (* build cache *)
 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   if input.(y).[x] = '@' then
   let a = adj y x in
   let _ = adj_cache.(y).(x) <- a in
   if a < 4 then Queue.push (y,x) q
  done
 done;

 let removed = ref 0 in
 while not @@ Queue.is_empty q do
  let (y,x) = Queue.take q in
  incr removed ;
  [y-1,x-1; y-1,x; y-1,x+1
  ;y  ,x-1;        y  ,x+1
  ;y+1,x-1; y+1,x; y+1,x+1] |>
  List.filter (fun (y,x) -> y >= 0 && y < h && x >= 0 && x < w && input.(y).[x] = '@') |>
  List.iter (fun (y,x) -> adj_cache.(y).(x) <- adj_cache.(y).(x) - 1 ; if adj_cache.(y).(x) = 3 then Queue.add (y,x) q)
 done ;
 !removed

(* ranges can overlap! *)
(* probably want to combine ranges, but let's do a simple search first *)
let problem_05a () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "05e.txt" else "05.txt") input_lines) |> Array.of_list in
 let midpoint = Array.find_index ((=)"") input |> Option.get in
 let ranges =
  Array.sub input 0 midpoint |>
  Array.map (fun s ->
   let midpoint = String.index s '-' in
   let low = int_of_string @@ String.sub s 0 midpoint in
   let high = int_of_string @@ String.sub s (midpoint+1) (String.length s - midpoint - 1) in
   low,high) in
 let ids =
  Array.sub input (midpoint+1) (Array.length input - midpoint - 1) |>
  Array.map (int_of_string) in

 (* pre-sort to guarantee some assumptions and reduce redundancy *)
 Array.sort compare ids ;
 Array.sort compare ranges ;

 let rec loop a i j =
  if j >= Array.length ids then a else
  if i >= Array.length ranges then a else
  let (rmin,rmax) = ranges.(i) in
  if ids.(j) >= rmin && ids.(j) <= rmax then loop (a+1) i (j+1) else
  if ids.(j) < rmin then loop a i (j+1) else
  loop a (i+1) j
 in

 loop 0 0 0

(* guess what? this one is about combining ranges! *)
let problem_05b () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "05e.txt" else "05.txt") input_lines) |> Array.of_list in
 let midpoint = Array.find_index ((=)"") input |> Option.get in
 let ranges =
  Array.sub input 0 midpoint |>
  Array.map (fun s ->
   let midpoint = String.index s '-' in
   let low = int_of_string @@ String.sub s 0 midpoint in
   let high = int_of_string @@ String.sub s (midpoint+1) (String.length s - midpoint - 1) in
   low,high) in

 Array.sort compare ranges ;

 (* create a list of only disjoint ranges! *)
 (* because we've presorted we can make some assumptions: e.g., min0 <= min1 *)
 (* generic loop can be collapsed into a simpler fold *)

 Array.fold_left
  (fun a (min1, max1) ->
   match a with [] -> (min1, max1)::a | (min0, max0)::tl ->
   if min1 > max0 + 1 then (min1, max1)::a
   else (min0, max max0 max1)::tl) [] ranges |>

 List.fold_left (fun a (rmin,rmax) -> a + rmax - rmin + 1) 0

let problem_06a () =
 let example = false in
 let input =
  In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |>
  List.map
   (fun s -> s |>
    String.split_on_char ' ' |>
    List.filter ((<>)"") |>
    List.map
    (fun s ->
     (* encode op as negative int *)
     if s.[0] = '+' then (-1) else
     if s.[0] = '*' then (-2) else
     int_of_string s) |>
    Array.of_list) |>
  Array.of_list in

 (* decode op *)
 let fold_of_op = function
  | -1 -> Seq.fold_left ( + ) 0
  | -2 -> Seq.fold_left ( * ) 1
  | _ -> assert false in

 let h = Array.length input in
 let w = Array.length input.(0) in

 (* shadow to cleanup fold *)
 let ops = input.(h-1) in
 let h = h - 1 in

 Seq.init w Fun.id |>
 Seq.fold_left
 (fun a x ->
  Seq.init h Fun.id |>
  Seq.map (fun y -> input.(y).(x)) |>
  fold_of_op (ops.(x)) |>
  (+) a) 0

let problem_06b () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |> Array.of_list in

 let h = Array.length input in
 let w = String.length input.(0) in
 let sbuf = Buffer.create h in

 (* right-to-left traversal, as intended, works w/o extra column of blanks *)
 Seq.init w (fun i -> w - i - 1) |>
 Seq.fold_left
 (fun (res,nbuf) x ->
  Buffer.clear sbuf ;
  for y = 0 to h - 2 do if input.(y).[x] <> ' ' then Buffer.add_char sbuf input.(y).[x] done ;
  match int_of_string_opt (Buffer.contents sbuf) with
  | Some n when input.(h-1).[x] = '+' -> res + (List.fold_left ( + ) 0 (n::nbuf)), []
  | Some n when input.(h-1).[x] = '*' -> res + (List.fold_left ( * ) 1 (n::nbuf)), []
  | Some n -> res, n::nbuf
  | None -> res, [])
 (0, []) |>
 fst

(* splitter *)
(* only 0-indexed even lines contain ^s *)
(* merging beams do not count twice *)
let problem_07a () =
 let example = false in
 let module ISet = Set.Make(Int) in
 let input = In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_lines) in

 let (start, input) =
  match input with
  | [] -> assert false
  | line0::tl ->
    (String.index line0 'S', List.tl tl) in

 (* custom fold to skip empty lines *)
 let splitters =
  let rec loop a = function
  | hd::_::tl | hd::tl ->
    (* messy because String.fold_lefti is not a stdlib function *)
    loop ((String.fold_left (fun (a,i) c -> if c = '^' then ISet.add i a, i+1 else a, i+1) (ISet.empty,0) hd |> fst)::a) tl
  | [] -> List.rev a
  in loop [] input in

 let update (count,beams) splitters =
  let contacts = ISet.inter beams splitters in
  let left_split = ISet.map ((+) 1) contacts in
  let right_split = ISet.map (Fun.flip (-) 1) contacts in
  let count = count + ISet.cardinal contacts in
  let beams =
   ISet.diff beams contacts |>
   ISet.union left_split |>
   ISet.union right_split in
  (count, beams) in

 List.fold_left update (0, ISet.singleton start) splitters |> fst

(* this time, overlaps do count *)
let problem_07b () =
 let example = false in
 let module ISet = Set.Make(Int) in
 (* using an array for caching would probably be faster, but this also works *)
 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in
 let input = In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_lines) in

 let (start, input) =
  match input with
  | [] -> assert false
  | line0::tl ->
    (String.index line0 'S', List.tl tl) in

 (* custom fold to skip empty lines *)
 (* + arrayify for index access *)
 let splitters =
  let rec loop a = function
  | hd::_::tl | hd::tl ->
    (* messy because String.fold_lefti is not a stdlib function *)
    loop ((String.fold_left (fun (a,i) c -> if c = '^' then ISet.add i a, i+1 else a, i+1) (ISet.empty,0) hd |> fst)::a) tl
  | [] -> List.rev a
  in loop [] input |> Array.of_list in

 let max_depth = Array.length splitters - 1 in
 let cache = ref YXMap.empty in
 let rec dfs (depth,x) =
  if depth > max_depth then 1 else
  match YXMap.find_opt (depth,x) !cache with
  | Some n -> n
  | None ->
    let res =
     if not @@ ISet.mem x splitters.(depth)
     then dfs (depth+1,x)
     else dfs (depth+1,x-1) + dfs (depth+1,x+1) in
    cache := YXMap.add (depth,x) res !cache; res in

 dfs (0,start)

(* this "faster" version uses a flat array cache, with cursed isosceles-triangular indexing *)
let problem_07b2 () =
 let example = false in
 let module ISet = Set.Make(Int) in
 let input = In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_lines) in

 let (start, input) =
  match input with
  | [] -> assert false
  | line0::tl ->
    (String.index line0 'S', List.tl tl) in

 (* custom fold to skip empty lines *)
 (* + arrayify for index access *)
 let splitters =
  let rec loop a = function
  | hd::_::tl | hd::tl ->
    (* messy because String.fold_lefti is not a stdlib function *)
    loop ((String.fold_left (fun (a,i) c -> if c = '^' then ISet.add i a, i+1 else a, i+1) (ISet.empty,0) hd |> fst)::a) tl
  | [] -> List.rev a
  in loop [] input |> Array.of_list in

 let max_depth = Array.length splitters - 1 in
 let[@inline] idx_of_yx (y,x) = y * y + (x + y - start) in
 let cache = Array.make ((max_depth+1)*(max_depth+1)) (-1) in

 let rec dfs (depth,x) =
  if depth > max_depth then 1 else
  if cache.(idx_of_yx (depth,x)) >= 0 then cache.(idx_of_yx (depth,x)) else
   let res =
    if not @@ ISet.mem x splitters.(depth)
    then dfs (depth+1,x)
    else dfs (depth+1,x-1) + dfs (depth+1,x+1) in
   (cache.(idx_of_yx (depth,x)) <- res; res) in

 dfs (0,start)

(* use Pqueue (MinHeap) to reduce sorting requirements *)
let problem_08a () =
 let example = false in
 let (.%()) = Dynarray.get in
 let (.%()<-) = Dynarray.set in
 let max_n = if example then 10 else 1000 in

 let input =
  In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_lines) |>
  List.map (fun s -> Scanf.sscanf s "%d,%d,%d" (fun a b c -> a,b,c)) |>
  Array.of_list in

 (* no need to sqrt *)
 let d2 (x0,y0,z0) (x1,y1,z1) =
   (x0 - x1) * (x0 - x1) +
   (y0 - y1) * (y0 - y1) +
   (z0 - z1) * (z0 - z1) in

 (* use can use an array of size (len * (len - 1) / 2) and sort once, but heaps are better suited to this *)
 let module Heap = Pqueue.MakeMin(struct type t = int * int * int let compare = compare end) in
 let dcache = Heap.create () in

 let len = Array.length input in
 for y = 0 to len - 2 do
  for x = y + 1 to len - 1 do
   Heap.add dcache (d2 input.(y) input.(x), y, x) ;
  done
 done ;

 let module ISet = Set.Make(Int) in
 let circuits = Dynarray.create () in

 (* this counts only meaningful operations; the solution uses i, not count, so this can be removed *)
 let count = ref 0 in
 (* this counts meaningful and nop operations *)
 let i = ref 0 in

 while !i < max_n do
  let (_,a,b) = Heap.pop_min dcache |> Option.get in incr i; incr count ;
  (match
    Dynarray.find_index (ISet.mem a) circuits,
    Dynarray.find_index (ISet.mem b) circuits
   with
   | None, None -> Dynarray.add_last circuits (ISet.of_list [a;b])
   | Some idx, None -> circuits.%(idx) <- ISet.add b circuits.%(idx)
   | None, Some idx -> circuits.%(idx) <- ISet.add a circuits.%(idx)
   | Some idx0, Some idx1 when idx0 = idx1 -> decr count (* nothing happens, reverse incr count *)
   | Some idx0, Some idx1 ->
     let idx0 = min idx0 idx1
     and idx1 = max idx0 idx1 in
     circuits.%(idx0) <- ISet.union circuits.%(idx0) circuits.%(idx1) ;
     if idx1 = Dynarray.length circuits - 1
     then Dynarray.remove_last circuits
     else circuits.%(idx1) <- Dynarray.pop_last circuits)
 done ;

 let top = Dynarray.create () in

 Dynarray.to_seq circuits |>
 Seq.map ISet.cardinal |>
 Seq.iter
  (fun n ->
   if Dynarray.length top < 3 then Dynarray.add_last top n else
   let (idx,_,_) = Dynarray.fold_left (fun (idx,a,i) x -> if x < a then (i,x,i+1) else (idx,a,i+1)) (-1,n,0) top in
   if idx >= 0 then top.%(idx) <- n else ()) ;

 Dynarray.fold_left ( * ) 1 top

(* use heap to reduce sorting *)
(* 225ms (heap) vs. 570ms (full sorting) *)
let problem_08b () =
 let example = false in
 let debug = false in
 let (.%()) = Dynarray.get in
 let (.%()<-) = Dynarray.set in

 let input =
  In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_lines) |>
  List.map (fun s -> Scanf.sscanf s "%d,%d,%d" (fun a b c -> a,b,c)) |>
  Array.of_list in

 (* no need to sqrt *)
 let d2 (x0,y0,z0) (x1,y1,z1) =
   (x0 - x1) * (x0 - x1) +
   (y0 - y1) * (y0 - y1) +
   (z0 - z1) * (z0 - z1) in

 (* use can use an array of size (len * (len - 1) / 2) and sort once, but heaps are better suited to this *)
 let module Heap = Pqueue.MakeMin(struct type t = int * int * int let compare = compare end) in
 let dcache = Heap.create () in

 let len = Array.length input in
 for y = 0 to len - 2 do
  for x = y+1 to len - 1 do
   Heap.add dcache (d2 input.(y) input.(x), y, x) ;
  done
 done ;

 let module ISet = Set.Make(Int) in
 let circuits = Dynarray.create () in

 (* this counts only meaningful operations *)
 let count = ref 0 in
 let last_a = ref (-1) in
 let last_b = ref (-1) in

 (* you require len - 1 meaningful connections to do a final merge *)
 while !count < Array.length input - 1 do
  let (_,a,b) = Heap.pop_min dcache |> Option.get in incr count ;
  last_a := a ; last_b := b ;
  (match
    Dynarray.find_index (ISet.mem a) circuits,
    Dynarray.find_index (ISet.mem b) circuits
   with
   | None, None -> Dynarray.add_last circuits (ISet.of_list [a;b])
   | Some idx, None -> circuits.%(idx) <- ISet.add b circuits.%(idx)
   | None, Some idx -> circuits.%(idx) <- ISet.add a circuits.%(idx)
   | Some idx0, Some idx1 when idx0 = idx1 -> decr count (* nothing happens, reverse incr count *)
   | Some idx0, Some idx1 ->
     let idx0 = min idx0 idx1
     and idx1 = max idx0 idx1 in
     circuits.%(idx0) <- ISet.union circuits.%(idx0) circuits.%(idx1) ;
     if idx1 = Dynarray.length circuits - 1
     then Dynarray.remove_last circuits
     else circuits.%(idx1) <- Dynarray.pop_last circuits)
 done ;

 let (x0,y0,z0) = input.(!last_a) in
 let (x1,y1,z1) = input.(!last_b) in
 if debug then Printf.printf "%d:(%d,%d,%d), %d:(%d,%d,%d)\n" !last_a x0 y0 z0 !last_b x1 y1 z1 ;
 x0 * x1

(* brute-force search will work for part a *)
let problem_09a () =
 let example = false in

 let area (y0,x0) (y1,x1) =
  (abs (y0 - y1) + 1) *
  (abs (x0 - x1) + 1) in

 let input =
  In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_lines) |>
  List.map (fun s -> Scanf.sscanf s "%d,%d" (fun a b -> a,b)) |>
  Array.of_list in

 let res = ref 0 in
 for i = 0 to Array.length input - 2 do
  for j = i+1 to Array.length input - 1 do
   res := max !res (area input.(i) input.(j))
  done
 done ;
 !res

(* not very efficient, but it works *)
let problem_09b () =
 let example = false in
 let debug = true in

 let area (y0,x0) (y1,x1) =
  (abs (y0 - y1) + 1) *
  (abs (x0 - x1) + 1) in

 let ylines = Dynarray.create () in
 let xlines = Dynarray.create () in

 let catalog_line (y0,x0) (y1,x1) =
  if y0 = y1
  then Dynarray.add_last ylines (y0, min x0 x1, max x0 x1)
  else Dynarray.add_last xlines (x0, min y0 y1, max y0 y1) in

 let input =
  In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_lines) |>
  List.map (fun s -> Scanf.sscanf s "%d,%d" (fun a b -> b,a)) |>
  Array.of_list in

 for i = 0 to Array.length input - 2 do
  catalog_line input.(i) input.(i+1)
 done ;
 catalog_line input.(0) input.(Array.length input - 1) ;

 let ylines = Dynarray.to_array ylines in
 let xlines = Dynarray.to_array xlines in

 (* a binary search-based Array.exists could probably speed things up... *)
 Array.sort compare ylines ;
 Array.sort compare xlines ;

 let intersects_yx (y,x',x'') (x,y',y'') = y >= y' && y <= y'' && x >= x' && x <= x'' in
 let intersects_within_yx (y,x',x'') (x,y',y'') = y > y' && y < y'' && x > x' && x < x'' in

 let module Heap = Pqueue.MakeMin(struct type t = int * int * int let compare (a,b,c) (d,e,f) = compare (~-a,b,c) (~-d,e,f) end) in
 let areas = Heap.create () in
 for i = 0 to Array.length input - 2 do
  for j = i+1 to Array.length input - 1 do
   Heap.add areas (area input.(i) input.(j), i, j)
  done
 done ;

 (* in order to be valid, all four corners must reside within the polygon and...
    no sides of the area may intersect within the polygon's sides (touching is fine) *)
 let res = ref 0 in
 while !res = 0 do
  let (d,i,j) = Heap.pop_min areas |> Option.get in
  let (y0,x0) = input.(i) in
  let (y1,x1) = input.(j) in
  if
   (* test whether all corners are valid by removing all valid cases and testing against [] *)
   ([min y0 y1, min x0 x1
    ;min y0 y1, max x0 x1
    ;max y0 y1, min x0 x1
    ;max y0 y1, max x0 x1] |>
    (* skip corners we already know are valid points *)
    List.filter (fun p -> p <> input.(i) && p <> input.(j)) |>
    (* a point is inside if we can cannot draw an infinite NSWE ray that intersects no lines *)
    List.filter (fun (y,x) ->
     let rayN = (x,Int.min_int,y) in
     let rayS = (x,y,Int.max_int) in
     let rayW = (y,Int.min_int,x) in
     let rayE = (y,x,Int.max_int) in
     not
     ((Array.exists (intersects_yx rayE) xlines) &&
      (Array.exists (intersects_yx rayW) xlines) &&
      (Array.exists (intersects_yx rayN) ylines) &&
      (Array.exists (intersects_yx rayS) ylines))) |>
    ((=)[])) &&
   (* test whether the rectangle we draw is valid; i.e., it does not cross that pesky inner cutout *)
   (
    let sideN = (min y0 y1, min x0 x1, max x0 x1) in
    let sideS = (max y0 y1, min x0 x1, max x0 x1) in
    let sideW = (min x0 x1, min y0 y1, max y0 y1) in
    let sideE = (max x0 x1, min y0 x1, max y0 y1) in
    not
    (Array.exists (intersects_within_yx sideN) xlines ||
     Array.exists (intersects_within_yx sideS) xlines ||
     Array.exists (intersects_within_yx sideW) ylines ||
     Array.exists (intersects_within_yx sideE) ylines)
   )
  then
   let _ = if debug then Printf.printf "(%d,%d), (%d,%d)\n" y0 x0 y1 x1 else () in
   res := d
 done ;
 !res
