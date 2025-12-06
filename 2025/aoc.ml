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
   (fun s ->
    s |> String.split_on_char ' ' |> List.filter ((<>)"") |>
    List.map (fun s -> if s.[0] = '+' then (-1) else if s.[0] = '*' then (-2) else int_of_string s) |>
    Array.of_list) |>
  Array.of_list in
  let fold_of_n = function
   | -1 -> Seq.fold_left ( + ) 0
   | -2 -> Seq.fold_left ( * ) 1
   | _ -> raise_notrace (Invalid_argument "Invalid OpNum") in

 let h = Array.length input in
 let w = Array.length input.(0) in

 Seq.ints 0 |> Seq.take w |>
 Seq.map
 (fun x ->
  Seq.ints 0 |> Seq.take (h-1) |>
  Seq.map (fun y -> input.(y).(x)) |>
  fold_of_n (input.(h-1).(x))) |>
 Seq.fold_left (+) 0

(* transpose as you go solution, accumulate in chunks *)
let problem_06b () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |> Array.of_list in

 let h = Array.length input in
 let w = String.length input.(0) in

 let fold_of_c = function
  | '+' -> List.fold_left ( + ) 0
  | '*' -> List.fold_left ( * ) 1
  | _ -> raise_notrace (Invalid_argument "Invalid OpNum") in

 let sbuf = Buffer.create (h+1) in

 Seq.ints 0 |> Seq.take w |>
 Seq.fold_left
 (fun (res,nbuf,a) x ->
  let a = if Option.is_none a then Some input.(h-1).[x] else a in
  Buffer.clear sbuf ;
  for y = 0 to h - 2 do Buffer.add_char sbuf input.(y).[x] done ;
  match int_of_string_opt (String.trim (Buffer.contents sbuf)) with
  | Some n -> res,n :: nbuf, a
  | None -> fold_of_c (Option.get a) nbuf :: res, [], None)
  ([], [], None) |>
 (* final step *)
 (fun (res,nbuf,a) -> (fold_of_c (Option.get a) nbuf) :: res) |>
 (* fold for answer *)
 List.fold_left (+) 0

(* pre-transpose, accumulate as you go *)
let problem_06b2() =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |> Array.of_list in

 let h = Array.length input in
 let w = String.length input.(0) in

 let ops = input.(h-1) in

 (* transpose and shadow *)
 let input =
  let sbuf = Buffer.create 8 in
  let input' = Array.make w None in
  for x = 0 to w - 1 do
   Buffer.clear sbuf ;
   for y = 0 to h - 2 do Buffer.add_char sbuf input.(y).[x] done ;
   input'.(x) <- (sbuf |> Buffer.contents |> String.trim |> int_of_string_opt)
  done ;
  input' in

 Seq.zip (String.to_seq ops) (Array.to_seq input) |>
 (* the fold *)
 Seq.fold_left
 (fun (res,a,op) (c,n) ->
  let (a,op) =
   match op with
   (* reset mini-accumulator *)
   | None -> if c = '+' then (0, Some ( + )) else (1, Some ( * ))
   | _ -> (a,op) in
  match n with
   (* add to result and queue mini-accumulator reset *)
  | None -> (a+res, 0, None)
  | Some n -> (res, (Option.get op) a n, op))
 (0,0,None) |>
 (* final step *)
 (fun (res,a,_) -> res+a)
