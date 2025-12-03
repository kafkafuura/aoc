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

