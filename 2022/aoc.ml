(* aoc.ml - Advent of Code 2022 *)

let read_lines fname =
 let ic = open_in fname
 and lines = ref [] in
 try
  while true do
   lines := input_line ic :: !lines
  done ; assert false
 with End_of_file -> close_in ic ; List.rev !lines

let problem_01a () =
 let input_lines = read_lines "prob01.txt" in
 let (sums, first) = List.fold_right
  begin
   fun s accs ->
   match s, accs with 
   | "" , (acc, None) -> acc, None
   | "" , (acc, Some sum) -> sum :: acc, None
   | n  , (acc, None) -> (acc, Some (int_of_string n))
   | n  , (acc, Some sum) -> (acc, Some (sum + (int_of_string n)))
  end input_lines ([], None) in
  let sums' = if Option.is_some first then Option.get first :: sums else sums in
  List.fold_left max 0 sums'

let problem_01b () =
 let input_lines = read_lines "prob01.txt" in
 let (sums, first) = List.fold_right
  begin
   fun s accs ->
   match s, accs with 
   | "" , (acc, None) -> acc, None
   | "" , (acc, Some sum) -> sum :: acc, None
   | n  , (acc, None) -> (acc, Some (int_of_string n))
   | n  , (acc, Some sum) -> (acc, Some (sum + (int_of_string n)))
  end input_lines ([], None) in
  let sums' = ref (if Option.is_some first then Option.get first :: sums else sums) in
  let maxs = Array.make 3 0 in
  for i = 0 to 2 do
   maxs.(i) <- List.fold_left max 0 !sums' ;
   sums' := List.filter (fun x -> x < maxs.(i)) !sums' ;
  done ;
  maxs, Array.fold_left (+) 0 maxs

(*
 A,X: rock
 B,Y: paper
 C,Z: scissors
*)

let n_of_rps = function
 | 'X' -> 1 | 'Y' -> 2 | 'Z' -> 3
 | 'A' -> 1 | 'B' -> 2 | 'C' -> 3
 | _ -> assert false

let rps_score_n = function
 | a, b when a = b -> 3 + b
 | a, b when b - a = 1 || b - a = -2 -> 6 + b
 | _, b -> b

let problem_02a () =
 let input_lines = read_lines "prob02.txt" in
 let inputs = List.fold_right
  begin fun s acc ->
    match s with
    | "" -> acc
    | _ -> (Scanf.sscanf s "%c %c" (fun x y -> (n_of_rps x, n_of_rps y) |> rps_score_n))::acc
  end input_lines [] in
  List.fold_left (+) 0 inputs

let rps_score_n2 = function (* X -> lose, Y -> draw, Z -> win *)
 | a, 2 -> rps_score_n (a, a)
 | a, 1 -> rps_score_n (a, if a = 1 then 3 else a - 1)
 | a, _ -> rps_score_n (a, if a = 3 then 1 else a + 1)

let problem_02b () =
 let input_lines = read_lines "prob02.txt" in
 let inputs = List.fold_right
  begin fun s acc ->
    match s with
    | "" -> acc
    | _ -> (Scanf.sscanf s "%c %c" (fun x y -> (n_of_rps x, n_of_rps y) |> rps_score_n2))::acc
  end input_lines [] in
  List.fold_left (+) 0 inputs
 
let problem_03a () =
 let input_lines = read_lines "prob03.txt" in
 let priority_of_char c =
  match Char.code c with
  | x when x > 90 -> x - 96
  | x -> x - 64 + 26 in
 let find_matching (xs, ys) =
  List.fold_left begin fun acc x ->
   match acc, x with
   | Some _, _ -> acc
   | None, x -> if List.exists ((=)x) ys then Some x else acc
  end None xs in
 let inputs = List.map begin fun s ->
  let slen = String.length s in
  let s1 = String.sub s 0 (slen / 2)
  and s2 = String.sub s (slen / 2) (slen - slen / 2) in
  (String.fold_right (fun c acc -> priority_of_char c :: acc) s1 []),
  (String.fold_right (fun c acc -> priority_of_char c :: acc) s2 [])
 end input_lines in
 List.map (fun x -> x |> find_matching |> Option.get) inputs
 |> List.fold_left (+) 0

let problem_03b () =
 let input_lines = read_lines "prob03.txt" in
 let priority_of_char c =
  match Char.code c with
  | x when x > 90 -> x - 96
  | x -> x - 64 + 26 in
 let find_matching (xs, ys, zs) =
  List.fold_left begin fun acc x ->
   match acc, x with
   | Some _, _ -> acc
   | None, x -> if List.exists ((=)x) ys && List.exists ((=)x) zs then Some x else acc
  end None xs in
 let (_,inputs) = List.fold_right begin fun s (acc_hd, acc) ->
  let fold_into_priority s = String.fold_right (fun c acc -> priority_of_char c :: acc) s [] in
  match s, acc_hd, acc with
  | x, y::z::[], acc -> ([],(fold_into_priority x, fold_into_priority y, fold_into_priority z)::acc)
  | _ -> (s::acc_hd, acc)
 end input_lines ([],[]) in
 List.map (fun x -> x |> find_matching |> Option.get) inputs
 |> List.fold_left (+) 0

(* identify pairs where one range fully contains the other! *)
(* x1-x2,y1-y2 *)
let problem_04a () =
 let input_lines = read_lines "prob04.txt" in
 (* c-d is fully contained within a-b OR a-b is fully contained within b-c *)
 let contained (a,b,c,d) = (a >= c && b <= d) || (a <= c && b >= d) in
 let inputs = List.map begin fun s ->
  Scanf.sscanf s "%u-%u,%u-%u" (fun a b c d -> contained (a,b,c,d))
 end input_lines |> List.rev in
 List.fold_left (fun acc x -> if x then succ acc else acc) 0 inputs

(* identify pairs with any overlap! *)
let problem_04b () =
 let input_lines = read_lines "prob04.txt" in
 (* a or b are within c-d OR c-d is fully contained within a-b *)
 let overlap (a,b,c,d) = (a <= d && a >= c) || (b <= d && b >= c) || (a <= c && b >= d) in
 let inputs = List.map begin fun s ->
  Scanf.sscanf s "%u-%u,%u-%u" (fun a b c d -> overlap (a,b,c,d))
 end input_lines |> List.rev in
 List.fold_left (fun acc x -> if x then succ acc else acc) 0 inputs

let problem_05a () =
 let input_lines = read_lines "prob05.txt" in
 let move_crate stks (a,b,c) =
  for i = 1 to a do
   match stks.(b), stks.(c) with
   | [],_ -> ();
   | bt::bs, cs -> stks.(b) <- bs ; stks.(c) <- bt::cs
  done in
 let (initial_state, instructions, _) = 
  List.fold_right begin fun s (acc0, acc1, past_split) ->
   match past_split, s with
   | false, "" -> (acc0, acc1, true)
   | false, s -> acc0, Scanf.sscanf s "move %u from %u to %u" (fun x y z -> x,pred y,pred z) :: acc1, false
   | true, s -> s::acc0, acc1, true
  end input_lines ([], [], false) in
  let nbins = (List.nth initial_state 0 |> String.length) / 4 + 1 in
  let stacks = Array.make nbins [] in
  List.iter begin fun s ->
   for j = 0 to pred nbins do
    let c = s.[j*4+1] in if Char.equal c ' ' |> not then stacks.(j) <- c::stacks.(j)
   done
  end ((List.rev initial_state) |> List.tl) ;
  List.iter (move_crate stacks) instructions ;
  Array.map (function [] -> ' ' | c::cs -> c) stacks |> Array.to_seq |> String.of_seq

let problem_05b () =
 let input_lines = read_lines "prob05.txt" in
 let move_crate stks (a,b,c) =
  let holding = ref [] in
  for i = 1 to a do
   match stks.(b), stks.(c) with
   | [],_ -> ();
   | bt::bs, cs -> stks.(b) <- bs ; holding := bt::!holding
  done ; stks.(c) <- List.rev_append !holding stks.(c) in
 let (initial_state, instructions, _) = 
  List.fold_right begin fun s (acc0, acc1, past_split) ->
   match past_split, s with
   | false, "" -> (acc0, acc1, true)
   | false, s -> acc0, Scanf.sscanf s "move %u from %u to %u" (fun x y z -> x, pred y, pred z) :: acc1, false
   | true, s -> s::acc0, acc1, true
  end input_lines ([], [], false) in
  let nbins = (List.nth initial_state 0 |> String.length) / 4 + 1 in
  let stacks = Array.make nbins [] in
  List.iter begin fun s ->
   for j = 0 to pred nbins do
    let c = s.[j*4+1] in if Char.equal c ' ' |> not then stacks.(j) <- c::stacks.(j)
   done
  end ((List.rev initial_state) |> List.tl) ;
  List.iter (move_crate stacks) instructions ;
  Array.map (function [] -> ' ' | c::cs -> c) stacks |> Array.to_seq |> String.of_seq

(* read by char instead of by line *)
let problem_06a () =
 let msg_len = 4 in
 let ic = open_in_bin "prob06.txt"
 and rbuf = Array.make msg_len ' '
 and i = ref 0 in
 let unique buf =
  let test = ref true in
  for i = 0 to pred msg_len do for j = succ i to pred msg_len do
    if !test && buf.(i) = buf.(j) then test := false
  done done ; !test in
 begin try
  while !i < msg_len || unique rbuf |> not do
   rbuf.(!i mod msg_len) <- input_char ic ; incr i
  done with End_of_file -> () end;
 close_in ic ; !i

(* only difference is msg_len *)
let problem_06b () =
 let msg_len = 14 in
 let ic = open_in_bin "prob06.txt"
 and rbuf = Array.make msg_len ' '
 and i = ref 0 in
 let unique buf =
  let test = ref true in
  for i = 0 to pred msg_len do for j = succ i to pred msg_len do
    if !test && buf.(i) = buf.(j) then test := false
  done done ; !test in
 begin try
  while !i < msg_len || unique rbuf |> not do
   rbuf.(!i mod msg_len) <- input_char ic ; incr i
  done with End_of_file -> () end;
 close_in ic ; !i

(* indirect method: *)
(* keep track of cwd w/ directory stack, and mark each file with an address *)
(* names do not need to be stored (unless you need to invalidate duplicate ls calls) *)
(* "dir input" can be ignored (if you assume cd commands are valid) *)
let problem_07a () =
 let input_lines = read_lines "prob07.txt"
 and hs = Hashtbl.create 1024
 and string_of_locs locs = List.fold_right (Fun.flip (^)) locs "" in
 let rec add_file_to_hs (locs, size) =
  match locs with | [] -> ()
  | _::locs' ->
    let locs_str = string_of_locs locs in
    match Hashtbl.find_opt hs locs_str with
    | None -> Hashtbl.add hs locs_str size ; add_file_to_hs (locs', size)
    | Some v -> Hashtbl.replace hs locs_str (v+size) ; add_file_to_hs (locs',size) in
 let inputs = List.filter begin fun s -> (* remove unnecessary info *)
  (String.starts_with ~prefix:"dir" s ||
   String.starts_with ~prefix:"$ ls" s ) |> not
 end input_lines in
 let cwd = ref [] in (* cwd stack *)
 let files = List.fold_left begin fun acc s ->
  if s.[0] = '$' then
   if s.[5] = '.' then (cwd := List.tl !cwd ; acc)
   else (cwd := String.sub s 5 (String.length s - 5)::!cwd ; acc)
  else let size = Scanf.sscanf s "%u " (Fun.id) in (!cwd, size)::acc
 end [] inputs in
 List.iter add_file_to_hs files ;
 Hashtbl.to_seq_values hs |> Seq.filter ((>)100000) |> Seq.fold_left (+) 0

let problem_07b () =
 let input_lines = read_lines "prob07.txt"
 and hs = Hashtbl.create 1024
 and string_of_locs locs = List.fold_right (Fun.flip (^)) locs "" in
 let rec add_file_to_hs (locs, size) =
  match locs with
  | [] -> ()
  | _::locs' ->
    let locs_str = string_of_locs locs in
    match Hashtbl.find_opt hs locs_str with
    | None -> Hashtbl.add hs locs_str size ; add_file_to_hs (locs', size)
    | Some v -> Hashtbl.replace hs locs_str (v+size) ; add_file_to_hs (locs',size) in
 let inputs = List.filter begin fun s -> (* remove unnecessary info *)
  (String.starts_with ~prefix:"dir" s ||
   String.starts_with ~prefix:"$ ls" s ) |> not
 end input_lines in
 let cwd = ref [] in (* cwd stack *)
 let files = List.fold_left begin fun acc s ->
  if s.[0] = '$' then
   if s.[5] = '.' then (cwd := List.tl !cwd ; acc)
   else (cwd := String.sub s 5 (String.length s - 5)::!cwd ; acc)
  else let size = Scanf.sscanf s "%u " (Fun.id) in (!cwd, size)::acc
 end [] inputs in
 List.iter add_file_to_hs files ;
 let size_to_free = Hashtbl.find hs "/" - 40000000
 and sorted_sizes = Hashtbl.to_seq_values hs |> List.of_seq |> List.sort compare |> List.to_seq in
 Seq.drop_while (Fun.flip (<) size_to_free) sorted_sizes |> Seq.uncons |> Option.get |> fst

(* input is 99x99 *)
(* sample is 5x5 *)
(* start from edges to short-circuit *)
let problem_08a () =
 let input_lines = read_lines "prob08.txt" in
 let dim = List.length input_lines in
 let m = Array.make_matrix dim dim 0 in
 let safe y x =
  let t = m.(y).(x) in
  let rec is_safe_w = function i when i < x -> (m.(y).(i) >= t) || is_safe_w (succ i) | _ -> false in
  let rec is_safe_e = function i when i > x -> (m.(y).(i) >= t) || is_safe_e (pred i) | _ -> false in
  let rec is_safe_n = function j when j < y -> (m.(j).(x) >= t) || is_safe_n (succ j) | _ -> false in
  let rec is_safe_s = function j when j > y -> (m.(j).(x) >= t) || is_safe_s (pred j) | _ -> false in
  is_safe_w 0 && is_safe_e (pred dim) && is_safe_n 0 && is_safe_s (pred dim) in
 List.iteri ( fun y s -> String.iteri ( fun x c -> m.(y).(x) <- Char.code c - 0x30 ) s ) input_lines ;
 let count = ref 0 in
 for j = 1 to dim - 2 do for i = 1 to dim - 2 do
   if safe i j then incr count
 done done;
 dim * dim - !count

(* short circuiting is significantly faster *)
let problem_08b () =
 let input_lines = read_lines "prob08.txt" in
 let dim = List.length input_lines in
 let m = Array.make_matrix dim dim 0 in
 let scores = Array.make_matrix dim dim 0 in
 let score y x =
  let t = m.(y).(x) in
  let rec score_w a =
   function i when i >= 0 -> if m.(y).(i) >= t then succ a else score_w (succ a) (pred i) | _ -> a in
  let rec score_e a =
   function i when i < dim -> if m.(y).(i) >= t then succ a else score_e (succ a) (succ i) | _ -> a in
  let rec score_n a =
   function j when j >= 0 -> if m.(j).(x) >= t then succ a else score_n (succ a) (pred j) | _ -> a in
  let rec score_s a =
   function j when j < dim -> if m.(j).(x) >= t then succ a else score_s (succ a) (succ j) | _ -> a in
  score_w 0 (pred x) * score_e 0 (succ x) * score_n 0 (pred y) * score_s 0 (succ y) in
 List.iteri ( fun y s -> String.iteri ( fun x c -> m.(y).(x) <- Char.code c - 0x30 ) s ) input_lines ;
 for i = 0 to pred dim do for j = 0 to pred dim do
  scores.(i).(j) <- score i j
 done done;
 Array.fold_left (fun acc row -> Array.fold_left (max) 0 row |> max acc) 0 scores

let problem_09a () =
 let input_lines = read_lines "prob09.txt" in
 let dir_to_vec = function 'U' -> (0,1) | 'D' -> (0,~-1) | 'L' -> (~-1,0) | _ -> (1,0) in
 let inputs = List.map (fun s -> Scanf.sscanf s "%c %u" (fun x y -> (dir_to_vec x,y))) input_lines in
 let h = ref (0,0) and t = ref (0,0) in
 let tail_set = Hashtbl.create 1024 in
 let adjust_tail () =
  let (hx,hy) = !h and (tx,ty) = !t in
  match (hx-tx)/2, (hy-ty)/2 with
  | mx,_ when mx <> 0 -> t := (tx+mx, hy)
  | _,my when my <> 0 -> t := (hx, ty+my)
  | _ -> () in
 let record_tail () = Hashtbl.replace tail_set !t () in
 let rec step ((mx,my), ctr) =
  if ctr > 0 then begin
   let (hx,hy) = !h in (h := (hx+mx,hy+my)) ; adjust_tail () ; record_tail () ; step ((mx,my),(pred ctr))
  end else () in
 List.iter step inputs ;  Hashtbl.to_seq_keys tail_set |> Seq.length

let problem_09a2 () =
 let input_lines = read_lines "prob09.txt" in
 let snake_len = 2 in
 let dir_to_vec = function 'U' -> (0,1) | 'D' -> (0,~-1) | 'L' -> (~-1,0) | _ -> (1,0) in
 let inputs = List.map (fun s -> Scanf.sscanf s "%c %u" (fun x y -> (dir_to_vec x,y))) input_lines in
 let snake = Array.make snake_len (0,0) in
 let tail_set = Hashtbl.create 1024 in
 let update_tail (hx,hy) (tx,ty) =
  match (hx-tx)/2, (hy-ty)/2 with
  | mx, my when mx <> 0 && my <> 0 -> (tx+mx, ty+my) (* this case only occurs when snake_len > 2 *)
  | mx,_   when mx <> 0 -> (tx+mx, hy)
  | _,my   when my <> 0 -> (hx, ty+my)
  | _ -> (tx,ty) in
 let record_tail () = Hashtbl.replace tail_set snake.(pred snake_len) () in
 let rec step ((mx,my), ctr) =
  if ctr > 0 then begin
   let (hx,hy) = snake.(0) in (snake.(0) <- (hx+mx,hy+my)) ;
   for i = 0 to snake_len - 2 do
    snake.(succ i) <- update_tail snake.(i) snake.(succ i) ;
    record_tail ()
   done ; step ((mx,my), pred ctr)
  end else () in
 List.iter step inputs ;  Hashtbl.to_seq_keys tail_set |> Seq.length

let problem_09b () =
 let input_lines = read_lines "prob09.txt" in
 let snake_len = 10 in
 let dir_to_vec = function 'U' -> (0,1) | 'D' -> (0,~-1) | 'L' -> (~-1,0) | _ -> (1,0) in
 let inputs = List.map (fun s -> Scanf.sscanf s "%c %u" (fun x y -> (dir_to_vec x,y))) input_lines in
 let snake = Array.make snake_len (0,0) in
 let tail_set = Hashtbl.create 1024 in
 let update_tail (hx,hy) (tx,ty) =
  match (hx-tx)/2, (hy-ty)/2 with
  | mx, my when mx <> 0 && my <> 0 -> (tx+mx, ty+my) (* this case only occurs when snake_len > 2 *)
  | mx,_   when mx <> 0 -> (tx+mx, hy)
  | _,my   when my <> 0 -> (hx, ty+my)
  | _ -> (tx,ty) in
 let record_tail () = Hashtbl.replace tail_set snake.(pred snake_len) () in
 let rec step ((mx,my), ctr) =
  if ctr > 0 then begin
   let (hx,hy) = snake.(0) in (snake.(0) <- (hx+mx,hy+my)) ;
   for i = 0 to snake_len - 2 do
    snake.(succ i) <- update_tail snake.(i) snake.(succ i) ;
    record_tail ()
   done ; step ((mx,my), pred ctr)
  end else () in
 List.iter step inputs ;  Hashtbl.to_seq_keys tail_set |> Seq.length

(* debug:
 |> print_int ; print_newline () ; for i = 15 downto ~-5 do for j = ~-15 to 15 do
   if Hashtbl.mem tail_set (j,i) then print_char '#' else print_char '.' done ; print_newline () done
*)

let problem_10a () =
 let inputs = read_lines "prob10.txt"
  |> List.map (function "noop" -> (1,0) | s -> Scanf.sscanf s "addx %d" (fun x -> (2,x))) in
 let reg_x = ref 1 and clk = ref 0 and rep = ref 0 in
 let report () =
  if !clk < 221 && ((!clk - 20) mod 40 = 0) then rep := !rep + !reg_x * !clk ;
  Printf.printf "%d %d %d\n" !clk !reg_x !rep in (* debug *)
 let rec execute = function
 | (0,x) -> reg_x := !reg_x+x (* only update X between clock cycles *)
 | (c,x) when c > 0 -> incr clk ; report () ; execute (pred c,x)
 | _ -> assert false in
 List.iter execute inputs ; !rep

let problem_10b () =
 let inputs = read_lines "prob10.txt"
  |> List.map (function "noop" -> (1,0) | s -> Scanf.sscanf s "addx %d" (fun x -> (2,x))) in
 let screen = Array.make 240 ' ' in
 let draw_screen () =
  for i = 0 to 5 do for j = 0 to 39 do print_char screen.(i*40+j) done ; print_char '\n' done in
 let reg_x = ref 1 and clk = ref 0 and rep = ref 0 in
 let report () =
  if !clk < 221 && ((!clk - 20) mod 40 = 0) then rep := !rep + !reg_x * !clk ;
  begin match pred !clk, !reg_x, pred !clk mod 40 with
  | pos, x, xpos when xpos >= pred x && xpos <= succ x -> screen.(pos) <- '#'
  | pos, _, _ -> screen.(pos) <- '.' end (*; Printf.printf "%d %d %d\n" !clk !reg_x !rep*) in
 let rec execute = function
 | (0,x) -> reg_x := !reg_x+x (* only update X between clock cycles *)
 | (c,x) when c > 0 -> incr clk ; report () ; execute (pred c,x)
 | _ -> assert false in
 List.iter execute inputs ; draw_screen () ; !rep

module Monkey =
 struct
   type t =
   { items : int list
   ; worry_op : int -> int
   ; throw_divisor : int
   ; dest_true : int
   ; dest_false : int }
   let empty = {items=[]; worry_op=(fun _ -> 0); throw_divisor=1; dest_true=0; dest_false=0}
 end

let problem_11_read_monkeys () =
 let input_lines = read_lines "prob11.txt" in
 let n_of_monkeys = List.length input_lines / 7 + 1 in
 let monkeys = Array.make n_of_monkeys Monkey.empty in
 List.iteri begin fun i s ->
  match i mod 7 with
  | 1 ->
   let items =
   String.split_on_char ':' s |> Fun.flip List.nth 1 |> String.split_on_char ',' |>
   List.map (fun s -> String.trim s |> int_of_string ) in
   monkeys.(i/7) <- { monkeys.(i/7) with items }
  | 2 ->
  let worry_op =
   Scanf.sscanf s " Operation: new = old %c %s "
    begin fun c s -> 
     let op = (match c with '+' -> (+) | _ -> ( * )) in
     match s.[0] with
     | 'o' -> (fun x -> op x x)
     | _ -> let n = int_of_string s in (fun x -> op x n)
    end in
    monkeys.(i/7) <- { monkeys.(i/7) with worry_op }
  | 3 ->
   let divisor = Scanf.sscanf s " Test: divisible by %d" Fun.id in
   let throw_divisor = divisor in
   monkeys.(i/7) <- { monkeys.(i/7) with throw_divisor }
  | 4 ->
   let dest_true = Scanf.sscanf s " If true: throw to monkey %d" Fun.id in
   monkeys.(i/7) <- { monkeys.(i/7) with dest_true }
  | 5 ->
   let dest_false = Scanf.sscanf s " If false: throw to monkey %d" Fun.id in
   monkeys.(i/7) <- { monkeys.(i/7) with dest_false }
  | _ -> () (*ignore*)
 end input_lines ; monkeys

let problem_11a () =
 let monkeys = problem_11_read_monkeys () in
 let inspect = Array.make (Array.length monkeys) 0 in
 let run_round () =
  Array.iteri begin fun idx m ->
   List.iter begin fun i ->
    let open Monkey in (* expose Monkey.t record fields to the typechecker *)
    inspect.(idx) <- succ inspect.(idx) ;
    let w = m.worry_op i / 3 in
    let dest = if w mod m.throw_divisor = 0 then m.dest_true else m.dest_false in
    monkeys.(dest) <- { monkeys.(dest) with items = monkeys.(dest).items @ [w] }
   end m.items ; monkeys.(idx) <- {monkeys.(idx) with items = []}
  end monkeys in
 for i = 1 to 20 do run_round () done;
 let ins_max,ins_max' =
  begin match Array.to_list inspect |> List.sort (Fun.flip compare) with
  | a::b::_ -> a,b | _ -> 0,0 end in
 monkeys, inspect, ins_max * ins_max'

let problem_11b () =
 let open Monkey in (* expose Monkey.t record fields to the typechecker *)
 let monkeys = problem_11_read_monkeys () in
 let inspect = Array.make (Array.length monkeys) 0 in
 let max_modulo = Array.fold_left (fun acc m -> m.throw_divisor * acc) 1 monkeys in
 let destress x = x mod max_modulo in
 let run_round () =
  Array.iteri begin fun idx m ->
   List.iter begin fun i ->
    inspect.(idx) <- succ inspect.(idx) ;
    let w = m.worry_op i |> destress in
    let dest = if w mod m.throw_divisor = 0 then m.dest_true else m.dest_false in
    monkeys.(dest) <- { monkeys.(dest) with items = monkeys.(dest).items @ [w] }
   end m.items ; monkeys.(idx) <- {monkeys.(idx) with items = []}
  end monkeys in
 for i = 1 to 10000 do run_round () done;
 let ins_max,ins_max' =
  begin match Array.to_list inspect |> List.sort (Fun.flip compare) with
  | a::b::_ -> a,b | _ -> 0,0 end in
 monkeys, inspect, ins_max * ins_max'

(* simplified dijkstra *)
let problem_12a () =
 let inputs = read_lines "prob12.txt" |> Array.of_list in
 let y_len = Array.length inputs and x_len = String.length inputs.(0) in
 let dist = Array.make_matrix y_len x_len Int.max_int
 and start_point = ref (0,0) and end_point = ref (0,0) and found = ref false
 and dQ = Queue.create () in
(*
 let print_map () = (* debug *)
  for y = 0 to pred y_len do for x = 0 to pred x_len do
   let v = min dist.(y).(x) 999 in
   Printf.printf "%03d " v done; print_newline () done in
*)
 let valid_path (y,x) (y',x') =
  (y <> y' || x <> x') && (* short-circuit logic *)
  (abs (y'-y) + abs (x'-x) < 2) && (* max dist = 1, check may be unnecessary *)
  y >= 0 && y < y_len && x >= 0 && x < x_len && (* throw indexing caution to the wind *)
  y' >= 0 && y' < y_len && x' >= 0 && x' < x_len &&
  match inputs.(y).[x], inputs.(y').[x'] with
  | 'S', c when c = 'a' || c = 'b' -> true
  | c, 'E' when c = 'z' || c = 'y' -> true
  | _, 'E' -> false | 'E',_ -> true
  | c, c' when Char.(code c' - code c) < 2 -> true (* hop down is always valid *)
  | _ -> false in
 for y = 0 to pred y_len do for x = 0 to pred x_len do
  match inputs.(y).[x] with
  | 'S' -> start_point := (y,x) ; Queue.add (y,x) dQ ; dist.(y).(x) <- 0
  | 'E' -> end_point := (y,x)
  | _ -> ()
 done done ;
 let start_point = !start_point and end_point = !end_point in (* mask refs no longer needed *)
 (* dijsktra loop *)
 while not !found && (Queue.is_empty dQ |> not) do
  let (y,x) = Queue.take dQ in
  [(pred y,x);(succ y,x);(y,pred x);(y,succ x)]
  |> List.iter begin fun (y',x') ->
   if valid_path (y,x) (y',x') then
    let d = succ dist.(y).(x) in
    if (d < dist.(y').(x')) then begin
     Queue.add (y',x') dQ ;
     dist.(y').(x') <- d ;
     if end_point = (y',x') then found := true
    end
  end done;
  (*print_map ();*)
  start_point, end_point, (let y,x = end_point in dist.(y).(x))

(* reverse dijkstra *)
let problem_12b () =
 let inputs = read_lines "prob12.txt" |> Array.of_list in
 let y_len = Array.length inputs and x_len = String.length inputs.(0) in
 let dist = Array.make_matrix y_len x_len Int.max_int
 and start_points = ref [] and end_point = ref (0,0) and found = ref false
 and dQ = Queue.create () in
 (*
 let print_map () =
  for y = 0 to pred y_len do for x = 0 to pred x_len do
   let v = min dist.(y).(x) 999 in
   Printf.printf "%03d%c " v inputs.(y).[x] done; print_newline () done in
 *)
 let valid_path (y,x) (y',x') =
  (y <> y' || x <> x') && (* short-circuit logic *)
  (abs (y'-y) + abs (x'-x) < 2) && (* max dist = 1, check may be unnecessary *)
  y >= 0 && y < y_len && x >= 0 && x < x_len &&
  y' >= 0 && y' < y_len && x' >= 0 && x' < x_len &&
  match inputs.(y).[x], inputs.(y').[x'] with
  | _,'a' -> false (* end search when 'a' is dest *)
  | 'S', c when c = 'a' || c = 'b' -> true
  | c, 'E' when c = 'z' || c = 'y' -> true
  | _, 'E' -> false | 'E',_ -> true
  | c, c' when Char.(code c' - code c) < 2 -> true (* hop down is always valid *)
  | _ -> false in
 for y = 0 to pred y_len do for x = 0 to pred x_len do
  match inputs.(y).[x] with
  | 'S' -> start_points := (y,x)::!start_points
  | 'a' -> start_points := (y,x)::!start_points
  | 'E' -> end_point := (y,x) ; Queue.add (y,x) dQ ; dist.(y).(x) <- 0
  | _ -> ()
 done done ;
 let start_points = !start_points and end_point = !end_point in (* mask refs no longer needed *)
 (* dijsktra loop *)
 while not !found && (Queue.is_empty dQ |> not) do
  let (y,x) = Queue.take dQ in
  [(pred y,x);(succ y,x);(y,pred x);(y,succ x)]
  |> List.iter begin fun (y',x') ->
   if valid_path (y',x') (y,x) then
    let d = succ dist.(y).(x) in
    if (d < dist.(y').(x')) then begin
     Queue.add (y',x') dQ ;
     dist.(y').(x') <- d
    end end done;
  (* print_map (); *)
  end_point, List.fold_left (fun acc (y,x) -> min acc dist.(y).(x)) Int.max_int start_points

module Sexp = struct
 type t =
 | Atom of string
 | List of t list
  let of_string str =
   let rec parse i =
     match str.[i] with
       | exception _ -> failwith "incomplete s-expression"
       | ')' -> failwith "unbalanced parentheses @ ')'"
       | ' ' -> parse (i+1)
       | '(' -> start_list (i+1)
       | _ -> start_atom i
   and start_list i = parse_list [] i
   and parse_list acc i =
     match str.[i] with
       | exception _ -> failwith "incomplete list-expression"
       | ')' -> finish_list acc (i+1)
       | ' ' -> parse_list acc (i+1)
       | _ ->
         let elem, j = parse i in
         parse_list (elem :: acc) j
   and finish_list acc i =
     List (List.rev acc), i
   and start_atom i = parse_atom (Buffer.create 3) i
   and parse_atom acc i =
     match str.[i] with
       | exception _ -> finish_atom acc i
       | ')' | ' ' -> finish_atom acc i
       | _ -> parse_atom (Buffer.add_char acc str.[i]; acc) (i + 1)
   and finish_atom acc i =
     Atom (Buffer.contents acc), i
  in
  let res, rem = parse 0 in
  res, String.sub str rem (String.length str - rem)
end

(* parse nested lists as sexps *)
let problem_13a () =
 let inputs = read_lines "prob13.txt" |> List.filter ((<>)"")
 |> List.map begin fun s -> s |> String.to_seq
  |> Seq.map begin function ',' -> ' ' | '[' -> '(' | ']' -> ')' | c -> c end
  |> String.of_seq end |> List.map (fun s -> Sexp.of_string s |> fst) |> Array.of_list in
 let valid s1 s2 =
  let rec valid' s1 s2 =
  Sexp.(match s1, s2 with
  | Atom a1, Atom a2 -> compare (int_of_string a1) (int_of_string a2)
  | Atom _, List _ -> valid' (List [s1]) s2
  | List _, Atom _ -> valid' s1 (List [s2])
  | List ([]), List (s2h::s2s) -> ~-1
  | List (s1h::s1s), List ([]) -> 1
  | List ([]), List ([]) -> 0
  | List (s1h::s1s), List (s2h::s2s) ->
    match valid' s1h s2h with
    | v when v <> 0 -> v
    | _ -> valid' (List s1s) (List s2s))
  in valid' s1 s2 < 0 in 
 let res = Array.(make (length inputs/2)) 0 in
 for i = 0 to Array.length res |> pred do
  res.(i) <- if valid inputs.(i*2) inputs.(i*2+1) then (i+1) else 0
 done; Array.fold_left (+) 0 res

let problem_13b () =
 let inputs = read_lines "prob13.txt"
  |> List.filter ((<>)"")
  |> (@) ["[[2]]";"[[6]]"] (* divider packets *)
  |> List.map begin fun s -> s |> String.to_seq
  |> Seq.map begin function ',' -> ' ' | '[' -> '(' | ']' -> ')' | c -> c end
  |> String.of_seq end |> List.map (fun s -> Sexp.of_string s |> fst) |> Array.of_list in
 let sexp_compare s1 s2 =
  let rec valid' s1 s2 =
  Sexp.(match s1, s2 with
  | Atom a1, Atom a2 -> compare (int_of_string a1) (int_of_string a2)
  | Atom _, List _ -> valid' (List [s1]) s2
  | List _, Atom _ -> valid' s1 (List [s2])
  | List ([]), List (s2h::s2s) -> ~-1
  | List (s1h::s1s), List ([]) -> 1
  | List ([]), List ([]) -> 0
  | List (s1h::s1s), List (s2h::s2s) ->
    match valid' s1h s2h with
    | v when v <> 0 -> v
    | _ -> valid' (List s1s) (List s2s))
  in valid' s1 s2 in 
  Array.sort sexp_compare inputs;
  let s1 = Sexp.of_string "((2))" |> fst and s2 = Sexp.of_string "((6))" |> fst in
  let d1 = ref 0 and d2 = ref 0 in
  for i = 1 to Array.length inputs do
   if !d1 = 0 && sexp_compare inputs.(pred i) s1 = 0 then d1 := i;
   if !d2 = 0 && sexp_compare inputs.(pred i) s2 = 0 then d2 := i;
  done; !d1 * !d2

let problem_14a () =
 let inputs = read_lines "prob14.txt"
 |> List.map begin fun s ->
  s |> String.split_on_char ' ' |> List.filter ((<>) "->")
    |> List.map (fun s -> Scanf.sscanf s "%u,%u" (fun x y -> x,y)) end
 and sand_source = (500,0) in
 let x_min, x_max, y_max = inputs |>
  List.fold_left begin fun (axmin,axmax,aymax) ->
    List.fold_left (fun (axmin,axmax,aymax) (x,y) -> (min axmin x, max axmax x, max aymax y)) (axmin, axmax, aymax)
  end (Int.max_int, Int.min_int, Int.min_int) in
 let grid = Array.make_matrix (y_max+2) (x_max-x_min+3) '.' in
 let grid_get (x,y) = grid.(y).(x-x_min)
 and grid_set (x,y) v = grid.(y).(x-x_min) <- v in
 let draw_line (x,y) (x',y') =
  if x = x' then for i = min y y' to max y y' do grid_set (x,i) '#' done
  else for i = min x x' to max x x' do grid_set (i,y) '#' done in
 (* returns true if caught / successfully placed *)
 let drop_sand () =
  let rec drop_sand' (x,y) =
   match grid_get (x,y) with
   | _ when y > y_max -> (x,y)
   | '.' -> drop_sand' (x,succ y)
   | '#' | 'o' ->
    if grid_get (pred x,y) = '.' then drop_sand' (pred x,y)
    else if grid_get (succ x,y) = '.' then drop_sand' (succ x,y)
    else (x,pred y)
   | _ -> assert false in
  let pos = drop_sand' sand_source in
  grid_set pos 'o' ; snd pos < y_max in 
 let print_grid () =
  Array.iter (fun line -> Array.iter (print_char) line ; print_newline ()) grid in
 let fill_grid () =
  List.iter begin fun s -> s |>
   List.fold_left begin fun a (x',y') ->
    (match a with Some (x,y) -> draw_line (x,y) (x',y') | _ -> ());
    Some (x',y')
   end None |> ignore end inputs in
 let ctr = ref 0 in
 fill_grid ();
 while drop_sand () do incr ctr done;
 print_grid ();
 x_min, x_max, y_max, !ctr

let problem_14b () =
 let inputs = read_lines "prob14.txt"
 |> List.map begin fun s ->
  s |> String.split_on_char ' ' |> List.filter ((<>) "->")
    |> List.map (fun s -> Scanf.sscanf s "%u,%u" (fun x y -> x,y)) end
 and sand_source = (500,0) in
 let y_max = inputs |>
  List.fold_left (List.fold_left (fun aymax (_,y) -> max aymax y)) Int.min_int in
 let x_min, x_max = (fst sand_source) - (y_max+3), (fst sand_source) + (y_max+3) in
 let grid = Array.make_matrix (y_max+3) (x_max-x_min+3) '.' in
 let grid_get (x,y) = grid.(y).(x-x_min)
 and grid_set (x,y) v = grid.(y).(x-x_min) <- v in
 let draw_line (x,y) (x',y') =
  if x = x' then for i = min y y' to max y y' do grid_set (x,i) '#' done
  else for i = min x x' to max x x' do grid_set (i,y) '#' done in
 let draw_floor () =
  for i = 0 to Array.length grid.(y_max+2) |> pred do grid.(y_max+2).(i) <- '#' done in
 (* returns true if caught / successfully placed *)
 let drop_sand () =
  let rec drop_sand' (x,y) =
   match grid_get (x,y) with
   | '.' -> drop_sand' (x,succ y)
   | '#' | 'o' ->
    if grid_get (pred x,y) = '.' then drop_sand' (pred x,y)
    else if grid_get (succ x,y) = '.' then drop_sand' (succ x,y)
    else (x,pred y)
   | _ -> assert false in
  let pos = drop_sand' sand_source in
  grid_set pos 'o' ; pos <> sand_source in 
 let print_grid () =
  Array.iter (fun line -> Array.iter (print_char) line ; print_newline ()) grid in
 let fill_grid () =
  List.iter begin fun s -> s |>
   List.fold_left begin fun a (x',y') ->
    (match a with Some (x,y) -> draw_line (x,y) (x',y') | _ -> ());
    Some (x',y')
   end None |> ignore end inputs in
 let ctr = ref 0 in
 fill_grid (); draw_floor ();
 while drop_sand () do incr ctr done; incr ctr; (* +1 for last grain *)
 print_grid ();
 x_min, x_max, y_max, !ctr

let problem_15a () =
 let raw_inputs =
  read_lines "prob15.txt" |> List.map (fun s -> 
   Scanf.sscanf s "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
   (fun x y x' y' -> x,y,x',y')) in
 let inputs = List.map (fun (x,y,x',y') -> x,y,(abs @@ x'-x)+(abs @@ y'-y)) raw_inputs in
 let row_y = if true then 2000000 else 10 in
 let relevant_beacons = Hashtbl.create 10 in
 List.iter (fun (_,_,x',y') -> if y' = row_y then Hashtbl.replace relevant_beacons x' ()) raw_inputs;
 let beacons_in_row_y = Hashtbl.to_seq_keys relevant_beacons |> Seq.length in
 let n_of_range (x,x') = succ @@ abs (x'-x) in
 let intersection_range (x,y,r) =
  match r - abs (row_y-y) with v when v >= 0 -> Some (x-v,x+v) | _ -> None in
 let try_merge_range (x,x') (x1,x1') =
  (* sticky edges *)
  let x' = if x' = pred x1 then succ x' else x' in
  let x1' = if x1' = pred x then succ x1' else x1' in
  if x' >= x1  && x' <= x1' || x  <= x1' && x  >= x1  ||
     x  >= x1  && x' <= x1' || x  <= x1  && x' >= x1' then Some (min x x1, max x' x1') else None in
 let ranges = List.fold_left begin fun acc i ->
  match intersection_range i with None -> acc | Some v -> v::acc
  end [] inputs in
 let rec merge_ranges acc =
  let m = List.fold_left begin fun ((min_x,max_x) as a) v ->
   match try_merge_range (List.hd acc) v with None -> a | Some (x,x') -> (min x min_x, max x' max_x)
  end (Int.max_int,Int.min_int) acc in
  let continue = List.fold_left
   (fun a v -> match try_merge_range m v with None -> a | Some v' -> a || v' <> m) false acc in
  if continue then merge_ranges (m::acc) else
  (m, List.filter (fun x -> try_merge_range m x |> Option.is_none) acc) in
 let fold_ranges rs =
  let rec fold_ranges' acc rem =
   let a,r = merge_ranges rem in
   if r <> [] then fold_ranges' (a::acc) r else (a::acc) in
  fold_ranges' [] rs in
 (fold_ranges ranges |> List.map n_of_range |> List.fold_left (+) 0) - beacons_in_row_y

let problem_15b () =
 let inputs = read_lines "prob15.txt" |>
  List.map (fun s -> 
   Scanf.sscanf s "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
   (fun x y x' y' -> x,y,(abs @@ x'-x)+(abs @@ y'-y))) in
 let row_y,min_dim,max_dim = (* switch for local global variables: use false for demo/example *)
  if true then 3204480,0,4000000
  else 11,0,20 in
 let fix_range (x,x') =
  if x > max_dim || x' < min_dim then None
  else Some (max min_dim x, min max_dim x') in
 let intersection_range row_y (x,y,r) =
  let intersection_range' (x,y,r) =
   match r - abs (row_y-y) with v when v >= 0 -> Some (x-v,x+v) | _ -> None in
  match intersection_range' (x,y,r) with None -> None | Some v -> fix_range v in
 let try_merge_range (x,x') (x1,x1') =
  (* sticky edges *)
  let x' = if x' = pred x1 then succ x' else x' in
  let x1' = if x1' = pred x then succ x1' else x1' in
  if x' >= x1  && x' <= x1' || x  <= x1' && x  >= x1  ||
     x  >= x1  && x' <= x1' || x  <= x1  && x' >= x1' then Some (min x x1, max x' x1') else None in
 let rec merge_ranges acc =
  let m = List.fold_left begin fun ((min_x,max_x) as a) v ->
   match try_merge_range (List.hd acc) v with None -> a | Some (x,x') -> (min x min_x, max x' max_x)
  end (Int.max_int,Int.min_int) acc in
  let continue = List.fold_left
   (fun a v -> match try_merge_range m v with None -> a | Some v' -> a || v' <> m) false acc in
  if continue then merge_ranges (m::acc) else
  (m, List.filter (fun x -> try_merge_range m x |> Option.is_none) acc) in
 let fold_ranges rs =
  let rec fold_ranges' acc rem =
   let a,r = merge_ranges rem in
   if r <> [] then fold_ranges' (a::acc) r else (a::acc) in
  fold_ranges' [] rs in
 let ranges row_y = List.fold_left begin fun acc i ->
  match intersection_range row_y i with None -> acc | Some v -> v::acc
  end [] inputs in
 let find_target () =
  let module M = struct exception Exit of int end in
  try for i = min_dim to max_dim do
   if ranges i |> fold_ranges |> List.length > 1 then raise_notrace (M.Exit i)
  done; -1 with M.Exit target -> target in
(*
 let find_target_nobreak () =
  let target = ref ~-1 in
  for i = min_dim to max_dim do
   if ranges i |> fold_ranges |> List.length > 1 then target := i
  done ; !target in
*)
 let find_gap = function
 | (a,n)::(_,_)::[] when a = min_dim -> succ n
 | (_,_)::(a,n)::[] when a = min_dim -> succ n
 | (a,_)::[] -> if a = min_dim then max_dim else min_dim
 | _ -> assert false in
 if false then  (* if true, go get a sandwich *)
  let row_y = find_target () in
  Printf.printf "%u\n" row_y;
  row_y + (ranges row_y |> fold_ranges |> find_gap) * 4000000
 else row_y + (ranges row_y |> fold_ranges |> find_gap) * 4000000

(* Possible Method: *)
(* execute every permutation of non-zero valves to open + shortest path to each valve *)
let problem_16a () =
 let inputs = read_lines "prob16.txt" |>
  List.map begin fun s ->
   match String.split_on_char ';' s with
   | s1::s2::_ ->
    let label, flow_rate = Scanf.sscanf s1 "Valve %s has flow rate=%u" (fun a b -> a,b) in
    let s2init = String.to_seq s2 |> Seq.take_while (fun c -> c = ' ' || Char.code c > 96) |> Seq.length in
    let paths = String.sub s2 s2init (String.length s2 - s2init)
             |> String.split_on_char ',' |> List.map String.trim
    in label,(flow_rate, paths)
   | _ -> assert false
  end in
 let input_table = List.to_seq inputs |> Hashtbl.of_seq in
 let start = "AA" in
 let module Action = struct type t = Move of string | Open | Idle end in
(*
 let testcase =
  Action.[
   Move "DD"; Open; Move "CC"; Move "BB"; Open; Move "AA"; Move "II"; Move "JJ"; Open; Move "II";
   Move "AA"; Move "DD"; Move "EE"; Move "FF"; Move "GG"; Move "HH"; Open; Move "GG"; Move "FF"; Move "EE";
   Open; Move "DD"; Move "CC"; Open] in
*)
 let set_30 a_list =
  Seq.append (List.to_seq a_list) (Seq.repeat Action.Idle) |> Seq.take 30 |> List.of_seq in
 (* returns action list using dijkstra pathfinding *)
 let path_to_valve start_pos end_pos =
  let dMap = Hashtbl.(of_seq @@ Seq.zip (to_seq_keys input_table) (Seq.repeat (Int.max_int,[])))
  and dQ = Queue.create () and found = ref false in
  Queue.add start_pos dQ; Hashtbl.replace dMap start_pos (0,[]);
  (* dijkstra loop *)
  while not !found && (Queue.is_empty dQ |> not) do
   let cur = Queue.take dQ in
   let neighbors = Hashtbl.find input_table cur |> snd
   and d, path = Hashtbl.find dMap cur |> (fun (x,y) -> (succ x,y)) in
   List.iter begin fun k ->
    if d < (Hashtbl.find dMap k |> fst)
    then (Hashtbl.replace dMap k (d, path @ [k]); Queue.add k dQ);
    if k = end_pos then found := true
   end neighbors
  done; Hashtbl.find dMap end_pos |> snd |> List.map (fun s -> Action.Move s) in
(*
 let permutate xs =
  let rec perm' x ys = function
   | [] -> [[x]]
   | z::[] -> (ys @ [x;z])::(ys @ [z;x])::[]
   | z::zs -> (ys @ (x::z::zs))::(perm' x (ys @ [z]) zs) in
  List.fold_right begin fun x perms ->
   List.map (fun xs -> perm' x [] xs) perms |> List.concat
  end xs [[]] in
*)
 let open_valves = Hashtbl.create 128 in
 let eval a_list =
  let ctr = ref 0 and pos = ref start in
  Hashtbl.reset open_valves;
  List.iter begin fun a ->
   ctr := !ctr + (Hashtbl.to_seq_values open_valves |> Seq.fold_left (+) 0);
   Action.(match a with
   | Move new_pos -> pos := new_pos
   | Open -> Hashtbl.replace open_valves !pos (Hashtbl.find input_table !pos |> fst)
   | Idle -> ())
  end (set_30 a_list) ; !ctr in
 let open_list_to_actions l =
  List.fold_left (fun (acc,lastx) x -> (acc @ path_to_valve lastx x @ [Action.Open],x)) ([],"AA") l |> fst in
 (* inputs *)
 (* eval testcase *) (* for demo, brute forcing permutations of openable valves renders the solution *)
 (*Hashtbl.to_seq_keys input_table |> List.of_seq*)
 (*open_list_to_actions ["DD";"BB";"JJ";"HH";"EE";"CC"] |> eval*) (* demo solution *)
 open_list_to_actions ["MC";"XL";"ED";"XZ";"JY";"EM"] |> eval (* problem solution *)

(*
  List.iteri (fun i xs -> Printf.printf "%u: %u\n" i (open_list_to_actions xs |> eval))
  (permutate ["DD";"BB";"JJ";"HH";"EE";"CC"]);
  List.iteri (fun i xs -> Printf.printf "%u: %u\n" i (open_list_to_actions xs |> eval))
  (permutate ["MC";"XL";"ED";"XZ";"JY";"EM"]);
  List.iteri (fun i xs -> Printf.printf "AF %u: %u\n" i (open_list_to_actions xs |> eval))
  (perm' "AF" [] ["MC";"XL";"ED";"XZ";"JY";"EM"]);
  path_to_valve "AA" "DD" @ [Action.Open] @
  path_to_valve "DD" "BB" @ [Action.Open] @
  path_to_valve "BB" "JJ" @ [Action.Open] @
  path_to_valve "JJ" "HH" @ [Action.Open] @
  path_to_valve "HH" "EE" @ [Action.Open] @
  path_to_valve "EE" "CC" @ [Action.Open] |> eval
*)

let permutate_by_ins ns xs =
 let insert_at i y ys =
   let y_head = Seq.take i ys and y_tail = Seq.drop i ys in
   Seq.append y_head (Seq.cons y y_tail) in
 let len = Array.length xs in
 let seq = ref (Seq.cons xs.(len-1) Seq.empty) in
 for i = 2 to len do
  seq := insert_at ns.(i-2) xs.(len-i) !seq
 done; !seq

let problem_16b () =
 let inputs = read_lines "prob16.txt" |>
  List.map begin fun s ->
   match String.split_on_char ';' s with
   | s1::s2::_ ->
    let label, flow_rate = Scanf.sscanf s1 "Valve %s has flow rate=%u" (fun a b -> a,b) in
    let s2init = String.to_seq s2 |> Seq.take_while (fun c -> c = ' ' || Char.code c > 96) |> Seq.length in
    let paths = String.sub s2 s2init (String.length s2 - s2init)
             |> String.split_on_char ',' |> List.map String.trim
    in label,(flow_rate, paths)
   | _ -> assert false
  end in
 let input_table = List.to_seq inputs |> Hashtbl.of_seq in
 let start = "AA" in
 let module Action = struct type t = Move of string | Open | Idle end in
 let set_26 a_list =
  Seq.append (List.to_seq a_list) (Seq.repeat Action.Idle) |> Seq.take 26 |> List.of_seq in
 (* returns action list using dijkstra pathfinding *)
 let path_to_valve start_pos end_pos =
  let dMap = Hashtbl.(of_seq @@ Seq.zip (to_seq_keys input_table) (Seq.repeat (Int.max_int,[])))
  and dQ = Queue.create () and found = ref false in
  Queue.add start_pos dQ; Hashtbl.replace dMap start_pos (0,[]);
  (* dijkstra loop *)
  while not !found && (Queue.is_empty dQ |> not) do
   let cur = Queue.take dQ in
   let neighbors = Hashtbl.find input_table cur |> snd
   and d, path = Hashtbl.find dMap cur |> (fun (x,y) -> (succ x,y)) in
   List.iter begin fun k ->
    if d < (Hashtbl.find dMap k |> fst)
    then (Hashtbl.replace dMap k (d, path @ [k]); Queue.add k dQ);
    if k = end_pos then found := true
   end neighbors
  done; Hashtbl.find dMap end_pos |> snd |> List.map (fun s -> Action.Move s) in
 let open_valves = Hashtbl.create 128 in
 let eval a_list =
  let ctr = ref 0 and pos = ref start in
  Hashtbl.reset open_valves;
  List.iter begin fun a ->
   ctr := !ctr + (Hashtbl.to_seq_values open_valves |> Seq.fold_left (+) 0);
   Action.(match a with
   | Move new_pos -> pos := new_pos
   | Open -> Hashtbl.replace open_valves !pos (Hashtbl.find input_table !pos |> fst)
   | Idle -> ())
  end (set_26 a_list) ; !ctr in
 let open_list_to_actions l =
  List.fold_left (fun (acc,lastx) x -> (acc @ path_to_valve lastx x @ [Action.Open],x)) ([],"AA") l |> fst in
 let find_solution () =
  let max_v = ref ~-1 and max_ns = Array.make 14 0 in
  let to_p = [|"XZ";"MC";"ED";"EX";"XL";"FX";"RW";"JY";"DI";"PK";"VF";"KR";"AF";"EM";"JS"|] in
  for a = 0 to 1 do for b = 0 to 2 do for c = 0 to 3 do for d = 0 to 4 do for e = 0 to 5 do
  for f = 0 to 6 do for g = 2 to 5 do for h = 0 to 1 do for i = 0 to 1 do for j = 6 to 6 do
  for k = 0 to 0 do for l = 7 to 8 do for m = 7 to 7 do for n = 9 to 10 do
   let ns = [|a;b;c;d;e;f;g;h;i;j;k;l;m;n|] in
   let p = permutate_by_ins ns to_p in
   let v1 = p |> Seq.take 7 |> List.of_seq |> open_list_to_actions |> eval in
   let v2 = p |> Seq.drop 7 |> List.of_seq |> open_list_to_actions |> eval in
   if (v1+v2) > !max_v then (
    max_v := (v1+v2);
    Array.blit ns 0 max_ns 0 14;
    Printf.printf "max: %u @ %u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u,%u\n" !max_v a b c d e f g h i j k l m n);
    flush stdout
  done done done done done done done done done done done done done done in
  (*permutate_by_ins [|0;0;0;0;0;2;4;1;1;6;0;8;7;10|] to_p |> List.of_seq in*)
 if false then find_solution ();
(*
 let testcaseH = open_list_to_actions ["JJ";"BB";"CC"]
 and testcaseE = open_list_to_actions ["DD";"HH";"EE"] in
 *)
 let testcaseH = open_list_to_actions ["EX"; "PK"; "FX"; "RW"; "VF"; "DI"; "KR"]
 and testcaseE = open_list_to_actions ["MC"; "XL"; "ED"; "XZ"; "JY"; "AF"; "EM"; "JS"] in
 eval testcaseH + eval testcaseE

(*
(* Full Loop *)
 for a = 0 to 1 do for b = 0 to 2 do for c = 0 to 3 do for d = 0 to 4 do for e = 0 to 5 do
 for f = 0 to 6 do for g = 0 to 7 do for h = 0 to 8 do for i = 0 to 9 do for j = 0 to 10 do
 for k = 0 to 11 do for l = 0 to 12 do for m = 0 to 13 do for n = 0 to 14 do
(* Tailored Loop *)
 for a = 0 to 1 do for b = 0 to 2 do for c = 0 to 3 do for d = 0 to 4 do for e = 0 to 5 do
 for f = 0 to 6 do for g = 2 to 5 do for h = 0 to 1 do for i = 0 to 1 do for j = 6 to 6 do
 for k = 0 to 0 do for l = 7 to 8 do for m = 7 to 7 do for n = 9 to 10 do
*)
(* max: 2414 @ 0,0,0,0,0,0,2,0,0,6,0,7,7,9   *)
(* max: 2418 @ 0,0,0,0,0,0,2,0,0,6,0,8,7,10  *)
(* max: 2509 @ 0,0,0,0,0,0,4,0,0,6,0,8,7,10  *)
(* max: 2559 @ 0,0,0,0,0,1,2,1,1,6,0,7,7,9   *)
(* max: 2563 @ 0,0,0,0,0,1,2,1,1,6,0,8,7,10  *)
(* max: 2577 @ 0,0,0,0,0,1,3,1,1,6,0,7,7,9   *)
(* max: 2581 @ 0,0,0,0,0,1,3,1,1,6,0,8,7,10  *)
(* max: 2672 @ 0,0,0,0,0,1,4,1,1,6,0,8,7,10  *)
(* max: 2705 @ 0,0,0,0,0,2,4,1,1,6,0,8,7,10  *)

(* tetris simulator *)
(* use bit matrix with byte array *)
(* maximum height per 5 drops is 2+4+3+3+1 *)
let problem_17a () =
 let inputs = read_lines "prob17.txt" |> List.hd |> String.to_seq |> Seq.map ((=) '>') |> Array.of_seq in
 let input_len = Array.length inputs in
 let input_cur = ref 0 in
 let tower = Bytes.make 5265 '\x01' in Bytes.set tower 0 (Char.chr 255); (* set floor *)
 let tower_height () =
  let i = ref 0 in while ((Bytes.get tower !i |> Char.code) land 0b11111110) <> 0 do incr i done; (!i - 1) in
 let block_ctr = ref ~-1 in
 let test_collision t y x =
  match t with
  | 0 -> (((0b11110000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) (*-*)
  | 1 -> (((0b01000000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) || (*+*)
         (((0b11100000 lsr x) land Char.code (Bytes.get tower (y+1))) <> 0) ||
         (((0b01000000 lsr x) land Char.code (Bytes.get tower (y+2))) <> 0)
  | 2 -> (((0b11100000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) || (*inv L*)
         (((0b00100000 lsr x) land Char.code (Bytes.get tower (y+1))) <> 0) ||
         (((0b00100000 lsr x) land Char.code (Bytes.get tower (y+2))) <> 0)  
  | 3 -> (((0b10000000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) || (*I*)
         (((0b10000000 lsr x) land Char.code (Bytes.get tower (y+1))) <> 0) ||
         (((0b10000000 lsr x) land Char.code (Bytes.get tower (y+2))) <> 0) ||
         (((0b10000000 lsr x) land Char.code (Bytes.get tower (y+3))) <> 0)
  | 4 -> (((0b11000000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) || (*o*)
         (((0b11000000 lsr x) land Char.code (Bytes.get tower (y+1))) <> 0)
  | _ -> assert false in
 let set_block t y x =
  match t with
  | 0 -> Bytes.set tower (y)   @@ Char.chr ((0b11110000 lsr x) lor Char.code (Bytes.get tower (y))) (*-*)
  | 1 -> Bytes.set tower (y)   @@ Char.chr ((0b01000000 lsr x) lor Char.code (Bytes.get tower (y))); (*+*)
         Bytes.set tower (y+1) @@ Char.chr ((0b11100000 lsr x) lor Char.code (Bytes.get tower (y+1))); 
         Bytes.set tower (y+2) @@ Char.chr ((0b01000000 lsr x) lor Char.code (Bytes.get tower (y+2)))
  | 2 -> Bytes.set tower (y)   @@ Char.chr ((0b11100000 lsr x) lor Char.code (Bytes.get tower (y))); (*inv L*)
         Bytes.set tower (y+1) @@ Char.chr ((0b00100000 lsr x) lor Char.code (Bytes.get tower (y+1)));
         Bytes.set tower (y+2) @@ Char.chr ((0b00100000 lsr x) lor Char.code (Bytes.get tower (y+2)))  
  | 3 -> Bytes.set tower (y)   @@ Char.chr ((0b10000000 lsr x) lor Char.code (Bytes.get tower (y))); (*I*)
         Bytes.set tower (y+1) @@ Char.chr ((0b10000000 lsr x) lor Char.code (Bytes.get tower (y+1)));
         Bytes.set tower (y+2) @@ Char.chr ((0b10000000 lsr x) lor Char.code (Bytes.get tower (y+2)));
         Bytes.set tower (y+3) @@ Char.chr ((0b10000000 lsr x) lor Char.code (Bytes.get tower (y+3)))
  | 4 -> Bytes.set tower (y)   @@ Char.chr ((0b11000000 lsr x) lor Char.code (Bytes.get tower (y))); (*o*)
         Bytes.set tower (y+1) @@ Char.chr ((0b11000000 lsr x) lor Char.code (Bytes.get tower (y+1)))
  | _ -> assert false in
 let move_right (t,y,x) = if test_collision t y (x+1) then (t,y,x) else (t,y,x+1) in
 let move_left (t,y,x) =
  if x = 0 then (t,y,x) else if test_collision t y (x-1) then (t,y,x) else (t,y,x-1) in
 let try_move_down (t,y,x) =
  if test_collision t (y-1) (x) then (set_block t y x ; None) else Some (t,y-1,x) in
 let new_block () = incr block_ctr; (!block_ctr mod 5, tower_height () + 4,2) in
 let simulate () =
  while !block_ctr < 2021 do (*do-while style, because ctr activates after check*)
   let blk = ref (new_block ()) in
   let break = ref false in
   while not !break do
    if inputs.(!input_cur) then blk := move_right !blk else blk := move_left !blk ;
    input_cur := succ !input_cur mod input_len ;
    match try_move_down !blk with
    | Some b -> blk := b
    | None -> break := true
   done
  done in
  simulate (); tower_height ()

let problem_17b () =
 let inputs = read_lines "prob17.txt" |> List.hd |> String.to_seq |> Seq.map ((=) '>') |> Array.of_seq in
 let state_checker = Array.(make (length inputs) 10) in
 let last_state = ref 0 in
 (*let blocks_to_sim = 3840 in*)
 let blocks_to_sim = 202200 in
 let input_len = Array.length inputs in
 let input_cur = ref 0 in
 (* use rolling window *)
 let tower = Bytes.make 64010 '\x01' in Bytes.set tower 0 (Char.chr 255); (* set floor *)
 let tower_height () =
  let i = ref 0 in while ((Bytes.get tower !i |> Char.code) land 0b11111110) <> 0 do incr i done; (!i - 1) in
 let block_ctr = ref ~-1 in
 let test_collision t y x =
  match t with
  | 0 -> (((0b11110000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) (*-*)
  | 1 -> (((0b01000000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) || (*+*)
         (((0b11100000 lsr x) land Char.code (Bytes.get tower (y+1))) <> 0) ||
         (((0b01000000 lsr x) land Char.code (Bytes.get tower (y+2))) <> 0)
  | 2 -> (((0b11100000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) || (*inv L*)
         (((0b00100000 lsr x) land Char.code (Bytes.get tower (y+1))) <> 0) ||
         (((0b00100000 lsr x) land Char.code (Bytes.get tower (y+2))) <> 0)  
  | 3 -> (((0b10000000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) || (*I*)
         (((0b10000000 lsr x) land Char.code (Bytes.get tower (y+1))) <> 0) ||
         (((0b10000000 lsr x) land Char.code (Bytes.get tower (y+2))) <> 0) ||
         (((0b10000000 lsr x) land Char.code (Bytes.get tower (y+3))) <> 0)
  | 4 -> (((0b11000000 lsr x) land Char.code (Bytes.get tower (y))) <> 0) || (*o*)
         (((0b11000000 lsr x) land Char.code (Bytes.get tower (y+1))) <> 0)
  | _ -> assert false in
 let set_block t y x =
  match t with
  | 0 -> Bytes.set tower (y)   @@ Char.chr ((0b11110000 lsr x) lor Char.code (Bytes.get tower (y))) (*-*)
  | 1 -> Bytes.set tower (y)   @@ Char.chr ((0b01000000 lsr x) lor Char.code (Bytes.get tower (y))); (*+*)
         Bytes.set tower (y+1) @@ Char.chr ((0b11100000 lsr x) lor Char.code (Bytes.get tower (y+1))); 
         Bytes.set tower (y+2) @@ Char.chr ((0b01000000 lsr x) lor Char.code (Bytes.get tower (y+2)))
  | 2 -> Bytes.set tower (y)   @@ Char.chr ((0b11100000 lsr x) lor Char.code (Bytes.get tower (y))); (*inv L*)
         Bytes.set tower (y+1) @@ Char.chr ((0b00100000 lsr x) lor Char.code (Bytes.get tower (y+1)));
         Bytes.set tower (y+2) @@ Char.chr ((0b00100000 lsr x) lor Char.code (Bytes.get tower (y+2)))  
  | 3 -> Bytes.set tower (y)   @@ Char.chr ((0b10000000 lsr x) lor Char.code (Bytes.get tower (y))); (*I*)
         Bytes.set tower (y+1) @@ Char.chr ((0b10000000 lsr x) lor Char.code (Bytes.get tower (y+1)));
         Bytes.set tower (y+2) @@ Char.chr ((0b10000000 lsr x) lor Char.code (Bytes.get tower (y+2)));
         Bytes.set tower (y+3) @@ Char.chr ((0b10000000 lsr x) lor Char.code (Bytes.get tower (y+3)))
  | 4 -> Bytes.set tower (y)   @@ Char.chr ((0b11000000 lsr x) lor Char.code (Bytes.get tower (y))); (*o*)
         Bytes.set tower (y+1) @@ Char.chr ((0b11000000 lsr x) lor Char.code (Bytes.get tower (y+1)))
  | _ -> assert false in
 let move_right (t,y,x) = if test_collision t y (x+1) then (t,y,x) else (t,y,x+1) in
 let move_left (t,y,x) =
  if x = 0 then (t,y,x) else if test_collision t y (x-1) then (t,y,x) else (t,y,x-1) in
 let try_move_down (t,y,x) =
  if test_collision t (y-1) (x) then (set_block t y x ; None) else Some (t,y-1,x) in
 let new_block () = incr block_ctr; (!block_ctr mod 5, tower_height () + 4,2) in
 let check_duplicate_state (t,_,_) =
  if state_checker.(!input_cur) = t && !input_cur = 22 then (
   Printf.printf "Identical State Detected: type: %u @ height: %u @ cur: %u @ block#: %u block-diff: %u\n" t (tower_height () ) (!input_cur) (!block_ctr) (!block_ctr - !last_state);
   flush stdout;
   last_state := !block_ctr
  ) else state_checker.(!input_cur) <- t in
 let simulate () =
  while !block_ctr < (pred blocks_to_sim) do (*do-while style, because ctr activates after check*)
   let blk = ref (new_block ()) in
   let break = ref false in
   check_duplicate_state !blk;
   while not !break do
    if inputs.(!input_cur) then blk := move_right !blk else blk := move_left !blk ;
    input_cur := succ !input_cur mod input_len ;
    match try_move_down !blk with
    | Some b -> blk := b
    | None -> break := true
   done
  done in
  (* input cycle detected w/state checker: height_of_blocks (1760x+3509) = 5503+2737x *)
  (* nearest cycle to 1000000000000 (10**12) -> x = (Int.of_float (10.**12.) - 3509) / 1760 = 568181816 ; rem= 331 *)
  (* height_of_blocks (999999999669) = 1555113635895 *)
  (* remaining blocks found by simulating with value 3509 + 331 = 3840 *)
  (* and subtracting 5503 : 5993 - 5503 = 490 *)
  (* answer = 1555113635895 + 490 = 1555113636385 *)
  simulate (); tower_height ()

(* count non-touching voxel faces *)
let problem_18a () =
 let inputs = read_lines "prob18.txt" |> List.to_seq |> Seq.map (fun s -> Scanf.sscanf s "%u,%u,%u" (fun x y z -> x,y,z)) in
 let input_table = Hashtbl.of_seq Seq.(zip inputs (repeat ())) in
 let ctr = ref 0 in
 Seq.iter begin fun (x,y,z) -> 
  for i = 0 to 1 do if Hashtbl.mem input_table (x-1+i*2,y,z) then () else incr ctr done;
  for i = 0 to 1 do if Hashtbl.mem input_table (x,y-1+i*2,z) then () else incr ctr done;
  for i = 0 to 1 do if Hashtbl.mem input_table (x,y,z-1+i*2) then () else incr ctr done
 end inputs; !ctr

(* count non-touching EXTERIOR voxel faces *)
(* input size dimensions range from 0 to 19 *)
let problem_18b () =
 let inputs = read_lines "prob18.txt" |> List.to_seq |> Seq.map (fun s -> Scanf.sscanf s "%u,%u,%u" (fun x y z -> x,y,z)) in
 let input_table = Hashtbl.of_seq Seq.(zip inputs (repeat ())) in
 let exposed_air = Hashtbl.create 8196 in
 let lower_bound = ~-1 and upper_bound = 20 in
 let dQ = Queue.create () in
 let ctr = ref 0 in
 let is_valid_neighbor (x,y,z) =
  if x < lower_bound || x > upper_bound ||
     y < lower_bound || y > upper_bound ||
     z < lower_bound || z > upper_bound then false else
  if Hashtbl.mem input_table (x,y,z) then false else
  if Hashtbl.mem exposed_air (x,y,z) then false else true in
 let build_exposed_air () =
  Queue.add (0,0,0) dQ;
  Hashtbl.add exposed_air (0,0,0) ();
  while Queue.is_empty dQ |> not do
   let (x,y,z) = Queue.take dQ in
   for i = 0 to 1 do if is_valid_neighbor (x-1+i*2,y,z) then
    (Queue.add (x-1+i*2,y,z) dQ ; Hashtbl.add exposed_air (x-1+i*2,y,z) ()) done;
   for i = 0 to 1 do if is_valid_neighbor (x,y-1+i*2,z) then
    (Queue.add (x,y-1+i*2,z) dQ ; Hashtbl.add exposed_air (x,y-1+i*2,z) ()) done;
   for i = 0 to 1 do if is_valid_neighbor (x,y,z-1+i*2) then
    (Queue.add (x,y,z-1+i*2) dQ ; Hashtbl.add exposed_air (x,y,z-1+i*2) ()) done
  done in
 let is_exposed_air (x,y,z) = Hashtbl.mem exposed_air (x,y,z) in
 build_exposed_air ();
 (*Hashtbl.to_seq_keys exposed_air |> List.of_seq*)
 Seq.iter begin fun (x,y,z) -> 
  for i = 0 to 1 do if is_exposed_air (x-1+i*2,y,z) then incr ctr done;
  for i = 0 to 1 do if is_exposed_air (x,y-1+i*2,z) then incr ctr done;
  for i = 0 to 1 do if is_exposed_air (x,y,z-1+i*2) then incr ctr done
 end inputs; !ctr

let problem_20a () =
 let inputs = read_lines "prob20.txt" |> List.to_seq
  |> Seq.map (fun s -> Scanf.sscanf s "%d" Fun.id) |> Seq.(zip (ints 0)) in
 let len = Seq.length inputs in
 let mut_inputs = Array.of_seq inputs in
 let mod_plus a b = (a mod b + b) mod b in
 let mix_val ((_,n) as v) =
  let new_head = Seq.take_while ((<>) v) (Array.to_seq mut_inputs) in
  let i = Seq.length new_head in
  let i' = mod_plus (i + n) (len - 1) in
  (*Printf.printf "i: %d i': %d\n" i i'; flush stdout;*)
  if i = i' then () else
  if i' > i then
   (Array.blit mut_inputs (i+1) mut_inputs i (i'-i); mut_inputs.(i') <- v)
  else
   (Array.blit mut_inputs (i') mut_inputs (i'+1) (i-i'); mut_inputs.(i') <- v) in
 let ans () =
  let new_head = Seq.take_while (fun (_,b) -> b <> 0) (Array.to_seq mut_inputs) in
  let i = Seq.length new_head in
  let idx_a = mod_plus (i+1000) len
  and idx_b = mod_plus (i+2000) len
  and idx_c = mod_plus (i+3000) len in
  let _,a = mut_inputs.(idx_a) and _,b = mut_inputs.(idx_b) and _,c = mut_inputs.(idx_c) in
  Printf.printf "a: %d, b: %d, c; %d\n" a b c;
  a + b + c in
 Seq.iter mix_val inputs;
 (*List.of_seq !mut_inputs*)
 ans ()

(* execution time: 15s *)
let problem_20b () =
 let key = 811589153 in
 let inputs = read_lines "prob20.txt" |> List.to_seq
  |> Seq.map (fun s -> Scanf.sscanf s "%d" (( * ) key)) |> Seq.(zip (ints 0)) |> Seq.memoize in
 let len = Seq.length inputs in
 let mut_inputs = Array.of_seq inputs in
 let mod_plus a b = (a mod b + b) mod b in
 let mix_val ((_,n) as v) =
  let i = ref 0 in
  Array.iteri (fun j x -> if x = v then i := j) mut_inputs; (* slightly faster than using seqs *)
  let i = !i in
  let i' = mod_plus (i + n) (len - 1) in
  (*Printf.printf "i: %d i': %d\n" i i'; flush stdout;*)
  if i = i' then () else
  if i' > i then (* insert by blit is an order of magnitude faster than insert by seq *)
   (Array.blit mut_inputs (i+1) mut_inputs i (i'-i); mut_inputs.(i') <- v)
  else
   (Array.blit mut_inputs (i') mut_inputs (i'+1) (i-i'); mut_inputs.(i') <- v) in
 let ans () =
  let i = ref 0 in Array.iteri (fun j (_,b) -> if b = 0 then i := j) mut_inputs;
  let i = !i in
  let idx_a = mod_plus (i+1000) len
  and idx_b = mod_plus (i+2000) len
  and idx_c = mod_plus (i+3000) len in
  let _,a = mut_inputs.(idx_a) and _,b = mut_inputs.(idx_b) and _,c = mut_inputs.(idx_c) in
  Printf.printf "a: %d, b: %d, c; %d\n" a b c;
  a + b + c in
 for i = 1 to 10 do Seq.iter mix_val inputs done;
 (*List.of_seq !mut_inputs*)
 ans ()

(* doubly linked lists *)
let problem_20a2 () =
 let inputs = read_lines "prob20.txt"
  |> List.map (fun s -> Scanf.sscanf s "%d" (Fun.id)) |> Array.of_list in
 let len = Array.length inputs in
 let ll = Array.init len (fun i -> (i-1,i+1)) in
 ll.(0) <- (len-1,snd ll.(0));
 ll.(len-1) <- (fst ll.(0),0);
 let mod_plus a b = (a mod b + b) mod b in
 let mix_val i x =
  let p,n = ll.(i) in
  let x = mod_plus x (len-1) in
  if x = 0 then () else begin (* remove i *)
   ll.(p) <- (fst ll.(p),n); ll.(n) <- (p,snd ll.(n));
   (* search forward - searching backward time-save minimal at best *)
   let idx = ref p in
   for i = 0 to x - 1 do idx := snd ll.(!idx) done;
   let p' = !idx and n' = snd ll.(!idx) in
   ll.(p') <- (fst ll.(p'),i);
   ll.(i) <- (p',n');
   ll.(n') <- (i,snd ll.(n'));
  end in
 let ans () =
  let idx = ref 0 in
  for i = 0 to len - 1 do if inputs.(i) = 0 then idx := i done;
  for i = 1 to 1000 mod len do idx := snd ll.(!idx) done;
  let a = inputs.(!idx) in
  for i = 1 to 1000 mod len do idx := snd ll.(!idx) done;
  let b = inputs.(!idx) in
  for i = 1 to 1000 mod len do idx := snd ll.(!idx) done;
  let c = inputs.(!idx) in
  Printf.printf "a: %d, b: %d, c; %d\n" a b c;
  a + b + c in
 Array.iteri mix_val inputs;
 ans ()

(* doubly linked lists *)
(* execution time: 3s *)
let problem_20b2 () =
 let key = 811589153 in
 let inputs = read_lines "prob20.txt"
  |> List.map (fun s -> Scanf.sscanf s "%d" (( * ) key)) |> Array.of_list in
 let len = Array.length inputs in
 let ll = Array.init len (fun i -> (i-1,i+1)) in
 ll.(0) <- (len-1,snd ll.(0));
 ll.(len-1) <- (fst ll.(0),0);
 let mod_plus a b = (a mod b + b) mod b in
 let mix_val i x =
  let p,n = ll.(i) in
  let x = mod_plus x (len-1) in
  if x = 0 then () else begin (* remove i *)
   ll.(p) <- (fst ll.(p),n); ll.(n) <- (p,snd ll.(n));
   (* forward find next p - no point in branching for backward find *)
   let idx = ref p in
   for i = 0 to x - 1 do idx := snd ll.(!idx) done;
   let p' = !idx and n' = snd ll.(!idx) in
   ll.(p') <- (fst ll.(p'),i);
   ll.(i) <- (p',n');
   ll.(n') <- (i,snd ll.(n'));
  end in
 let ans () =
  let idx = ref 0 in
  for i = 0 to len - 1 do if inputs.(i) = 0 then idx := i done;
  for i = 1 to 1000 mod len do idx := snd ll.(!idx) done;
  let a = inputs.(!idx) in
  for i = 1 to 1000 mod len do idx := snd ll.(!idx) done;
  let b = inputs.(!idx) in
  for i = 1 to 1000 mod len do idx := snd ll.(!idx) done;
  let c = inputs.(!idx) in
  Printf.printf "a: %d, b: %d, c; %d\n" a b c;
  a + b + c in
 for i = 1 to 10 do Array.iteri mix_val inputs done;
 ans ()

let problem_21a () =
 let solved = Hashtbl.create 2048 in
 let unsolved = Hashtbl.create 2048 in
 let op_of_c = function '+' -> (+) | '-' -> (-) | '*' -> ( * ) | '/' -> (/) | _ -> assert false in
 let inputs = read_lines "prob21.txt" in
 List.iter begin fun s ->
   if (let c = Char.code s.[6] in c <= 0x39 && c >= 0x30) then
   (let k,v = Scanf.sscanf s "%s %d" (fun s n -> String.sub s 0 4, n) in Hashtbl.add solved k v) else
   (let k,a,c,b = Scanf.sscanf s "%s %s %c %s" (fun s a c b -> String.sub s 0 4,a,c,b) in Hashtbl.add unsolved k (a,c,b))
  end inputs;
 let rec solve key =
  if Hashtbl.mem solved key then Hashtbl.find solved key else
  let a,c,b = Hashtbl.find unsolved key in
  let ans = (op_of_c c) (solve a) (solve b) in
  Hashtbl.add solved key ans ; ans in
 solve "root"

let problem_21b () =
 let solved = Hashtbl.create 2048 in
 let unsolved = Hashtbl.create 2048 in
 let al_stk = Stack.create () in
 let op_of_c = function '+' -> (+) | '-' -> (-) | '*' -> ( * ) | '/' -> (/) | _ -> assert false in
 let flag_humn_encountered = ref false in
 let inputs = read_lines "prob21.txt" in
 let rebuild_tables () = 
 Hashtbl.reset solved;
 Hashtbl.reset unsolved;
 List.iter begin fun s ->
   if (let c = Char.code s.[6] in c <= 0x39 && c >= 0x30) then
   (let k,v = Scanf.sscanf s "%s %d" (fun s n -> String.sub s 0 4, n) in Hashtbl.add solved k (Some v)) else
   (let k,a,op,b = Scanf.sscanf s "%s %s %c %s" (fun s a c b -> String.sub s 0 4, a, c, b) in Hashtbl.add unsolved k (a,op,b)) end inputs in
 rebuild_tables ();
 let rec solve key =
  if Hashtbl.mem solved key then Hashtbl.find solved key |> Option.get else
  let a,c,b = Hashtbl.find unsolved key in
  let ans = (op_of_c c) (solve a) (solve b) in
  if a = "humn" then (Printf.printf "Encountered humn @:a %s\n" key ; flag_humn_encountered := true);
  if b = "humn" then (Printf.printf "Encountered humn @:b %s\n" key ; flag_humn_encountered := true);
  Hashtbl.add solved key (Some ans) ; ans in
 let al_solve key c = (*luckily humn only appears at one leaf*)
  Hashtbl.add solved "humn" None;
  let rec al key =
   if Hashtbl.mem solved key then Hashtbl.find solved key else
   match (let a,o,b = Hashtbl.find unsolved key in (al a),o,(al b)) with
   | Some v,'+',None | None,'+',Some v -> Stack.push (fun c -> c - v) al_stk; None
   | Some v,'*',None | None,'*',Some v -> Stack.push (fun c -> c / v) al_stk; None
   | Some v,'-',None -> Stack.push (fun c -> v - c) al_stk; None
   | None,'-',Some v -> Stack.push (fun c -> v + c) al_stk; None
   | Some v,'/',None -> Stack.push (fun c -> v / c) al_stk; None
   | None,'/',Some v -> Stack.push (fun c -> v * c) al_stk; None
   | Some a,opc,Some b -> let ans = Some ((op_of_c opc) a b) in Hashtbl.add solved key ans; ans
   | _ -> assert false in
 al key |> ignore ; Stack.fold (Fun.flip (@@)) c al_stk in
 let root_a,_,root_b = Hashtbl.find unsolved "root" in
 (*automate by solving one side of root and conditionally tagging humn ; but tables must be rebuilt*)
 (*al_solve may also be designated manually: e.g., *)
 (*al_solve "pppw" 150*)
 (*al_solve "ddzt" 77247625979730*)
 let v = solve root_a in rebuild_tables();
 let k,v = if (!flag_humn_encountered |> not) then (root_b,v) else (root_a, solve root_b) in
 al_solve k v

let problem_22a () =
 let inputs = read_lines "prob22.txt" |> Array.of_list in
 let map_h = Array.length inputs - 2 in
 let x_bounds = Array.make map_h (0,0) in
 let is_digit c = let x = Char.code c in x <= 0x39 && x >= 0x30 in
 let mod_plus a b = (a mod b + b) mod b in
 let cur_x = ref 0 and cur_y = ref 0 and cur_dir = ref 0 in (* > = 0, CW -> +1, CCW -> -1 *)
 (* specialized jit wrap function *)
 let wrap_y dy =
  let rec wrap_y' y dy =
   let y' = y - dy in
   if y' >= map_h || y' < 0 || (!cur_x < fst x_bounds.(y')) || (!cur_x > snd x_bounds.(y'))
   then y else wrap_y' y' dy in
  wrap_y' !cur_y dy in
 let try_move () =
  let dx,dy = match !cur_dir with 0 -> (1,0) | 1 -> (0,1) | 2 -> (~-1,0) | 3 -> (0,~-1) | _ -> assert false in
  let x' = !cur_x + dx and y' = !cur_y + dy in
  (* validation masking *)
  (* ensure in array bounds + try simple wrap *)
  let y' = if y' < 0 then map_h -1 else y' in
  let y' = if y' >= map_h then 0 else y' in
  (* wrap y if necessary *)
  let y' = if dy <> 0 && ((x' < fst x_bounds.(y')) || (x' > snd x_bounds.(y'))) then wrap_y dy else y' in
  (* wrap x if necessary *)
  let x' = if x' < (fst x_bounds.(y')) then snd x_bounds.(y') else x' in
  let x' = if x' > (snd x_bounds.(y')) then fst x_bounds.(y') else x' in
  if inputs.(y').[x'] = '.' then (cur_y := y'; cur_x := x' ; Printf.printf "%u %u\n" y' x') else () in
 let next_token r =
  match Seq.uncons r with
  | None -> None
  | Some (c,rs) when c = 'L' || c = 'R' -> Some (Either.right c,rs)
  | Some (c,_) ->
   let n = Seq.take_while (is_digit) r |> String.of_seq |> int_of_string
   and rs = Seq.drop_while (is_digit) r in Some (Either.left n,rs) in
 for i = 0 to pred map_h do
  let x_max = String.length inputs.(i) |> pred
  and x_min = Seq.length (inputs.(i) |> String.to_seq |> Seq.take_while ((=)' ')) in
  x_bounds.(i) <- (x_min,x_max)
 done ; cur_x := fst x_bounds.(0);
 let rec run_loop = function
  | None -> ()
  | Some (t,rs) ->
   Either.(match t with
    | Left n -> for i = 1 to n do try_move () done
    | Right d when d = 'R' -> cur_dir := mod_plus (!cur_dir + 1) 4
    | Right d when d = 'L' -> cur_dir := mod_plus (!cur_dir - 1) 4
    | _ -> assert false
   ); run_loop (next_token rs)
 in run_loop (next_token (inputs.(map_h+1) |> String.to_seq));
 1000*(!cur_y+1)+4*(!cur_x+1)+(!cur_dir)

let problem_22b () =
 let inputs = read_lines "prob22.txt" |> Array.of_list in
 let map_h = Array.length inputs - 2 in
 let x_bounds = Array.make map_h (0,0) in
 let is_digit c = let x = Char.code c in x <= 0x39 && x >= 0x30 in
 let mod_plus a b = (a mod b + b) mod b in
 let cur_f = ref 0 and cur_x = ref 0 and cur_y = ref 0 and cur_dir = ref 0 in (* > = 0, CW -> +1, CCW -> -1 *)
 let cube_get f y x =
  match f with
  | 0 -> inputs.(y).[x+50]
  | 1 -> inputs.(y).[x+100]
  | 2 -> inputs.(y+50).[x+50]
  | 3 -> inputs.(y+100).[x]
  | 4 -> inputs.(y+100).[x+50]
  | 5 -> inputs.(y+150).[x]
  | _ -> assert false in
 let map_yx_of_cube f y x =
  match f with
  | 0 -> y,x+50
  | 1 -> y,x+100
  | 2 -> y+50,x+50
  | 3 -> y+100,x
  | 4 -> y+100,x+50
  | 5 -> y+150,x
  | _ -> assert false in
 let wrap_cube f y x d =
  if x >= 0 && y >= 0 && x < 50 && y < 50 then f,y,x,d else
  let m = 49 in (* max dim *)
  match f,d with
  (* verify edge pats: _,0,0 0,_,1 _,m,2 m,_,3 *)
  (* loop 0,1,4,3 *)    (* loop 0,2,4,5 *)  (* loop 2,1,5,3 *)
  | 0,0 -> 1,y,0,0   | 0,1 -> 2,0,x,1 | 2,0 -> 1,m,y,3
  | 1,0 -> 4,m-y,m,2 | 2,1 -> 4,0,x,1 | 1,3 -> 5,m,x,3
  | 4,2 -> 3,y,m,2   | 4,1 -> 5,x,m,2 | 5,3 -> 3,m,x,3
  | 3,2 -> 0,m-y,0,0 | 5,2 -> 0,0,y,1 | 3,3 -> 2,x,0,0
  | 0,2 -> 3,m-y,0,0 | 0,3 -> 5,x,0,0 | 2,2 -> 3,0,y,1
  | 3,0 -> 4,y,0,0   | 5,0 -> 4,m,y,3 | 3,1 -> 5,0,x,1
  | 4,0 -> 1,m-y,m,2 | 4,3 -> 2,m,x,3 | 5,1 -> 1,0,x,1
  | 1,2 -> 0,y,m,2   | 2,3 -> 0,m,x,3 | 1,1 -> 2,x,m,2
  | _ -> assert false
 in
 let try_move () =
  let dx,dy = match !cur_dir with 0 -> (1,0) | 1 -> (0,1) | 2 -> (~-1,0) | 3 -> (0,~-1) | _ -> assert false in
  let f',y',x',d' = wrap_cube !cur_f (!cur_y + dy) (!cur_x + dx) !cur_dir in
  if cube_get f' y' x' = '.' then
  (cur_f := f' ; cur_dir := d' ; cur_y := y'; cur_x := x'
   (*; (let y,x = map_yx_of_cube f' y' x' in Printf.printf "%u %u\n" y x)*)
  )
  else () in
 let next_token r =
  match Seq.uncons r with
  | None -> None
  | Some (c,rs) when c = 'L' || c = 'R' -> Some (Either.right c,rs)
  | Some (c,_) ->
   let n = Seq.take_while (is_digit) r |> String.of_seq |> int_of_string
   and rs = Seq.drop_while (is_digit) r in Some (Either.left n,rs) in
 for i = 0 to pred map_h do
  let x_max = String.length inputs.(i) |> pred
  and x_min = Seq.length (inputs.(i) |> String.to_seq |> Seq.take_while ((=)' ')) in
  x_bounds.(i) <- (x_min,x_max)
 done ;
 let rec run_loop = function
  | None -> ()
  | Some (t,rs) ->
   Either.(match t with
    | Left n -> for i = 1 to n do try_move () done
    | Right d when d = 'R' -> cur_dir := mod_plus (!cur_dir + 1) 4
    | Right d when d = 'L' -> cur_dir := mod_plus (!cur_dir - 1) 4
    | _ -> assert false
   ); run_loop (next_token rs)
 in run_loop (next_token (inputs.(map_h+1) |> String.to_seq));
 let y,x = map_yx_of_cube !cur_f !cur_y !cur_x in
 let x = x - fst x_bounds.(y) in
 1000*(y+1)+4*(x+1)+(!cur_dir)

(* for demo only *)
let problem_22b2 () =
 let inputs = read_lines "prob22.txt" |> Array.of_list in
 let map_h = Array.length inputs - 2 in
 let x_bounds = Array.make map_h (0,0) in
 let is_digit c = let x = Char.code c in x <= 0x39 && x >= 0x30 in
 let mod_plus a b = (a mod b + b) mod b in
 let cur_f = ref 0 and cur_x = ref 0 and cur_y = ref 0 and cur_dir = ref 0 in (* > = 0, CW -> +1, CCW -> -1 *)
 let cube_get f y x =
  match f with
  | 0 -> inputs.(y).[x+8]
  | 1 -> inputs.(y+4).[x]
  | 2 -> inputs.(y+4).[x+4]
  | 3 -> inputs.(y+4).[x+8]
  | 4 -> inputs.(y+8).[x+8]
  | 5 -> inputs.(y+8).[x+12]
  | _ -> assert false in
 let map_yx_of_cube f y x =
  match f with (* x and y must be [0,49] *)
  | 0 -> y,x+8
  | 1 -> y+4,x
  | 2 -> y+4,x+4
  | 3 -> y+4,x+8
  | 4 -> y+8,x+8
  | 5 -> y+8,x+12
  | _ -> assert false in
 let wrap_cube f y x d =
  if x >= 0 && y >= 0 && x < 4 && y < 4 then f,y,x,d else
  let m = 3 in
  match f,d with (* determination by loop is less error prone *)
  | 0,0 -> 5,m-y,m,2   | 2,0 -> 3,y,0,0   | 4,0 -> 5,y,0,0
  | 0,1 -> 3,0,x,1     | 2,1 -> 4,m-x,0,0 | 4,1 -> 1,m,m-x,3
  | 0,2 -> 2,0,y,1     | 2,2 -> 1,y,m,2   | 4,2 -> 2,m,m-y,3
  | 0,3 -> 1,0,m-x,1   | 2,3 -> 0,x,0,0   | 4,3 -> 3,m,x,3
  | 1,0 -> 2,y,0,0     | 3,0 -> 5,0,m-y,1 | 5,0 -> 0,m-y,m,2
  | 1,1 -> 4,m,m-x,3   | 3,1 -> 4,0,x,1   | 5,1 -> 1,x,0,0
  | 1,2 -> 5,m,m-y,3   | 3,2 -> 2,y,m,2   | 5,2 -> 4,y,m,2
  | 1,3 -> 0,0,m-x,1   | 3,3 -> 0,m,x,3   | 5,3 -> 3,m-x,4,2
  | _ -> assert false
 in
 let try_move () =
  let dx,dy = match !cur_dir with 0 -> (1,0) | 1 -> (0,1) | 2 -> (~-1,0) | 3 -> (0,~-1) | _ -> assert false in
  let f',y',x',d' = wrap_cube !cur_f (!cur_y + dy) (!cur_x + dx) !cur_dir in
  if cube_get f' y' x' = '.' then
  (cur_f := f' ; cur_dir := d' ; cur_y := y'; cur_x := x'
   ; (let y,x = map_yx_of_cube f' y' x' in Printf.printf "%u %u\n" y x)
  )
  else () in
 let next_token r =
  match Seq.uncons r with
  | None -> None
  | Some (c,rs) when c = 'L' || c = 'R' -> Some (Either.right c,rs)
  | Some (c,_) ->
   let n = Seq.take_while (is_digit) r |> String.of_seq |> int_of_string
   and rs = Seq.drop_while (is_digit) r in Some (Either.left n,rs) in
 for i = 0 to pred map_h do
  let x_max = String.length inputs.(i) |> pred
  and x_min = Seq.length (inputs.(i) |> String.to_seq |> Seq.take_while ((=)' ')) in
  x_bounds.(i) <- (x_min,x_max)
 done ;
 let rec run_loop = function
  | None -> ()
  | Some (t,rs) ->
   Either.(match t with
    | Left n -> for i = 1 to n do try_move () done
    | Right d when d = 'R' -> cur_dir := mod_plus (!cur_dir + 1) 4
    | Right d when d = 'L' -> cur_dir := mod_plus (!cur_dir - 1) 4
    | _ -> assert false
   ); run_loop (next_token rs)
 in run_loop (next_token (inputs.(map_h+1) |> String.to_seq));
 let y,x = map_yx_of_cube !cur_f !cur_y !cur_x in
 let x = x - fst x_bounds.(y) in
 1000*(y+1)+4*(x+1)+(!cur_dir)

(* next: navigate by mext action rather than state by state *)
(* if no clay_robots, obs is not an option *)
(* if no obs_robots, geo is not an option *)
(* winning solution for demo is: clay, clay, clay, obs, clay, obs, geo, geo *)
(* not efficient enough *)
let problem_19a () =
 let def_limit = 24 in
 let module B = struct
  type t =
  { o4o : int ; o4c : int ; o4obs : int ; o4g : int
  ; c4obs : int; obs4g : int }
  let make a b c d e f = { o4o = a; o4c = b; o4obs = c; c4obs = d; o4g = e; obs4g = f }
 end in
 let module S = struct
  type t =
  { o : int; c : int; obs: int; g : int
  ; o_r : int; c_r : int; obs_r : int; g_r : int}
  let init = { o = 0; c = 0; obs = 0; g = 0; o_r = 1; c_r = 0; obs_r = 0; g_r = 0}
  let update s =
   {s with
    o = s.o + s.o_r; 
    c = s.c + s.c_r; 
    obs = s.obs + s.obs_r; 
    g = s.g + s.g_r} 
  let can_build_obs s = s.c_r > 0
  let can_build_g s = s.obs_r > 0
  (* fst return value of build functions is number of extra steps required *)
  let build_o (bp : B.t) s = 
   let rec l a s =
    if bp.o4o <= s.o then (let s = update s in a,{s with o = s.o-bp.o4o; o_r = s.o_r + 1}) else
    l (a+1) (update s)
   in l 0 s
  let build_c (bp : B.t) s = 
   let rec l a s =
    if bp.o4c <= s.o then (let s = update s in a,{s with o = s.o-bp.o4c; c_r = s.c_r + 1}) else
    l (a+1) (update s)
   in l 0 s
  let build_obs (bp : B.t) s = 
   assert (can_build_obs s);
   let rec l a s =
    if bp.o4obs <= s.o && bp.c4obs <= s.c then
     (let s = update s in a,{s with o = s.o-bp.o4obs; c = s.c-bp.c4obs; obs_r = s.obs_r + 1}) else
    l (a+1) (update s)
   in l 0 s
  let build_g (bp : B.t) s = 
   assert (can_build_g s);
   let rec l a s =
    if bp.o4g <= s.o && bp.obs4g <= s.obs then
     (let s = update s in a,{s with o = s.o-bp.o4g; obs = s.obs-bp.obs4g; g_r = s.g_r + 1}) else
    l (a+1) (update s)
   in l 0 s
  let print s = Printf.printf "%u %u %u %u | %u %u %u %u\n" s.o s.c s.obs s.g s.o_r s.c_r s.obs_r s.g_r
 end in
 let report_and_pass (a,b) = print_int a; print_newline (); S.print b; b in
 let build_path (bp : B.t) limit input =
  let rec l t i s =
   if i >= String.length input then raise_notrace Exit else
   let op =
    (match input.[i] with 'o' -> S.build_o | 'c' -> S.build_c
                      | 's' -> S.build_obs | 'g' -> S.build_g | _ -> assert false) bp in
   let t',s' = op s in
   if (t+t'+1) < limit then l (t+t'+1) (i+1) s' else
   if (t+t'+1) = limit then s' else begin
    let s' = ref s in
    for x = t+1 to limit do s' := S.update !s' done; !s'
   end
  in l 0 0 (S.init)
 in
 let solutions = Hashtbl.create 1024 in
 let run_dijkstra (bp : B.t) =
  let local_max = ref 0 in
  Hashtbl.reset solutions;
  let dQ = Queue.create () in
  Queue.add "o" dQ ; Queue.add "c" dQ;
  while Queue.is_empty dQ |> not do
   let path = Queue.take dQ in
   try
    let s =  build_path bp def_limit path in
    if s.g > !local_max then (Hashtbl.add solutions path S.(s.g); local_max := (s.g) ; print_endline path)
   with Exit ->
   ( if String.contains path 'g' then Queue.add (path ^ "g") dQ else
     (  Queue.add (path ^ "o") dQ; Queue.add (path ^ "c") dQ;
     if String.contains path 'c' then Queue.add (path ^ "s") dQ;
     if String.contains path 's' then Queue.add (path ^ "g") dQ))
  done in
 let bf_no_il (bp : B.t) =
  let local_max = ref 0 in
  Hashtbl.reset solutions;
  for a = 0 to 12 do for b = 1 to 20 - a do for c = 1 to 20 - a - b do
   let path = String.make a 'o' ^ String.make b 'c' ^ String.make c 's' ^ String.make (def_limit - (a+b+c)) 'g' in
   let s = build_path bp def_limit path in
   if s.g > !local_max then (Hashtbl.add solutions path S.(s.g); local_max := (s.g) ; Printf.printf "%u: %s\n" !local_max path)
  done done done; !local_max in
 let bf_w_il (bp : B.t) =
  let local_max = ref 0 in
  Hashtbl.reset solutions;
  for a = 0 to 12 do for b = 1 to 20 - a do for c = 1 to 20 - a - b do (* non-interleaved *)
  for d = 0 to 1 do for e = 0 to 1 do for f = 0 to 1 do (* interleaved *)
   let path =
     String.make a 'o' ^
     String.sub "cococo" 0 (d*2) ^
     String.make b 'c' ^
     String.sub "scscsc" 0 (e*2) ^
     String.make c 's' ^
     String.sub "gsgsgs" 0 (f*2) ^
     String.make (def_limit - (a+b+c)) 'g' in
   let s = build_path bp def_limit path in
   if s.g > !local_max then (Hashtbl.add solutions path S.(s.g); local_max := (s.g) ; Printf.printf "%u: %s\n" !local_max path)
  done done done done done done; !local_max in
 let inputs = read_lines "prob19.txt" |> List.map begin fun s ->
  Scanf.sscanf s "Blueprint %u: Each ore robot costs %u ore. Each clay robot costs %u ore. Each obsidian robot costs %u ore and %u clay. Each geode robot costs %u ore and %u obsidian." (Fun.const B.make) end |> Array.of_list in
 (*run_dijkstra inputs.(1)*)
 let sols = Array.make (Array.length inputs) 0 in
 for i = 0 to Array.length inputs - 1 do
  sols.(i) <- bf_w_il (inputs.(i))
 done; sols
(*
 for i = 0 to Array.length inputs - 1 do
  sols.(i) <- sols.(i)*(i+1)
 done; Array.fold_left (+) 0 sols
*)

(* bf_w_il *)
(* [|2; 3; 5; 7; 0; 0; 2; 9; 0; 4; 3; 3; 2; 6; 3; 0; 0; 13; 0; 1; 0; 13; 0; 0; 12; 0; 1; 0; 3; 8|] *)
 (*build_path inputs.(1) 24 "oocccccsssssgggo" |> S.print*)
 (* build_path inputs.(0) 24 "cccscsggg" |> S.print;*)
  (* step by step *)
(*
  S.build_c (inputs.(0)) (S.init) |> report_and_pass
  |> S.build_c (inputs.(0))       |> report_and_pass
  |> S.build_c (inputs.(0))       |> report_and_pass
  |> S.build_obs (inputs.(0))     |> report_and_pass
  |> S.build_c (inputs.(0))       |> report_and_pass
  |> S.build_obs (inputs.(0))     |> report_and_pass
  |> S.build_g (inputs.(0))       |> report_and_pass
  |> S.build_g (inputs.(0))       |> report_and_pass
  |> S.update |> S.update |> S.update
  |> S.print
*)

