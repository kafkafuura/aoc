(* Revisiting AOC 2022 *)

let problem_19a () =
 let example = false in
 (* blueprint type *)
 let module B = struct
  type t =
   { o4o : int ; o4c : int ; o4obs : int ; o4g : int
   ; c4obs : int; obs4g : int; max_o : int }
  let make a b c d e f = { o4o = a; o4c = b; o4obs = c; c4obs = d; o4g = e; obs4g = f ; max_o = max a (max b (max c e))}
  let of_string s =
   Scanf.sscanf s
    "Blueprint %u: Each ore robot costs %u ore. Each clay robot costs %u ore. Each obsidian robot costs %u ore and %u clay. Each geode robot costs %u ore and %u obsidian."
    (Fun.const make)
 end in
 (* state type *)
 let module S = struct
  type t = 
   { o : int; c : int; obs: int; g : int
   ; o_r : int; c_r : int; obs_r : int; g_r : int}
  type action = Wait | BuildO | BuildC | BuildOBS | BuildG
  let init = { o = 0; c = 0; obs = 0; g = 0; o_r = 1; c_r = 0; obs_r = 0; g_r = 0}
  let step s =
    { s with
      o   = s.o   + s.o_r
    ; c   = s.c   + s.c_r
    ; obs = s.obs + s.obs_r
    ; g   = s.g   + s.g_r }
  let act (bpt : B.t) s = function
   | Wait     -> step s
   | BuildO   -> let s = step s in {s with o = s.o - bpt.o4o; o_r = s.o_r + 1 }
   | BuildC   -> let s = step s in {s with o = s.o - bpt.o4c; c_r = s.c_r + 1 }
   | BuildOBS -> let s = step s in {s with o = s.o - bpt.o4obs; c = s.c - bpt.c4obs; obs_r = s.obs_r + 1 }
   | BuildG   -> let s = step s in {s with o = s.o - bpt.o4g; obs = s.obs - bpt.obs4g; g_r = s.g_r + 1 }
  let can_act (bpt : B.t) s = function
   | Wait     -> true
   | BuildO   -> bpt.o4o <= s.o && s.o_r < bpt.max_o
   | BuildC   -> bpt.o4c <= s.o && s.c_r < bpt.c4obs
   | BuildOBS -> bpt.o4obs <= s.o && bpt.c4obs <= s.c && s.obs_r < bpt.obs4g
   | BuildG   -> bpt.o4g <= s.o && bpt.obs4g <= s.obs
  let action_list = [BuildO ; BuildC ; BuildOBS ; BuildG; Wait]
  let to_string s = Printf.sprintf "%d %d %d %d | %d %d %d %d" s.o s.c s.obs s.g s.o_r s.c_r s.obs_r s.g_r
 end in
 (* use memoization cache *)
 let time_limit = 24 in
 let cache = Hashtbl.create 8_192_000 in
 let blueprints = In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_lines) |> List.map B.of_string |> Array.of_list in
 let blueprint_score (bpt : B.t) =
  let max_g = ref 0 in
  Hashtbl.clear cache ;
  let simplify_state (s : S.t) t =
   let s =
   { s with
     o_r   = min s.o_r bpt.max_o
   ; c_r   = min s.c_r bpt.c4obs 
   ; obs_r = min s.obs_r bpt.obs4g } in
   { s with
     o     = min s.o   (max (bpt.max_o) (t * bpt.max_o - s.o_r * (t-1)))
   ; c     = min s.c   (max (bpt.c4obs) (t * bpt.c4obs - s.c_r * (t-1)))
   ; obs   = min s.obs (max (bpt.obs4g) (t * bpt.obs4g - s.obs_r * (t-1)))} in
  let rec loop t (s : S.t) =
   if t = 0 then max_g := max s.g !max_g else
   if Hashtbl.mem cache (t,s) then () else (
    Hashtbl.add cache (t,s) true ;
    S.action_list |> List.filter (S.can_act bpt s) |>
    List.iter (fun a -> loop (t-1) (simplify_state (S.act bpt s a) t))
   ) in loop time_limit S.init ;
  !max_g in
 let res = Array.make (Array.length blueprints) 0 in
 for i = 1 to Array.length blueprints do
  res.(i-1) <- blueprint_score blueprints.(i-1);
  Printf.printf "%02d/%02d : %d" i (Array.length blueprints) res.(i-1) ;
  print_newline ()
 done;
 Array.to_seq res |> Seq.zip (Seq.ints 1) |> Seq.fold_left (fun a (i,g) -> i*g+a) 0

(*
(* without optimizations *)
01/30 : 2
02/30 : 3
03/30 : 5
04/30 : 7
05/30 : 0
06/30 : 0
07/30 : 2
08/30 : 9
09/30 : 0
10/30 : 4
11/30 : 3
12/30 : 3
13/30 : 2
14/30 : 6
15/30 : 3
16/30 : 0
17/30 : 0
18/30 : 14
19/30 : 0
20/30 : 1
21/30 : 0
22/30 : 13
23/30 : 0
24/30 : 0
25/30 : 12
26/30 : 0
27/30 : 1
28/30 : 0
29/30 : 3
30/30 : 8
1613 
./aoc_opt  452.58s user 1385.50s system 99% cpu 30:46.15 total
*)

let problem_19b () =
 let example = false in
 (* blueprint type *)
 let module B = struct
  type t =
   { o4o : int ; o4c : int ; o4obs : int ; o4g : int
   ; c4obs : int; obs4g : int; max_o : int }
  let make a b c d e f = { o4o = a; o4c = b; o4obs = c; c4obs = d; o4g = e; obs4g = f ; max_o = max a (max b (max c e))}
  let of_string s =
   Scanf.sscanf s
    "Blueprint %u: Each ore robot costs %u ore. Each clay robot costs %u ore. Each obsidian robot costs %u ore and %u clay. Each geode robot costs %u ore and %u obsidian."
    (Fun.const make)
 end in
 (* state type *)
 let module S = struct
  type t = 
   { o : int; c : int; obs: int; g : int
   ; o_r : int; c_r : int; obs_r : int; g_r : int}
  type action = Wait | BuildO | BuildC | BuildOBS | BuildG
  let init = { o = 0; c = 0; obs = 0; g = 0; o_r = 1; c_r = 0; obs_r = 0; g_r = 0}
  let step s =
    { s with
      o   = s.o   + s.o_r
    ; c   = s.c   + s.c_r
    ; obs = s.obs + s.obs_r
    ; g   = s.g   + s.g_r }
  let act (bpt : B.t) s = function
   | Wait     -> step s
   | BuildO   -> let s = step s in {s with o = s.o - bpt.o4o; o_r = s.o_r + 1 }
   | BuildC   -> let s = step s in {s with o = s.o - bpt.o4c; c_r = s.c_r + 1 }
   | BuildOBS -> let s = step s in {s with o = s.o - bpt.o4obs; c = s.c - bpt.c4obs; obs_r = s.obs_r + 1 }
   | BuildG   -> let s = step s in {s with o = s.o - bpt.o4g; obs = s.obs - bpt.obs4g; g_r = s.g_r + 1 }
  let can_act (bpt : B.t) s = function
   | Wait     -> true
   | BuildO   -> bpt.o4o <= s.o && s.o_r < bpt.max_o
   | BuildC   -> bpt.o4c <= s.o && s.c_r < bpt.c4obs
   | BuildOBS -> bpt.o4obs <= s.o && bpt.c4obs <= s.c && s.obs_r < bpt.obs4g
   | BuildG   -> bpt.o4g <= s.o && bpt.obs4g <= s.obs
  let action_list = [BuildO ; BuildC ; BuildOBS ; BuildG; Wait]
  let to_string s = Printf.sprintf "%d %d %d %d | %d %d %d %d" s.o s.c s.obs s.g s.o_r s.c_r s.obs_r s.g_r
 end in
 (* use memoization cache *)
 let time_limit = 32 in
 let cache = Hashtbl.create 8_192_000 in
 let blueprints = In_channel.(with_open_bin (if example then "19e.txt" else "19.txt") input_lines) |> List.map B.of_string |> Array.of_list in
 let blueprint_score (bpt : B.t) =
  let max_g = ref 0 in
  Hashtbl.clear cache ;
  (* discard overabundant states to overlap maximal states *)
  (* methodology credit: Johnathan Paulson *)
  (* slight miscalculations can give incorrect results *)
  let simplify_state (s : S.t) t =
   let s =
   { s with
     o_r   = min s.o_r bpt.max_o
   ; c_r   = min s.c_r bpt.c4obs 
   ; obs_r = min s.obs_r bpt.obs4g } in
   { s with
     o     = min s.o   (max (bpt.max_o) (t * bpt.max_o - s.o_r * (t-1)))
   ; c     = min s.c   (max (bpt.c4obs) (t * bpt.c4obs - s.c_r * (t-1)))
   ; obs   = min s.obs (max (bpt.obs4g) (t * bpt.obs4g - s.obs_r * (t-1)))} in
  let rec loop t (s : S.t) =
   if t = 0 then max_g := max s.g !max_g else
   (* trying to optimize remaining time (djikstra style) does not seem to work effectively; use (t,s) for key, not s *)
   if Hashtbl.mem cache (t,s) then () else (
    Hashtbl.add cache (t,s) true ;
    S.action_list |> List.filter (S.can_act bpt s) |>
    List.iter (fun a -> loop (t-1) (simplify_state (S.act bpt s a) t))
   ) in loop time_limit S.init ;
  !max_g in
 let res = Array.make 3 0 in
 for i = 1 to 3 do
  res.(i-1) <- blueprint_score blueprints.(i-1);
  Printf.printf "%02d/%02d : %d" i (Array.length blueprints) res.(i-1) ;
  print_newline ()
 done;
 Array.to_seq res |> Seq.fold_left ( * ) 1

(* Unstable Diffusion *)
(* # = elf ; . = ground *)
(* Rules *)
(* If no elves are in an adjacent square, the current elf does nothing. *)
(* Otherwise, the elf plans to move in the first valid direction of the following. *)
(* N, S, W, E, if all diagonals in that direction are empty: e.g., N if NW, N, NE are empty *)
(* if two or more elves try to move to the same position, none move *)
(* the priority cycles by round number; round 1: N, 2: S, 3: W,... *)
(* answer, empty space of rectangle bounded by min/max y and x *)

(* 10 rounds *)
let problem_23a () =
 let example = false in
 let rounds = 10 in
 let debug = false in
 let module YXSet = Set.Make (struct type t = int * int let compare = compare end) in
 let module Dir = struct
  type t = North | South | West | East
  let cycle = [|North;South;West;East|]
  let priority_seq_of_round r = Array.to_seq cycle |> Seq.cycle |> Seq.drop ((r-1) mod 4) |> Seq.take 4
  (* neighbors: CW from (-1,-1) *)
  let valid_of_neighbors dir ns =
   match dir with
   | North -> Seq.take 3 ns |> Seq.exists (Fun.id) |> not
   | South -> Seq.drop 4 ns |> Seq.take 3 |> Seq.exists (Fun.id) |> not
   | East ->  Seq.drop 2 ns |> Seq.take 3 |> Seq.exists (Fun.id) |> not
   | West ->
     let hd = Seq.take 1 ns in
     Seq.drop 6 ns |> Seq.append hd |> Seq.exists (Fun.id) |> not
  let neighbors_of_set (y,x) s =
   [~-1,~-1;~-1,0;~-1,1;0,1;1,1;1,0;1,~-1;0,~-1] |>
   List.map (fun pt -> (fst pt + y, snd pt + x)) |> List.to_seq |>
   Seq.map (fun pt -> YXSet.mem pt s)
  let shift (y,x) = function
   | North -> (y-1,x)
   | South -> (y+1,x)
   | East  -> (y,x+1)
   | West  -> (y,x-1)
 end in
 let empty_space set =
  let ymin = fst @@ YXSet.min_elt set
  and ymax = fst @@ YXSet.max_elt set in
  let xset = YXSet.map (fun (y,x) -> (x,y)) set in
  let xmin = fst @@ YXSet.min_elt xset
  and xmax = fst @@ YXSet.max_elt xset in
  (ymax - ymin + 1) * (xmax - xmin + 1) - YXSet.cardinal set in
 let input = In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_lines) |> Array.of_list in 
 let map = ref YXSet.empty in
 for y = 0 to Array.length input - 1 do
  for x = 0 to String.length input.(y) - 1 do
   if input.(y).[x] = '#' then map := YXSet.add (y,x) !map
  done
 done ;
 let collision_map = Hashtbl.create (YXSet.cardinal !map) in
 let queue_move pt set round =
  let ns = Dir.neighbors_of_set pt set |> Seq.memoize in
  let move =
   if Seq.exists (Fun.id) ns |> not then Seq.empty else
   Dir.priority_seq_of_round round |>
   Seq.filter (fun dir -> Dir.valid_of_neighbors dir ns) |>
   Seq.take 1 in
  match Seq.uncons move with
  | Some (dir,_) ->
    if   Hashtbl.mem collision_map (Dir.shift pt dir)
    then Hashtbl.replace collision_map (Dir.shift pt dir) None
    else Hashtbl.add collision_map (Dir.shift pt dir) (Some pt)
  | None -> () in
 let run_round round =
  Hashtbl.clear collision_map  ;
  let set = !map in
  YXSet.iter (fun pt -> queue_move pt set round) set ;
  Hashtbl.iter
   (fun dst src_wrapped -> match src_wrapped with None -> () | Some src -> map := !map |> YXSet.remove src |> YXSet.add dst)
   collision_map in
 let print_map set =
  let ymin = fst @@ YXSet.min_elt set
  and ymax = fst @@ YXSet.max_elt set in
  let xset = YXSet.map (fun (y,x) -> (x,y)) set in
  let xmin = fst @@ YXSet.min_elt xset
  and xmax = fst @@ YXSet.max_elt xset in
  for y = ymin to ymax do
   for x = xmin to xmax do
    print_char (if YXSet.mem (y,x) set then '#' else '.')
   done ; print_newline ()
  done in
 for i = 1 to rounds do
  if debug then
  (
   print_map !map ;
   Printf.printf "----- %d -----\n" i ;
   Seq.iter Dir.(function North -> print_char 'N' | South -> print_char 'S' | West -> print_char 'W' | East -> print_char 'E') (Dir.priority_seq_of_round i) ;
   print_newline ()
  );
  run_round i
 done ;
 if debug then print_map !map ;
 empty_space !map

(* number of rounds until static *)
(* possible speed up, use Hashtbl for map instead of YXSet *)
(* 5s optimized *)
let problem_23b () =
 let example = false in
 let module YXSet = Set.Make (struct type t = int * int let compare = compare end) in
 let module Dir = struct
  type t = North | South | West | East
  let cycle = [|North;South;West;East|]
  let priority_seq_of_round r = Array.to_seq cycle |> Seq.cycle |> Seq.drop ((r-1) mod 4) |> Seq.take 4
  (* neighbors: CW from (-1,-1) *)
  let valid_of_neighbors dir ns =
   match dir with
   | North -> Seq.take 3 ns |> Seq.exists (Fun.id) |> not
   | South -> Seq.drop 4 ns |> Seq.take 3 |> Seq.exists (Fun.id) |> not
   | East ->  Seq.drop 2 ns |> Seq.take 3 |> Seq.exists (Fun.id) |> not
   | West ->
     let hd = Seq.take 1 ns in
     Seq.drop 6 ns |> Seq.append hd |> Seq.exists (Fun.id) |> not
  let neighbors_of_set (y,x) s =
   [~-1,~-1;~-1,0;~-1,1;0,1;1,1;1,0;1,~-1;0,~-1] |>
   List.map (fun pt -> (fst pt + y, snd pt + x)) |> List.to_seq |>
   Seq.map (fun pt -> YXSet.mem pt s)
  let shift (y,x) = function
   | North -> (y-1,x)
   | South -> (y+1,x)
   | East  -> (y,x+1)
   | West  -> (y,x-1)
 end in
 let input = In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_lines) |> Array.of_list in 
 let map = ref YXSet.empty in
 for y = 0 to Array.length input - 1 do
  for x = 0 to String.length input.(y) - 1 do
   if input.(y).[x] = '#' then map := YXSet.add (y,x) !map
  done
 done ;
 let collision_map = Hashtbl.create (YXSet.cardinal !map) in
 let queue_move pt set round =
  let ns = Dir.neighbors_of_set pt set |> Seq.memoize in
  let move =
   if Seq.exists (Fun.id) ns |> not then Seq.empty else
   Dir.priority_seq_of_round round |>
   Seq.filter (fun dir -> Dir.valid_of_neighbors dir ns) |>
   Seq.take 1 in
  match Seq.uncons move with
  | Some (dir,_) ->
    if   Hashtbl.mem collision_map (Dir.shift pt dir)
    then Hashtbl.replace collision_map (Dir.shift pt dir) None
    else Hashtbl.add collision_map (Dir.shift pt dir) (Some pt)
  | None -> () in
 let run_round round =
  Hashtbl.clear collision_map  ;
  let set = !map in
  YXSet.iter (fun pt -> queue_move pt set round) set ;
  Hashtbl.iter
   (fun dst src_wrapped -> match src_wrapped with None -> () | Some src -> map := !map |> YXSet.remove src |> YXSet.add dst)
   collision_map in

 let i = ref 1 in
 let run = ref true in
 while !run do
  let before = !map in
  run_round !i ;
  if before = !map then run := false else incr i
 done ;
 !i

(* number of rounds until static *)
(* speed up, uses Hashtbl for map instead of YXSet *)
(* 2.7s opt *)
let problem_23b2 () =
 let example = false in
 let module Dir = struct
  type t = North | South | West | East
  let cycle = [|North;South;West;East|]
  let priority_seq_of_round r = Array.to_seq cycle |> Seq.cycle |> Seq.drop ((r-1) mod 4) |> Seq.take 4
  (* neighbors: CW from (-1,-1) *)
  let valid_of_neighbors dir ns =
   match dir with
   | North -> Seq.take 3 ns |> Seq.exists (Fun.id) |> not
   | South -> Seq.drop 4 ns |> Seq.take 3 |> Seq.exists (Fun.id) |> not
   | East ->  Seq.drop 2 ns |> Seq.take 3 |> Seq.exists (Fun.id) |> not
   | West ->
     let hd = Seq.take 1 ns in
     Seq.drop 6 ns |> Seq.append hd |> Seq.exists (Fun.id) |> not
  let neighbors_of_set (y,x) s =
   [~-1,~-1;~-1,0;~-1,1;0,1;1,1;1,0;1,~-1;0,~-1] |>
   List.map (fun pt -> (fst pt + y, snd pt + x)) |> List.to_seq |>
   Seq.map (fun pt -> Hashtbl.mem s pt) 
  let shift (y,x) = function
   | North -> (y-1,x)
   | South -> (y+1,x)
   | East  -> (y,x+1)
   | West  -> (y,x-1)
 end in
 let input = In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_lines) |> Array.of_list in 
 let initials = ref [] in
 for y = 0 to Array.length input - 1 do
  for x = 0 to String.length input.(y) - 1 do
   if input.(y).[x] = '#' then initials := ((y,x),())::!initials
  done
 done ;
 let map = Hashtbl.of_seq (List.to_seq !initials) in
 let collision_map = Hashtbl.create (Hashtbl.length map) in
 let queue_move pt set round =
  let ns = Dir.neighbors_of_set pt set |> Seq.memoize in
  let move =
   if Seq.exists (Fun.id) ns |> not then Seq.empty else
   Dir.priority_seq_of_round round |>
   Seq.filter (fun dir -> Dir.valid_of_neighbors dir ns) |>
   Seq.take 1 in
  match Seq.uncons move with
  | Some (dir,_) ->
    if   Hashtbl.mem collision_map (Dir.shift pt dir)
    then Hashtbl.replace collision_map (Dir.shift pt dir) None
    else Hashtbl.add collision_map (Dir.shift pt dir) (Some pt)
  | None -> () in
 let run_round round =
  Hashtbl.clear collision_map  ;
  Hashtbl.iter (fun pt _ -> queue_move pt map round) map;
  Hashtbl.iter
   (fun dst src_wrapped -> match src_wrapped with None -> () | Some src -> Hashtbl.remove map src ; Hashtbl.add map dst ())
   collision_map in

 let i = ref 1 in
 let run = ref true in
 while !run do
  let before = Hashtbl.copy map in
  run_round !i ;
  if before = map then run := false else incr i
 done ;
 !i

let problem_24a () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "24e.txt" else "24.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length input.(0) in
 let ww = w - 2 in
 let wh = h - 2 in
 let westerlies = Array.init (wh) (fun _ -> Bytes.make (ww) '.') in
 let easterlies = Array.init (wh) (fun _ -> Bytes.make (ww) '.') in
 let northerlies = Array.init (ww) (fun _ -> Bytes.make (wh) '.') in
 let southerlies = Array.init (ww) (fun _ -> Bytes.make (wh) '.') in
 let pmod a b = if a mod b < 0 then a mod b + b else a mod b in
 let w_at y x round =
  Bytes.get westerlies.(y-1) (pmod (x-1-round) ww) in
 let e_at y x round =
  Bytes.get easterlies.(y-1) (pmod (x-1+round) ww) in
 let n_at y x round =
  Bytes.get northerlies.(x-1) (pmod (y-1-round) wh) in
 let s_at y x round =
  Bytes.get southerlies.(x-1) (pmod (y-1+round) wh) in
 (* initialize winds *)
 for y = 1 to h - 2 do
  for x = 1 to w - 2 do
   match input.(y).[x] with
   | '>' -> Bytes.set westerlies.(y-1) (x-1) '>'
   | '<' -> Bytes.set easterlies.(y-1) (x-1) '<'
   | 'v' -> Bytes.set northerlies.(x-1) (y-1) 'v'
   | '^' -> Bytes.set southerlies.(x-1) (y-1) '^'
   | _ -> ()
  done
 done;
 let exception Break of int in
 let start = (0,1) in
 let dest = (h-1,w-2) in
 let valid (y,x,r) =
  (*Printf.printf "(%d,%d,%d)\n" y x r;*)
  if (y,x) = start then true else
  if (y,x) = dest then raise_notrace (Break r) else
  if y <= 0 || y >= h - 1 || x <= 0 || x >= w - 1 then false else
  w_at y x r = '.' && e_at y x r = '.' && n_at y x r = '.'  && s_at y x r = '.' in
 let valid_points (y,x,r) = 
 (* going into the blizzard is actually a valid move, fml *)
(*
  let headwind_free = 
  (* NXSWE *)
   [y <= 0 || y >= h - 1 || x <= 0 || x >= w - 1 || n_at y x r = '.'
   ;true
   ;y <= 0 || y >= h - 1 || x <= 0 || x >= w - 1 || s_at y x r = '.'
   ;y <= 0 || y >= h - 1 || x <= 0 || x >= w - 1 || w_at y x r = '.'
   ;y <= 0 || y >= h - 1 || x <= 0 || x >= w - 1 || e_at y x r = '.'] |> List.to_seq in
*)
  [~-1,0;0,0;1,0;0,~-1;0,1] |> List.to_seq |>
  Seq.map (fun (dy,dx) -> (y+dy,x+dx,r)) |>
(*
  Seq.zip headwind_free |> Seq.filter_map (fun (a,b) -> if a then Some b else None) |>
*)
  Seq.filter valid in
(*
 let print_winds r =
  for y = 1 to h - 2 do
   for x = 1 to w - 2 do
    if n_at y x r = '.' && s_at y x r = '.' && w_at y x r = '.' && e_at y x r = '.' then print_char '.' else
    if n_at y x r <> '.' && s_at y x r = '.' && w_at y x r = '.' && e_at y x r = '.' then print_char 'v' else
    if n_at y x r = '.' && s_at y x r <> '.' && w_at y x r = '.' && e_at y x r = '.' then print_char '^' else
    if n_at y x r = '.' && s_at y x r = '.' && w_at y x r <> '.' && e_at y x r = '.' then print_char '>' else
    if n_at y x r = '.' && s_at y x r = '.' && w_at y x r = '.' && e_at y x r <> '.' then print_char '<' else
    print_char '*'
  done; print_newline () ; done in
*)
 let move_queue = Queue.create () in
 Queue.add (0,1,0) move_queue ;

 (*find the periodicity of the map: max 600 (LCD of 25 120)*)
 (* periodicity is 600 *)
(*
 let exception Break0 in
 try
  for r = 1 to 600 do
   try 
    for y = 1 to h-2 do
     for x = 1 to w-2 do
      if n_at y x 0 <> n_at y x r then raise_notrace Break0;
      if s_at y x 0 <> s_at y x r then raise_notrace Break0;
      if w_at y x 0 <> w_at y x r then raise_notrace Break0;
      if e_at y x 0 <> e_at y x r then raise_notrace Break0;
     done
    done ;
    raise_notrace (Break r)
   with Break0 -> ()
  done ;
  0
 with Break n -> n
*)

 let visit_cache = Array.make (600*120*25) false in
 let visited y x r = visit_cache.((r mod 600)*120*25 + (y-1)*120 + x - 1) in
 let set_visited y x r = visit_cache.((r mod 600)*120*25 + (y-1)*120 + x - 1) <- true in
 let result =
 try
  while not @@ Queue.is_empty move_queue do
   let (y,x,r) = Queue.take move_queue in
   if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 && visited y x r then () else
   (if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 then set_visited y x r ;
   Queue.add_seq move_queue (valid_points (y,x,r+1)))
  done ;
  0
 with Break n -> n in
 result

let problem_24b () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "24e.txt" else "24.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length input.(0) in
 let ww = w - 2 in
 let wh = h - 2 in
 let westerlies = Array.init (wh) (fun _ -> Bytes.make (ww) '.') in
 let easterlies = Array.init (wh) (fun _ -> Bytes.make (ww) '.') in
 let northerlies = Array.init (ww) (fun _ -> Bytes.make (wh) '.') in
 let southerlies = Array.init (ww) (fun _ -> Bytes.make (wh) '.') in
 let pmod a b = if a mod b < 0 then a mod b + b else a mod b in
 let w_at y x round =
  Bytes.get westerlies.(y-1) (pmod (x-1-round) ww) in
 let e_at y x round =
  Bytes.get easterlies.(y-1) (pmod (x-1+round) ww) in
 let n_at y x round =
  Bytes.get northerlies.(x-1) (pmod (y-1-round) wh) in
 let s_at y x round =
  Bytes.get southerlies.(x-1) (pmod (y-1+round) wh) in
 (* initialize winds *)
 for y = 1 to h - 2 do
  for x = 1 to w - 2 do
   match input.(y).[x] with
   | '>' -> Bytes.set westerlies.(y-1) (x-1) '>'
   | '<' -> Bytes.set easterlies.(y-1) (x-1) '<'
   | 'v' -> Bytes.set northerlies.(x-1) (y-1) 'v'
   | '^' -> Bytes.set southerlies.(x-1) (y-1) '^'
   | _ -> ()
  done
 done;
 let exception Break of int in
 let start = ref (0,1) in
 let dest = ref (h-1,w-2) in
 let valid (y,x,r) =
  if (y,x) = !start then true else
  if (y,x) = !dest then raise_notrace (Break r) else
  if y <= 0 || y >= h - 1 || x <= 0 || x >= w - 1 then false else
  w_at y x r = '.' && e_at y x r = '.' && n_at y x r = '.'  && s_at y x r = '.' in
 let valid_points (y,x,r) = 
  [~-1,0;0,0;1,0;0,~-1;0,1] |> List.to_seq |>
  Seq.map (fun (dy,dx) -> (y+dy,x+dx,r)) |>
  Seq.filter valid in

 let move_queue = Queue.create () in
 Queue.add (0,1,0) move_queue ;

 (*find the periodicity of the map: max 600 (LCD of 25 120)*)
 (* periodicity of the example is LCD of 4 and 6 which is 12 *)
 (* periodicity is 600 via test *)
(*
 let exception Break0 in
 try
  for r = 1 to 600 do
   try 
    for y = 1 to h-2 do
     for x = 1 to w-2 do
      if n_at y x 0 <> n_at y x r then raise_notrace Break0;
      if s_at y x 0 <> s_at y x r then raise_notrace Break0;
      if w_at y x 0 <> w_at y x r then raise_notrace Break0;
      if e_at y x 0 <> e_at y x r then raise_notrace Break0;
     done
    done ;
    raise_notrace (Break r)
   with Break0 -> ()
  done ;
  0
 with Break n -> n
*)

 let visit_cache = Array.make (600*wh*ww) false in
 let visited y x r = visit_cache.((r mod 600)*wh*ww + (y-1)*ww + x - 1) in
 let set_visited y x r = visit_cache.((r mod 600)*wh*ww + (y-1)*ww+ x - 1) <- true in
 let clear_visited () = Array.fill visit_cache 0 (600*wh*ww) false in
 let result1 =
 try
  while not @@ Queue.is_empty move_queue do
   let (y,x,r) = Queue.take move_queue in
   if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 && visited y x r then () else
   (if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 then set_visited y x r ;
   Queue.add_seq move_queue (valid_points (y,x,r+1)))
  done ;
  0
 with Break n -> n in
 print_int result1 ;
 print_newline ();
 Queue.clear move_queue ;
 clear_visited ();
 start := (h-1,w-2) ;
 dest := (0,1) ;
 Queue.add (h-1,w-2,result1) move_queue ;
 let result2 =
 try
  while not @@ Queue.is_empty move_queue do
   let (y,x,r) = Queue.take move_queue in
   if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 && visited y x r then () else
   (if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 then set_visited y x r ;
   Queue.add_seq move_queue (valid_points (y,x,r+1)))
  done ;
  0
 with Break n -> n in
 print_int result2 ;
 print_newline ();
 Queue.clear move_queue ;
 clear_visited ();
 start := (0,1) ;
 dest := (h-1,w-2);
 Queue.add (0,1,result2) move_queue ;
 let result3 =
 try
  while not @@ Queue.is_empty move_queue do
   let (y,x,r) = Queue.take move_queue in
   if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 && visited y x r then () else
   (if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 then set_visited y x r ;
   Queue.add_seq move_queue (valid_points (y,x,r+1)))
  done ;
  0
 with Break n -> n in
 print_int result3 ;
 print_newline ();
 result3

(* base 5 numbers: 2 → -2 *)
let problem_25a () =
 let example = false in
 let snafu_of_int n =
  let rec snafu_of_int' acc n =
   if n = 0 then acc |> List.to_seq |> String.of_seq else
   match n mod 5 with
   | -2 | 3 -> snafu_of_int' ('='::acc) ((n+2)/5)
   | -1 | 4 -> snafu_of_int' ('-'::acc) ((n+1)/5)
   | 0 -> snafu_of_int' ('0'::acc) (n/5)
   | 1 -> snafu_of_int' ('1'::acc) ((n-1)/5)
   | 2 -> snafu_of_int' ('2'::acc) ((n-2)/5)
   | _ -> assert false in
  snafu_of_int' [] n in
 let rec npow a b exp = if exp < 0 then 0 else if exp = 0 then a else npow (a*b) b (exp-1) in
 let int_of_snafu s =
  String.fold_right
   (fun c (a,e) ->
    match c with
    | '2' -> (a + (npow 2 5 e), e+1)
    | '1' -> (a + (npow 1 5 e), e+1)
    | '0' -> (a, e+1)
    | '-' -> (a + (npow ~-1 5 e), e+1)
    | '=' -> (a + (npow ~-2 5 e), e+1)
    | _ -> assert false)
   s (0,0) |> fst in
 (*snafu_of_int 4890*)
 (*int_of_snafu "2=-1=0"*)
 In_channel.(with_open_bin (if example then "25e.txt" else "25.txt") input_lines) |>
 List.map int_of_snafu |> List.fold_left ( + ) 0 |>
 snafu_of_int
