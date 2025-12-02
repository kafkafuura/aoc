(* AOC 2018 Time Slip *)
(* Uses OCaml 5.4 *)

let problem_01a () =
 In_channel.(with_open_bin "01.txt" input_lines) |> List.map int_of_string |> List.fold_left (+) 0

(* simpler *)
let problem_01b () =
 let module ISet = Set.Make(Int) in
 let rec loop seen acc seq =
 match seq () with
 | Seq.Nil -> assert false
 | Seq.Cons (el, tl) when ISet.mem acc seen -> (acc, ISet.cardinal seen)
 | Seq.Cons (el, tl) -> loop (ISet.add acc seen) (acc+el) tl in
 In_channel.(with_open_bin "01.txt" input_lines) |>
 List.map int_of_string |> Array.of_list |>
 Array.to_seq |> Seq.cycle |>
 loop ISet.empty 0

(* better, faster, but more caveats *)
let problem_01b2 () =
 let partials = 
  In_channel.(with_open_bin "01.txt" input_lines) |>
  List.map int_of_string |> List.to_seq |>
  Seq.scan (+) 0 |> Array.of_seq in
 let len = Array.length partials in
 let lowest_x = ref Int.max_int in
 let lowest_n = ref Int.max_int in
 let res = ref Int.max_int in
 (* last one wraps around to 0 on next row, so exclude len - 1 *)
 for y = 0 to len - 2 do for x = 0 to len - 2 do
  if x <> y &&
     (* ensure n will be a non-negative integer *)
     (partials.(y) >= partials.(x)) = (partials.(len-1) >= 0) &&
     (partials.(y) - partials.(x)) mod partials.(len-1) = 0
  then (
    let n = ((partials.(y) - partials.(x)) / partials.(len-1)) in
    (* special case for n = 0, pick only the 2nd position for x *)
    let x = if n = 0 then max x y else x in
    if n < !lowest_n || (n = !lowest_n && x < !lowest_x) then (
     lowest_x := x ;
     lowest_n := n ;
     res := partials.(y)))
 done done ;
 !res

(* count which have 2 of any letter and then exactly 3 of any letter *)
let problem_02a () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "02e.txt" else "02.txt") input_lines) in
 let letters = Array.make 26 0 in
 let score s =
  Array.fill letters 0 26 0 ;
  String.iter (fun c -> let idx = Char.code c - 0x61 (* 'a' *) in letters.(idx) <- letters.(idx) + 1) s ;
  Array.fold_left (fun (a1, a2) n -> (a1 || (n = 2), a2 || (n = 3))) (false, false) letters in
 let a1, a2 =
  input |>
  List.map score |>
  List.fold_left (fun (a1,a2) (t1,t2) -> (if t1 then succ a1 else a1), (if t2 then succ a2 else a2)) (0,0) in
 a1*a2

let problem_02b () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "02e.txt" else "02.txt") input_lines) |> Array.of_list in
 let slen = String.length input.(0) in
 let valid s1 s2 =
  let rec valid i a =
   if i >= slen then a else
   if s1.[i] <> s2.[i]
   then
    (if Option.is_some a then None else valid (i+1) (Some i))
   else valid (i+1) a in
  valid 0 None in
 (* essentially a 2d for loop, but with early exit *)
 let rec loop idx1 idx2 =
  if idx1 >= Array.length input then None else
  if idx2 >= Array.length input then loop (succ idx1) 0 else
  if idx1 = idx2 then loop idx1 (succ idx2) else (
   match valid input.(idx1) input.(idx2) with
   | None -> loop idx1 (succ idx2)
   | Some n ->
     let buf = Buffer.create 26 in
     String.iteri (fun i c -> if i <> n then Buffer.add_char buf c else ()) input.(idx1) ;
     Some (Buffer.contents buf)
  ) in
 loop 0 0 |>
 Option.get

let problem_03a () =
 let example = false in
 let module Rect = struct
  type t = {x : int; y : int; w : int; h : int}
  let of_string s = Scanf.sscanf s " %u,%u: %ux%u " (fun x y w h -> {x;y;w;h})
  let to_string r = Printf.sprintf "%u,%u: %ux%u" r.x r.y r.w r.h
 end in
 let map = Array.make_matrix 1000 1000 0 in
 let add_rect map (r : Rect.t) =
  for y = r.y to r.y + r.h - 1 do
   for x = r.x to r.x + r.w - 1 do
    map.(y).(x) <- map.(y).(x) + 1
   done
  done in
 let input =
  In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
  List.map (fun s ->
   let idx = String.index s '@' in
   Rect.of_string (String.sub s (idx + 1) (String.length s - idx - 1))) |>
  Array.of_list in
 Array.iter (add_rect map) input ;
 let res = ref 0 in
 for y = 0 to 1000 - 1 do
  for x = 0 to 1000 - 1 do
   if map.(y).(x) > 1 then incr res else ()
  done
 done ;
 !res

let problem_03b () =
 let example = false in
 let module Rect = struct
  type t = {x : int; y : int; w : int; h : int}
  let disjoint r1 r2 =
   r1.x > (r2.x + r2.w - 1) ||
   r2.x > (r1.x + r1.w - 1) ||
   r1.y > (r2.y + r2.h - 1) ||
   r2.y > (r1.y + r1.h - 1)
  let of_string s = Scanf.sscanf s " %u,%u: %ux%u " (fun x y w h -> {x;y;w;h})
  let to_string r = Printf.sprintf "%u,%u: %ux%u" r.x r.y r.w r.h
 end in
 let input =
  In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
  List.map (fun s ->
   let idx = String.index s '@' in
   Rect.of_string (String.sub s (idx + 1) (String.length s - idx - 1))) |>
  Array.of_list in
 let rec loop i j =
  if i > Array.length input - 1 then 0 (* not found *) else
  if j > Array.length input - 1 then i+1 else
  if i = j || Rect.disjoint input.(i) input.(j) then loop i (j+1) else
  loop (i+1) 0 in
 loop 0 0

(* more flexible, complex version *)
(* low priority todo: defragmentation? *)
let problem_03a2 () =
 let example = false in
 let debug = false in
 let module Rect = struct
  type t = {x : int; y : int; w : int; h : int}
  let is_disjoint r1 r2 =
   r1.x > (r2.x + r2.w - 1) ||
   r2.x > (r1.x + r1.w - 1) ||
   r1.y > (r2.y + r2.h - 1) ||
   r2.y > (r1.y + r1.h - 1)
  let intersect r1 r2 =
   if is_disjoint r1 r2
   then None
   else Some
    { x = max r1.x r2.x
    ; y = max r1.y r2.y
    ; w = min (r1.x+r1.w) (r2.x+r2.w) - max r1.x r2.x
    ; h = min (r1.y+r1.h) (r2.y+r2.h) - max r1.y r2.y }
  (* result is a list of disjoint rectangles equivalent to r2 - r1 *)
  let diff r1 r2 =
   match intersect r1 r2 with
   | None -> [r2]
   | Some r3 when r2 = r3 -> [] (* r2 is fully within r1 *)
   | Some r3 -> (* x_scan style ; treat as if r1 is fully within r2, remove 0 dim (w/h) fragments *)
     (* left, top, bottom, right *)
     [{x = r2.x; w = r3.x - r2.x; y = r2.y; h = r2.h}
     ;{x = r3.x; w = r3.w; y = r2.y; h = r3.y - r2.y}
     ;{x = r3.x; w = r3.w; y = r3.y + r3.h; h = r2.y + r2.h - r3.y - r3.h}
     ;{x = r3.x + r3.w; w = r2.x + r2.w - r3.x - r3.w; y = r2.y; h = r2.h}] |>
     List.filter (fun r -> r.w > 0 && r.h > 0)
  let area r = r.w * r.h
  let of_string s = Scanf.sscanf s " %u,%u: %ux%u " (fun x y w h -> {x;y;w;h})
  let to_string r = Printf.sprintf "%u,%u: %ux%u" r.x r.y r.w r.h
 end in
 let rectset_add rectset r =
  let inset = rectset |> Hashtbl.to_seq_keys |> List.of_seq in
  let rec add_disjoint outset inset r =
   match inset with
   | [] -> Hashtbl.replace outset r ()
   | hd::tl when Rect.is_disjoint hd r ->
     add_disjoint outset tl r
   | hd::tl ->
     Rect.diff hd r |>
     List.iter (fun r -> add_disjoint outset tl r) in
  add_disjoint rectset inset r in
 let input =
  In_channel.(with_open_bin (if example then "03e.txt" else "03.txt") input_lines) |>
  List.map (fun s ->
   let idx = String.index s '@' in
   Rect.of_string (String.sub s (idx + 1) (String.length s - idx - 1))) |>
  Array.of_list in
 let intersections input =
  let sections = Dynarray.create () in
  for i = 0 to Array.length input - 2 do
   for j = i+1 to Array.length input - 1 do
    match Rect.intersect input.(i) input.(j) with
    | None -> ()
    | Some r -> Dynarray.add_last sections r
   done
  done ;
  sections in
 let rectset = Hashtbl.create 4096 in
 let sections = intersections input in

 Dynarray.iter (rectset_add rectset) sections ;

 if debug then
  Hashtbl.to_seq_keys rectset |>
  Seq.iter (fun r -> r |> Rect.to_string |> print_endline) ;

 if debug then (
  print_string "Raw Intersections: " ; print_int @@ Dynarray.length sections; print_newline ();
  print_string "Disjoint Sections: " ; print_int @@ Hashtbl.length rectset; print_newline ()) ;

 Hashtbl.to_seq_keys rectset |>
 Seq.fold_left (fun a r -> Rect.area r + a ) 0

(* note: no wraparound issues exist; all guards are awake at the beginning and end of their shifts *)
(* use assoc list dicts because hashtbls are probably overkill *)
let problem_04a () =
 let example = false in
 let guards = ref [] in
 let times = ref [] in
 (* [1518-11-03 00:05] Guard #10 begins shift *)
 let increment_date (m,d) =
  match (m, d+1) with
  | (2,29) -> (3,1) (* 1518 is not a leap year *)
  | (12,32) -> (1,1)
  | (m,32) -> (m+1,1)
  | (m,31) when List.mem m [4;6;9;11] -> (m+1,1)
  | (m,d) -> (m,d) in
 let register s =
  match s.[19] with
  | 'G' -> (* Guard info *)
    let date_m = String.sub s 6 2 |> int_of_string in
    let date_d = String.sub s 9 2 |> int_of_string in
    let time_h = String.sub s 12 2 |> int_of_string in
    let date_m, date_d = (if time_h = 0 then Fun.id else increment_date) (date_m, date_d) in
    let g_id = String.sub s 26 (String.index_from s 26 ' ' - 26) |> int_of_string in
    (match List.assoc_opt g_id !guards with
     | Some days ->
       guards := (g_id, (date_m,date_d)::days)::(List.remove_assoc g_id !guards)
     | None -> guards := (g_id,[(date_m,date_d)])::!guards)
  | c when c = 'f' || c = 'w' -> (* falls asleep / wakes up *)
    let date_m = String.sub s 6 2 |> int_of_string in
    let date_d = String.sub s 9 2 |> int_of_string in
    let time_m = String.sub s 15 2 |> int_of_string in
    let asleep = c = 'f' in
    (match List.assoc_opt (date_m, date_d) !times with
     | Some info ->
       times := ((date_m,date_d),(time_m,asleep)::info)::(List.remove_assoc (date_m, date_d) !times)
     | None -> times := ((date_m,date_d),[time_m,asleep])::!times)
  | _ -> raise @@ Invalid_argument "Improper Input Line" in
 let rec total_asleep acc = function
 | (t1,true)::(t2,false)::tl -> total_asleep (acc + t2 - t1) tl
 | (t1,true)::(_,true)::tl -> total_asleep acc ((t1,true)::tl)
 | (_,false)::tl -> total_asleep acc tl
 | _ -> acc in
 In_channel.(with_open_bin (if example then "04e.txt" else "04.txt") input_lines) |>
 List.iter register ;
 let times =
  List.map
   (fun ((m,d),ts) ->
    (m,d),
    (List.sort (fun (t1,_) (t2,_) -> compare t1 t2) ts))
   !times in
 let counts =
  List.map
   (fun ((m,d),ts) ->
    (m,d),(total_asleep 0 ts))
  times in
 let guards = !guards in
 let max_guard =
  guards |>
  List.map
   (fun (g_id, days) ->
    let sum =
     days |>
     List.filter_map (fun key -> List.assoc_opt key counts) |>
     List.fold_left (+) 0 in
    g_id, sum ) |>
  List.fold_left
   (fun (g_id', cnt' as acc) (g_id, cnt as el) -> if cnt > cnt' then el else acc)
   (-1,-1) |>
  fst in
 let max_guard_times =
  guards |>
  List.assoc max_guard |>
  List.filter_map (fun key -> List.assoc_opt key times) in
 let time_acc = Array.make 60 0 in
 (
  (* inspected input is proper: if it weren't, you can add (0,false;...;60,false) to everything *)
  let rec layer_asleep = function
  | (t1,true)::(t2,false)::tl ->
    for i = t1 to t2 - 1 do time_acc.(i) <- time_acc.(i) + 1 done ; layer_asleep tl
  | (t1,true)::(_,true)::tl -> assert false 
  | (_,false)::tl -> assert false
  | _ -> () in
  List.concat max_guard_times |>
  layer_asleep
 ) ;
 let max_n, max_time, _ = 
  Array.fold_left
  (fun (max_n,max_i,i) n -> if n > max_n then (n,i,i+1) else (max_n,max_i,i+1))
  (-1,-1,0) time_acc in
 (*max_guard, max_time, max_n*)
 max_guard * max_time

(* note: no wraparound issues exist; all guards are awake at the beginning and end of their shifts *)
let problem_04b () =
 let example = false in
 let guards = ref [] in
 let times = ref [] in
 (* [1518-11-03 00:05] Guard #10 begins shift *)
 let increment_date (m,d) =
  match (m, d+1) with
  | (2,29) -> (3,1) (* 1518 is not a leap year *)
  | (12,32) -> (1,1)
  | (m,32) -> (m+1,1)
  | (m,31) when List.mem m [4;6;9;11] -> (m+1,1)
  | (m,d) -> (m,d) in
 let register s =
  match s.[19] with
  | 'G' -> (* Guard info *)
    let date_m = String.sub s 6 2 |> int_of_string in
    let date_d = String.sub s 9 2 |> int_of_string in
    let time_h = String.sub s 12 2 |> int_of_string in
    let date_m, date_d = (if time_h = 0 then Fun.id else increment_date) (date_m, date_d) in
    let g_id = String.sub s 26 (String.index_from s 26 ' ' - 26) |> int_of_string in
    (match List.assoc_opt g_id !guards with
     | Some days ->
       guards := (g_id, (date_m,date_d)::days)::(List.remove_assoc g_id !guards)
     | None -> guards := (g_id,[(date_m,date_d)])::!guards)
  | c when c = 'f' || c = 'w' -> (* falls asleep / wakes up *)
    let date_m = String.sub s 6 2 |> int_of_string in
    let date_d = String.sub s 9 2 |> int_of_string in
    let time_m = String.sub s 15 2 |> int_of_string in
    let asleep = c = 'f' in
    (match List.assoc_opt (date_m, date_d) !times with
     | Some info ->
       times := ((date_m,date_d),(time_m,asleep)::info)::(List.remove_assoc (date_m, date_d) !times)
     | None -> times := ((date_m,date_d),[time_m,asleep])::!times)
  | _ -> raise @@ Invalid_argument "Improper Input Line" in
 In_channel.(with_open_bin (if example then "04e.txt" else "04.txt") input_lines) |>
 List.iter register ;
 let times =
  List.map
   (fun ((m,d),ts) ->
    (m,d),
    (List.sort (fun (t1,_) (t2,_) -> compare t1 t2) ts))
   !times in
 let guards = !guards in
 let time_acc = Array.make_matrix (List.length guards) 60 0 in
 (
  let rec layer_asleep idx = function
  | (t1,true)::(t2,false)::tl ->
    for i = t1 to t2 - 1 do time_acc.(idx).(i) <- time_acc.(idx).(i) + 1 done ; layer_asleep idx tl
  | (t1,true)::(_,true)::tl -> assert false 
  | (_,false)::tl -> assert false
  | _ -> () in
  guards |>
  List.iteri
   (fun i (_, dates) ->
    dates |>
    List.filter_map (fun key -> List.assoc_opt key times) |>
    List.concat |>
    layer_asleep i)
 ) ;
 let max_n, max_time, max_guard =
  let max_n = ref (-1) in
  let max_idx = ref (-1) in
  let max_t = ref (-1) in
  for idx = 0 to Array.length time_acc - 1 do
   for t = 0 to 59 do
    if time_acc.(idx).(t) > !max_n
    then (
     max_n := time_acc.(idx).(t);
     max_idx := idx;
     max_t := t)
    else ()
   done
  done ;
  !max_n, !max_t, (List.nth guards !max_idx |> fst) in
 (*max_guard, max_time, max_n*)
 max_guard * max_time

(* possible optimization: calculate len during loop instead of after *)
let problem_05a () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "05e.txt" else "05.txt") input_line) |> Option.get in
 String.to_seq input |>
 Seq.fold_left
  (fun a c ->
   match a with
   | hd::tl when abs (Char.code hd - Char.code c) = 32 -> tl
   | a -> c::a ) [] |>
 List.length

let problem_05b () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "05e.txt" else "05.txt") input_line) |> Option.get in
 let compress_with_removal n =
  String.to_seq input |>
  Seq.fold_left
   (fun a c ->
    if Char.code c - 0x41 = n ||
       Char.code c - 0x61 = n
    then a
    else
    (match a with
    | hd::tl when abs (Char.code hd - Char.code c) = 32 -> tl
    | a -> c::a )) [] |>
  List.length in
 Seq.ints 0 |> Seq.take 26 |>
 Seq.map compress_with_removal |>
 Seq.fold_left min Int.max_int

let problem_06a () =
 let debug = false in
 let example = false in
 let markers = 
  In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |>
  List.map
  (fun s ->
   let mid = String.index s ',' in
   let x = int_of_string @@ String.sub s 0 mid in
   let y = int_of_string @@ String.sub s (mid+2) (String.length s - mid - 2) in
   (y,x)) in
 let min_dim, max_dim =
  List.fold_left
   (fun (a_min,a_max) (y,x) -> (a_min |> min y |> min x), (a_max |> max y |> max x))
   (Int.max_int, -1)
   markers in
 let markers =
  List.map
   (fun (y,x) -> y - min_dim + 1, x - min_dim + 1)
   markers in
 let max_dim = max_dim - min_dim + 3 in
 let graph = Array.make_matrix max_dim max_dim None in
 let module ISet = Set.Make(Int) in
 let unbounded = ref ISet.empty in
 let q = Queue.create () in
 List.to_seq markers |>
 Seq.mapi (fun i (y,x) -> (i,y,x,0)) |>
 Queue.add_seq q ;
 while not (Queue.is_empty q) do
  let (id,y,x,depth) = Queue.take q in
  if y < 0 || y >= max_dim || x < 0 || x >= max_dim then unbounded := ISet.add id !unbounded else
  (
   match graph.(y).(x) with
   | Some (i,d) when d < depth -> ()
   | Some (i,d) when i = id -> ()
   | Some (i,d) when d = depth ->
     graph.(y).(x) <- Some (-1,depth) ;
     Queue.add (-1,y-1,x,depth+1) q;
     Queue.add (-1,y+1,x,depth+1) q;
     Queue.add (-1,y,x-1,depth+1) q;
     Queue.add (-1,y,x+1,depth+1) q
   | Some (i,d) -> assert false (* if this happens, we have a logic error *)
   | None ->
     graph.(y).(x) <- Some (id, depth) ;
     Queue.add (id,y-1,x,depth+1) q;
     Queue.add (id,y+1,x,depth+1) q;
     Queue.add (id,y,x-1,depth+1) q;
     Queue.add (id,y,x+1,depth+1) q
  )
 done ;
 let unbounded = !unbounded in
 let counts = Array.make (List.length markers) 0 in
 for y = 0 to max_dim - 1 do
  for x = 0 to max_dim - 1 do
   match graph.(y).(x) with
   | Some (id,_) when not (id < 0 || ISet.mem id unbounded) -> counts.(id) <- counts.(id) + 1
   | _ -> ()
  done
 done ;
 let draw_graph () =
  for y = 0 to max_dim - 1 do
   for x = 0 to max_dim - 1 do
    match graph.(y).(x) with
    | Some (id, d) when d > 0 -> print_char (Char.chr (id + 0x61))
    | Some (id, d) -> print_char (Char.chr (id + 0x41))
    | _ -> print_char '_'
   done ;
   print_newline ()
  done in
 if debug then draw_graph () else () ;
 Array.fold_left max 0 counts

(* there is probably a cleverer way to do this, but this works *)
let problem_06b () =
 let example = false in
 let cutoff = if example then 32 else 10000 in
 let markers = 
  In_channel.(with_open_bin (if example then "06e.txt" else "06.txt") input_lines) |>
  List.map
  (fun s ->
   let mid = String.index s ',' in
   let x = int_of_string @@ String.sub s 0 mid in
   let y = int_of_string @@ String.sub s (mid+2) (String.length s - mid - 2) in
   (y,x)) |> Array.of_list in
 (* start at approximate center of mass *)
 let cy, cx =
  let len = Array.length markers in
  let sy, sx = 
  Array.fold_left (fun (ay,ax) (y,x) -> (ay+y,ax+x)) (0,0) markers in
  sy / len, sx / len in
 let dsum (y,x) = Array.fold_left (fun a (y',x') -> a + abs (y-y') + abs (x-x')) 0 markers in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let seen = ref YXSet.empty in
 let q = Queue.create () in
 Queue.add (cy,cx) q ;
 while not (Queue.is_empty q) do
  let y, x = Queue.take q in
  if YXSet.mem (y,x) !seen ||
     dsum (y,x) >= cutoff
  then ()
  else
  (seen := YXSet.add (y,x) !seen ;
   Queue.add (y-1,x) q ;
   Queue.add (y+1,x) q ;
   Queue.add (y,x-1) q ;
   Queue.add (y,x+1) q)
 done ;
 YXSet.cardinal !seen

(* topological sorting! *)
(* Step C must be finished before step A can begin. *)
let problem_07a () =
 let example = false in
 let module ISet = Set.Make(Int) in
 let nodes = Array.make 26 None in
 In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_lines) |>
 List.iter (fun s ->
  let edge = Char.code s.[5] - 0x41 in
  let node = Char.code s.[36] - 0x41 in
  (match nodes.(edge) with None -> nodes.(edge) <- Some ISet.empty | _ -> ()) ;
  nodes.(node) <-
  (match nodes.(node) with
   | None -> Some (ISet.singleton edge)
   | Some deps -> Some (ISet.add edge deps))) ;
 let sorted = Dynarray.create () in
 let empty = ref ISet.empty in
 Array.iteri (fun i n -> match n with Some deps when ISet.is_empty deps -> empty := ISet.add i !empty | _ -> ()) nodes;
 while not (ISet.is_empty !empty) do
  let n = ISet.min_elt !empty in
  empty := ISet.remove n !empty ;
  nodes.(n) <- None ;
  Dynarray.add_last sorted n ;
  for i = 0 to Array.length nodes - 1 do
   match nodes.(i) with
   | None -> ()
   | Some deps ->
     let deps' = ISet.remove n deps in
     if deps' = ISet.empty then empty := ISet.add i !empty ;
     nodes.(i) <- Some deps'
  done
 done ;
 Dynarray.to_seq sorted |>
 Seq.map (fun n -> Char.chr (n + 0x41)) |>
 String.of_seq

let problem_07b () =
 let example = false in
 let increment = if example then 1 else 61 in
 let max_workers = if example then 2 else 5 in
 let module ISet = Set.Make(Int) in
 let nodes = Array.make 26 None in
 In_channel.(with_open_bin (if example then "07e.txt" else "07.txt") input_lines) |>
 List.iter (fun s ->
  let edge = Char.code s.[5] - 0x41 in
  let node = Char.code s.[36] - 0x41 in
  (match nodes.(edge) with None -> nodes.(edge) <- Some ISet.empty | _ -> ()) ;
  nodes.(node) <-
  (match nodes.(node) with
   | None -> Some (ISet.singleton edge)
   | Some deps -> Some (ISet.add edge deps))) ;
 let sorted = Dynarray.create () in
 let empty = ref ISet.empty in
 Array.iteri (fun i n -> match n with Some deps when ISet.is_empty deps -> empty := ISet.add i !empty | _ -> ()) nodes;
 let t = ref 0 in
 let workers = ref [] in
 while not (ISet.is_empty !empty && !workers = []) do
  if not (ISet.is_empty !empty) && List.length !workers < max_workers
  then (
   let n = ISet.min_elt !empty in
   empty := ISet.remove n !empty ;
   nodes.(n) <- None ;
   workers := (n,n+increment)::!workers)
  else (
   let t' = List.fold_left (fun a (id,t') -> min a t') 100 !workers in
   workers :=
    List.filter_map
    (fun (id, wait) -> if wait - t' = 0
     then (
      Dynarray.add_last sorted id ;
      for i = 0 to Array.length nodes - 1 do
       match nodes.(i) with
       | None -> ()
       | Some deps ->
         let deps' = ISet.remove id deps in
         if deps' = ISet.empty then empty := ISet.add i !empty ;
         nodes.(i) <- Some deps'
      done ;
      None)
     else Some (id, wait - t')) !workers ;
    t := !t + t')
 done ;
 !t,
 Dynarray.to_seq sorted |>
 Seq.map (fun n -> Char.chr (n + 0x41)) |>
 String.of_seq

let problem_08a () =
 let example = false in
 let module Tree = struct
  type t =
   | Leaf of int array
   | Stem of t array * int array
  let rec parse_from idx arr =
   let node_len = arr.(idx) in
   let meta_len = arr.(idx+1) in
   if node_len = 0
   then
    (idx+2+meta_len, Leaf (Array.sub arr (idx+2) meta_len))
   else (
    let q = Queue.create () in
    let cur = ref (idx+2) in
    for i = 1 to node_len do
     let cur', node = parse_from !cur arr in
     cur := cur' ; Queue.add node q
    done ;
    (!cur+meta_len, Stem (Queue.to_seq q |> Array.of_seq, Array.sub arr !cur meta_len))
   )
  let sum_meta tree =
   let rec sum_meta acc = function
   | Leaf meta -> Array.fold_left (+) acc meta
   | Stem (nodes, meta) -> Array.fold_left sum_meta (Array.fold_left (+) acc meta) nodes
   in sum_meta 0 tree
 end in
 let data =
  In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_line) |> Option.get |>
  String.split_on_char ' ' |> List.map int_of_string |> Array.of_list in
 let tree = Tree.parse_from 0 data |> snd in
 Tree.sum_meta tree

let problem_08b () =
 let example = false in
 let module Tree = struct
  type t =
   | Leaf of int array
   | Stem of t array * int array
  let rec parse_from idx arr =
   let node_len = arr.(idx) in
   let meta_len = arr.(idx+1) in
   if node_len = 0
   then
    (idx+2+meta_len, Leaf (Array.sub arr (idx+2) meta_len))
   else (
    let q = Queue.create () in
    let cur = ref (idx+2) in
    for i = 1 to node_len do
     let cur', node = parse_from !cur arr in
     cur := cur' ; Queue.add node q
    done ;
    (!cur+meta_len, Stem (Queue.to_seq q |> Array.of_seq, Array.sub arr !cur meta_len))
   )
  let sum_meta tree =
   let rec sum_meta acc = function
   | Leaf meta -> Array.fold_left (+) acc meta
   | Stem (nodes, meta) -> Array.fold_left sum_meta (Array.fold_left (+) acc meta) nodes
   in sum_meta 0 tree
  let rec sum_score = function
  | Leaf meta -> Array.fold_left (+) 0 meta
  | Stem (nodes, meta) ->
    (* minor memoization *)
    let seen = ref [] in
    let len = Array.length nodes in
    meta |>
    Array.to_seq |>
    Seq.map
     (fun n ->
      if n < 1 || n > len
      then 0
      else
       (match List.assoc_opt n !seen with
        | Some sum -> sum
        | None ->
          let sum = sum_score nodes.(n-1) in
          seen := (n,sum)::!seen ;
          sum)) |>
     Seq.fold_left (+) 0
 end in
 let data =
  In_channel.(with_open_bin (if example then "08e.txt" else "08.txt") input_line) |> Option.get |>
  String.split_on_char ' ' |> List.map int_of_string |> Array.of_list in
 let tree = Tree.parse_from 0 data |> snd in
 Tree.sum_score tree

(* cyclically doubly linked list *)
let problem_09a () =

 let input = In_channel.(with_open_bin "09.txt" input_line) |> Option.get in
 let players, stop = Scanf.sscanf input "%d players; last marble is worth %d points" (fun a b -> (a,b)) in

(*
 let players = 430 in
 let stop = 7158800 in
*)

 let (.%()) = Dynarray.get in
 let (.%()<-) = Dynarray.set in
 let ring = Dynarray.create () in
 Dynarray.add_last ring (0,0) ;

 let scores = Array.make players 0 in

 let rec addr from n =
  if n = 0 then from else
  if n > 0 then addr (snd ring.%(from)) (n-1) else
                addr (fst ring.%(from)) (n+1) in

 (* does not destroy removed marbles, this is by design *)
 let remove idx =
  let l = addr idx (-1)
  and r = addr idx 1 in
  let (ll,_) = ring.%(l) in
  let (_,rr) = ring.%(r) in
  ring.%(l) <- (ll,r) ;
  ring.%(r) <- (l,rr) in

 let insert l el =
  let r = addr l 1 in
  let (ll,_) = ring.%(l) in
  ring.%(l) <- (ll,el) ;
  let (_,rr) = ring.%(r) in
  ring.%(r) <- (el,rr) ;
  ring.%(el) <- (l,r) in

 let rec run p cur =
  let m = Dynarray.length ring in (* new marble *)
  Dynarray.add_last ring (-1,-1) ;
  if m > stop then () else
  if m mod 23 = 0 then (
   let to_remove = addr cur (-7) in
   remove to_remove ;
   scores.(p) <- to_remove + m + scores.(p) ;
   (* if to_remove + m = stop then () else *) (* early stop based on different interpretation of instructions *)
   run ((p + 1) mod players) (addr to_remove 1)
  ) else (
   insert (addr cur 1) m ;
   run ((p + 1) mod players) m
  ) in

 run 0 0 ;
 Array.fold_left (max) 0 scores

(* optimize by using an Array instead of Dynarray and using packed tuples! *)
let problem_09b () =

 let input = In_channel.(with_open_bin "09.txt" input_line) |> Option.get in
 let players, stop = Scanf.sscanf input "%d players; last marble is worth %d points" (fun a b -> (a,b)) in
 let stop = stop * 100 in

 let module Tup = struct
  let r tup = tup land 0x7FFFFFFF
  let l tup = tup lsr 32
  let set_l tup n = ((r n) lsl 32) lor (r tup)
  let set_r tup n = tup lxor (r tup) lor (r n)
  let make (left, right) = set_l right left
 end in

 let ring = Array.make (stop+1) (-1) in
 ring.(0) <- 0 ;
 let len = ref 1 in

 let scores = Array.make players 0 in

 let rec addr from n =
  if n = 0 then from else
  if n > 0 then addr (Tup.r ring.(from)) (n-1) else
                addr (Tup.l ring.(from)) (n+1) in

 (* does not destroy removed marbles, this is by design *)
 let remove idx =
  let l = addr idx (-1) and r = addr idx 1 in
  ring.(l) <- Tup.set_r ring.(l) r ;
  ring.(r) <- Tup.set_l ring.(r) l in

 let insert l el =
  let r = addr l 1 in
  ring.(l) <- Tup.set_r ring.(l) el ;
  ring.(r) <- Tup.set_l ring.(r) el ;
  ring.(el) <- Tup.make (l,r) in

 let rec run p cur =
  let m = !len in incr len ;
  if m > stop then () else
  if m mod 23 = 0 then (
   let to_remove = addr cur (-7) in
   remove to_remove ;
   scores.(p) <- to_remove + m + scores.(p) ;
   run ((p + 1) mod players) (addr to_remove 1)
  ) else (
   insert (addr cur 1) m ;
   run ((p + 1) mod players) m
  ) in

 run 0 0 ;
 Array.fold_left (max) 0 scores

(* text height is 8 in the example, 10 in the full problem *)
(* solves both part a and part b *)
let problem_10a () =
 let debug = true in
 let module Star = struct
  type t = {x : int; y: int; dx: int; dy: int}
  let step n star =
   {star with x = star.x + star.dx * n; y = star.y + star.dy * n}
  let of_string s =
   Scanf.sscanf s "position=< %d, %d > velocity=< %d, %d >" (fun x y dx dy -> {x; y; dx; dy})
 end in

 let stars = In_channel.(with_open_bin "10.txt" input_lines) |> List.map Star.of_string in

 let print stars =
  let min_x, min_y, max_x, max_y =
   List.fold_left
    Star.(fun (nx,ny,mx,my) s -> (min nx s.x, min ny s.y, max mx s.x, max my s.y))
    (Int.max_int, Int.max_int, Int.min_int, Int.min_int)
    stars in
  let buf = Array.make_matrix (max_y - min_y + 1) (max_x - min_x + 1) ' ' in
  List.iter Star.(fun s -> buf.(s.y-min_y).(s.x-min_x) <- '#') stars ;
  for y = 0 to max_y - min_y do
   for x = 0 to max_x - min_x do
    print_char buf.(y).(x)
   done ;
   print_newline ()
  done;
  if debug then (
   print_string "y:" ;
   print_int min_y; print_string ".." ;
   print_int max_y; print_string " x:" ;
   print_int min_x; print_string ".." ;
   print_int max_x; print_newline ()
  ) else () in

 (* perform a statistical systems of equations on intersections of y for an initial guess, adjust if necessary *)
 let guess0 =
 (
  let min_dy, max_dy =
   List.fold_left
    Star.(fun (ndy,mdy) s -> (min ndy s.dy, max mdy s.dy))
    (Int.max_int, Int.min_int)
    stars in
   let sums   = Array.make (max_dy-min_dy+1) 0 in
   let counts = Array.make (max_dy-min_dy+1) 0 in
   List.iter
    Star.(fun s -> sums.(s.dy-min_dy) <- sums.(s.dy-min_dy) + s.y; counts.(s.dy-min_dy) <- counts.(s.dy-min_dy) + 1)
    stars ;
   for i = 0 to Array.length sums - 1 do
    if counts.(i) <> 0 then sums.(i) <- (sums.(i) + (counts.(i) lsr 1)) / counts.(i) else ()
   done ;
   let diffs = ref [] in
   for i = 0 to Array.length sums - 2 do
    if sums.(i) <> 0 && sums.(i+1) <> 0 then diffs := (sums.(i) - sums.(i+1)) :: !diffs
   done ;
  (* int division accounting for rounding *)
  ((List.fold_left (+) 0 !diffs) + (List.length !diffs lsr 1)) / (List.length !diffs)
 ) in
 if debug then (print_int guess0 ; print_newline ()) else ();

 let guess = guess0 (* 10946 *) in
 let stars = List.map (Star.step guess) stars in
 print stars

(* 300x300 grid, [1-300] inclusive, top-left = 1,1 *)
(* choose the 3x3 square with the largest total power *)
(* rack ID = X+10 *)
(* power = (rack ID * Y + serial) * rack ID => take hundreds digit - 5 *)
(* identify square using top-left fuel cell *)

let problem_11a () =
 let serial = In_channel.(with_open_bin "11.txt" input_line) |> Option.get |> int_of_string (* 7857 *) in
 let size = 300 in
 let grid = Array.make_matrix size size 0 in
 for y = 1 to size do
  for x = 1 to size  do
   let n = ((x + 10) * y + serial) * (x + 10) in
   grid.(y-1).(x-1) <- (n / 100) mod 10 - 5
  done
 done ;
 let max_power = ref ((-5) * 9) in
 let max_pos = ref (0,0) in
 let get_power y x =
  grid.(y-1).(x-1) +
  grid.(y-1).(x) +
  grid.(y-1).(x+1) +
  grid.(y).(x-1) +
  grid.(y).(x) +
  grid.(y).(x+1) +
  grid.(y+1).(x-1) +
  grid.(y+1).(x) +
  grid.(y+1).(x+1) in
 for y = 1 to size - 2 do
  for x = 1 to size - 2 do
   let p = get_power y x in
   if p > !max_power then (max_power := p ; max_pos := (y,x)) else ()
  done
 done ;
 let y, x = !max_pos in
 (x,y, !max_power)

let problem_11b () =
 let serial = In_channel.(with_open_bin "11.txt" input_line) |> Option.get |> int_of_string (* 7857 *) in
 let gsize = 300 in
 let grid = Array.make_matrix gsize gsize 0 in
 for y = 1 to gsize do
  for x = 1 to gsize  do
   let n = ((x + 10) * y + serial) * (x + 10) in
   grid.(y-1).(x-1) <- (n / 100) mod 10 - 5
  done
 done ;

 (* partial sums *)
 let y_partials = Array.make_matrix gsize gsize 0 in
 let x_partials = Array.make_matrix gsize gsize 0 in

 (* scan rows *)
 for y = 1 to 300 do
  x_partials.(y-1).(0) <- grid.(y-1).(0) ;
  for x = 2 to 300 do
   x_partials.(y-1).(x-1) <- x_partials.(y-1).(x-2) + grid.(y-1).(x-1)
  done
 done ;

 (* scan columns *)
 for x = 1 to 300 do
  y_partials.(0).(x-1) <- grid.(0).(x-1) ;
  for y = 2 to 300 do
   y_partials.(y-1).(x-1) <- y_partials.(y-2).(x-1) + grid.(y-1).(x-1)
  done
 done ;

 (* memoization == partial squares *)
 let memo = Array.init 300 (fun s -> if s = 0 then grid else Array.make_matrix (gsize-s) (gsize-s) (Int.min_int)) in
 let rec get_power y x size =
  if memo.(size-1).(y-1).(x-1) > Int.min_int then memo.(size-1).(y-1).(x-1) else
  let res = 
   get_power y x (size-1) +
   y_partials.(y+size-3).(x+size-2) - (if y = 1 then 0 else y_partials.(y-2).(x+size-2)) +
   x_partials.(y+size-2).(x+size-3) - (if x = 1 then 0 else x_partials.(y+size-2).(x-2)) +
   grid.(y+size-2).(x+size-2) in
  (memo.(size-1).(y-1).(x-1) <- res ; res) in

 let max_power = ref (Int.min_int) in
 let max_pos = ref (0,0,0) in
 (* instead of gize, statistically, 20-30 will work for a max *)
 for s = 1 to gsize do 
  for y = 1 to gsize - s + 1 do
   for x = 1 to gsize - s + 1 do
    let p = get_power y x s in
    if p > !max_power then (max_power := p ; max_pos := (y,x,s)) else ()
   done
  done
 done ;
 let y, x, size = !max_pos in
 (x,y,size, !max_power)

(* game of life-like simulation, with map extending infinitely in each direction 1D *)
(* add up all indexes which contain a plant *)
let problem_12a () =
 let example = false in
 let mappings = Array.make 32 false in
 let state = Array.init 2 (fun _ -> Hashtbl.create 1024) in
 let input = In_channel.(with_open_bin (if example then "12e.txt" else "12.txt") input_lines) in
 (* initial state: #..#.#..##......###...### *)
 (
  match input with
  | s::_::ms ->
   (* load initial state *)
   let s = List.hd input in
   for i = 15 (* skip "initial state: " *) to String.length s - 1 do
    if s.[i] = '#' then Hashtbl.replace state.(0) (i-15) () else ()
   done ;
   (* read mappings *)
   List.iter 
   (fun s ->
    if s.[9] = '#' then
    mappings.(String.fold_left (fun a c -> (a lsl 1) + (if c = '#' then 1 else 0)) 0 (String.sub s 0 5)) <- true
    else ()) ms
  | _ -> () (* invalid input *)
 ) ;
 let get_next state i =
  let idx =
   [ Hashtbl.mem state (i-2)
   ; Hashtbl.mem state (i-1)
   ; Hashtbl.mem state (i)
   ; Hashtbl.mem state (i+1)
   ; Hashtbl.mem state (i+2) ] |>
   List.fold_left (fun a x -> (a lsl 1) + (if x then 1 else 0)) 0 in
  mappings.(idx) in

 Hashtbl.to_seq_keys state.(0) |>
 Seq.fold_left (+) 0 |> print_int ; print_newline () ;
 for i = 1 to 20 do
  let idx_from = (i land 1) lxor 1 in
  let idx_to   = (i land 1) in
  Hashtbl.to_seq_keys state.(idx_from) |>
  Seq.iter (fun k -> [k-2,();k-1,();k,();k+1,();k+2,()] |> List.to_seq |> Hashtbl.replace_seq state.(idx_to)) ;
  Hashtbl.to_seq_keys state.(idx_to) |>
  Seq.iter (fun k -> if get_next state.(idx_from) k then Hashtbl.replace state.(idx_to) k () else Hashtbl.remove state.(idx_to) k)
 done ;

 Hashtbl.to_seq_keys state.(0) |>
 Seq.fold_left (+) 0
 
(* this automaton achieves a forward moving steady state @ ~ step 200 *)
let problem_12b () =
 let debug = true in
 let example = false in
 let module IntSet = Set.Make(Int) in
 let mappings = Array.make 32 false in
 let state = Array.init 2 (fun _ -> Hashtbl.create 1024) in
 let input = In_channel.(with_open_bin (if example then "12e.txt" else "12.txt") input_lines) in
 (* initial state: #..#.#..##......###...### *)
 (
  match input with
  | s::_::ms ->
   (* load initial state *)
   let s = List.hd input in
   for i = 15 (* skip "initial state: " *) to String.length s - 1 do
    if s.[i] = '#' then Hashtbl.replace state.(0) (i-15) () else ()
   done ;
   (* read mappings *)
   List.iter 
   (fun s ->
    if s.[9] = '#' then
    mappings.(String.fold_left (fun a c -> (a lsl 1) + (if c = '#' then 1 else 0)) 0 (String.sub s 0 5)) <- true
    else ()) ms
  | _ -> () (* invalid input *)
 ) ;
 let get_next state i =
  let idx =
   [ Hashtbl.mem state (i-2)
   ; Hashtbl.mem state (i-1)
   ; Hashtbl.mem state (i)
   ; Hashtbl.mem state (i+1)
   ; Hashtbl.mem state (i+2) ] |>
   List.fold_left (fun a x -> (a lsl 1) + (if x then 1 else 0)) 0 in
  mappings.(idx) in

 let rec loop i =
  let idx_from = (i land 1) lxor 1 in
  let idx_to   = (i land 1) in
  Hashtbl.clear state.(idx_to) ;
  Hashtbl.to_seq_keys state.(idx_from) |>
  Seq.iter (fun k -> [k-2,();k-1,();k,();k+1,();k+2,()] |> List.to_seq |> Hashtbl.replace_seq state.(idx_to)) ;
  Hashtbl.to_seq_keys state.(idx_to) |>
  Seq.iter (fun k -> if get_next state.(idx_from) k then Hashtbl.replace state.(idx_to) k () else Hashtbl.remove state.(idx_to) k) ;
(*
  (* used in loop to print current state to visually confirm *)
  for x = 0 to 200 do
   if Hashtbl.mem state.(idx_to) x then print_char '#' else print_char '.' 
  done; print_newline () ; loop (i+1)
*)
  let ks0 = Hashtbl.to_seq_keys state.(idx_from) |> Seq.map ((+)1) |> IntSet.of_seq in
  let ks1 = Hashtbl.to_seq_keys state.(idx_to) |> IntSet.of_seq in
  if IntSet.equal ks0 ks1 || i > 1000 then (i, ks1) else loop (i+1) in

 let i, ks = loop 1 in
 if debug then (
  Printf.printf "steady state found @ %d->%d!\n" (i-1) i ;
  print_int (IntSet.min_elt ks) ; print_char ' ' ;
  for x = IntSet.min_elt ks to IntSet.max_elt ks do
   if IntSet.mem x ks then print_char '#' else print_char '.'
  done ; print_char ' ' ; print_int (IntSet.max_elt ks) ; print_newline ()
 ) ;
 let cardinal = IntSet.cardinal ks in
 let sum = (IntSet.fold (+) ks 0) in
 if debug then Printf.printf "cardinal: %d, sum @ %d = %d\n" cardinal i sum ;
 cardinal * (50_000_000_000 - i) + sum

(* using Sets only *)
let problem_12b2 () =
 let debug = true in
 let example = false in
 let module IntSet = Set.Make(Int) in
 let mappings = Array.make 32 false in
 let state = Array.make 2 IntSet.empty in
 let input = In_channel.(with_open_bin (if example then "12e.txt" else "12.txt") input_lines) in
 (* initial state: #..#.#..##......###...### *)
 (
  match input with
  | s::_::ms ->
   (* load initial state *)
   let s = List.hd input in
   for i = 15 (* skip "initial state: " *) to String.length s - 1 do
    if s.[i] = '#' then state.(0) <- IntSet.add (i-15) state.(0) else ()
   done ;
   (* read mappings *)
   List.iter 
   (fun s ->
    if s.[9] = '#' then
    mappings.(String.fold_left (fun a c -> (a lsl 1) + (if c = '#' then 1 else 0)) 0 (String.sub s 0 5)) <- true
    else ()) ms
  | _ -> () (* invalid input *)
 ) ;
 let get_next state i =
  let idx =
   Seq.ints (i-2) |> Seq.take 5 |>
   Seq.map (fun i -> IntSet.mem i state) |>
   Seq.fold_left (fun a x -> (a lsl 1) + (if x then 1 else 0)) 0 in
  mappings.(idx) in

 let rec loop i =
  let idx_from = (i land 1) lxor 1 in
  let idx_to   = (i land 1) in
  (* alternative test scoping, more efficient in practice *)
   Seq.ints (IntSet.min_elt state.(idx_from) - 2) |>
   Seq.take (IntSet.max_elt state.(idx_from) - IntSet.min_elt state.(idx_from) + 4) |>
   Seq.filter (get_next state.(idx_from)) |>
   (fun s -> state.(idx_to) <- IntSet.add_seq s IntSet.empty) ;
  let ks0 = IntSet.map ((+)1) state.(idx_from) in
  if IntSet.equal ks0 state.(idx_to) || i > 1000 (* failsafe *) then (i, state.(idx_to)) else loop (i+1) in

 let i, ks = loop 1 in
 if debug then (
  Printf.printf "steady state found @ %d->%d!\n" (i-1) i ;
  print_int (IntSet.min_elt ks) ; print_char ' ' ;
  for x = IntSet.min_elt ks to IntSet.max_elt ks do
   if IntSet.mem x ks then print_char '#' else print_char '.'
  done ; print_char ' ' ; print_int (IntSet.max_elt ks) ; print_newline ()
 ) ;
 let cardinal = IntSet.cardinal ks in
 let sum = (IntSet.fold (+) ks 0) in
 if debug then Printf.printf "cardinal: %d, sum @ %d = %d\n" cardinal i sum ;
 cardinal * (50_000_000_000 - i) + sum

(* cart simulation *)
(* [L,^,R] loop (each card holds state containing next turn direction at an intersection *)
(* turns occur on arrival ; carts move at one space per tick/turn; ONLY ONE CART MOVES AT ANY GIVEN TIME! *)
(* cart turn order depends on Y, then X coordinates *)
(* answer is location of first crash *)

let problem_13a () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "13e.txt" else "13.txt") input_lines) in
 let module Cart = struct
  type dir = North | South | East | West
  type state = Left | Straight | Right
  type t = {y: int; x: int; d : dir; s: state}
  let cw = function North -> East | East -> South | South -> West | West -> North
  let ccw = function North -> West | West -> South | South -> East | East -> North
  let next = function Left -> Straight | Straight -> Right | Right -> Left
  let turn cart =
   match cart.s with
   | Left ->  {cart with d = ccw cart.d; s = next cart.s}
   | Straight -> {cart with s = next cart.s }
   | Right -> {cart with d = cw cart.d; s = next cart.s}
  let move cart =
   match cart.d with
   | North -> {cart with y = cart.y - 1}
   | South -> {cart with y = cart.y + 1}
   | East  -> {cart with x = cart.x + 1}
   | West  -> {cart with x = cart.x - 1}
  let compare c1 c2 = if c1.y <> c2.y then compare c1.y c2.y else compare c1.x c2.x
 end in
 let module CartSet = Set.Make(Cart) in
 let carts =
  List.fold_left (fun (a, y) s ->
   (String.fold_left (fun (a, x) c ->
    match c with
    | '^' -> (Seq.cons Cart.{y;x;d = North;s = Left} a, x+1)
    | 'v' -> (Seq.cons Cart.{y;x;d = South;s = Left} a, x+1)
    | '>' -> (Seq.cons Cart.{y;x;d = East ;s = Left} a, x+1)
    | '<' -> (Seq.cons Cart.{y;x;d = West ;s = Left} a, x+1)
    | _ -> (a,x+1)) (Seq.empty, 0) s |> fst |> Seq.append a), y+1)
   (Seq.empty, 0) input |> fst |> CartSet.of_seq in
 let map =
  input |>
  List.map (fun line -> String.map (function '^' | 'v' -> '|' | '<' | '>' -> '-' | c -> c) line) |>
  Array.of_list in
 let collisions = Queue.create () in
 let rec step carts =
  let carts_seq = CartSet.to_seq carts in
  let carts =
   Seq.fold_left
    Cart.(fun set cart -> 
     let set = CartSet.remove cart set in
     let cart = move cart in
     let cart =
     (match map.(cart.y).[cart.x] with
     | '+' -> turn cart
     | '/' when cart.d = North || cart.d = South -> Cart.{cart with d = cw cart.d}
     | '/' -> Cart.{cart with d = ccw cart.d}
     | '\\' when cart.d = North || cart.d = South -> Cart.{cart with d = ccw cart.d}
     | '\\' -> Cart.{cart with d = cw cart.d}
     | _ -> cart) in
     if CartSet.mem cart set then (Queue.add cart collisions; set) else CartSet.add cart set) carts carts_seq in
   if Queue.is_empty collisions then step carts else Queue.take collisions in
 let crash = step carts in
 (crash.x, crash.y)

let problem_13b () =
 let example = false in
 let input = In_channel.(with_open_bin (if example then "13e.txt" else "13.txt") input_lines) in
 let module Cart = struct
  type dir = North | South | East | West
  type state = Left | Straight | Right
  type t = {y: int; x: int; d : dir; s: state}
  let cw = function North -> East | East -> South | South -> West | West -> North
  let ccw = function North -> West | West -> South | South -> East | East -> North
  let next = function Left -> Straight | Straight -> Right | Right -> Left
  let turn cart =
   match cart.s with
   | Left ->  {cart with d = ccw cart.d; s = next cart.s}
   | Straight -> {cart with s = next cart.s }
   | Right -> {cart with d = cw cart.d; s = next cart.s}
  let move cart =
   match cart.d with
   | North -> {cart with y = cart.y - 1}
   | South -> {cart with y = cart.y + 1}
   | East  -> {cart with x = cart.x + 1}
   | West  -> {cart with x = cart.x - 1}
  let compare c1 c2 = if c1.y <> c2.y then compare c1.y c2.y else compare c1.x c2.x
 end in
 let module CartSet = Set.Make(Cart) in
 let carts =
  List.fold_left (fun (a, y) s ->
   (String.fold_left (fun (a, x) c ->
    match c with
    | '^' -> (Seq.cons Cart.{y;x;d = North;s = Left} a, x+1)
    | 'v' -> (Seq.cons Cart.{y;x;d = South;s = Left} a, x+1)
    | '>' -> (Seq.cons Cart.{y;x;d = East ;s = Left} a, x+1)
    | '<' -> (Seq.cons Cart.{y;x;d = West ;s = Left} a, x+1)
    | _ -> (a,x+1)) (Seq.empty, 0) s |> fst |> Seq.append a), y+1)
   (Seq.empty, 0) input |> fst |> CartSet.of_seq in
 let map =
  input |>
  List.map (fun line -> String.map (function '^' | 'v' -> '|' | '<' | '>' -> '-' | c -> c) line) |>
  Array.of_list in
 let rec step carts =
  let carts_seq = CartSet.to_seq carts in
  let carts =
   Seq.fold_left
    Cart.(fun set cart -> 
     (* skip if already destroyed *)
     if not (CartSet.mem cart set) then set else
     let set = CartSet.remove cart set in
     let cart = move cart in
     let cart =
     (match map.(cart.y).[cart.x] with
     | '+' -> turn cart
     | '/'  when cart.d = North || cart.d = South -> {cart with d = cw cart.d}
     | '/' ->  {cart with d = ccw cart.d}
     | '\\' when cart.d = North || cart.d = South -> {cart with d = ccw cart.d}
     | '\\' -> {cart with d = cw cart.d}
     | _ -> cart) in
     if CartSet.mem cart set then CartSet.remove cart set else CartSet.add cart set) carts carts_seq in
   if CartSet.cardinal carts > 1 then step carts else CartSet.min_elt carts in
 let survivor = step carts in
 (survivor.x, survivor.y)

(* recipes *)
let problem_14a () =
 let example = false in
 let (.%()) = Dynarray.get in
 let input = In_channel.(with_open_bin (if example then "14e.txt" else "14.txt") input_line) |> Option.get |> int_of_string in
 let recipes = Dynarray.create () in
 (* initial state *)
 Dynarray.add_last recipes 3 ;
 Dynarray.add_last recipes 7 ;
 (* loop *)
 let rec step i j =
  if Dynarray.length recipes >= input + 10 then ()
  else (
   let next = recipes.%(i) + recipes.%(j) in
   if next > 9 then (Dynarray.add_last recipes 1 ; Dynarray.add_last recipes (next - 10)) else Dynarray.add_last recipes next ;
   let len = Dynarray.length recipes in
   step ((i + recipes.%(i) + 1) mod len) ((j + recipes.%(j) + 1) mod len)
  ) in
 step 0 1 ;
 for i = input to input + 9 do
  print_int recipes.%(i) ;
 done ;
 print_newline ()

(* recipes *)
let problem_14b () =
 let example = false in
 let (.%()) = Dynarray.get in
 let input = In_channel.(with_open_bin (if example then "14e.txt" else "14.txt") input_line) |> Option.get in
 let sig_len = String.length input in
 let signature = Array.init (sig_len) (fun i -> Char.code input.[i] - 0x30) in
 let sig_seq = Array.to_seq signature in
 let recipes = Dynarray.create () in
 (* initial state *)
 Dynarray.add_last recipes 3 ;
 Dynarray.add_last recipes 7 ;
 (* loop *)
 let rec step i j =
  let next = recipes.%(i) + recipes.%(j) in
  (* determine if you need to check two separate sequences *)
  if next > 9 then (Dynarray.add_last recipes 1 ; Dynarray.add_last recipes (next - 10)) else Dynarray.add_last recipes next ;
  let len = Dynarray.length recipes in
  (* check pos - 1 if adding two numbers *)
  if next > 9 && Seq.equal (=) sig_seq (Seq.ints (len - sig_len - 1) |> Seq.take sig_len |> Seq.drop_while ((>)0) |> Seq.map (Dynarray.get recipes)) then len - sig_len - 1 else
  if Seq.equal (=) sig_seq (Seq.ints (len - sig_len) |> Seq.take sig_len |> Seq.drop_while ((>)0) |> Seq.map (Dynarray.get recipes)) then len - sig_len else
  step ((i + recipes.%(i) + 1) mod len) ((j + recipes.%(j) + 1) mod len) in
 step 0 1

(* recipes *)
(* marginally faster *)
let problem_14b2 () =
 let example = false in
 let (.%()) = Dynarray.get in
 let input = In_channel.(with_open_bin (if example then "14e.txt" else "14.txt") input_line) |> Option.get in
 let sig_len = String.length input in
 let signature = Array.init (sig_len) (fun i -> Char.code input.[i] - 0x30) in
 let sig_seq = Array.to_seq signature in
 let recipes = Dynarray.create () in
 (* initial state *)
 Dynarray.add_last recipes 3 ;
 Dynarray.add_last recipes 7 ;
 (* could speed up by removing length-safety checks, probably *)
 let rec seq_sub i n () =
  if n = 0 || i < 0 (* || i >= Dynarray.length recipes *) then Seq.Nil else
  Seq.Cons (recipes.%(i), seq_sub (i+1) (n-1)) in
 (* loop *)
 let rec step i j =
  let next = recipes.%(i) + recipes.%(j) in
  if next > 9 then (Dynarray.add_last recipes 1 ; Dynarray.add_last recipes (next - 10)) else Dynarray.add_last recipes next ;
  let len = Dynarray.length recipes in
  (* check pos - 1 if adding two numbers *)
  if next > 9 && Seq.equal (=) sig_seq (seq_sub (len - sig_len - 1) sig_len) then len - sig_len - 1 else
  if Seq.equal (=) sig_seq (seq_sub (len - sig_len) sig_len) then len - sig_len else
  step ((i + recipes.%(i) + 1) mod len) ((j + recipes.%(j) + 1) mod len) in
 step 0 1

let problem_15a () =
 let example = false in
 (* polymorphic compare respects "reading order" constraints *)
 let module Entity = struct
  type race_t = Elf | Goblin
  (* how much state do we have to actually keep in the type? *)
  type t = {race : race_t; atk : int; mutable hp : int}
 end in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module DYXSet = Set.Make(struct type t = int * int * int let compare = compare end) in
 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in
 (* TODO: pathfind, nearest_step functions *)
 let input = In_channel.(with_open_bin (if example then "15e.txt" else "15.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length input.(0) in

 (* walls is meant to be immutable *)
 let walls = Array.init (h*w) (fun i -> if input.(i/w).[i mod w] = '#' then (-1) else Int.max_int) in
 let entities = ref YXMap.empty in

 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   match input.(y).[x] with
   | 'E' -> entities := YXMap.add (y,x) Entity.{race = Elf; atk = 3; hp = 200}    !entities
   | 'G' -> entities := YXMap.add (y,x) Entity.{race = Goblin; atk = 3; hp = 200} !entities
   | _ -> ()
  done
 done ;

 let floodmap = Array.copy walls in
 YXMap.iter (fun (y,x) _ -> floodmap.(y*w+x) <- (-1)) !entities ;

 let reset_floodmap () =
  Array.blit walls 0 floodmap 0 (w*h) ;
  YXMap.iter (fun (y,x) _ -> floodmap.(y*w+x) <- (-1)) !entities in

 let flood (y,x) =
  let q = Queue.create () in
  Queue.push (y-1,x,1) q ;
  Queue.push (y+1,x,1) q ;
  Queue.push (y,x-1,1) q ;
  Queue.push (y,x+1,1) q ;
  while not (Queue.is_empty q) do
   let (y,x,d) = Queue.take q in
   if y >= 0 && y < h && x >= 0 && x < w && floodmap.(y*w+x) > d then (
    floodmap.(y*w+x) <- d ;
    Queue.push (y-1,x,d+1) q ;
    Queue.push (y+1,x,d+1) q ;
    Queue.push (y,x-1,d+1) q ;
    Queue.push (y,x+1,d+1) q
   ) else ()
  done in

 let shortest_step_to (y,x) =
  let res = ref YXSet.empty in
  let q = Queue.create () in
  let d = floodmap.(y*w+x) in
  Queue.push (y-1,x,d) q ;
  Queue.push (y+1,x,d) q ;
  Queue.push (y,x-1,d) q ;
  Queue.push (y,x+1,d) q ;
  while not (Queue.is_empty q) do
   let (y,x,d) = Queue.take q in
   if y >= 0 && y < h && x >= 0 && x < w then
    if floodmap.(y*w+x) = 1 then res := YXSet.add (y,x) !res else
    if floodmap.(y*w+x) = d - 1 then (
     Queue.push (y-1,x,d-1) q ;
     Queue.push (y+1,x,d-1) q ;
     Queue.push (y,x-1,d-1) q ;
     Queue.push (y,x+1,d-1) q ;
    ) else ()
  done ;
  YXSet.min_elt !res in

 let reachable (entity : Entity.t) =
  let res = ref DYXSet.empty in
  YXMap.to_seq !entities |>
  Seq.filter Entity.(fun (k,v) -> v.race <> entity.race) |>
  Seq.iter
  (fun ((y,x),_) ->
   [y-1,x;y+1,x;y,x-1;y,x+1] |> List.to_seq |>
   Seq.filter (fun (y,x) -> y >= 0 && y < h && x >= 0 && x < w && floodmap.(y*w+x) > 0 && floodmap.(y*w+x) < Int.max_int) |>
   Seq.map (fun (y,x) -> (floodmap.(y*w+x),y,x)) |>
   (fun s -> res := DYXSet.add_seq s !res)) ;
  if DYXSet.is_empty !res then None else Some (DYXSet.min_elt !res) in

 let attackable (y,x) (entity : Entity.t) =
  let adj = YXSet.of_list [y-1,x;y+1,x;y,x-1;y,x+1] in
  let res =
   YXMap.filter Entity.(fun k v -> v.race <> entity.race && YXSet.mem k adj) !entities |>
   YXMap.to_seq |> Seq.map Entity.(fun ((y,x),v) -> (v.hp,y,x)) |> DYXSet.of_seq in
  if DYXSet.is_empty res then None else Some (DYXSet.min_elt res) in

 let rec step (y,x) (e : Entity.t) =
  if e.hp <= 0 then () else
  match attackable (y,x) e with
  | Some (_,y,x) -> 
    let target = YXMap.find (y,x) !entities in
    target.hp <- target.hp - e.atk ;
    if target.hp <= 0 then entities := YXMap.remove (y,x) !entities else ()
  | None ->
    reset_floodmap () ;
    flood (y,x) ;
    (match reachable e with
    | None -> ()
    | Some (d,yt,xt) ->
      let (y',x') =
       if d = 1 then (yt,xt)
       else shortest_step_to (yt,xt) in
      entities := YXMap.remove (y,x) !entities ;
      entities := YXMap.add (y',x') e !entities ;
      if d = 1 then step (y',x') e else ()
    ) in

 let counts () = YXMap.fold Entity.(fun _ v (e,g) -> match v.race with Elf -> (e+1,g) | Goblin -> (e,g+1)) !entities (0,0) in

(*
 for i = 1 to 3 do
  YXMap.to_list !entities |> List.iter (fun (k,v) -> step k v)
 done ;
*)

 let rounds = ref 0 in
 while 
  let (e,g) = counts () in
  not (e = 0 || g = 0)
 do
  (* hack to measure "full rounds" *)
  let res = YXMap.to_list !entities |> List.filter_map (fun (k,v) -> step k v; if v.hp > 0 then Some (counts ()) else None) in
  let res_seq = Seq.take (max 0 (List.length res - 1)) (List.to_seq res) in
  if not (Seq.exists (fun (e,g) -> e = 0 || g = 0) res_seq) then incr rounds
 done ;

 YXMap.iter (fun (y,x) _ -> Printf.printf "(%d,%d)\n" y x) !entities ;
 let hp = (YXMap.fold Entity.(fun _ v a -> v.hp + a) !entities 0) in
 (!rounds, hp, !rounds * hp)

let problem_15b () =
 let example = false in
 (* polymorphic compare respects "reading order" constraints *)
 let module Entity = struct
  type race_t = Elf | Goblin
  (* how much state do we have to actually keep in the type? *)
  type t = {race : race_t; atk : int; mutable hp : int}
 end in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module DYXSet = Set.Make(struct type t = int * int * int let compare = compare end) in
 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in

 let input = In_channel.(with_open_bin (if example then "15e.txt" else "15.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length input.(0) in

 (* walls is meant to be immutable *)
 let walls = Array.init (h*w) (fun i -> if input.(i/w).[i mod w] = '#' then (-1) else Int.max_int) in
 let entities = ref YXMap.empty in

 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   match input.(y).[x] with
   | 'E' -> entities := YXMap.add (y,x) Entity.{race = Elf; atk = 3; hp = 200}    !entities
   | 'G' -> entities := YXMap.add (y,x) Entity.{race = Goblin; atk = 3; hp = 200} !entities
   | _ -> ()
  done
 done ;
 let entities0 = !entities in

 let floodmap = Array.copy walls in
 YXMap.iter (fun (y,x) _ -> floodmap.(y*w+x) <- (-1)) !entities ;

 let reset_floodmap () =
  Array.blit walls 0 floodmap 0 (w*h) ;
  YXMap.iter (fun (y,x) _ -> floodmap.(y*w+x) <- (-1)) !entities in

 let flood (y,x) =
  let q = Queue.create () in
  Queue.push (y-1,x,1) q ;
  Queue.push (y+1,x,1) q ;
  Queue.push (y,x-1,1) q ;
  Queue.push (y,x+1,1) q ;
  while not (Queue.is_empty q) do
   let (y,x,d) = Queue.take q in
   if y >= 0 && y < h && x >= 0 && x < w && floodmap.(y*w+x) > d then (
    floodmap.(y*w+x) <- d ;
    Queue.push (y-1,x,d+1) q ;
    Queue.push (y+1,x,d+1) q ;
    Queue.push (y,x-1,d+1) q ;
    Queue.push (y,x+1,d+1) q
   ) else ()
  done in

 let shortest_step_to (y,x) =
  let res = ref YXSet.empty in
  let q = Queue.create () in
  let d = floodmap.(y*w+x) in
  Queue.push (y-1,x,d) q ;
  Queue.push (y+1,x,d) q ;
  Queue.push (y,x-1,d) q ;
  Queue.push (y,x+1,d) q ;
  while not (Queue.is_empty q) do
   let (y,x,d) = Queue.take q in
   if y >= 0 && y < h && x >= 0 && x < w then
    if floodmap.(y*w+x) = 1 then res := YXSet.add (y,x) !res else
    if floodmap.(y*w+x) = d - 1 then (
     Queue.push (y-1,x,d-1) q ;
     Queue.push (y+1,x,d-1) q ;
     Queue.push (y,x-1,d-1) q ;
     Queue.push (y,x+1,d-1) q ;
    ) else ()
  done ;
  YXSet.min_elt !res in

 let reachable (entity : Entity.t) =
  let res = ref DYXSet.empty in
  YXMap.to_seq !entities |>
  Seq.filter Entity.(fun (k,v) -> v.race <> entity.race) |>
  Seq.iter
  (fun ((y,x),_) ->
   [y-1,x;y+1,x;y,x-1;y,x+1] |> List.to_seq |>
   Seq.filter (fun (y,x) -> y >= 0 && y < h && x >= 0 && x < w && floodmap.(y*w+x) > 0 && floodmap.(y*w+x) < Int.max_int) |>
   Seq.map (fun (y,x) -> (floodmap.(y*w+x),y,x)) |>
   (fun s -> res := DYXSet.add_seq s !res)) ;
  if DYXSet.is_empty !res then None else Some (DYXSet.min_elt !res) in

 let attackable (y,x) (entity : Entity.t) =
  let adj = YXSet.of_list [y-1,x;y+1,x;y,x-1;y,x+1] in
  let res =
   YXMap.filter Entity.(fun k v -> v.race <> entity.race && YXSet.mem k adj) !entities |>
   YXMap.to_seq |> Seq.map Entity.(fun ((y,x),v) -> (v.hp,y,x)) |> DYXSet.of_seq in
  if DYXSet.is_empty res then None else Some (DYXSet.min_elt res) in

 let rec step (y,x) (e : Entity.t) =
  if e.hp <= 0 then () else
  match attackable (y,x) e with
  | Some (_,y,x) -> 
    let target = YXMap.find (y,x) !entities in
    target.hp <- target.hp - e.atk ;
    if target.hp <= 0 then entities := YXMap.remove (y,x) !entities else ()
  | None ->
    reset_floodmap () ;
    flood (y,x) ;
    (match reachable e with
    | None -> ()
    | Some (d,yt,xt) ->
      let (y',x') =
       if d = 1 then (yt,xt)
       else shortest_step_to (yt,xt) in
      entities := YXMap.remove (y,x) !entities ;
      entities := YXMap.add (y',x') e !entities ;
      if d = 1 then step (y',x') e else ()
    ) in

 let counts () = YXMap.fold Entity.(fun _ v (e,g) -> match v.race with Elf -> (e+1,g) | Goblin -> (e,g+1)) !entities (0,0) in

(*
 for i = 1 to 3 do
  YXMap.to_list !entities |> List.iter (fun (k,v) -> step k v)
 done ;
*)

 let atk' = 23 in
 entities := YXMap.map Entity.(fun v -> match v.race with Elf -> {v with atk = atk'} | Goblin -> v) entities0 ;
 let (e0,_) = counts () in

 let rounds = ref 0 in
 while 
  let (e,g) = counts () in
  not (e = 0 || g = 0 || e < e0)
 do
  (* hack to measure "full rounds" *)
  let res = YXMap.to_list !entities |> List.filter_map (fun (k,v) -> step k v; if v.hp > 0 then Some (counts ()) else None) in
  let res_seq = Seq.take (max 0 (List.length res - 1)) (List.to_seq res) in
  if not (Seq.exists (fun (e,g) -> e = 0 || g = 0) res_seq) then incr rounds
 done ;

 (
  let (e,_) = counts () in
  if e < e0 then print_endline "ELF DOWN!"
 );

 YXMap.iter (fun (y,x) v -> Printf.printf "%c (%d,%d)\n" Entity.(match v.race with Elf -> 'E' | Goblin -> 'G') y x) !entities ;
 let hp = (YXMap.fold Entity.(fun _ v a -> v.hp + a) !entities 0) in
 (!rounds, hp, !rounds * hp)

(* auto version! *)
let problem_15b2 () =
 let example = false in
 (* polymorphic compare respects "reading order" constraints *)
 let module Entity = struct
  type race_t = Elf | Goblin
  (* how much state do we have to actually keep in the type? *)
  type t = {race : race_t; atk : int; mutable hp : int}
 end in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module DYXSet = Set.Make(struct type t = int * int * int let compare = compare end) in
 let module YXMap = Map.Make(struct type t = int * int let compare = compare end) in

 let input = In_channel.(with_open_bin (if example then "15e.txt" else "15.txt") input_lines) |> Array.of_list in
 let h = Array.length input in
 let w = String.length input.(0) in

 (* walls is meant to be immutable *)
 let walls = Array.init (h*w) (fun i -> if input.(i/w).[i mod w] = '#' then (-1) else Int.max_int) in
 let entities = ref YXMap.empty in

 for y = 0 to h - 1 do
  for x = 0 to w - 1 do
   match input.(y).[x] with
   | 'E' -> entities := YXMap.add (y,x) Entity.{race = Elf; atk = 3; hp = 200}    !entities
   | 'G' -> entities := YXMap.add (y,x) Entity.{race = Goblin; atk = 3; hp = 200} !entities
   | _ -> ()
  done
 done ;
 let entities0 = !entities in

 let floodmap = Array.copy walls in
 YXMap.iter (fun (y,x) _ -> floodmap.(y*w+x) <- (-1)) !entities ;

 let reset_floodmap () =
  Array.blit walls 0 floodmap 0 (w*h) ;
  YXMap.iter (fun (y,x) _ -> floodmap.(y*w+x) <- (-1)) !entities in

 let flood (y,x) =
  let q = Queue.create () in
  Queue.push (y-1,x,1) q ;
  Queue.push (y+1,x,1) q ;
  Queue.push (y,x-1,1) q ;
  Queue.push (y,x+1,1) q ;
  while not (Queue.is_empty q) do
   let (y,x,d) = Queue.take q in
   if y >= 0 && y < h && x >= 0 && x < w && floodmap.(y*w+x) > d then (
    floodmap.(y*w+x) <- d ;
    Queue.push (y-1,x,d+1) q ;
    Queue.push (y+1,x,d+1) q ;
    Queue.push (y,x-1,d+1) q ;
    Queue.push (y,x+1,d+1) q
   ) else ()
  done in

 let shortest_step_to (y,x) =
  let res = ref YXSet.empty in
  let q = Queue.create () in
  let d = floodmap.(y*w+x) in
  Queue.push (y-1,x,d) q ;
  Queue.push (y+1,x,d) q ;
  Queue.push (y,x-1,d) q ;
  Queue.push (y,x+1,d) q ;
  while not (Queue.is_empty q) do
   let (y,x,d) = Queue.take q in
   if y >= 0 && y < h && x >= 0 && x < w then
    if floodmap.(y*w+x) = 1 then res := YXSet.add (y,x) !res else
    if floodmap.(y*w+x) = d - 1 then (
     Queue.push (y-1,x,d-1) q ;
     Queue.push (y+1,x,d-1) q ;
     Queue.push (y,x-1,d-1) q ;
     Queue.push (y,x+1,d-1) q ;
    ) else ()
  done ;
  YXSet.min_elt !res in

 let reachable (entity : Entity.t) =
  let res = ref DYXSet.empty in
  YXMap.to_seq !entities |>
  Seq.filter Entity.(fun (k,v) -> v.race <> entity.race) |>
  Seq.iter
  (fun ((y,x),_) ->
   [y-1,x;y+1,x;y,x-1;y,x+1] |> List.to_seq |>
   Seq.filter (fun (y,x) -> y >= 0 && y < h && x >= 0 && x < w && floodmap.(y*w+x) > 0 && floodmap.(y*w+x) < Int.max_int) |>
   Seq.map (fun (y,x) -> (floodmap.(y*w+x),y,x)) |>
   (fun s -> res := DYXSet.add_seq s !res)) ;
  if DYXSet.is_empty !res then None else Some (DYXSet.min_elt !res) in

 let attackable (y,x) (entity : Entity.t) =
  let adj = YXSet.of_list [y-1,x;y+1,x;y,x-1;y,x+1] in
  let res =
   YXMap.filter Entity.(fun k v -> v.race <> entity.race && YXSet.mem k adj) !entities |>
   YXMap.to_seq |> Seq.map Entity.(fun ((y,x),v) -> (v.hp,y,x)) |> DYXSet.of_seq in
  if DYXSet.is_empty res then None else Some (DYXSet.min_elt res) in

 let rec step (y,x) (e : Entity.t) =
  if e.hp <= 0 then () else
  match attackable (y,x) e with
  | Some (_,y,x) -> 
    let target = YXMap.find (y,x) !entities in
    target.hp <- target.hp - e.atk ;
    if target.hp <= 0 then entities := YXMap.remove (y,x) !entities else ()
  | None ->
    reset_floodmap () ;
    flood (y,x) ;
    (match reachable e with
    | None -> ()
    | Some (d,yt,xt) ->
      let (y',x') =
       if d = 1 then (yt,xt)
       else shortest_step_to (yt,xt) in
      entities := YXMap.remove (y,x) !entities ;
      entities := YXMap.add (y',x') e !entities ;
      if d = 1 then step (y',x') e else ()
    ) in

 let counts () = YXMap.fold Entity.(fun _ v (e,g) -> match v.race with Elf -> (e+1,g) | Goblin -> (e,g+1)) !entities (0,0) in

 let (e0,_) = counts () in
 let high_atk = ref 100 in
 let low_atk = ref 3 in
 let res = ref (-1,-1,-1) in

 while
  !low_atk + 1 <> !high_atk
 do
  let atk' = (!high_atk - !low_atk) / 2 + !low_atk in
  (* {v with hp = 200} is required to make a copy of goblin instead of reusing previous values! *)
  entities := YXMap.map Entity.(fun v -> match v.race with Elf -> {v with atk = atk'} | Goblin -> {v with hp = 200}) entities0 ;

  let rounds = ref 0 in
  while 
   let (e,g) = counts () in
   not (e = 0 || g = 0 || e < e0)
  do
   (* hack to measure "full rounds" *)
   let res = YXMap.to_list !entities |> List.filter_map (fun (k,v) -> step k v; if v.hp > 0 then Some (counts ()) else None) in
   let res_seq = Seq.take (max 0 (List.length res - 1)) (List.to_seq res) in
   if not (Seq.exists (fun (e,g) -> e = 0 || g = 0) res_seq) then incr rounds
  done ;

 let (e,_) = counts () in
 if e < e0 then (
  low_atk := atk' ;
  Printf.printf "low: %d e: %d\n" atk' e
 ) else (
  high_atk := atk' ;
  Printf.printf "high: %d e: %d\n" atk' e ;
  let hp = (YXMap.fold Entity.(fun _ v a -> v.hp + a) !entities 0) in
  res := (!rounds, hp, !rounds * hp)
 )
 done ;
 Printf.printf "min_atk: %d\n" !high_atk ;
 !res

(* 16 op codes, 2 in, 1 out, 4 registers *)
let problem_16a () =
 let ( let* ) = Option.bind in
 let (.%()) r i = if i >= 0 && i < 4 then Some (r.(i)) else None in
 let (.%()<-) r i x = if i >= 0 && i < 4 then Some (r.(i)<-x) else None in
 let exec_opt regs op a b c =
  match op with
  | 0  -> (* addr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va+vb
  | 1  -> (* addi *)
    let* va = regs.%(a) in
    regs.%(c) <- va+b
  | 2  -> (* mulr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va*vb
  | 3  -> (* muli *)
    let* va = regs.%(a) in
    regs.%(c) <- va*b
  | 4  -> (* banr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va land vb
  | 5  -> (* bani *)
    let* va = regs.%(a) in
    regs.%(c) <- va land b
  | 6  -> (* borr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va lor vb
  | 7  -> (* bori *)
    let* va = regs.%(a) in
    regs.%(c) <- va lor b
  | 8  -> (* setr *)
    let* va = regs.%(a) in
    regs.%(c) <- va
  | 9  -> (* seti *)
    regs.%(c) <- a
  | 10 -> (* gtir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a > vb then 1 else 0
  | 11 -> (* gtri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va > b then 1 else 0
  | 12 -> (* gtrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va > vb then 1 else 0
  | 13 -> (* eqir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a = vb then 1 else 0
  | 14 -> (* eqri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va = b then 1 else 0
  | 15 -> (* eqrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va = vb then 1 else 0
  | _ -> None (* Invalid OpCode *) in

 let test (regs_in,regs_out,_,a,b,c) =
  let regs = Array.make 4 0 in
  Seq.ints 0 |> Seq.take 16 |>
  Seq.filter_map (fun op ->
   Array.blit regs_in 0 regs 0 4 ;
   let* _ = exec_opt regs op a b c in
   if regs = regs_out then Some op else None) |>
  List.of_seq in
  
 let example = false in
 let input = In_channel.(with_open_bin (if example then "16e.txt" else "16.txt") input_lines) in
 let tests = Queue.create () in
 let _ =
  List.fold_left (fun (before, ops) after ->
   if String.starts_with ~prefix:"Before:" before &&
      String.starts_with ~prefix:"After:" after then
   let regs_in  = Scanf.sscanf before "Before: [ %d , %d , %d , %d ]" (fun a b c d -> [|a;b;c;d|]) in
   let (op,a,b,c) = Scanf.sscanf ops  " %d %d %d %d " (fun a b c d -> a,b,c,d) in
   let regs_out = Scanf.sscanf after  "After: [ %d , %d , %d , %d ]"  (fun a b c d -> [|a;b;c;d|]) in
   (Queue.add (regs_in, regs_out, op, a, b, c) tests ; ("",""))
   else (ops, after)) ("","") input in
 Queue.to_seq tests |>
 Seq.map test |>
 Seq.fold_left (fun a l -> if List.length l >= 3 then succ a else a) 0

(* fun with monads *)
let problem_16b () =
 let ( let* ) = Option.bind in
 let (.%()) r i = if i >= 0 && i < 4 then Some (r.(i)) else None in
 let (.%()<-) r i x = if i >= 0 && i < 4 then Some (r.(i)<-x) else None in
 let exec_opt regs op a b c =
  match op with
  | 0  -> (* addr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va+vb
  | 1  -> (* addi *)
    let* va = regs.%(a) in
    regs.%(c) <- va+b
  | 2  -> (* mulr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va*vb
  | 3  -> (* muli *)
    let* va = regs.%(a) in
    regs.%(c) <- va*b
  | 4  -> (* banr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va land vb
  | 5  -> (* bani *)
    let* va = regs.%(a) in
    regs.%(c) <- va land b
  | 6  -> (* borr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va lor vb
  | 7  -> (* bori *)
    let* va = regs.%(a) in
    regs.%(c) <- va lor b
  | 8  -> (* setr *)
    let* va = regs.%(a) in
    regs.%(c) <- va
  | 9  -> (* seti *)
    regs.%(c) <- a
  | 10 -> (* gtir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a > vb then 1 else 0
  | 11 -> (* gtri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va > b then 1 else 0
  | 12 -> (* gtrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va > vb then 1 else 0
  | 13 -> (* eqir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a = vb then 1 else 0
  | 14 -> (* eqri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va = b then 1 else 0
  | 15 -> (* eqrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va = vb then 1 else 0
  | _ -> None (* Invalid OpCode *) in

 let test (regs_in,regs_out,_,a,b,c) =
  let regs = Array.make 4 0 in
  Seq.ints 0 |> Seq.take 16 |>
  Seq.filter_map (fun op ->
   Array.blit regs_in 0 regs 0 4 ;
   let* _ = exec_opt regs op a b c in
   if regs = regs_out then Some op else None) |>
  List.of_seq in
  
 let example = false in
 let debug = true in
 let input = In_channel.(with_open_bin (if example then "16e.txt" else "16.txt") input_lines) in
 let tests = Queue.create () in
 let _ =
  List.fold_left (fun (before, ops) after ->
   if String.starts_with ~prefix:"Before:" before &&
      String.starts_with ~prefix:"After:" after then
   let regs_in  = Scanf.sscanf before "Before: [ %d , %d , %d , %d ]" (fun a b c d -> [|a;b;c;d|]) in
   let (op,a,b,c) = Scanf.sscanf ops  " %d %d %d %d " (fun a b c d -> a,b,c,d) in
   let regs_out = Scanf.sscanf after  "After: [ %d , %d , %d , %d ]"  (fun a b c d -> [|a;b;c;d|]) in
   (Queue.add (regs_in, regs_out, op, a, b, c) tests ; ("",""))
   else (ops, after)) ("","") input in
 let module ISet = Set.Make(Int) in
(*
 let ops = Queue.to_seq tests |> Seq.map (fun (_,_,op,_,_,_) -> op) |> Array.of_seq in
 let valids = Queue.to_seq tests |> Seq.map test |> Seq.map ISet.of_list |> Array.of_seq in
*)
 let kv = Queue.to_seq tests |> Seq.map (fun ((_,_,op,_,_,_) as x) -> (op, ISet.of_list (test x))) |> Seq.memoize in
 let fullset = Seq.ints 0 |> Seq.take 16 |> ISet.of_seq in
 let tmap = Array.make 16 fullset in

 Seq.iter (fun (op, valid) -> tmap.(op) <- ISet.inter valid tmap.(op)) kv ;

 (* determine mapping by removing iff matches *)
 (let rec loop () =
  let (ks, vs) =
   Seq.fold_lefti
    (fun (ks,vs) k s ->
     if ISet.cardinal s = 1 then
      let v = ISet.min_elt s in
      (ISet.add k ks, ISet.add v vs)
     else (ks, vs)) (ISet.empty, ISet.empty)
   (Array.to_seq tmap) in
  if ISet.cardinal ks = 16 then () else (
   for i = 0 to 15 do
    if not (ISet.mem i ks) then tmap.(i) <- ISet.diff tmap.(i) vs else ()
   done ;
   loop ()
  ) in loop ()) ;

 (* print mapping *)
 if debug then Array.iteri (fun i s -> if ISet.cardinal s = 1 then Printf.printf "%02d -> %02d\n" i (ISet.min_elt s)) tmap ;
 (* unwrap set *)
 let tmap = Array.map ISet.min_elt tmap in
 let skip = Queue.length tests * 4 + 2 in (* 4 lines of input per test *)
 let regs = Array.make 4 0 in
 let success = 
  List.drop skip input |>
  List.fold_left (fun last s -> Option.bind last (fun () -> Scanf.sscanf s " %d %d %d %d " (fun op a b c -> exec_opt regs tmap.(op) a b c ))) (Option.some ()) in
 if Option.is_none success then prerr_endline "Error Encountered!" ;
 regs.(0)

let problem_17 () =
 let module IMap = Map.Make(Int) in
 let module LSet = struct
  type t = { mutable h : (int * int) list IMap.t; mutable v : (int * int) list IMap.t }
  type line_t = X | Y
  let create () = {h = IMap.empty ; v = IMap.empty}
  let mem (y,x) lset =
   (IMap.mem x lset.v && (List.exists (fun (y1,y2) -> y1 <= y && y <= y2) (IMap.find x lset.v))) ||
   (IMap.mem y lset.h && (List.exists (fun (x1,x2) -> x1 <= x && x <= x2) (IMap.find y lset.h)))
  let add (dim : line_t) maj mins lset =
   match dim with
   | X -> lset.v <- IMap.update maj (function None -> Some(mins::[]) | Some tl -> Some(mins::tl)) lset.v
   | Y -> lset.h <- IMap.update maj (function None -> Some(mins::[]) | Some tl -> Some(mins::tl)) lset.h
  let min_max_yx lset =
   let (min_y, max_y) =
    IMap.fold
    (fun _ ys a -> List.fold_left (fun (ymin,ymax) (y1,y2) -> min y1 ymin, max y2 ymax) a ys)
    lset.v
    (Option.value ~default:Int.max_int (Option.map fst (IMap.min_binding_opt lset.h)),
     Option.value ~default:Int.min_int (Option.map fst (IMap.max_binding_opt lset.h))) in
   let (min_x, max_x) =
    IMap.fold
    (fun _ xs a -> List.fold_left (fun (xmin,xmax) (x1,x2) -> min x1 xmin, max x2 xmax) a xs)
    lset.h
    (Option.value ~default:Int.max_int (Option.map fst (IMap.min_binding_opt lset.v)),
     Option.value ~default:Int.min_int (Option.map fst (IMap.max_binding_opt lset.v))) in
   (min_y, max_y, min_x, max_x)
 end in
 let module Water = struct
  type t =
   | Drop    of {x : int ; ymin : int ; ymax : int}
   | Flood   of {y : int ; xmin : int ; xmax : int}
   | SpillR  of {y : int ; xmin : int ; xmax : int}
   | SpillL  of {y : int ; xmin : int ; xmax : int}
   | SpillLR of {y : int ; xmin : int ; xmax : int}
  let to_string = function
   | Drop    {x;ymin;ymax} -> Printf.sprintf "Drop: x=%d, y=%d..%d" x ymin ymax
   | Flood   {y;xmin;xmax} -> Printf.sprintf "Flood: y=%d, x=%d..%d" y xmin xmax
   | SpillR  {y;xmin;xmax} -> Printf.sprintf "SpillR: y=%d, x=%d..%d" y xmin xmax
   | SpillL  {y;xmin;xmax} -> Printf.sprintf "SpillL: y=%d, x=%d..%d" y xmin xmax
   | SpillLR {y;xmin;xmax} -> Printf.sprintf "SpillLR: y=%d, x=%d..%d" y xmin xmax
 end in
 let clay = LSet.create () in (* stores both vertical and horizontal slices *)
 let water = LSet.create () in (* only stores horizonal slices : integratable *)
 let example = false in
 let debug = false in
 (* read clay *)
 In_channel.(
  with_open_bin (if example then "17e.txt" else "17.txt")
  (fun ic ->
   Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.iter (fun s -> Scanf.sscanf s "%c=%d, %c=%d..%d" (fun axis maj _ min1 min2 -> LSet.add (if axis = 'x' then LSet.X else LSet.Y) maj (min1,min2) clay)))) ;
 let (ymin,ymax,xmin,xmax) = LSet.min_max_yx clay in
 (* sanity check *)
 if debug then Printf.printf "y:(%d,%d), x:(%d,%d)\n" ymin ymax xmin xmax ;
 (* create a water structure from a single source point *)
 let eval (y,x) =
  if not (LSet.mem (y+1,x) clay || LSet.mem (y+1,x) water) then
   let rec loop y =
    if (y+1) <= ymax && not (LSet.mem (y+1,x) clay || LSet.mem (y+1,x) water) then loop (y+1) else y in
   let y' = loop (y+1) in
   Water.Drop {x; ymin = y; ymax = y'}
  else
   let rec loop_r x =
    match (LSet.mem (y+1,x+1) clay || LSet.mem (y+1,x+1) water), (LSet.mem (y,x+1) clay || LSet.mem (y,x+1) water) with
    | true, true -> (true, x)
    | true, false -> loop_r (x+1)
    | false, _ -> (false, x+1) in
   let rec loop_l x =
    match (LSet.mem (y+1,x-1) clay || LSet.mem (y+1,x-1) water), (LSet.mem (y,x-1) clay || LSet.mem (y,x-1) water) with
    | true, true -> (true, x)
    | true, false -> loop_l (x-1)
    | false, _ -> (false, x-1) in
   match loop_l x, loop_r x with
   | (true, xmin), (true, xmax) -> Water.Flood {y;xmin;xmax}
   | (true, xmin), (false, xmax) -> Water.SpillR {y;xmin;xmax}
   | (false, xmin), (true, xmax) -> Water.SpillL {y;xmin;xmax}
   | (false, xmin), (false, xmax) -> Water.SpillLR {y;xmin;xmax}
 in
 (* use hashset memo to eliminate retracing within a step *)
 let rec fill hs q (y,x) =
  let res = eval (y,x) in
  if Hashtbl.mem hs res then () else
  match Hashtbl.add hs res () ; res with
  | Drop    {x;ymin;ymax = y} when y >= ymax -> ()
  | Drop    {x;ymin;ymax = y} -> fill hs q (y,x)
  | Flood   {y;xmin;xmax} -> Queue.add (y,xmin,xmax) q
  | SpillR  {y;xmin;xmax = x} -> fill hs q (y,x)
  | SpillL  {y;xmin = x;xmax} -> fill hs q (y,x)
  | SpillLR {y;xmin;xmax} -> fill hs q (y,xmin) ; fill hs q (y,xmax)
 in
 (* accumulate with hash memo: store only streams, not floods, to count *)
 let rec count_stream hs (y,x) =
  let res = eval (y,x) in
  match res with
  | Drop    {x;ymin = y0; ymax = y} when y >= ymax -> Hashtbl.replace hs res ()
  | Drop    {x;ymin = y0; ymax = y} -> if Hashtbl.mem hs res then () else (Hashtbl.add hs res () ; count_stream hs (y,x))
  | Flood   {y;xmin;xmax} -> ()
  | SpillR  {y;xmin = x0;xmax = x} -> if Hashtbl.mem hs res then () else (Hashtbl.add hs res () ; count_stream hs (y,x))
  | SpillL  {y;xmin = x;xmax = x1} -> if Hashtbl.mem hs res then () else (Hashtbl.add hs res () ; count_stream hs (y,x))
  | SpillLR {y;xmin;xmax} -> if Hashtbl.mem hs res then () else (Hashtbl.add hs res (); count_stream hs (y,xmin) ;count_stream hs (y,xmax))
 in
 let w_to_cnt : Water.t -> int = function
  (* count low edge only on final drops *)
  | Drop    {x;ymin = y0; ymax = y} when y >= ymax -> y - y0 + 1
  | Drop    {x;ymin = y0; ymax = y} -> y - (max ymin y0)
  (* count both edges *)
  | Flood   {y;xmin;xmax} -> xmax - xmin + 1
  (* remove edges that spill *)
  | SpillR  {y;xmin = x0;xmax = x} -> x - x0
  | SpillL  {y;xmin = x;xmax = x1} -> x1 - x
  | SpillLR {y;xmin;xmax} -> xmax - xmin - 1 in
 let count_water () =
  IMap.to_seq water.h |>
  Seq.fold_left (fun a (_,v) -> List.fold_left (fun a (x0,x1) -> a + x1 - x0 + 1) a v) 0 in
 (* print clay map *)
 if debug then (
  for y = ymin-1 to ymax+1 do
   for x = xmin-1 to xmax+1 do
    if LSet.mem (y,x) clay then print_char '#' else print_char '.'
   done ;
   print_newline ()
  done
 ) else () ;
 let q = Queue.create () in
 let hs = Hashtbl.create 2048 in
 while 
  Queue.clear q ;
  Hashtbl.clear hs;
  fill hs q (0,500) ;
  not (Queue.is_empty q)
 do
  Queue.iter (fun (y,xmin,xmax) -> LSet.add (LSet.Y) y (xmin,xmax) water) q
 done ;
 Hashtbl.clear hs ;
 count_stream hs (0,500) ;
 let stream_sum = Hashtbl.to_seq_keys hs |> Seq.fold_left (fun a w -> w_to_cnt w + a) 0 in
 let cw = count_water () in
 (stream_sum+cw, cw)

(* game of life : using labeled tuples : Ocaml 5.4 / OxCaml feature *)
let problem_18a () =
 let example = false in
 let steps = 10 in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let input =
  In_channel.(with_open_bin (if example then "18e.txt" else "18.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let dim = Array.length input in
 let acres = Array.init 2 (fun i -> if i = 0 then input else Array.init dim (fun _ -> Bytes.make dim '.')) in
 let conditions m (y,x) =
  [y-1,x-1;y-1,x;y-1,x+1
  ;  y,x-1;        y,x+1
  ;y+1,x-1;y+1,x;y+1,x+1] |>
  List.filter (fun (y,x) -> y >= 0 && y < dim && x >= 0 && x < dim) |>
  List.map (fun (y,x) -> m.(y).%[x]) |>
  List.fold_left (fun (~t,~l) c -> match c with '|' -> ~t:(t+1),~l | '#' -> ~t,~l:(l+1) | _ -> ~t,~l) (~t:0,~l:0) in
 let step i =
  for y = 0 to dim - 1 do
   for x = 0 to dim - 1 do
    (match acres.(i land 1).(y).%[x], conditions acres.(i land 1) (y,x) with
    | '.', (~t,~l:_) when t >= 3 -> acres.((i+1) land 1).(y).%[x] <- '|'
    | '|', (~t:_,~l) when l >= 3 -> acres.((i+1) land 1).(y).%[x] <- '#'
    | '#', (~t,~l) when t >= 1 && l >= 1 -> acres.((i+1) land 1).(y).%[x] <- '#'
    | '#', _ -> acres.((i+1) land 1).(y).%[x] <- '.'
    | c, _ -> acres.((i+1) land 1).(y).%[x] <- c)
   done
  done in
 let show i =
  for y = 0 to dim - 1 do
   output_bytes stdout acres.(i land 1).(y) ;
   print_newline ()
  done in
 let count i =
  acres.(i land 1) |>
  Array.fold_left
   (fun (~t,~l) bs -> 
    bs |> Bytes.fold_left
    (fun (~t,~l) c -> match c with '|' -> ~t:(t+1),~l | '#' -> ~t,~l:(l+1) | _ -> ~t,~l) (~t,~l))
   (~t:0,~l:0) in
 for i = 0 to steps - 1 do step i done ;
 show steps;
 let (~t,~l) = count steps in
 (t,l,t*l)

let problem_18b () =
 let example = false in
 let steps = 1_000_000_000 in
 let (.%[]) = Bytes.get in
 let (.%[]<-) = Bytes.set in
 let input =
  In_channel.(with_open_bin (if example then "18e.txt" else "18.txt") input_lines) |>
  List.map (Bytes.unsafe_of_string) |> Array.of_list in
 let dim = Array.length input in
 let acres = Array.init 2 (fun i -> if i = 0 then input else Array.init dim (fun _ -> Bytes.make dim '.')) in
 let conditions m (y,x) =
  [y-1,x-1;y-1,x;y-1,x+1
  ;  y,x-1;        y,x+1
  ;y+1,x-1;y+1,x;y+1,x+1] |>
  List.filter (fun (y,x) -> y >= 0 && y < dim && x >= 0 && x < dim) |>
  List.map (fun (y,x) -> m.(y).%[x]) |>
  List.fold_left (fun (~t,~l) c -> match c with '|' -> ~t:(t+1),~l | '#' -> ~t,~l:(l+1) | _ -> ~t,~l) (~t:0,~l:0) in
 let step i =
  for y = 0 to dim - 1 do
   for x = 0 to dim - 1 do
    (match acres.(i land 1).(y).%[x], conditions acres.(i land 1) (y,x) with
    | '.', (~t,~l:_) when t >= 3 -> acres.((i+1) land 1).(y).%[x] <- '|'
    | '|', (~t:_,~l) when l >= 3 -> acres.((i+1) land 1).(y).%[x] <- '#'
    | '#', (~t,~l) when t >= 1 && l >= 1 -> acres.((i+1) land 1).(y).%[x] <- '#'
    | '#', _ -> acres.((i+1) land 1).(y).%[x] <- '.'
    | c, _ -> acres.((i+1) land 1).(y).%[x] <- c)
   done
  done in
 let compress buf i =
  Buffer.clear buf ;
  let bbuf = ref 0 in
  for y = 0 to dim - 1 do
   for x = 0 to dim - 1 do
    bbuf := !bbuf lsl 2 ;
    (match acres.(i land 1).(y).%[x] with
    | '|' -> bbuf := !bbuf lor 1
    | '#' -> bbuf := !bbuf lor 2
    | _ -> ()) ;
    if (y*dim+x) land 3 = 3 then Buffer.add_uint8 buf (!bbuf land 0xFF)
   done
  done ;
  Buffer.add_uint8 buf (!bbuf land 0xFF) ;
  Buffer.contents buf in
 let show i =
  for y = 0 to dim - 1 do
   output_bytes stdout acres.(i land 1).(y) ;
   print_newline ()
  done in
 let count i =
  acres.(i land 1) |>
  Array.fold_left
   (fun (~t,~l) bs -> 
    bs |> Bytes.fold_left
    (fun (~t,~l) c -> match c with '|' -> ~t:(t+1),~l | '#' -> ~t,~l:(l+1) | _ -> ~t,~l) (~t,~l))
   (~t:0,~l:0) in
 let seen = Hashtbl.create 4096 in
 let buf = Buffer.create 630 in
 let i = ref 0 in
 while
  let comp = compress buf !i in
  let continue = not (Hashtbl.mem seen comp) in
  Hashtbl.add seen comp !i ; continue
 do
  step !i;
  incr i
 done;
 let t1 = !i in
 print_int t1 ; print_newline () ;
 show t1 ;
 let comp = Buffer.contents buf in
 Hashtbl.remove seen comp ;
 let t0 = Hashtbl.find seen comp in
 print_int t0 ; print_newline () ;
 let cycle = t1 - t0 in
 let rem = (steps - t1) mod cycle in
 let steps = t1 + rem in
 for i = t1 to steps - 1 do step i done ;
 show steps;
 let (~t,~l) = count steps in
 (t,l,t*l)

let problem_19a () =
 let regsize = 6 in
 let ( let* ) = Option.bind in
 let (.%()) r i = if i >= 0 && i < regsize then Some (r.(i)) else None in
 let (.%()<-) r i x = if i >= 0 && i < regsize then Some (r.(i)<-x) else None in

 let op_of_string = function
  | "addr" -> 0
  | "addi" -> 1
  | "mulr" -> 2
  | "muli" -> 3
  | "banr" -> 4
  | "bani" -> 5
  | "borr" -> 6
  | "bori" -> 7
  | "setr" -> 8
  | "seti" -> 9
  | "gtir" -> 10
  | "gtri" -> 11
  | "gtrr" -> 12
  | "eqir" -> 13
  | "eqri" -> 14
  | "eqrr" -> 15
  | _ -> (-1) in

 let exec_opt regs op a b c =
  match op with
  | 0  -> (* addr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va+vb
  | 1  -> (* addi *)
    let* va = regs.%(a) in
    regs.%(c) <- va+b
  | 2  -> (* mulr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va*vb
  | 3  -> (* muli *)
    let* va = regs.%(a) in
    regs.%(c) <- va*b
  | 4  -> (* banr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va land vb
  | 5  -> (* bani *)
    let* va = regs.%(a) in
    regs.%(c) <- va land b
  | 6  -> (* borr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va lor vb
  | 7  -> (* bori *)
    let* va = regs.%(a) in
    regs.%(c) <- va lor b
  | 8  -> (* setr *)
    let* va = regs.%(a) in
    regs.%(c) <- va
  | 9  -> (* seti *)
    regs.%(c) <- a
  | 10 -> (* gtir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a > vb then 1 else 0
  | 11 -> (* gtri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va > b then 1 else 0
  | 12 -> (* gtrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va > vb then 1 else 0
  | 13 -> (* eqir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a = vb then 1 else 0
  | 14 -> (* eqri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va = b then 1 else 0
  | 15 -> (* eqrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va = vb then 1 else 0
  | _ -> None (* Invalid OpCode *) in

 let example = false in
 let debug = false in
 let (ip_reg, program) =
  let open In_channel in
   with_open_bin
   (if example then "19e.txt" else "19.txt")
   (fun ic ->
    let ic = Scanf.Scanning.from_channel ic in
    let ip_reg = Scanf.bscanf ic "#ip %d" Fun.id in
    let program =
     Seq.of_dispenser (fun () -> Scanf.bscanf_opt ic "%s %d %d %d" (fun s a b c -> op_of_string s, a, b, c)) |>
     Array.of_seq in
    (ip_reg, program)) in
 
 let regs = Array.make regsize 0 in
 while
  regs.(ip_reg) >= 0 && regs.(ip_reg) < Array.length program
 do
  let (op,a,b,c) = program.(regs.(ip_reg)) in
  let _ = exec_opt regs op a b c in
  if debug then (Array.iter (fun x -> print_int x ; print_char ' ' ) regs ; print_newline ()) ;
  regs.(ip_reg) <- regs.(ip_reg) + 1
 done ;
 regs.(0)

let problem_19b () =
 let regsize = 6 in
 let ( let* ) = Option.bind in
 let (.%()) r i = if i >= 0 && i < regsize then Some (r.(i)) else None in
 let (.%()<-) r i x = if i >= 0 && i < regsize then Some (r.(i)<-x) else None in

 let op_of_string = function
  | "addr" -> 0
  | "addi" -> 1
  | "mulr" -> 2
  | "muli" -> 3
  | "banr" -> 4
  | "bani" -> 5
  | "borr" -> 6
  | "bori" -> 7
  | "setr" -> 8
  | "seti" -> 9
  | "gtir" -> 10
  | "gtri" -> 11
  | "gtrr" -> 12
  | "eqir" -> 13
  | "eqri" -> 14
  | "eqrr" -> 15
  | _ -> (-1) in

 let exec_opt regs op a b c =
  match op with
  | 0  -> (* addr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va+vb
  | 1  -> (* addi *)
    let* va = regs.%(a) in
    regs.%(c) <- va+b
  | 2  -> (* mulr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va*vb
  | 3  -> (* muli *)
    let* va = regs.%(a) in
    regs.%(c) <- va*b
  | 4  -> (* banr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va land vb
  | 5  -> (* bani *)
    let* va = regs.%(a) in
    regs.%(c) <- va land b
  | 6  -> (* borr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va lor vb
  | 7  -> (* bori *)
    let* va = regs.%(a) in
    regs.%(c) <- va lor b
  | 8  -> (* setr *)
    let* va = regs.%(a) in
    regs.%(c) <- va
  | 9  -> (* seti *)
    regs.%(c) <- a
  | 10 -> (* gtir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a > vb then 1 else 0
  | 11 -> (* gtri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va > b then 1 else 0
  | 12 -> (* gtrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va > vb then 1 else 0
  | 13 -> (* eqir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a = vb then 1 else 0
  | 14 -> (* eqri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va = b then 1 else 0
  | 15 -> (* eqrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va = vb then 1 else 0
  | _ -> None (* Invalid OpCode *) in

 let example = false in
 let debug = false in
 let (ip_reg, program) =
  let open In_channel in
   with_open_bin
   (if example then "19e.txt" else "19.txt")
   (fun ic ->
    let ic = Scanf.Scanning.from_channel ic in
    let ip_reg = Scanf.bscanf ic "#ip %d" Fun.id in
    let program =
     Seq.of_dispenser (fun () -> Scanf.bscanf_opt ic "%s %d %d %d" (fun s a b c -> op_of_string s, a, b, c)) |>
     Array.of_seq in
    (ip_reg, program)) in
 
 let regs = Array.make regsize 0 in
 (* complete setup to find number we need to factor *)
 regs.(0) <- 1 ;
 let choke = ref @@ Array.length program in
 while
  !choke > 0 && regs.(ip_reg) >= 0 && regs.(ip_reg) < Array.length program
 do
  let (op,a,b,c) = program.(regs.(ip_reg)) in
  let _ = exec_opt regs op a b c in
  if debug then (Array.iter (fun x -> print_int x ; print_char ' ' ) regs ; print_newline ()) ;
  decr choke ;
  regs.(ip_reg) <- regs.(ip_reg) + 1
 done ;
 (* number to factor will be a large constant after setup  *)
 let factor_n = Array.fold_left max 0 regs in
(*
 (* sub-optimal *)
 let acc = ref 0 in
 for i = 1 to factor_n do
  if factor_n mod i = 0 then acc := !acc + i
 done;
 !acc
*)
(*
 (* optimization, assuming mostly small factors - check with gnu factor *)
 let small_factors = (1+2+3+4+6+9+12+18+36) in
 let big_factor = 293093 in
 small_factors+small_factors*big_factor
*)
 let acc = ref 0 in
 let n = ref factor_n in
 for i = 2 to 19 do
  while !n mod i = 0 do n := !n / i done ;
 done;
 for i = 1 to factor_n / !n do
  if factor_n mod i = 0 then acc := !acc + i
 done;
 acc := !acc + !n * !acc ;
 !acc

(* attempt to use compressed states *)
let problem_20a () =
 let debug = false in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module Dir = struct
  type t = N | E | S | W
  let of_char = function 'N' -> N | 'E' -> E | 'S' -> S | 'W' -> W | _ -> raise (Invalid_argument "Invalid Direction")
  let to_char = function N -> 'N' | E -> 'E' | S -> 'S' | W -> 'W'
 end in
 let radius = 150 in
 let vs = Array.make_matrix (radius*2+1) (radius*2+1) 0L in
 let hs = Array.make_matrix (radius*2+1) (radius*2+1) 0L in
 (*let pmod a b = let x = a mod b in if x < 0 then x + b else x in*)
 let open_gate d (y,x) =
  let (y,x) = Dir.(match d with N | W -> (y,x) | E -> (y,x+1) | S -> (y+1,x)) in
  let y' = (y asr 3) + radius in
  let x' = (x asr 3) + radius in
  let y'' = y land 7 in
  let x'' = x land 7 in
  let cache = Dir.(match d with | N | S -> vs | W | E -> hs) in
  cache.(y').(x') <- Int64.(logor cache.(y').(x') (shift_left 1L (8*y''+x''))) in
 let test_gate d (y,x) =
  let (y,x) = Dir.(match d with N | W -> (y,x) | E -> (y,x+1) | S -> (y+1,x)) in
  let y' = (y asr 3) + radius in
  let x' = (x asr 3) + radius in
  let y'' = y land 7 in
  let x'' = x land 7 in
  let cache = Dir.(match d with | N | S -> vs | W | E -> hs) in
  Int64.(logand cache.(y').(x') (shift_left 1L (8*y''+x''))) <> 0L in
 let walk_simple d (y,x) =
  if debug then Printf.printf "Visiting: %d, %d @%c\n" y x (Dir.to_char d) ;
  open_gate d (y,x) ;
  Dir.(match d with
   | N -> y-1,x
   | E -> y,x+1
   | S -> y+1,x
   | W -> y,x-1) in
 let walk_seq s =
  let stk = Stack.create () in
  s |> Seq.iter
   (function
    | '$' -> ()
    | '^' -> Stack.push (YXSet.singleton (0,0)) stk
    | '(' ->
      let last = Stack.pop stk in
      Stack.push YXSet.empty stk ;
      Stack.push last stk ;
      Stack.push last stk ;
    | '|' ->
      let last = Stack.pop stk in
      let next = Stack.pop stk in
      let others = Stack.pop stk in
      Stack.push (YXSet.union last others) stk ;
      Stack.push next stk;
      Stack.push next stk;
    | ')' ->
      let last = Stack.pop stk in
      Stack.drop stk ;
      let others = Stack.pop stk in
      Stack.push (YXSet.union last others) stk
    | c ->
      Stack.push (YXSet.map (walk_simple (Dir.of_char c)) (Stack.pop stk)) stk) in

 let furthest (y,x) =
  let seen = ref YXSet.empty in
  let last = ref (y,x,0) in
  let q = Queue.create () in
  Queue.add (y,x,0) q ;
  while not (Queue.is_empty q) do
   let (y,x,d) = Queue.take q in
   seen := YXSet.add (y,x) !seen ;
   if debug then Printf.printf "%d, %d, %d\n" y x d ;
   if not (YXSet.mem (y-1,x) !seen) && test_gate Dir.N (y,x) then Queue.add (y-1,x,d+1) q;
   if not (YXSet.mem (y+1,x) !seen) && test_gate Dir.S (y,x) then Queue.add (y+1,x,d+1) q;
   if not (YXSet.mem (y,x-1) !seen) && test_gate Dir.W (y,x) then Queue.add (y,x-1,d+1) q;
   if not (YXSet.mem (y,x+1) !seen) && test_gate Dir.E (y,x) then Queue.add (y,x+1,d+1) q;
   last := (y,x,d)
  done ;
  !last in

 let example = false in
 let input = In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_line) |> Option.get in
 walk_seq (String.to_seq input) ;
 furthest (0,0)

(* attempt to use compressed states *)
let problem_20b () =
 let debug = false in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module Dir = struct
  type t = N | E | S | W
  let of_char = function 'N' -> N | 'E' -> E | 'S' -> S | 'W' -> W | _ -> raise (Invalid_argument "Invalid Direction")
  let to_char = function N -> 'N' | E -> 'E' | S -> 'S' | W -> 'W'
 end in
 let radius = 150 in
 let vs = Array.make_matrix (radius*2+1) (radius*2+1) 0L in
 let hs = Array.make_matrix (radius*2+1) (radius*2+1) 0L in
 (*let pmod a b = let x = a mod b in if x < 0 then x + b else x in*)
 let open_gate d (y,x) =
  let (y,x) = Dir.(match d with N | W -> (y,x) | E -> (y,x+1) | S -> (y+1,x)) in
  let y' = (y asr 3) + radius in
  let x' = (x asr 3) + radius in
  let y'' = y land 7 in
  let x'' = x land 7 in
  let cache = Dir.(match d with | N | S -> vs | W | E -> hs) in
  cache.(y').(x') <- Int64.(logor cache.(y').(x') (shift_left 1L (8*y''+x''))) in
 let test_gate d (y,x) =
  let (y,x) = Dir.(match d with N | W -> (y,x) | E -> (y,x+1) | S -> (y+1,x)) in
  let y' = (y asr 3) + radius in
  let x' = (x asr 3) + radius in
  let y'' = y land 7 in
  let x'' = x land 7 in
  let cache = Dir.(match d with | N | S -> vs | W | E -> hs) in
  Int64.(logand cache.(y').(x') (shift_left 1L (8*y''+x''))) <> 0L in
 let walk_simple d (y,x) =
  if debug then Printf.printf "Visiting: %d, %d @%c\n" y x (Dir.to_char d) ;
  open_gate d (y,x) ;
  Dir.(match d with
   | N -> y-1,x
   | E -> y,x+1
   | S -> y+1,x
   | W -> y,x-1) in
 let walk_seq s =
  let stk = Stack.create () in
  s |> Seq.iter
   (function
    | '$' -> ()
    | '^' -> Stack.push (YXSet.singleton (0,0)) stk
    | '(' ->
      let last = Stack.pop stk in
      Stack.push YXSet.empty stk ;
      Stack.push last stk ;
      Stack.push last stk ;
    | '|' ->
      let last = Stack.pop stk in
      let next = Stack.pop stk in
      let others = Stack.pop stk in
      Stack.push (YXSet.union last others) stk ;
      Stack.push next stk;
      Stack.push next stk;
    | ')' ->
      let last = Stack.pop stk in
      Stack.drop stk ;
      let others = Stack.pop stk in
      Stack.push (YXSet.union last others) stk
    | c ->
      Stack.push (YXSet.map (walk_simple (Dir.of_char c)) (Stack.pop stk)) stk) in

 let furthest (y,x) =
  let seen = ref YXSet.empty in
  let count1000 = ref 0 in
  let last = ref (y,x,0) in
  let q = Queue.create () in
  Queue.add (y,x,0) q ;
  while not (Queue.is_empty q) do
   let (y,x,d) = Queue.take q in
   if d >= 1000 then incr count1000 ;
   seen := YXSet.add (y,x) !seen ;
   if debug then Printf.printf "%d, %d, %d\n" y x d ;
   if not (YXSet.mem (y-1,x) !seen) && test_gate Dir.N (y,x) then Queue.add (y-1,x,d+1) q;
   if not (YXSet.mem (y+1,x) !seen) && test_gate Dir.S (y,x) then Queue.add (y+1,x,d+1) q;
   if not (YXSet.mem (y,x-1) !seen) && test_gate Dir.W (y,x) then Queue.add (y,x-1,d+1) q;
   if not (YXSet.mem (y,x+1) !seen) && test_gate Dir.E (y,x) then Queue.add (y,x+1,d+1) q;
   last := (y,x,d)
  done ;
  !last, !count1000 in

 let example = false in
 let input = In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_line) |> Option.get in
 walk_seq (String.to_seq input) ;
 furthest (0,0) |> snd

(* find state @ eqrr 3 0 1 instruction (28)! and set r0 to whatever r3 is right before execution! *)
let problem_21a () =
 let regsize = 6 in
 let ( let* ) = Option.bind in
 let (.%()) r i = if i >= 0 && i < regsize then Some (r.(i)) else None in
 let (.%()<-) r i x = if i >= 0 && i < regsize then Some (r.(i)<-x) else None in

 let op_of_string = function
  | "addr" -> 0
  | "addi" -> 1
  | "mulr" -> 2
  | "muli" -> 3
  | "banr" -> 4
  | "bani" -> 5
  | "borr" -> 6
  | "bori" -> 7
  | "setr" -> 8
  | "seti" -> 9
  | "gtir" -> 10
  | "gtri" -> 11
  | "gtrr" -> 12
  | "eqir" -> 13
  | "eqri" -> 14
  | "eqrr" -> 15
  | _ -> (-1) in

 let exec_opt regs op a b c =
  match op with
  | 0  -> (* addr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va+vb
  | 1  -> (* addi *)
    let* va = regs.%(a) in
    regs.%(c) <- va+b
  | 2  -> (* mulr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va*vb
  | 3  -> (* muli *)
    let* va = regs.%(a) in
    regs.%(c) <- va*b
  | 4  -> (* banr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va land vb
  | 5  -> (* bani *)
    let* va = regs.%(a) in
    regs.%(c) <- va land b
  | 6  -> (* borr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va lor vb
  | 7  -> (* bori *)
    let* va = regs.%(a) in
    regs.%(c) <- va lor b
  | 8  -> (* setr *)
    let* va = regs.%(a) in
    regs.%(c) <- va
  | 9  -> (* seti *)
    regs.%(c) <- a
  | 10 -> (* gtir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a > vb then 1 else 0
  | 11 -> (* gtri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va > b then 1 else 0
  | 12 -> (* gtrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va > vb then 1 else 0
  | 13 -> (* eqir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a = vb then 1 else 0
  | 14 -> (* eqri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va = b then 1 else 0
  | 15 -> (* eqrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va = vb then 1 else 0
  | _ -> None (* Invalid OpCode *) in

 let debug = false in
 let (ip_reg, program) =
  let open In_channel in
   with_open_bin
   "21.txt"
   (fun ic ->
    let ic = Scanf.Scanning.from_channel ic in
    let ip_reg = Scanf.bscanf ic "#ip %d" Fun.id in
    let program =
     Seq.of_dispenser (fun () -> Scanf.bscanf_opt ic "%s %d %d %d" (fun s a b c -> op_of_string s, a, b, c)) |>
     Array.of_seq in
    (ip_reg, program)) in
 
 let res = ref (-1) in
 let regs = Array.make regsize 0 in
 regs.(0) <- 0 ;
 while
  !res = (-1) && regs.(ip_reg) >= 0 && regs.(ip_reg) < Array.length program
 do
  let (op,a,b,c) = program.(regs.(ip_reg)) in
  if op = 15 then (Array.iter (fun x -> print_int x ; print_char ' ' ) regs ; print_newline () ; res := regs.(3)) ;
  let _ = exec_opt regs op a b c in
  if debug then (Array.iter (fun x -> print_int x ; print_char ' ' ) regs ; print_newline ()) ;
  regs.(ip_reg) <- regs.(ip_reg) + 1
 done ;
 !res

(* find state @ eqrr 3 0 1 instruction (28)! and set r0 to whatever r3 is right before execution! *)
(* assume there is a steady state loop, find the last number before it cycles! *)
(* 17s on opt *)
let problem_21b () =
 let regsize = 6 in
 let ( let* ) = Option.bind in
 let (.%()) r i = if i >= 0 && i < regsize then Some (r.(i)) else None in
 let (.%()<-) r i x = if i >= 0 && i < regsize then Some (r.(i)<-x) else None in

 let op_of_string = function
  | "addr" -> 0
  | "addi" -> 1
  | "mulr" -> 2
  | "muli" -> 3
  | "banr" -> 4
  | "bani" -> 5
  | "borr" -> 6
  | "bori" -> 7
  | "setr" -> 8
  | "seti" -> 9
  | "gtir" -> 10
  | "gtri" -> 11
  | "gtrr" -> 12
  | "eqir" -> 13
  | "eqri" -> 14
  | "eqrr" -> 15
  | _ -> (-1) in

 let exec_opt regs op a b c =
  match op with
  | 0  -> (* addr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va+vb
  | 1  -> (* addi *)
    let* va = regs.%(a) in
    regs.%(c) <- va+b
  | 2  -> (* mulr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va*vb
  | 3  -> (* muli *)
    let* va = regs.%(a) in
    regs.%(c) <- va*b
  | 4  -> (* banr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va land vb
  | 5  -> (* bani *)
    let* va = regs.%(a) in
    regs.%(c) <- va land b
  | 6  -> (* borr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- va lor vb
  | 7  -> (* bori *)
    let* va = regs.%(a) in
    regs.%(c) <- va lor b
  | 8  -> (* setr *)
    let* va = regs.%(a) in
    regs.%(c) <- va
  | 9  -> (* seti *)
    regs.%(c) <- a
  | 10 -> (* gtir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a > vb then 1 else 0
  | 11 -> (* gtri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va > b then 1 else 0
  | 12 -> (* gtrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va > vb then 1 else 0
  | 13 -> (* eqir *)
    let* vb = regs.%(b) in
    regs.%(c) <- if a = vb then 1 else 0
  | 14 -> (* eqri *)
    let* va = regs.%(a) in
    regs.%(c) <- if va = b then 1 else 0
  | 15 -> (* eqrr *)
    let* va = regs.%(a) in
    let* vb = regs.%(b) in
    regs.%(c) <- if va = vb then 1 else 0
  | _ -> None (* Invalid OpCode *) in

 let debug = false in
 let (ip_reg, program) =
  let open In_channel in
   with_open_bin
   "21.txt"
   (fun ic ->
    let ic = Scanf.Scanning.from_channel ic in
    let ip_reg = Scanf.bscanf ic "#ip %d" Fun.id in
    let program =
     Seq.of_dispenser (fun () -> Scanf.bscanf_opt ic "%s %d %d %d" (fun s a b c -> op_of_string s, a, b, c)) |>
     Array.of_seq in
    (ip_reg, program)) in
 
 let seen = Hashtbl.create (4 * 4096) in
 let res = ref (-1) in
 let continue = ref true in
 let regs = Array.make regsize 0 in
 regs.(0) <- 0 ;
 while
  !continue && regs.(ip_reg) >= 0 && regs.(ip_reg) < Array.length program
 do
  let (op,a,b,c) = program.(regs.(ip_reg)) in
  if op = 15 && Hashtbl.mem seen regs.(3) then (Printf.printf "First Repeat: %d\n" regs.(3) ; continue := false) else
  if op = 15 then ((*Array.iter (fun x -> print_int x ; print_char ' ' ) regs ; print_newline () ; *)Hashtbl.add seen regs.(3) (); res := regs.(3)) ;
  let _ = exec_opt regs op a b c in
  if debug then (Array.iter (fun x -> print_int x ; print_char ' ' ) regs ; print_newline ()) ;
  regs.(ip_reg) <- regs.(ip_reg) + 1
 done ;
 print_string "Unique Values: " ;
 print_int (Hashtbl.length seen) ;
 print_newline () ;
 !res

(* rocky, wet, narrow (0,1,2) *)
let problem_22a () =
 let memo = Hashtbl.create 16384 in

 let rec elevel ((d,y,x) as idx) =
  match Hashtbl.find_opt memo idx with
  | Some res -> res
  | None ->
    if y = 0 then let res = (x * 16807 + d) mod 20183 in Hashtbl.add memo idx res ; res else
    if x = 0 then let res = (y * 48271 + d) mod 20183 in Hashtbl.add memo idx res ; res else
    let res = (elevel (d,(y-1),x) * elevel (d,y,(x-1)) + d) mod 20183 in
    Hashtbl.add memo idx res ; res in
 
 let example = false in

 let (d,ty,tx) =
  let open In_channel in
   with_open_bin
   (if example then "22e.txt" else "22.txt") 
   (fun ic ->
    let ic = Scanf.Scanning.from_channel ic in
    let d = Scanf.bscanf ic "depth: %d" Fun.id in
    let (ty,tx) = Scanf.bscanf ic "target: %d,%d" (fun x y -> y,x) in
    (d,ty,tx)) in
  
 Hashtbl.add memo (d,ty,tx) (elevel (d,0,0)) ;
 let res = ref 0 in
 for y = 0 to ty do
  for x = 0 to tx do
   res := !res + (elevel (d,y,x) mod 3)
  done ;
 done ;
 !res

(* rocky, wet, narrow (0,1,2) *)
(* gear switch time : 7 min *)
let problem_22b () =
 let example = false in

 let (d,ty,tx) =
  let open In_channel in
   with_open_bin
   (if example then "22e.txt" else "22.txt") 
   (fun ic ->
    let ic = Scanf.Scanning.from_channel ic in
    let d = Scanf.bscanf ic "depth: %d" Fun.id in
    let (ty,tx) = Scanf.bscanf ic "target: %d,%d " (fun x y -> y,x) in
    (d,ty,tx)) in

 let memo = Hashtbl.create 16384 in
 let rec elevel ((y,x) as idx) =
  match Hashtbl.find_opt memo idx with
  | Some res -> res
  | None ->
    if y = 0 then let res = (x * 16807 + d) mod 20183 in Hashtbl.add memo idx res ; res else
    if x = 0 then let res = (y * 48271 + d) mod 20183 in Hashtbl.add memo idx res ; res else
    let res = (elevel (y-1,x) * elevel (y,x-1) + d) mod 20183 in
    Hashtbl.add memo idx res ; res in
 Hashtbl.add memo (ty,tx) (elevel (0,0)) ;

 let module State = struct
  type gear_t = Torch | Climb | Neither
  type terrain_t = Rock | Wet | Narrow
  type t = {y : int; x : int ; g : gear_t ; t : int}
  let compare s0 s1 = compare s0.t s1.t
  let terrain_of_int n = match n mod 3 with 0 -> Rock | 1 -> Wet | 2 -> Narrow | _ -> assert false
  let terrain_to_string = function Rock -> "Rock" | Wet -> "Wet" | Narrow -> "Narrow"
  let gear_to_string = function Torch -> "Torch" | Climb -> "Climb" | Neither -> "Neither"
  let to_string {y;x;g;t} = Printf.sprintf "Y: %d, X: %d, G: %s, T: %d" y x (gear_to_string g) t
  let to_key s0 = (s0.y,s0.x,s0.g)

  let try_move s0 (y,x) =
   (* assume we aren't teleporting to save checking computation *)
   if y < 0 || x < 0 then None else
   (match (elevel (y,x) |> terrain_of_int), s0.g with
   | Rock, Climb
   | Rock, Torch
   | Wet, Climb
   | Wet, Neither
   | Narrow, Torch
   | Narrow, Neither -> Some ({s0 with y;x;t = s0.t+1})
   | _ -> None)

  let switch_gear s0 =
   let g' =
    (match (elevel (s0.y,s0.x) |> terrain_of_int), s0.g with
     | Rock, Climb -> Torch
     | Rock, Torch -> Climb
     | Wet, Climb -> Neither
     | Wet, Neither -> Climb
     | Narrow, Torch -> Neither
     | Narrow, Neither-> Torch
     | _ -> raise (Invalid_argument "Invalid Gear for Terrain")) in
   {s0 with g = g'; t=s0.t+7}
   
  let next s0 =
   (switch_gear s0) ::
   ([s0.y-1,s0.x
    ;s0.y+1,s0.x
    ;s0.y,s0.x-1
    ;s0.y,s0.x+1] |>
    List.filter_map (try_move s0))
 end in
  
 let seen = Hashtbl.create 16384 in
 let s0 = State.{y=0;x=0;g=Torch;t=0} in
 let kfinal = (ty,tx,State.Torch) in
 let res = ref None in
 let module PQ = Pqueue.MakeMin(State) in
 let pq = PQ.create () in
 PQ.add pq s0 ;

 while
  not (Option.is_some !res|| PQ.is_empty pq)
 do
  let s = PQ.pop_min pq |> Option.get in
  let k = State.to_key s in
  if k = kfinal then res := Some (s.t) else
  match Hashtbl.find_opt seen k with
  | Some t when s.t >= t -> ()
  | _ ->
    Hashtbl.replace seen k (s.t) ;
    State.next s |> PQ.add_iter pq (List.iter)
 done ;
 !res

let problem_23a () =
 let module Bot = struct
  type t = {x: int; y: int; z: int; r: int}
  let compare b0 b1 = compare b0.r b1.r
  let dist b0 b1 =
   abs (b1.x - b0.x) +
   abs (b1.y - b0.y) +
   abs (b1.z - b0.z)
  let to_string {x;y;z;r} = Printf.sprintf "pos=<%d,%d,%d>, r=%d" x y z r
  let of_string s = Scanf.sscanf s "pos=<%d,%d,%d>, r=%d" (fun x y z r -> {x;y;z;r})
 end in
 let example = false in
 let bots =
  In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_lines) |> List.map Bot.of_string in
 let maxbot =
  List.fold_left
   Bot.(fun a b -> if compare a b < 0 then b else a)
   (List.hd bots) (List.tl bots) in
 print_string "maxbot: " ;
 print_endline Bot.(to_string maxbot) ;
 List.fold_left
  Bot.(fun a b -> if dist maxbot b > maxbot.r then a else succ a)
  0 bots

(* first think in 1d, then 2d, then 3d *)
(* try overlapping subdivision: x,y,z,r -> x+-r/3,y,z,2r; x,y+-r/3,z,2r ; x,y,z+-r/3,2r *)
let problem_23b () =
 let module Bot = struct
  type t = {x: int; y: int; z: int; r: int}
  let dist0 b = abs (b.x) + abs (b.y) + abs (b.z)
  let dist b0 b1 =
   abs (b1.x - b0.x) +
   abs (b1.y - b0.y) +
   abs (b1.z - b0.z)
  let to_string {x;y;z;r} = Printf.sprintf "pos=<%d,%d,%d>, r=%d" x y z r
  let of_string s = Scanf.sscanf s "pos=<%d,%d,%d>, r=%d" (fun x y z r -> {x;y;z;r})
  let key blist b0 = (* min priority *)
   let mag = 
    List.fold_left
    (* increase the magnitude of the score iff
       there exists a point in the "search" space that is in range of both bots
       i.e., iff they intersect at at least one point,
       regardless whether they are in range of each other *)
    (fun a b -> if dist b b0 <= (b.r + b0.r) then succ a else a) 0 blist in
    (* maximize magnitude, minimize radius, minimize closest distance to origin *)
   (~-mag, b0.r, dist0 b0 - b0.r)
(*
  (* for hardcoded values for splitting r < 3 *)
  let points r =
   let range = Seq.ints (~-r) |> Seq.take (2*r+1) in
   Seq.product range (Seq.product range range) |>
   Seq.map (fun (x,(y,z)) -> x,y,z) |>
   Seq.filter (fun (x,y,z) -> abs x + abs y + abs z <= r) |>
   List.of_seq
*)
  let split {x;y;z;r} =
   let r' = (2 * (r+1)) / 3 in (* integer ceiling math to ensure no gaps *)
   if r >= 3 then
    [ {x = x + r / 3; y; z; r = r'}
    ; {x = x - r / 3; y; z; r = r'}
    ; {x; y = y + r / 3; z; r = r'}
    ; {x; y = y - r / 3; z; r = r'}
    ; {x; y; z = z + r / 3; r = r'}
    ; {x; y; z = z - r / 3; r = r'} ]
   else if r = 1 then
    [(0, 0, -1); (0, -1, 0); (-1, 0, 0); (0, 0, 0); (1, 0, 0); (0, 1, 0); (0, 0, 1)] |>
    List.map (fun (x',y',z') -> {x=x+x';y=y+y';z=z+z';r=0})
   else if r = 2 then
    [(0, 0, -2)  ;(0, -1, -1) ;(0, -2, 0) ;(-1, 0, -1) ;(0, 0, -1)
    ;(-1, -1, 0) ;(1, 0, -1)  ;(0, -1, 0) ;(1, -1, 0)  ;(-2, 0, 0)
    ;(0, 1, -1)  ;(-1, 0, 0)  ;(0, 0, 0)  ;(1, 0, 0)   ;(0, -1, 1)
    ;(2, 0, 0)   ;(-1, 1, 0)  ;(0, 1, 0)  ;(-1, 0, 1)  ;(1, 1, 0)
    ;(0, 0, 1)   ;(1, 0, 1)   ;(0, 2, 0)  ;(0, 1, 1)   ;(0, 0, 2)] |>
    List.map (fun (x',y',z') -> {x=x+x';y=y+y';z=z+z';r=0})
   else []
 end in
 let example = false in
 let debug = false in
 let bots =
  In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_lines) |> List.map Bot.of_string in
 let megabot =
  Bot.{x = 0 ; y = 0 ; z = 0 ; r =
   List.fold_left
   Bot.(fun a b -> max a (dist0 b + b.r)) 0 bots } in
 if debug then print_endline Bot.(to_string megabot) ;

 let module PQ = Pqueue.MakeMin(struct type t = (int * int * int) * Bot.t let compare (a,_) (b,_) = compare a b end) in
 let pq = PQ.create () in
 PQ.add pq Bot.(key bots megabot, megabot) ;
 let res = ref None in
 (* hashtbl is a necessary speedup for the example, but not the full input *)
 let seen = Hashtbl.create 16384 in
 while 
  not (Option.is_some !res || PQ.is_empty pq)
 do
  let (k,bot) = PQ.pop_min pq |> Option.get in
  if debug && bot.r < 10 then print_endline (Bot.to_string bot) ;
  if bot.r = 0 then res := Some (k,bot) else
  Bot.split bot |>
  List.filter (Fun.negate (Hashtbl.mem seen)) |>
  List.filter (fun bot -> Hashtbl.add seen bot () ; true) |>
  List.map Bot.(fun b -> key bots b, b) |> PQ.add_iter pq (List.iter)
 done ;
 let ((score,_,_),res) = !res |> Option.get in
 Printf.printf "%s\n:: Bots in Range: %d, Distance from Origin %d\n" (Bot.to_string res) (~-score) (Bot.dist0 res)

let problem_24a () =
 let ( let* ) = Option.bind in
 let module Unit = struct
  type t = {mutable n : int; hp : int; weak : string list; immunity : string list; atk : int; atk_type : string; initiative : int}

  let compare u0 u1 =
   compare (u0.n * u0.atk, u0.initiative) (u1.n * u1.atk, u1.initiative)

  let dmg u0 u1 =
   let factor =
    if List.mem u0.atk_type u1.immunity then 0 else
    if List.mem u0.atk_type u1.weak then 2 else 1 in
   factor * u0.n * u0.atk

  let target_compare (a,u0) (b,u1) =
   let c = Int.compare a b in
   if c <> 0 then c else
   let c = compare u0 u1 in
   if c <> 0 then c else
   Stdlib.compare u0 u1

  let apply u0 u1 =
   let deaths = dmg u0 u1 / u1.hp in
   u1.n <- max 0 (u1.n - deaths)
   
  let to_string x =
   let buf = Buffer.create 80 in
   x.weak |> List.iter (fun s -> Buffer.add_string buf s ; Buffer.add_char buf ','; Buffer.add_char buf ' ') ;
   Buffer.truncate buf (max 0 (Buffer.length buf - 2));
   let ws = Buffer.contents buf in
   Buffer.clear buf ;
   x.immunity |> List.iter (fun s -> Buffer.add_string buf s ; Buffer.add_char buf ',' ; Buffer.add_char buf ' ') ;
   Buffer.truncate buf (max 0 (Buffer.length buf - 2));
   let imms = Buffer.contents buf in
   Printf.sprintf
    "n: %d; hp: %d; weak: [%s]; immunity: [%s]; atk: %d; atk_type: %s; initiative: %d"
    x.n x.hp ws imms x.atk x.atk_type x.initiative

  let of_string s =
   let (~weak,~immunity) =
   (match String.index_opt s '(' with
   | None -> (~weak:[],~immunity:[])
   | Some l ->
     let r = String.index_from s l ')' in
     let s' = String.sub s (l+1) (r-l-1) in
     let opts = String.split_on_char ';' s' |> List.map (String.trim) in
     let immunity =
      let* s =
       List.filter (String.starts_with ~prefix:"immune to") opts |>
       (function [a] -> Some a | _ -> None) in
      let immune_len = String.length "immune to" in
      String.sub s immune_len (String.length s - immune_len) |>
      String.split_on_char ',' |> List.map String.trim |> Option.some in
     let weak =
      let* s =
       List.filter (String.starts_with ~prefix:"weak to") opts |>
       (function [a] -> Some a | _ -> None) in
      let weak_len = String.length "weak to" in
      String.sub s weak_len (String.length s - weak_len) |>
      String.split_on_char ',' |> List.map String.trim |> Option.some in
     let immunity = Option.value ~default:[] immunity
     and weak = Option.value ~default:[] weak in
     (~weak,~immunity)) in
   let words = String.split_on_char ' ' s in
   let words_len = List.length words in
   let n = int_of_string (List.nth words 0) in
   let hp = int_of_string (List.nth words 4) in
   let initiative = int_of_string (List.nth words (words_len - 1)) in
   let atk = int_of_string (List.nth words (words_len - 6)) in
   let atk_type = (List.nth words (words_len - 5)) in
   {n;hp;weak;immunity;atk;atk_type;initiative}

 end in

 let module UnitQ = Pqueue.MakeMax(struct type t = int * Unit.t let compare (_,u0) (_,u1) = Unit.compare u0 u1 end) in
 let module AtkQ =  Pqueue.MakeMax(struct type t = int * Unit.t * (Unit.t option) let compare : t -> t -> int = fun (_,u0,_) (_,u1,_) -> compare u0.initiative u1.initiative end) in

 let example = false in
 let input =
  let open In_channel in
   with_open_bin
   (if example then "24e.txt" else "24.txt")
   input_lines in
 let split_point =
  List.find_index ((=)"") input |> Option.get in
 let team_imm = input |> List.take split_point |> List.drop 1 |> List.map Unit.of_string in
 let team_inf = input |> List.drop (split_point+2) |> List.map Unit.of_string in

 let unitq =
  team_imm |> List.map (fun u -> (0,u)) |> UnitQ.of_list in
  team_inf |> List.map (fun u -> (1,u)) |>
  UnitQ.add_iter unitq List.iter ;
  
 let targets_imm = Dynarray.create () in
 let targets_inf = Dynarray.create () in
 let atkq = AtkQ.create () in
 let run unitq  =
  AtkQ.clear atkq ;
  Dynarray.clear targets_imm ;
  Dynarray.clear targets_inf ;
  UnitQ.iter_unordered
  (fun (team,u) ->
   if u.n = 0 then () else
   if team = 0
   then Dynarray.add_last targets_imm u
   else Dynarray.add_last targets_inf u) unitq ;
  if Dynarray.is_empty targets_imm then
   Some (1,Unit.(Dynarray.fold_left (fun a u -> a + u.n) 0 targets_inf))
  else if Dynarray.is_empty targets_inf then
   Some (0,Unit.(Dynarray.fold_left (fun a u -> a + u.n) 0 targets_imm))
  else begin
   while
    while match UnitQ.max_elt unitq with Some (_,u) -> u.n = 0 | _ -> false do UnitQ.remove_max unitq done ;
    not (UnitQ.is_empty unitq)
   do
    let (team, u0) = UnitQ.pop_max unitq |> Option.get in
    let candidate =
      Dynarray.fold_left
      (fun umax u -> match umax with None -> Some u | Some umax ->
       if Unit.target_compare (Unit.dmg u0 u, u) (Unit.dmg u0 umax, umax) > 0 then Some u else Some umax)
      None (if team = 0 then targets_inf else targets_imm) |>
      (function Some u -> if Unit.dmg u0 u = 0 then None else Some u | x -> x) in
    (match candidate with
    | None -> ()
    | Some u ->
       let targets = if team = 0 then targets_inf else targets_imm in
       let i = Dynarray.find_index ((=)u) targets |> Option.get in
       let u' = Dynarray.pop_last targets in
       if i >= Dynarray.length targets then () else Dynarray.set targets i u') ;
    AtkQ.add atkq (team, u0, candidate)
   done ;
   UnitQ.clear unitq ;
   let stale = ref 0 in
   let len = AtkQ.length atkq in
   while not (AtkQ.is_empty atkq) do
    let (team,atk,def) = AtkQ.pop_max atkq |> Option.get in
    (match def with
    | None -> incr stale
    | Some def ->
      if Unit.dmg atk def < def.hp then incr stale ;
      Unit.apply atk def) ;
    UnitQ.add unitq (team,atk)
   done ;
   if !stale = len then Some (-1,0) else None
  end in

 let rec loop () =
  match run unitq with
  | None -> loop ()
  | Some n -> n in
 loop ()

let problem_24b () =
 let ( let* ) = Option.bind in
 let module Unit = struct
  type t = {mutable n : int; hp : int; weak : string list; immunity : string list; atk : int; atk_type : string; initiative : int}

  let compare u0 u1 =
   compare (u0.n * u0.atk, u0.initiative) (u1.n * u1.atk, u1.initiative)

  let dmg u0 u1 =
   let factor =
    if List.mem u0.atk_type u1.immunity then 0 else
    if List.mem u0.atk_type u1.weak then 2 else 1 in
   factor * u0.n * u0.atk

  let target_compare (a,u0) (b,u1) =
   let c = Int.compare a b in
   if c <> 0 then c else
   let c = compare u0 u1 in
   if c <> 0 then c else
   Stdlib.compare u0 u1

  let apply u0 u1 =
   let deaths = dmg u0 u1 / u1.hp in
   u1.n <- max 0 (u1.n - deaths)
   
  let to_string x =
   let buf = Buffer.create 80 in
   x.weak |> List.iter (fun s -> Buffer.add_string buf s ; Buffer.add_char buf ','; Buffer.add_char buf ' ') ;
   Buffer.truncate buf (max 0 (Buffer.length buf - 2));
   let ws = Buffer.contents buf in
   Buffer.clear buf ;
   x.immunity |> List.iter (fun s -> Buffer.add_string buf s ; Buffer.add_char buf ',' ; Buffer.add_char buf ' ') ;
   Buffer.truncate buf (max 0 (Buffer.length buf - 2));
   let imms = Buffer.contents buf in
   Printf.sprintf
    "n: %d; hp: %d; weak: [%s]; immunity: [%s]; atk: %d; atk_type: %s; initiative: %d"
    x.n x.hp ws imms x.atk x.atk_type x.initiative

  let of_string s =
   let (~weak,~immunity) =
   (match String.index_opt s '(' with
   | None -> (~weak:[],~immunity:[])
   | Some l ->
     let r = String.index_from s l ')' in
     let s' = String.sub s (l+1) (r-l-1) in
     let opts = String.split_on_char ';' s' |> List.map (String.trim) in
     let immunity =
      let* s =
       List.filter (String.starts_with ~prefix:"immune to") opts |>
       (function [a] -> Some a | _ -> None) in
      let immune_len = String.length "immune to" in
      String.sub s immune_len (String.length s - immune_len) |>
      String.split_on_char ',' |> List.map String.trim |> Option.some in
     let weak =
      let* s =
       List.filter (String.starts_with ~prefix:"weak to") opts |>
       (function [a] -> Some a | _ -> None) in
      let weak_len = String.length "weak to" in
      String.sub s weak_len (String.length s - weak_len) |>
      String.split_on_char ',' |> List.map String.trim |> Option.some in
     let immunity = Option.value ~default:[] immunity
     and weak = Option.value ~default:[] weak in
     (~weak,~immunity)) in
   let words = String.split_on_char ' ' s in
   let words_len = List.length words in
   let n = int_of_string (List.nth words 0) in
   let hp = int_of_string (List.nth words 4) in
   let initiative = int_of_string (List.nth words (words_len - 1)) in
   let atk = int_of_string (List.nth words (words_len - 6)) in
   let atk_type = (List.nth words (words_len - 5)) in
   {n;hp;weak;immunity;atk;atk_type;initiative}

 end in

 let module UnitQ = Pqueue.MakeMax(struct type t = int * Unit.t let compare (_,u0) (_,u1) = Unit.compare u0 u1 end) in
 let module AtkQ =  Pqueue.MakeMax(struct type t = int * Unit.t * (Unit.t option) let compare : t -> t -> int = fun (_,u0,_) (_,u1,_) -> compare u0.initiative u1.initiative end) in

 let example = false in
 let input =
  let open In_channel in
   with_open_bin
   (if example then "24e.txt" else "24.txt")
   input_lines in
 let split_point =
  List.find_index ((=)"") input |> Option.get in
 let team_imm = input |> List.take split_point |> List.drop 1 |> List.map Unit.of_string in
 let team_inf = input |> List.drop (split_point+2) |> List.map Unit.of_string in

 let load_with_boost unitq n =
  UnitQ.clear unitq ;
  team_imm |> List.map (fun u -> (0,Unit.{u with atk = u.atk + n})) |>
  UnitQ.add_iter unitq List.iter ;
  team_inf |> List.map (fun u -> (1,Unit.{u with atk = u.atk})) |>
  UnitQ.add_iter unitq List.iter in

 let unitq = UnitQ.create () in
 load_with_boost unitq 0;
  
 let targets_imm = Dynarray.create () in
 let targets_inf = Dynarray.create () in
 let atkq = AtkQ.create () in
 let run unitq  =
  AtkQ.clear atkq ;
  Dynarray.clear targets_imm ;
  Dynarray.clear targets_inf ;
  UnitQ.iter_unordered
  (fun (team,u) ->
   if u.n = 0 then () else
   if team = 0
   then Dynarray.add_last targets_imm u
   else Dynarray.add_last targets_inf u) unitq ;
  if Dynarray.is_empty targets_imm then
   Some (1,Unit.(Dynarray.fold_left (fun a u -> a + u.n) 0 targets_inf))
  else if Dynarray.is_empty targets_inf then
   Some (0,Unit.(Dynarray.fold_left (fun a u -> a + u.n) 0 targets_imm))
  else begin
   while
    while match UnitQ.max_elt unitq with Some (_,u) -> u.n = 0 | _ -> false do UnitQ.remove_max unitq done ;
    not (UnitQ.is_empty unitq)
   do
    let (team, u0) = UnitQ.pop_max unitq |> Option.get in
    (*Printf.printf "Up At Bat: Team: %d - Unit: %d\n" team (u0.n) ;*)
    let candidate =
      Dynarray.fold_left
      (fun umax u -> match umax with None -> Some u | Some umax ->
       if Unit.target_compare (Unit.dmg u0 u, u) (Unit.dmg u0 umax, umax) > 0 then Some u else Some umax)
      None (if team = 0 then targets_inf else targets_imm) |>
      (function Some u -> if Unit.dmg u0 u = 0 then None else Some u | x -> x) in
    (match candidate with
    | None -> ()
    | Some u ->
       let targets = if team = 0 then targets_inf else targets_imm in
       let i = Dynarray.find_index ((=)u) targets |> Option.get in
       let u' = Dynarray.pop_last targets in
       if i >= Dynarray.length targets then () else Dynarray.set targets i u') ;
    AtkQ.add atkq (team, u0, candidate)
   done ;
   UnitQ.clear unitq ;
   let stale = ref 0 in
   let len = AtkQ.length atkq in
   while not (AtkQ.is_empty atkq) do
    let (team,atk,def) = AtkQ.pop_max atkq |> Option.get in
    (match def with
    | None -> incr stale
    | Some def ->
      if Unit.dmg atk def < def.hp then incr stale ;
      (*Printf.printf "%d atks %d with %d dmg\n" atk.n def.n (Unit.dmg atk def) ;*)
      Unit.apply atk def) ;
    UnitQ.add unitq (team,atk)
   done ;
   if !stale = len then Some (-1,0) else None
  end in
 let rec loop boost =
  match run unitq with
  | None -> loop boost
  | Some (t,n) when t <> 0 ->
    load_with_boost unitq (boost+1) ;  
    loop (boost+1)
  | Some (t,n) -> (boost,n) in
 loop 0

let problem_25a () =
 let example = false in
 let debug = false in
 let max_branch_len = 3 in
 let stars =
  let open In_channel in
  with_open_bin (if example then "25e.txt" else "25.txt")
  (fun ic ->
   let ic = Scanf.Scanning.from_channel ic in
   Seq.of_dispenser (fun () -> Scanf.bscanf_opt ic " %d , %d , %d , %d " (fun x y z t -> (x,y,z,t))) |>
   Array.of_seq) in
 let dist (x,y,z,t) (x',y',z',t') = abs (x-x') + abs (y-y') + abs (z-z') + abs (t-t') in
 let len = Array.length stars in

 let trimat = Array.init len (fun i -> Array.make (len - i) 0) in
(*
 (* easymodo *)
 let check x y =
  let x = min x y and y = max x y in
  trimat.(x).(y-x) = 1 in
*)
 let mark stk row n =
  Stack.clear stk ;
  Stack.push (row,n) stk ;
  let rec loop () =
   match Stack.pop_opt stk with
   | None -> ()
   | Some (row,n) when trimat.(row).(0) <> 0 -> loop ()
   | Some (row,n) ->
     trimat.(row).(0) <- n ;
(*
     (* easymodo *)
     for col = len - 1 downto 0 do
      if col <> row && check row col then Stack.push (col,n) stk
     done ; loop ()
*)
     for i = row downto 1 do
      if trimat.(row-i).(i) = 1 then Stack.push (row-i,n) stk
     done ;
     for i = Array.length trimat.(row) - 1 downto 1 do
      if trimat.(row).(i) = 1 then
       Stack.push (row+i,n) stk
     done ; loop ()
   in loop () in

 for row = 0 to len - 1 do
  for col = 1 to len - row - 1 do
   if dist stars.(row) stars.(row+col) <= max_branch_len then trimat.(row).(col) <- 1 else ()
  done
 done ;

 let stk = Stack.create () in
 let count = ref 0 in
 for row = 0 to len - 1 do
  if trimat.(row).(0) = 0 then (incr count ; mark stk row !count)
 done ;

 if debug then (
  for row = 0 to len - 1 do
   for i = 0 to row do (print_char ' ' ; print_char ' ') done ;
   for col = 0 to len - row - 1 do
    print_int trimat.(row).(col) ; print_char ' '
   done ; print_newline ()
  done
 ) ;
 !count

let problem_25a2 () =
 let example = false in
 let debug = false in
 let max_branch_len = 3 in
 let stars =
  let open In_channel in
  with_open_bin (if example then "25e.txt" else "25.txt")
  (fun ic ->
   let ic = Scanf.Scanning.from_channel ic in
   Seq.of_dispenser (fun () -> Scanf.bscanf_opt ic " %d , %d , %d , %d " (fun x y z t -> (x,y,z,t))) |>
   Array.of_seq) in
 let dist (x,y,z,t) (x',y',z',t') = abs (x-x') + abs (y-y') + abs (z-z') + abs (t-t') in
 let len = Array.length stars in

 let trimat = Array.init len (fun i -> Array.make (len - i) 0) in
 let module ISet = Set.Make(Int) in

 let mark row n =
  let stk = ref (ISet.singleton row) in
  let rec loop () =
   match ISet.choose_opt !stk with
   | None -> ()
   | Some row when trimat.(row).(0) <> 0 -> stk := ISet.remove row !stk ; loop ()
   | Some row ->
     stk := ISet.remove row !stk ;
     trimat.(row).(0) <- n ;
     (* trace triangular column *)
     for i = row downto 1 do
      if trimat.(row-i).(i) = 1 && trimat.(row-i).(0) = 0 then stk := ISet.add (row-i) !stk
     done ;
     (* trace row *)
     for i = Array.length trimat.(row) - 1 downto 1 do
      if trimat.(row).(i) = 1 then
       stk := ISet.add (row+i) !stk
     done ; loop ()
   in loop () in

 for row = 0 to len - 1 do
  for col = 1 to len - row - 1 do
   if dist stars.(row) stars.(row+col) <= max_branch_len then trimat.(row).(col) <- 1 else ()
  done
 done ;

 let count = ref 0 in
 for row = 0 to len - 1 do
  if trimat.(row).(0) = 0 then (incr count ; mark row !count)
 done ;

 if debug then (
  for row = 0 to len - 1 do
   for i = 0 to row do (print_char ' ' ; print_char ' ') done ;
   for col = 0 to len - row - 1 do
    print_int trimat.(row).(col) ; print_char ' '
   done ; print_newline ()
  done
 ) ;
 !count
