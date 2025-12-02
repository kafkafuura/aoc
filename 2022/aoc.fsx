(* Advent of Code 2022 - Selected Problems (F#) *)

module B =
 type t =
  { o4o : int ; o4c : int ; o4obs : int ; o4g : int
  ; c4obs : int; obs4g : int; max_o : int }
 let of_string (s : string) =
  let inputs =
   s.Split([|' ';':'|]) |>
   Array.filter (fun s -> s.Length > 0 && System.Char.IsAsciiDigit(s[0])) |>
   Array.map (fun s -> System.Int32.Parse(s)) in
  (* Scanf String *)
  (* "Blueprint %u: Each ore robot costs %u ore. Each clay robot costs %u ore. Each obsidian robot costs %u ore and %u clay. Each geode robot costs %u ore and %u obsidian." *)
  assert (Array.length inputs = 7) ;
  { o4o = inputs[1]
  ; o4c = inputs[2]
  ; o4obs = inputs[3]
  ; c4obs = inputs[4]
  ; o4g = inputs[5]
  ; obs4g = inputs[6]
  ; max_o = List.fold max inputs[1] [inputs[2]; inputs[3]; inputs[5]] }

module S =
 type t = 
  { o : int; c : int; obs: int; g : int
  ; o_r : int; c_r : int; obs_r : int; g_r : int}
 type action = Wait | BuildO | BuildC | BuildOBS | BuildG
 let init = { o = 0; c = 0; obs = 0; g = 0; o_r = 1; c_r = 0; obs_r = 0; g_r = 0}
 let step s =
  { s with
      o   = s.o   + s.o_r
      c   = s.c   + s.c_r
      obs = s.obs + s.obs_r
      g   = s.g   + s.g_r }
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
 let to_string s = sprintf "%d %d %d %d | %d %d %d %d" s.o s.c s.obs s.g s.o_r s.c_r s.obs_r s.g_r

let problem_19a () =
 let example = false in
 let time_limit = 24 in
 let blueprints =
  System.IO.File.ReadLines(if example then "19e.txt" else "19.txt") |>
  Seq.map B.of_string |> Seq.toArray in
 let cache = new System.Collections.Generic.HashSet<int * S.t>() in
 let blueprint_score (bpt : B.t) =
  let mutable max_g = 0 in
  cache.Clear() ;
  let simplify_state (s : S.t) t =
   let s =
    { s with
        o_r   = min s.o_r bpt.max_o
        c_r   = min s.c_r bpt.c4obs 
        obs_r = min s.obs_r bpt.obs4g } in
    { s with
        o     = min s.o   (max (bpt.max_o) (t * bpt.max_o - s.o_r * (t-1)))
        c     = min s.c   (max (bpt.c4obs) (t * bpt.c4obs - s.c_r * (t-1)))
        obs   = min s.obs (max (bpt.obs4g) (t * bpt.obs4g - s.obs_r * (t-1)))} in
  let rec loop t (s : S.t) =
   if t = 0 then max_g <- max s.g max_g else
   if cache.Contains((t,s)) then () else
    cache.Add((t,s)) |> ignore ;
    S.action_list |> List.filter (S.can_act bpt s) |>
    List.iter (fun a -> loop (t-1) (simplify_state (S.act bpt s a) t))
   in loop time_limit S.init ;
  max_g in
 let res = Array.create (Array.length blueprints) 0 in
 for i = 1 to Array.length blueprints do
  res[i-1] <- blueprint_score blueprints[i-1] ;
  printfn "%02d/%02d : %d" i (Array.length blueprints) res[i-1] ;
 done ;
 Array.toSeq res |> Seq.zip (Seq.unfold (fun a -> Some(a,a+1)) 1) |> Seq.fold (fun a (i,g) -> i*g+a) 0

let problem_19b () =
 let example = false in
 let time_limit = 32 in
 let blueprints =
  System.IO.File.ReadLines(if example then "19e.txt" else "19.txt") |>
  Seq.map B.of_string |> Seq.take 3 |> Seq.toArray in
 let cache = new System.Collections.Generic.HashSet<int * S.t>() in
 let blueprint_score (bpt : B.t) =
  let mutable max_g = 0 in
  cache.Clear() ;
  let simplify_state (s : S.t) t =
   let s =
    { s with
        o_r   = min s.o_r bpt.max_o
        c_r   = min s.c_r bpt.c4obs 
        obs_r = min s.obs_r bpt.obs4g } in
    { s with
        o     = min s.o   (max (bpt.max_o) (t * bpt.max_o - s.o_r * (t-1)))
        c     = min s.c   (max (bpt.c4obs) (t * bpt.c4obs - s.c_r * (t-1)))
        obs   = min s.obs (max (bpt.obs4g) (t * bpt.obs4g - s.obs_r * (t-1)))} in
  let rec loop t (s : S.t) =
   if t = 0 then max_g <- max s.g max_g else
   if cache.Contains((t,s)) then () else
    cache.Add((t,s)) |> ignore ;
    S.action_list |> List.filter (S.can_act bpt s) |>
    List.iter (fun a -> loop (t-1) (simplify_state (S.act bpt s a) t))
   in loop time_limit S.init ;
  max_g in
 let res = Array.create (Array.length blueprints) 0 in
 for i = 1 to Array.length blueprints do
  res[i-1] <- blueprint_score blueprints[i-1] ;
  printfn "%02d/%02d : %d" i (Array.length blueprints) res[i-1] ;
 done;
 Array.fold ( * ) 1 res

module Dir =
 type t = North | South | West | East
 let cycle = [|North;South;West;East|]
 let priority_seq_of_round r = Seq.initInfinite (fun i -> cycle[i%4]) |> Seq.skip ((r-1) % 4) |> Seq.take 4
 (* neighbors: CW from (-1,-1) *)
 let valid_of_neighbors dir ns =
  match dir with
  | North -> Seq.take 3 ns |> Seq.exists id |> not
  | South -> Seq.skip 4 ns |> Seq.take 3 |> Seq.exists id |> not
  | East ->  Seq.skip 2 ns |> Seq.take 3 |> Seq.exists id |> not
  | West ->
    let hd = Seq.take 1 ns in
    Seq.skip 6 ns |> Seq.append hd |> Seq.exists id |> not
 let neighbors_of_set (y,x) (s : System.Collections.Generic.HashSet<int * int>) =
  [-1,-1;-1,0;-1,1;0,1;1,1;1,0;1,-1;0,-1] |>
  List.map (fun pt -> (fst pt + y, snd pt + x)) |> List.toSeq |>
  Seq.map (fun pt -> s.Contains(pt)) 
 let shift (y,x) = function
  | North -> (y-1,x)
  | South -> (y+1,x)
  | East  -> (y,x+1)
  | West  -> (y,x-1)

let problem_23b () =
 let example = false in
 let input = System.IO.File.ReadLines(if example then "23e.txt" else "23.txt") |> Seq.toArray in
 let map = new System.Collections.Generic.HashSet<int * int>() in
 let collision_map = new System.Collections.Generic.Dictionary<int * int, option<int * int>>() in
 for y = 0 to Array.length input - 1 do
  for x = 0 to String.length input[y] - 1 do
   if input.[y].[x] = '#' then map.Add((y,x)) |> ignore
  done
 done ;
 let queue_move pt set rnd =
  let ns = Dir.neighbors_of_set pt set in
  let move =
   if Seq.exists id ns |> not then Seq.empty else
   Dir.priority_seq_of_round rnd |>
   Seq.filter (fun dir -> Dir.valid_of_neighbors dir ns) |>
   Seq.truncate 1 in
  match Seq.tryHead move with
  | Some dir ->
    if   collision_map.ContainsKey(Dir.shift pt dir)
    then (collision_map.Remove(Dir.shift pt dir) |> ignore; collision_map.Add(Dir.shift pt dir,None) |> ignore)
    else collision_map.Add(Dir.shift pt dir, Some pt)
  | None -> () in
 let run_round rnd =
  collision_map.Clear() ;
  map |> Seq.iter (fun pt -> queue_move pt map rnd) ;
  collision_map |>
  Seq.iter
   (fun (*dst src_wrapped*) kvp -> match kvp.Value with None -> () | Some src -> map.Remove(src) |> ignore ; map.Add(kvp.Key) |> ignore) in

 (* TODO: write code that calls a .NET function with out values to confirm usage *)

 let i = ref 1 in
 let run = ref true in
 let before = map |> Seq.toArray in
 let after = Array.create (map.Count) (0,0) in
 while run.Value do
  run_round i.Value ;
  if i.Value &&& 1 = 0 then map.CopyTo(before) else map.CopyTo(after) ;
  if before = after then run.Value <- false else i.Value <- i.Value + 1
 done ;
 i.Value

exception Break of int

let problem_24b () =
 let example = false in
 let input = System.IO.File.ReadLines (if example then "24e.txt" else "24.txt") |> Seq.toArray in
 let h = Array.length input in
 let w = String.length input.[0] in
 let ww = w - 2 in
 let wh = h - 2 in
 let westerlies = Array.init (wh) (fun _ -> Array.create (ww) false) in
 let easterlies = Array.init (wh) (fun _ -> Array.create (ww) false) in
 let northerlies = Array.init (ww) (fun _ -> Array.create (wh) false) in
 let southerlies = Array.init (ww) (fun _ -> Array.create (wh) false) in
 let pmod a b = if a % b < 0 then a % b + b else a % b in
 let w_at y x rnd =
  westerlies.[y-1].[pmod (x-1-rnd) ww]
 let e_at y x rnd =
  easterlies.[y-1].[pmod (x-1+rnd) ww] in
 let n_at y x rnd =
  northerlies.[x-1].[pmod (y-1-rnd) wh] in
 let s_at y x rnd =
  southerlies.[x-1].[pmod (y-1+rnd) wh] in
 (* initialize winds *)
 for y = 1 to h - 2 do
  for x = 1 to w - 2 do
   match input.[y].[x] with
   | '>' -> westerlies.[y-1].[x-1] <- true
   | '<' -> easterlies.[y-1].[x-1] <- true
   | 'v' -> northerlies.[x-1].[y-1] <- true
   | '^' -> southerlies.[x-1].[y-1] <- true
   | _ -> ()
  done
 done;
 (* exceptions must be declared at module level in F# *)
 (*let exception Break of int in*)
 let start = ref (0,1) in
 let dest = ref (h-1,w-2) in
 let valid (y,x,r) =
  if (y,x) = start.Value then true else
  if (y,x) = dest.Value then raise (Break r) else
  if y <= 0 || y >= h - 1 || x <= 0 || x >= w - 1 then false else
  not (w_at y x r || e_at y x r || n_at y x r || s_at y x r) in
 let valid_points (y,x,r) = 
  [-1,0;0,0;1,0;0,-1;0,1] |>
  Seq.map (fun (dy,dx) -> (y+dy,x+dx,r)) |>
  Seq.filter valid in

 (* y, x, step *)
 let move_queue = new System.Collections.Generic.Queue<int * int * int>() in

 (* lcd of input is fixed @ <= 600 *)
 let lcd = 600 in

 let visit_cache = Array.create (lcd*wh*ww) false in
 let visited y x r = visit_cache.[(r % lcd)*wh*ww + (y-1)*ww + x - 1] in
 let set_visited y x r = visit_cache.[(r % lcd)*wh*ww + (y-1)*ww + x - 1] <- true in
 let clear_visited () = Array.fill visit_cache 0 (lcd*wh*ww) false in
 let travel () = 
  try
   while move_queue.Count <> 0 do
    let (y,x,r) = move_queue.Dequeue() in
    if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 && visited y x r then () else
    (if x >= 1 && y >= 1 && x <= w - 2 && y <= h - 2 then set_visited y x r ;
     (valid_points (y,x,r+1)) |> Seq.iter (fun state -> move_queue.Enqueue(state)))
   done ;
   0
  with Break n -> n in

 move_queue.Enqueue((0,1,0)) ;
 let result1 = travel () in
 printfn "%d" result1 ;
 move_queue.Clear() ;
 clear_visited ();

 start.Value <- (h-1,w-2) ;
 dest.Value <- (0,1) ;
 move_queue.Enqueue((h-1,w-2,result1)) ;
 let result2 = travel () in
 printfn "%d" result2 ;
 move_queue.Clear() ;
 clear_visited ();

 start.Value <- (0,1) ;
 dest.Value <- (h-1,w-2);
 move_queue.Enqueue((0,1,result2)) ;
 let result3 = travel () in
 printfn "%d" result3 ;
 result3
