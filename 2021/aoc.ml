(* Advent of Code 2021 *)

let p1_1 () =
 let ic = open_in "01-1.txt" in
 let cnt = ref 0 and last = ref 0 in
 try
  last := int_of_string @@ input_line ic ;
  while true do
   match int_of_string @@ input_line ic with
   | i when i > !last -> last := i; incr cnt
   | i -> last := i
  done ; assert false
 with End_of_file -> close_in ic; !cnt

let p1_2 () =
 let ic = open_in "01-1.txt" and len = 3 in
 let cnt = ref 0 and window = Array.make len 0 in
 try
  for i = 0 to (len-1) do window.(i) <- int_of_string @@ input_line ic done ;
  while true do
   for i = 0 to (len-1) do
    let last = Array.fold_right (Int.add) window 0 in
    window.(i) <- int_of_string @@ input_line ic ;
    if last < (Array.fold_right (Int.add) window 0) then incr cnt else ()
   done
  done; assert false
 with End_of_file -> close_in ic; !cnt

let p2_1 () =
 let ic = open_in "02-1.txt" in
 let hz = ref 0 and depth = ref 0 in
 try
  while true do
   let s = input_line ic in
   match s.[0] with
   | s0 when s0 = 'f' -> hz := !hz + (Scanf.sscanf s "%s %d" (fun _ x -> x))
   | s0 when s0 = 'u' -> depth := !depth - (Scanf.sscanf s "%s %d" (fun _ x -> x))
   | s0 when s0 = 'd' -> depth := !depth + (Scanf.sscanf s "%s %d" (fun _ x -> x))
   | _ -> raise End_of_file
  done; assert false
 with End_of_file -> close_in ic; (!hz,!depth,!hz * !depth)

let p2_2 () =
 let ic = open_in "02-1.txt" in
 let hz = ref 0 and depth = ref 0 and aim = ref 0 in
 try
  while true do
   let s = input_line ic in
   match Scanf.sscanf s "%s %d" (fun s x -> (s,x)) with
   | (s,x) when s.[0] = 'f' -> hz := !hz + x ; depth := !depth + (!aim * x)
   | (s,x) when s.[0] = 'u' -> aim := !aim - x
   | (s,x) when s.[0] = 'd' -> aim := !aim + x
   | _ -> raise End_of_file
  done; assert false
 with End_of_file -> close_in ic; (!hz,!depth,!hz * !depth)

let p3_1 () =
 let ic = open_in "03-1.txt" in
 let rec read_loop acc = 
  match try Some (input_line ic) with End_of_file -> close_in ic; None with
  | None -> acc | Some s -> read_loop (s::acc) in
 let rev_input = read_loop [] in
 let bit_len = String.length @@ List.hd rev_input
 and len = List.length rev_input in
 let sum = Array.make bit_len 0 in
 List.iter (fun s -> String.iteri (fun i c -> if c = '1' then sum.(i) <- succ sum.(i)) s) rev_input;
 let gamma_s = Array.map (fun x -> if x+x < len then '0' else '1') sum |> Array.to_seq |> String.of_seq in
 let epsilon_s = Array.map (fun x -> if x+x < len then '1' else '0') sum |> Array.to_seq |> String.of_seq in
 let gamma = Scanf.sscanf ("0b" ^ gamma_s) "%i" (fun x -> x) in
 let epsilon = Scanf.sscanf ("0b" ^ epsilon_s) "%i" (fun x -> x) in
 (gamma,epsilon,gamma*epsilon)

let p3_2 () =
 let ic = open_in "03-1.txt" in
 let rec read_loop acc = 
  match try Some (input_line ic) with End_of_file -> close_in ic; None with
  | None -> acc | Some s -> read_loop (s::acc) in
 let rev_input = read_loop [] in
 let bit_len = String.length @@ List.hd rev_input in
 let sum = ref 0 in
 let testf s l = s+s < l in (*testf_oxy*)
 let rec trim testf i acc =
  if List.length acc = 1 || i >= bit_len then acc else
  begin
   sum := 0;
   List.iter (fun s -> if s.[i] = '1' then incr sum else ()) acc;
   let c = if testf !sum @@ List.length acc then '0' else '1' in
   let acc' = List.filter (fun x -> x.[i] = c) acc in
   trim testf (succ i) acc'
  end in
  let oxy_s = List.hd @@ trim testf 0 rev_input
  and co2_s = List.hd @@ trim (fun x y -> not @@ testf x y) 0 rev_input in
  let oxy = Scanf.sscanf ("0b" ^ oxy_s) "%i" (fun x -> x)
  and co2 = Scanf.sscanf ("0b" ^ co2_s) "%i" (fun x -> x) in
  (oxy, co2, oxy*co2)

let mat_selfmap (f:'a->'a) (mat:'a array array) : unit =
 let dim_y = Array.length mat and dim_x = Array.length @@ mat.(0) in
 for i = 0 to dim_y - 1 do
  for j = 0 to dim_x - 1 do
   mat.(i).(j) <- f mat.(i).(j)
  done
 done

(* fold for square/rect matrices *)
let mat_fold (f:'a->'b->'a) (init:'a) (mat:'b array array) : 'a =
 let acc = ref init in
 let dim_y = Array.length mat and dim_x = Array.length @@ mat.(0) in
 for i = 0 to dim_y - 1 do
  for j = 0 to dim_x - 1 do
   acc := f !acc mat.(i).(j)
  done
 done; !acc

let score mat =
 let dim = Array.length mat and valid_x = ref false in
 let valid_y = Array.make dim true in
 for i = 0 to dim-1 do
  Array.map2 (fun a (_,b) -> a && b) valid_y mat.(i) |> (fun x -> Array.blit x 0 valid_y 0 dim);
  valid_x := !valid_x || Array.fold_left (fun acc (_,b) -> acc && b) true mat.(i)
 done;
 if not (Array.fold_left (fun a b -> a || b) !valid_x valid_y) then None
 else mat_fold (fun acc (n,b) -> if not b then acc+n else acc) 0 mat |> (fun x -> Some x)

let p4_1 () =
 let ic = open_in "04-1.txt" in
 let nums = input_line ic |> String.split_on_char ',' |> List.map (int_of_string) in
 let dim = 5 in
 let rec read_cards i acc =
  match try Some (input_line ic) with End_of_file -> close_in ic; None with
  | None when i > 0 -> acc (* if last line of input is a card *)
  | None -> List.tl acc (* if last line of input is blank *)
  | Some s when s = "" -> read_cards 0 ((Array.make_matrix dim dim (0,false))::acc)
  | Some s ->
    let card = List.hd acc in
    Scanf.sscanf s " %d %d %d %d %d " (*ignore pre/post-whitespace*)
    begin fun a b c d e -> card.(i) <- [|a,false;b,false;c,false;d,false;e,false|] end;
    read_cards (succ i) acc in
  let cards_rev = read_cards 0 [] in
  let play_step num =
   List.iter begin fun card ->
    mat_selfmap begin function
    | (n,b) when n = num -> (n,true)
    | (n,b) -> (n,b)
    end card end cards_rev in
  let rec play = function
  | [] -> failwith "no bingo"
  | num :: nums' ->
    play_step num ;
    let scores = List.map score cards_rev in
    match List.fold_left max None scores with
    | None -> play nums' (*continue playing if no winner*)
    | Some s -> (s,num,s*num) (*if there is a winner, return the max score*)
   in play nums

let p4_2 () =
 let ic = open_in "04-1.txt" in
 let nums = input_line ic |> String.split_on_char ',' |> List.map (int_of_string) in
 let dim = 5 in
 let rec read_cards i acc =
  match try Some (input_line ic) with End_of_file -> close_in ic; None with
  | None when i > 0 -> acc (* if last line of input is a card *)
  | None -> List.tl acc (* if last line of input is blank *)
  | Some s when s = "" -> read_cards 0 ((Array.make_matrix dim dim (0,false))::acc)
  | Some s ->
    let card = List.hd acc in
    Scanf.sscanf s " %d %d %d %d %d "
    begin fun a b c d e -> card.(i) <- [|a,false;b,false;c,false;d,false;e,false|] end;
    read_cards (succ i) acc in
  let cards_rev = read_cards 0 [] in
  let play_step cards num = (*step w/ de-accumulator*)
   List.iter begin fun card ->
    mat_selfmap begin function
    | (n,b) when n = num -> (n,true)
    | (n,b) -> (n,b)
    end card end cards in
  let rec play acc = function
  | [] -> failwith "no nums remaining"
  | num :: nums' ->
    play_step acc num ;
    let scores_cmb = List.combine (List.map score acc) acc in
    let (_, acc') = List.filter (function (None,_) -> true | _ -> false) scores_cmb |> List.split in
    match acc' with
    | [] ->
      (* pick the minimum score in case of a tie *)
      begin match List.fold_left (fun a (s,_) -> min a s) (Some Int.max_int) scores_cmb with
       | Some s ->  (s,num,s*num)
       | _ -> assert false
      end
    | _ -> play acc' nums'
   in play cards_rev nums 

let p5_1 () =
 let ic = open_in "05-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic; None with
  | None -> acc
  | Some s ->
    let line = Scanf.sscanf s " %d,%d -> %d,%d " (fun x1 y1 x2 y2 -> (x1,y1,x2,y2)) in
    read_input (line::acc) in 
 let input = read_input [] in
 let rec max_dims (x,y) = function
 | (x1,y1,x2,y2)::lines' -> max_dims (max x @@ max x1 x2,max y @@ max y1 y2) lines'
 | _ -> (x,y) in
 let (dim_x, dim_y) = max_dims (0,0) input in
 let map = Array.make_matrix (dim_y+1) (dim_x+1) 0 in
 let rec draw_line' (x1,y1,x2,y2) = (*map is mutable, does not need to be passed*)
  map.(y1).(x1) <- succ map.(y1).(x1);
  if x1 = x2 && y1 = y2 then ()
  else if x1 = x2 then draw_line' (x1,succ y1,x1,y2)
  else if y1 = y2 then draw_line' (succ x1,y1,x2,y1)
  else map.(y1).(x1) <- pred map.(y1).(x1) ; () in (*undo markings for diagonal case, for now*)
 (*simplify draw_line by only drawing right and down*)
 let draw_line (x1,y1,x2,y2) = draw_line' (min x1 x2, min y1 y2, max x1 x2, max y1 y2) in
 List.iter (fun seg -> draw_line seg) input;
 let score = mat_fold (fun acc x -> if x > 1 then acc+1 else acc) 0 map in
 score

let p5_2 () =
 let ic = open_in "05-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic; None with
  | None -> acc
  | Some s ->
    let line = Scanf.sscanf s " %d,%d -> %d,%d " (fun x1 y1 x2 y2 -> (x1,y1,x2,y2)) in
    read_input (line::acc) in 
 let input = read_input [] in
 let rec max_dims (x,y) = function
 | (x1,y1,x2,y2)::lines' -> max_dims (max x @@ max x1 x2,max y @@ max y1 y2) lines'
 | _ -> (x,y) in
 let (dim_x, dim_y) = max_dims (0,0) input in
 let map = Array.make_matrix (dim_y+1) (dim_x+1) 0 in
 let rec draw_line (x1,y1,x2,y2) = (*allow drawing in all directions divisible by 45 deg*)
  map.(y1).(x1) <- map.(y1).(x1) + 1;
  if x1 = x2 && y1 = y2 then ()
  else if x1 = x2 then draw_line (x1,y1+(compare y2 y1),x1,y2) (*use compare for signum*)
  else if y1 = y2 then draw_line (x1+(compare x2 x1),y1,x2,y1)
  else if abs (y2 - y1) = abs (x2 - x1) then draw_line (x1+(compare x2 x1),y1+(compare y2 y1),x2,y2)
  else map.(y1).(x1) <- map.(y1).(x1) - 1; () in (*undo markings for non-(+/-)45 deg*)
 List.iter (fun seg -> draw_line seg) input;
 let score = mat_fold (fun acc x -> if x > 1 then acc+1 else acc) 0 map in
 score

let dtrange top bot step = begin
 if top < bot then []
 else if step < 1 then []
 else List.init (max (((top - bot) / step ) + 1) 0)
      begin fun i -> top - step * i end
 end

(* deep recursive solution - cannot handle day > 80 *)
let p6 ?(days = 80) () =
 let ic = open_in "06-1s.txt" in
 let mtime = 7 and ntime = 9 in
 let init_state = input_line ic |> String.split_on_char ',' |> List.map int_of_string in
 close_in ic;
 let
  rec m0 (t:int) : int =
   if t < 0 then 0
   else n0s (dtrange (t-1) 0 mtime)
  and n0s ts =
   List.map n0 ts |> List.fold_left (+) 0
  and n0 (t:int) : int =
   if t < 0 then 0 else 1 + m0 (t - (ntime - 1)) in
 let pop t = m0 (days-t) |> succ in
  List.map pop init_state |> List.fold_left (+) 0

(* mod-iterative solution *)
let p6' ?(days = 80) () =
 let ic = open_in "06-1.txt" in
 let mtime = 7 and ntime = 9 in
 let state = Array.make ntime 0 in
 let init_state = input_line ic |>
                  String.split_on_char ',' |>
                  List.map int_of_string in
 close_in ic; List.iter (fun x -> state.(x) <- succ state.(x)) init_state;
 for z_ptr = 0 to days-1 do
  (* neophyte birth is automatically implemented by state length = ntime *)
  (* recycle mature fish to d_ptr+mtime % ntime *)
  let d_ptr = (z_ptr+mtime) mod ntime in
  state.(d_ptr) <- state.(z_ptr mod ntime) + state.(d_ptr)
 done;
 Array.fold_left (+) 0 state

let p6_1 = p6'
let p6_2 = p6' ~days:256

let p7_1 () =
 let lar b xs = xs |> List.map (fun x -> abs (x - b)) |> List.fold_left (+) 0 in
 let ic = open_in "07-1.txt" in
 let crabs = input_line ic |> String.split_on_char ',' |>
             List.map int_of_string in close_in ic;
 let rec min_lar a i =
  let b = lar i crabs in if b > a then (pred i, a) else min_lar b (succ i) in
 min_lar Int.max_int 0

let p7_2 () =
 let rec cost x = if x mod 2 = 0 then (x+1) * (x/2) else x + cost (x-1) in
 let lar b xs = xs |> List.map (fun x -> abs (x - b) |> cost) |> List.fold_left (+) 0 in
 let ic = open_in "07-1.txt" in
 let crabs = input_line ic |> String.split_on_char ',' |>
             List.map int_of_string in close_in ic;
 let rec min_lar a i =
  let b = lar i crabs in if b > a then (pred i, a) else min_lar b (succ i) in
 min_lar Int.max_int 0

(*let segments = ["abcefg";"cf";"acdeg";"acdfg";"bcdf";"abdfg";"abdefg";"acf";"abcdefg";"abcdfg"]*)

let p8_1 () =
 let ic = open_in "08-1.txt" in
 let rec input_loop acc =
  try let line = input_line ic in
      let return = Scanf.sscanf line
       " %s %s %s %s %s %s %s %s %s %s | %s %s %s %s " (* ignore everything before | *)
        begin fun _ _ _ _ _ _ _ _ _ _ a b c d ->
         Array.map begin fun s ->
          (* check if 1,4,7,8 by unique code lengths *)
          if Array.mem (String.length s) [|2;3;4;7|] then 1 else 0
         end [|a;b;c;d|]
        end |> Array.fold_left (+) 0 in input_loop @@ return + acc
  with End_of_file -> close_in ic ; acc
 in input_loop 0

let fix_map (in_map : string array) =
 let module CS = Set.Make(Char) in
 let seq_hd seq = match seq () with Seq.Cons (s,_) -> s | _ -> assert false in
 let arr_find t arr = Array.fold_left (fun a s -> if t s then Some s else a) None arr |> Option.get in
 let in_sets = Array.map (fun s -> String.to_seq s |> CS.of_seq) in_map in
 let sets = Array.make 10 CS.empty in
 sets.(1) <- arr_find (fun s -> CS.cardinal s = 2) in_sets ;
 sets.(4) <- arr_find (fun s -> CS.cardinal s = 4) in_sets ;
 sets.(7) <- arr_find (fun s -> CS.cardinal s = 3) in_sets ;
 sets.(8) <- arr_find (fun s -> CS.cardinal s = 7) in_sets ;
 let s235 = Array.to_seq in_sets |> Seq.filter (fun s -> CS.cardinal s = 5)
 and s069 = Array.to_seq in_sets |> Seq.filter (fun s -> CS.cardinal s = 6) in
 let fs_set = Seq.fold_left begin fun acc s ->
  CS.union acc @@ CS.diff sets.(8) s
 end CS.empty s069 in
 sets.(2) <- Seq.filter (fun s -> CS.subset fs_set s) s235 |> seq_hd ;
 sets.(3) <- Seq.filter (fun s -> CS.subset sets.(1) s) s235 |> seq_hd ;
 sets.(5) <- Seq.filter (fun s -> not @@ (CS.equal s sets.(2) || CS.equal s sets.(3))) s235 |> seq_hd ;
 sets.(9) <- CS.union sets.(5) sets.(1) ;
 sets.(6) <- Seq.filter (fun s -> CS.subset sets.(1) s |> not) s069 |> seq_hd ;
 sets.(0) <- Seq.filter (fun s -> not @@ (CS.equal s sets.(6) || CS.equal s sets.(9)))  s069 |> seq_hd ;
 sets

let map_fixed def (in_map : string array) =
 let module CS = Set.Make(Char) in
 let in_sets = Array.map (fun s -> String.to_seq s |> CS.of_seq) in_map in
 Array.map begin fun s ->
  let rec aux = function
   | i when i > 9 -> assert false
   | i when CS.equal s def.(i) -> i
   | i -> aux @@ succ i in aux 0
 end in_sets

let p8_2_debug () =
 let ic = open_in "08-1s.txt" in
 let rec input_loop acc =
  try let line = input_line ic in
      let return = Scanf.sscanf line
       " %s %s %s %s %s %s %s %s %s %s | %s %s %s %s "
        begin fun s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 a b c d -> 
        let digits = map_fixed (fix_map [|s0;s1;s2;s3;s4;s5;s6;s7;s8;s9|]) [|a;b;c;d|] in
        digits.(0) * 1000 + digits.(1) * 100 + digits.(2) * 10 + digits.(3)
        end in input_loop @@ return::acc
  with End_of_file -> close_in ic ; acc in input_loop []

let p8_2 () =
 let ic = open_in "08-1.txt" in
 let rec input_loop acc =
  try let line = input_line ic in
      let return = Scanf.sscanf line
       " %s %s %s %s %s %s %s %s %s %s | %s %s %s %s "
        begin fun s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 a b c d -> 
        let digits = map_fixed (fix_map [|s0;s1;s2;s3;s4;s5;s6;s7;s8;s9|]) [|a;b;c;d|] in
        digits.(0) * 1000 + digits.(1) * 100 + digits.(2) * 10 + digits.(3)
        end in input_loop @@ return + acc
  with End_of_file -> close_in ic ; acc in input_loop 0

let p9_1 () =
 let ic = open_in "09-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | Some s -> read_input (s::acc)
  | None -> acc in
 let input = read_input [] |> List.to_seq |> Array.of_seq in
 let max_y = Array.length input
 and max_x = String.length input.(0) in
 let get_height x y =
  if x < 0 || y < 0 || x >= max_x || y >= max_y then 10 else (Char.code input.(y).[x]) - 48 in
 let acc = ref 0 in
 for y = 0 to max_y - 1 do
  for x = 0 to max_x - 1 do
   let p = get_height x y
   and u = get_height x (succ y)
   and r = get_height (succ x) y
   and d = get_height x (pred y)
   and l = get_height (pred x) y in
   if p < (List.fold_left min 10 [u;r;d;l]) then acc := !acc + (succ p)
  done
 done ; !acc

let p9_2 () =
 let ic = open_in "09-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | Some s -> read_input (s::acc)
  | None -> acc in
 let input = read_input [] |> List.to_seq |> Array.of_seq in
 let max_y = Array.length input
 and max_x = String.length input.(0) in
 let checked = Array.make_matrix max_y max_x false in
 let get_height x y =
  if x < 0 || y < 0 || x >= max_x || y >= max_y then 10 else (Char.code input.(y).[x]) - 48 in
 let rec basin_size x y =
  let h = get_height x y in
  if h >= 9 || checked.(y).(x) then 0
  else begin 
   checked.(y).(x) <- true ;
   1 + basin_size (pred x) y +
       basin_size (succ x) y +
       basin_size x (pred y) +
       basin_size x (succ y)
  end in
 let acc = [|0;0;0|] in
 for y = 0 to max_y - 1 do
  for x = 0 to max_x - 1 do
   let b_size = basin_size x y in
   if b_size > acc.(2) then begin
   acc.(2) <- acc.(1) ;
   if b_size > acc.(1) then begin
   acc.(1) <- acc.(0) ;
   if b_size > acc.(0) then acc.(0) <- b_size
   else acc.(1) <- b_size end
   else acc.(2) <- b_size end 
  done
 done ; acc |> Array.fold_left ( * ) 1

let p10_1 () =
 let sym = ['(',')',3;'[',']',57;'{','}',1197;'<','>',25137] in
 let ic = open_in "10-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | Some s -> read_input (s::acc)
  | None -> acc in
 let rec score_line acc stk i len s =
  if i >= len then (acc, stk) (* if stk is not null, the line is incomplete *)
  else
   match (List.filter (fun (a,b,c) -> s.[i] = a) sym) with
   | (a,b,c)::[] -> score_line acc ((a,b,c)::stk) (succ i) len s
   | _ ->
    match (List.filter (fun (a,b,c) -> s.[i] = b) sym), stk with
    | ((a,b,c)::[],[]) -> (~- c,[]) (* corrupted *)
    | ((a,b,c)::[],(a',b',c')::stk') when b = b' -> score_line (acc+c) (stk') (succ i) len s
    | ((_,_,c)::[],_) -> (~- c,stk) (* corrupted *)
    | _ -> failwith "invalid input" in
 let rec loop acc = function
  | [] -> acc
  | line::lines' ->
    let (s,stk) = score_line 0 [] 0 (String.length line) line in
    if s < 0 then loop (acc+s) lines' else loop acc lines'
 in loop 0 (read_input [])

let p10_2 () =
 let sym = ['(',')',1;'[',']',2;'{','}',3;'<','>',4] in
 let ic = open_in "10-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | Some s -> read_input (s::acc)
  | None -> acc in
 let rec score_line acc stk i len s =
  if i >= len then (acc, stk) (* if stk is not null, the line is incomplete *)
  else
   match (List.filter (fun (a,b,c) -> s.[i] = a) sym) with
   | (a,b,c)::[] -> score_line acc ((a,b,c)::stk) (succ i) len s
   | _ ->
    match (List.filter (fun (a,b,c) -> s.[i] = b) sym), stk with
    | ((a,b,c)::[],[]) -> (~- c,[]) (* corrupted *)
    | ((a,b,c)::[],(a',b',c')::stk') when b = b' -> score_line (acc+c) (stk') (succ i) len s
    | ((_,_,c)::[],_) -> (~- c,stk) (* corrupted *)
    | _ -> failwith "invalid input" in
 let rec score_stk acc = function
  | [] -> acc
  | (_,_,c)::stk' -> score_stk (acc*5+c) stk' in
 let rec loop acc = function
  | [] -> acc
  | line::lines' ->
    let (s,stk) = score_line 0 [] 0 (String.length line) line in
    if not (stk = [] || s < 0) then loop ((score_stk 0 stk)::acc) lines' else loop acc lines'
 in loop [] (read_input []) |> List.sort (compare) |> (fun ss -> List.nth ss (List.length ss / 2))

let p11_1 () =
 let byte_succ c = c |> Char.code |> succ |> Char.chr in
 let ic = open_in "11-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | Some s -> let data = Bytes.of_string s in read_input (data::acc) | None -> acc in
 let grid = read_input [] |> Array.of_list in
 let max_y = Array.length grid and max_x = Bytes.length grid.(0) in
 let rec try_flash x y = if x < 0 || x >= max_x || y < 0 || y >= max_y then 0 (*oob*)
  else begin
   let c = Bytes.get grid.(y) x in
   if compare c '9' < 0 then (Bytes.set grid.(y) x (byte_succ c) ; 0) (*no flash*)
   else begin (* flash *)
    Bytes.set grid.(y) x '\000' ; 1 +
    try_flash (pred x) (pred y) + try_flash (pred x) y + try_flash (pred x) (succ y) +
    try_flash x (pred y) + try_flash x (succ y) +
    try_flash (succ x) (pred y) + try_flash (succ x) y + try_flash (succ x) (succ y)
   end
  end in
 let step () =
  let acc = ref 0 in
  for i=0 to max_y - 1 do for j=0 to max_x - 1 do (* iterate try_flash *)
   acc:= !acc + try_flash j i
  done done ;
  for i=0 to max_y - 1 do for j=0 to max_x - 1 do (* reset flashed to '0' *)
   if compare '0' (Bytes.get grid.(i) j) > 0 then Bytes.set grid.(i) j '0'
  done done ; !acc in
 let rec days acc n =
  if n < 1 then acc
  else let c = step () in days (acc+c) (pred n)
 in days 0 100  

let p11_2 () =
 let byte_succ c = c |> Char.code |> succ |> Char.chr in
 let ic = open_in "11-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | Some s -> let data = Bytes.of_string s in read_input (data::acc) | None -> acc in
 let grid = read_input [] |> Array.of_list in
 let max_y = Array.length grid and max_x = Bytes.length grid.(0) in
 let rec try_flash x y = if x < 0 || x >= max_x || y < 0 || y >= max_y then 0 (*oob*)
  else begin
   let c = Bytes.get grid.(y) x in
   if compare c '9' < 0 then (Bytes.set grid.(y) x (byte_succ c) ; 0) (*no flash*)
   else begin (* flash *)
    Bytes.set grid.(y) x '\000' ; 1 +
    try_flash (pred x) (pred y) + try_flash (pred x) y + try_flash (pred x) (succ y) +
    try_flash x (pred y) + try_flash x (succ y) +
    try_flash (succ x) (pred y) + try_flash (succ x) y + try_flash (succ x) (succ y)
   end
  end in
 let step () =
  let acc = ref 0 in
  for i=0 to max_y - 1 do for j=0 to max_x - 1 do (* iterate try_flash *)
   acc:= !acc + try_flash j i
  done done ;
  for i=0 to max_y - 1 do for j=0 to max_x - 1 do (* reset flashed to '0' *)
   if compare '0' (Bytes.get grid.(i) j) > 0 then Bytes.set grid.(i) j '0'
  done done ; !acc in
  let zeros = Array.make max_y (Bytes.make max_x '0') in
  let rec test_sync i =
   ignore @@ step () ;
   if zeros = grid then i else test_sync (succ i)
  in test_sync 1

let p12_1 () =
 let ic = open_in "12-1.txt" in
 let rec read_input acc acc_s acc_e =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> (acc,acc_s,acc_e)
  | Some s ->
   begin match String.split_on_char '-' s with
   | a::b::[] when a = "start" -> read_input acc (b::acc_s) (acc_e)
   | a::b::[] when b = "start" -> read_input acc (a::acc_s) (acc_e)
   | a::b::[] when a = "end"   -> read_input acc acc_s (b::acc_e)
   | a::b::[] when b = "end"   -> read_input acc acc_s (a::acc_e)
   | a::b::[] -> read_input ((a,b)::acc) acc_s acc_e
   | _ -> failwith "invalid input" end in
  let (bidir,starts,ends) = read_input [] [] [] in
  (* starts and ends are unidirectional, others are bidirectional *)
  let rec pathfind c map xs dsts =
   let xs' = if (Char.code c.[0]) > 90 then (c::xs) else xs in
   let paths = map |> List.filter (fun (a,b) -> (a = c) || (b = c)) |>
               List.map (fun (a,b) -> if a = c then b else a) |>
               List.filter (fun a -> not (List.mem a xs)) in
   (*debug*)
   (*Printf.printf "%s: " c ; List.iter (fun x -> Printf.printf "%s, " x) paths ; print_newline () ;*)
   List.fold_left begin fun acc p ->
    (pathfind p map xs' dsts) + acc
   end (if List.mem c dsts then 1 else 0) paths in
   List.fold_left (fun acc s -> (pathfind s bidir [] ends) + acc) 0 starts

let p12_2 () =
 let ic = open_in "12-1.txt" in
 let rec read_input acc acc_s acc_e =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> (acc,acc_s,acc_e)
  | Some s ->
   begin match String.split_on_char '-' s with
   | a::b::[] when a = "start" -> read_input acc (b::acc_s) (acc_e)
   | a::b::[] when b = "start" -> read_input acc (a::acc_s) (acc_e)
   | a::b::[] when a = "end"   -> read_input acc acc_s (b::acc_e)
   | a::b::[] when b = "end"   -> read_input acc acc_s (a::acc_e)
   | a::b::[] -> read_input ((a,b)::acc) acc_s acc_e
   | _ -> failwith "invalid input" end in
  let (bidir,starts,ends) = read_input [] [] [] in
  (* starts and ends are unidirectional, others are bidirectional *)
  let rec pathfind c map xs xxs xxxs dsts = (* xxs: potential 2-trip cave, xxxs: engaged but not used *)
   match (xxs,xxxs) with
   | (Some s,false) when List.mem s xs ->
     let xs' = List.filter (fun x -> not (x=s)) xs in
     pathfind c map xs' (Some s) true dsts
   | (Some s,true) when c = s -> pathfind c map xs None false dsts
   | _ ->
   let xs' = if (Char.code c.[0]) > 90 then (c::xs) else xs in
   let paths = map |> List.filter (fun (a,b) -> (a = c) || (b = c)) |>
               List.map (fun (a,b) -> if a = c then b else a) |>
               List.filter (fun a -> not (List.mem a xs)) in
   (*debug*)
   (*Printf.printf "%s: " c ; List.iter (fun x -> Printf.printf "%s, " x) paths ; print_newline () ;*)
   List.fold_left begin fun acc p ->
    (pathfind p map xs' xxs xxxs dsts) + acc
   end (if List.mem c dsts && xxs = None then 1 else 0) paths in
   let small_caves = starts @ ends @ (List.split bidir |> (fun (a,b) -> a @ b)) |>
                     List.sort_uniq (compare) |> List.filter (fun s -> Char.code s.[0] > 90) in
   List.fold_left (fun acc s -> (pathfind s bidir [] None false ends) + acc) 0 starts +
   List.fold_left begin fun acc sc ->
    acc + List.fold_left (fun acc s -> (pathfind s bidir [] (Some sc) false ends) + acc) 0 starts
   end 0 small_caves

(* ^ Possible Optimization: Use bool matrix as a graph for connections *)

let p13_1 () =
 let ic = open_in "13-1.txt" in
 let rec read_input p1 acc folds =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> (acc, List.rev folds)
  | Some s when s = "" -> read_input false acc folds
  | Some s when p1 ->
    let xy = Scanf.sscanf s " %d,%d " (fun a b -> (a,b)) in
    read_input true (xy::acc) folds
  | Some s ->
    let fold = Scanf.sscanf s "fold along %c=%d" (fun a b -> (a,b)) in
    read_input false acc (fold::folds) in
 let (marks,folds) = read_input true [] [] in
 let count_marks map =
  Array.fold_left begin fun acc line ->
   acc + Array.fold_left (fun acc i -> if i then succ acc else acc) 0 line
  end 0 map in
 let print_map map =
  Array.iter begin fun line ->
   Array.iter begin fun col ->
    Printf.printf "%c" (if col then '#' else '.')
    end line ; print_newline ()
   end map in
 let fold_map map = function
 | ('y',l) ->
   let map_max_y = pred (Array.length map) and map_max_x = pred (Array.length (map.(0))) in
   let new_map = Array.make_matrix l (succ map_max_x) false in
   for i = 0 to pred l do
    let i' = l+l-i in if i' < 0 || i' > map_max_y then
     new_map.(i) <- map.(i)
    else new_map.(i) <- Array.map2 (fun a b -> a || b) map.(i) map.(i')
   done ; new_map
 | (_,c) ->
   let map_max_y = pred (Array.length map) and map_max_x = pred (Array.length (map.(0))) in
   let new_map = Array.make_matrix (succ map_max_y) c false in
   Array.iteri begin fun i line ->
    for j = 0 to pred c do
     let j' = c+c-j in if j' < 0 || j' > map_max_x then new_map.(i).(j) <- map.(i).(j)
     else new_map.(i).(j) <- map.(i).(j) || map.(i).(j')
    done
   end map ; new_map in
 let (max_x, max_y) = List.fold_left (fun (mx,my) (x,y) -> (max mx x, max my y)) (Int.min_int, Int.min_int) marks in
 let mark_map = ref (Array.make_matrix (succ max_y) (succ max_x) false) in
 List.iter (fun (x,y) -> !mark_map.(y).(x) <- true) marks ; (*set marks*)
 mark_map := fold_map !mark_map (List.hd folds); (*update map with first fold*)
 print_map !mark_map ;
 count_marks !mark_map

let p13_2 () =
 let ic = open_in "13-1.txt" in
 let rec read_input p1 acc folds =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> (acc, List.rev folds)
  | Some s when s = "" -> read_input false acc folds
  | Some s when p1 ->
    let xy = Scanf.sscanf s " %d,%d " (fun a b -> (a,b)) in
    read_input true (xy::acc) folds
  | Some s ->
    let fold = Scanf.sscanf s "fold along %c=%d" (fun a b -> (a,b)) in
    read_input false acc (fold::folds) in
 let (marks,folds) = read_input true [] [] in
 let count_marks map =
  Array.fold_left begin fun acc line ->
   acc + Array.fold_left (fun acc i -> if i then succ acc else acc) 0 line
  end 0 map in
 let print_map map =
  Array.iter begin fun line ->
   Array.iter begin fun col ->
    Printf.printf "%c" (if col then '#' else '.')
    end line ; print_newline ()
   end map in
 let fold_map map = function
 | ('y',l) ->
   let map_max_y = pred (Array.length map) and map_max_x = pred (Array.length (map.(0))) in
   let new_map = Array.make_matrix l (succ map_max_x) false in
   for i = 0 to pred l do
    let i' = l+l-i in if i' < 0 || i' > map_max_y then
     new_map.(i) <- map.(i)
    else new_map.(i) <- Array.map2 (fun a b -> a || b) map.(i) map.(i')
   done ; new_map
 | (_,c) ->
   let map_max_y = pred (Array.length map) and map_max_x = pred (Array.length (map.(0))) in
   let new_map = Array.make_matrix (succ map_max_y) c false in
   Array.iteri begin fun i line ->
    for j = 0 to pred c do
     let j' = c+c-j in if j' < 0 || j' > map_max_x then new_map.(i).(j) <- map.(i).(j)
     else new_map.(i).(j) <- map.(i).(j) || map.(i).(j')
    done
   end map ; new_map in
 let (max_x, max_y) = List.fold_left (fun (mx,my) (x,y) -> (max mx x, max my y)) (Int.min_int, Int.min_int) marks in
 let mark_map = ref (Array.make_matrix (succ max_y) (succ max_x) false) in
 List.iter (fun (x,y) -> !mark_map.(y).(x) <- true) marks ; (*set marks*)
 List.iter (fun fold -> mark_map := fold_map !mark_map fold) folds;
 print_map !mark_map ;
 count_marks !mark_map

let p14_1a () =
 let ic = open_in "14-1s.txt" in
 let iterations = 5 in
 let rec read_input p1 template rules =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> (template, rules)
  | Some s when p1 -> read_input false s rules
  | Some "" -> read_input false template rules
  | Some s -> (*rule = l,r,ins*)
    let rule = Scanf.sscanf s "%c%c -> %c" (fun a b c -> (a,b,c)) in
    read_input false template (rule::rules) in
 let (template,rules) = read_input true "" [] in
 let tlist = ref (template |> String.to_seq |> List.of_seq) in
 let histogram = Array.make 26 0 in
 let step tlist =
  let step' (acc,last) c =
   match (List.fold_left (fun a (l,r,ins) -> if last = l && c = r then Some ins else a) None rules) with
   | None -> (last::acc,c)
   | Some ins -> (ins::last::acc,c) in
  let (acc,c) = List.fold_left step' ([], List.hd tlist) (List.tl tlist) in List.rev (c::acc) in
 for i = 1 to iterations do
  tlist := step !tlist
 done ;
 List.iter (fun c -> let idx = Char.code c - Char.code 'A' in histogram.(idx) <- succ histogram.(idx)) !tlist ;
 let h' = List.filter (fun x -> x > 0) (histogram |> Array.to_seq |> List.of_seq) in
 let lmax = List.fold_left (max) 0 h'
 and lmin = List.fold_left (min) Int.max_int h' in
 lmax-lmin

let p14_2a () =
 let ic = open_in "14-1.txt" in
 let iterations = 10 in
 let char_to_idx c = Char.code c - Char.code 'A' in
 let rmat = Array.make_matrix 26 26 '\000' in
 let histogram = Array.make 26 0 in
 let rec read_input p1 template =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> template
  | Some s when p1 -> read_input false s
  | Some "" -> read_input false template
  | Some s -> (*rule = l,r,ins*)
    let (l,r,ins) = Scanf.sscanf s "%c%c -> %c" (fun a b c -> (char_to_idx a,char_to_idx b,c)) in
    rmat.(l).(r) <- ins ; read_input false template in
 let tlist = read_input true "" |> String.to_seq |> List.of_seq in
 let succ_char c = let idx = Char.code c - Char.code 'A' in histogram.(idx) <- succ histogram.(idx) in
 let next_char left right = rmat.(char_to_idx left).(char_to_idx right) in
 let step l r i =
  let rec step' l r i =
   if i = 0 then succ_char l
   else begin 
    let nc = next_char l r in
    step' l nc (pred i) ; step' nc r (pred i)
   end in (*succ_char r ;*)
  step' l r i in
 let last = List.fold_left (fun l r -> step l r iterations; r) (List.hd tlist) (List.tl tlist) in
 succ_char last ;
 let h' = List.filter (fun x -> x > 0) (histogram |> Array.to_seq |> List.of_seq) in
 let lmax = List.fold_left (max) 0 h'
 and lmin = List.fold_left (min) Int.max_int h' in
 h',lmax-lmin

let p14_2b iterations () =
 let ic = open_in "14-1.txt" in
 (*let iterations = 40 in*)
 let char_to_idx c = Char.code c - Char.code 'A' in
 let valid_idx i = i <> -65 in
 let rmat = Array.make_matrix 26 26 '\000' in
 let next_char l r = rmat.(l).(r) |> char_to_idx in
 let segs = ref (Array.make_matrix 26 26 0) in
 let histogram = Array.make 26 0 in
 let rec read_input p1 template =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> template
  | Some s when p1 -> read_input false s
  | Some "" -> read_input false template
  | Some s -> (*rule = l,r,ins*)
    let (l,r,ins) = Scanf.sscanf s "%c%c -> %c" (fun a b c -> (char_to_idx a,char_to_idx b,c)) in
    rmat.(l).(r) <- ins ; read_input false template in
 let tlist = read_input true "" |> String.to_seq |> List.of_seq in
 let seg_init () =
  List.fold_left begin fun last c -> 
   let l = char_to_idx last and r = char_to_idx c in
   !segs.(l).(r) <- succ !segs.(l).(r) ; histogram.(l) <- succ histogram.(l) ; c
  end (List.hd tlist) (List.tl tlist) in
 let last = seg_init() |> char_to_idx in histogram.(last) <- succ histogram.(last) ;
 let max_min () =
  let h' = List.filter (fun x -> x > 0) (histogram |> Array.to_seq |> List.of_seq) in
  let lmax = List.fold_left (max) 0 h'
  and lmin = List.fold_left (min) Int.max_int h' in (h',lmax-lmin) in
 let step () =
  let segs' = Array.make_matrix 26 26 0 in
  Array.iteri begin fun l ln -> Array.iteri begin fun r n ->
    let ins = next_char l r in if valid_idx ins then begin
     segs'.(l).(ins) <- n + segs'.(l).(ins) ;
     segs'.(ins).(r) <- n + segs'.(ins).(r) ;
     histogram.(ins) <- n + histogram.(ins)
    end end ln end !segs; segs := segs' in
 for i=1 to iterations do step () done;
 max_min ()

let p14_1 = p14_2b 10
let p14_2 = p14_2b 40

(* Implement Dijkstra's algorithm! *)
(*
let p15_1slow () =
 let int_of_char c = Char.code c - Char.code '0' in
 let ic = open_in "15-1.txt" in
 let rec read_input lines =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> lines |> List.rev |> List.to_seq |> Array.of_seq
  | Some s -> read_input (s::lines) in
 let input = read_input [] in
 let h = Array.length input
 and w = input.(0) |> String.length in
 let get_gate y x = input.(y).[x] |> int_of_char in
 let visited = Array.make_matrix h w false
 and shortest_path = Array.make_matrix h w Int.max_int in
 let cur = ref (0,0) in
 let next_cur () =
  let cd = ref Int.max_int in
  for i=0 to h-1 do for j=0 to w-1 do
   if not visited.(i).(j) && shortest_path.(i).(j) < !cd then
    begin cur := (i,j) ; cd := shortest_path.(i).(j) end
  done done in
 visited.(0).(0) <- true ; shortest_path.(0).(0) <- 0 ;
 let path_iter () =
   match !cur with (y,x) ->
     let d = shortest_path.(y).(x) in
     visited.(y).(x) <- true;
     List.filter (fun (y,x) -> x >= 0 && x < w && y >=0 && y < h && not visited.(y).(x))
       [y-1,x;y+1,x;y,x-1;y,x+1] |>
     List.iter (fun (y,x) -> shortest_path.(y).(x) <- min shortest_path.(y).(x) (d + get_gate y x)) ;
     next_cur () in
 while not visited.(h-1).(w-1) do path_iter () done ;
 (*shortest_path*)
 shortest_path.(h-1).(w-1)
*)

let p15_1 () =
 let int_of_char c = Char.code c - Char.code '0' in
 let ic = open_in "15-1.txt" in
 let rec read_input lines =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> lines |> List.rev |> List.to_seq |> Array.of_seq
  | Some s -> read_input (s::lines) in
 let input = read_input [] in
 let h = Array.length input
 and w = input.(0) |> String.length in
 let get_gate y x = input.(y).[x] |> int_of_char in
 let visited = Array.make_matrix h w false
 and shortest_path = Array.make_matrix h w Int.max_int in
 let ncurs = ref [0,0] in
 let next_cur () = (* slightly faster than a full search *)
  match List.filter (fun (y,x) -> not visited.(y).(x)) !ncurs with [] -> ()
  | nc::ncs -> ncurs := begin List.fold_left
    begin fun (y,x) (y',x') -> if shortest_path.(y).(x) < shortest_path.(y').(x') then (y,x) else (y',x') end
    nc ncs end :: (nc::ncs) in
 visited.(0).(0) <- true ; shortest_path.(0).(0) <- 0 ;
 let path_iter () =
   match List.hd !ncurs with (y,x) ->
     let d = shortest_path.(y).(x) in
     visited.(y).(x) <- true;
     List.filter (fun (y,x) -> x >= 0 && x < w && y >=0 && y < h && not visited.(y).(x))
       [y-1,x;y+1,x;y,x-1;y,x+1] |> List.iter begin fun (y,x) ->
        shortest_path.(y).(x) <- min shortest_path.(y).(x) (d + get_gate y x) ;
        ncurs := (y,x)::!ncurs
       end; next_cur () in
 while not visited.(h-1).(w-1) do path_iter () done ;
 shortest_path.(h-1).(w-1)

(* barely fast enough *)
let p15_2 () =
 let int_of_char c = Char.code c - Char.code '0' in
 let ic = open_in "15-1.txt" in
 let rec read_input lines =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> lines |> List.rev |> List.to_seq |> Array.of_seq
  | Some s -> read_input (s::lines) in
 let input = read_input [] in
 let h' = Array.length input and w' = input.(0) |> String.length  in
 let h = h' * 5 and w = w' * 5 in
 let get_gate y x =
  let yfactor = y / h' and xfactor = x / w'
  and g = input.(y mod h').[x mod w'] |> int_of_char in
  let g' = g + xfactor + yfactor in
  if g' > 9 then g' mod 10 + 1 else g' in
 let visited = Array.make_matrix h w false
 and shortest_path = Array.make_matrix h w Int.max_int in
 let ncurs = ref [0,0] in
 let next_cur () = (* slightly faster than a full search *)
  match List.filter (fun (y,x) -> not visited.(y).(x)) !ncurs with [] -> ()
  | nc::ncs -> ncurs := begin List.fold_left
    begin fun (y,x) (y',x') -> if shortest_path.(y).(x) < shortest_path.(y').(x') then (y,x) else (y',x') end
    nc ncs end :: (nc::ncs) in
 visited.(0).(0) <- true ; shortest_path.(0).(0) <- 0 ;
 let path_iter () =
   match List.hd !ncurs with (y,x) ->
     let d = shortest_path.(y).(x) in
     visited.(y).(x) <- true;
     List.filter (fun (y,x) -> x >= 0 && x < w && y >=0 && y < h && not visited.(y).(x))
       [y-1,x;y+1,x;y,x-1;y,x+1] |> List.iter begin fun (y,x) ->
        shortest_path.(y).(x) <- min shortest_path.(y).(x) (d + get_gate y x) ;
        ncurs := (y,x)::!ncurs
       end; next_cur () in
 while not visited.(h-1).(w-1) do path_iter () done ;
 shortest_path.(h-1).(w-1)

(* #require core_kernel.fheap *) (*slower than 2*)
(*
let p15_3 () =
 let int_of_char c = Char.code c - Char.code '0' in
 let ic = open_in "15-1.txt" in
 let rec read_input lines =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> lines |> List.rev |> List.to_seq |> Array.of_seq
  | Some s -> read_input (s::lines) in
 let input = read_input [] in
 let h' = Array.length input and w' = input.(0) |> String.length  in
 let h = h' * 5 and w = w' * 5 in
 let get_gate y x =
  let yfactor = y / h' and xfactor = x / w'
  and g = input.(y mod h').[x mod w'] |> int_of_char in
  let g' = g + xfactor + yfactor in
  if g' > 9 then g' mod 10 + 1 else g' in
 let visited = Array.make_matrix h w false
 and shortest_path = Array.make_matrix h w Int.max_int in
 let cur_cmp (y,x) (y',x') = compare shortest_path.(y).(x) shortest_path.(y').(x') in
 let ncurs = ref (Fheap.create ~cmp:cur_cmp) in ncurs := Fheap.add !ncurs (0,0);
 let next_cur () = 
  let (top,ncurs') = Fheap.pop_exn !ncurs in ncurs := ncurs' ; top
 in
 visited.(0).(0) <- true ; shortest_path.(0).(0) <- 0 ;
 let path_iter () =
   match next_cur () with (y,x) ->
     let d = shortest_path.(y).(x) in
     visited.(y).(x) <- true;
     List.filter (fun (y,x) -> x >= 0 && x < w && y >=0 && y < h && not visited.(y).(x))
       [y-1,x;y+1,x;y,x-1;y,x+1] |> List.iter begin fun (y,x) ->
        shortest_path.(y).(x) <- min shortest_path.(y).(x) (d + get_gate y x) ;
        if not (Fheap.mem !ncurs (y,x) ~equal:(=)) then ncurs := Fheap.add !ncurs (y,x)
       end in
 while not visited.(h-1).(w-1) do path_iter () done ;
 shortest_path.(h-1).(w-1)
*)

(* fastest once compiled *)
(*
let p15_4 () =
 let int_of_char c = Char.code c - Char.code '0' in
 let ic = open_in "15-1.txt" in
 let rec read_input lines =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> lines |> List.rev |> List.to_seq |> Array.of_seq
  | Some s -> read_input (s::lines) in
 let input = read_input [] in
 let h' = Array.length input and w' = input.(0) |> String.length  in
 let h = h' * 5 and w = w' * 5 in
 let get_gate y x =
  let yfactor = y / h' and xfactor = x / w'
  and g = input.(y mod h').[x mod w'] |> int_of_char in
  let g' = g + xfactor + yfactor in
  if g' > 9 then g' mod 10 + 1 else g' in
 let visited = Array.make_matrix h w false
 and shortest_path = Array.make_matrix h w Int.max_int in
 let cur_cmp (y,x) (y',x') = compare shortest_path.(y).(x) shortest_path.(y').(x') in
 let hash (y,x) = 100000 * y + x in
 let module CurHeap = Hash_heap.Make(Core.Int) in
 let ncurs = CurHeap.create ~min_size:1024 (cur_cmp) in ignore @@ CurHeap.push ncurs ~key:(hash (0,0)) ~data:(0,0);
 let next_cur () = CurHeap.pop_exn ncurs in
 visited.(0).(0) <- true ; shortest_path.(0).(0) <- 0 ;
 let path_iter () =
   match next_cur () with (y,x) ->
     let d = shortest_path.(y).(x) in
     visited.(y).(x) <- true;
     List.filter (fun (y,x) -> x >= 0 && x < w && y >=0 && y < h && not visited.(y).(x))
       [y-1,x;y+1,x;y,x-1;y,x+1] |> List.iter begin fun (y,x) ->
        shortest_path.(y).(x) <- min shortest_path.(y).(x) (d + get_gate y x) ;
       CurHeap.replace ncurs ~key:(hash (y,x)) ~data:(y,x)
       end in
 while not visited.(h-1).(w-1) do path_iter () done ;
 shortest_path.(h-1).(w-1)
*)

(* Leftist Heap *)

type 'a leftist =  
  | Leaf 
  | Node of 'a leftist * 'a * 'a leftist * int

let singleton k = Node (Leaf, k, Leaf, 1)

let rank = function Leaf -> 0 | Node (_,_,_,r) -> r  

let rec merge t1 t2 =  
  match t1,t2 with
    | Leaf, t | t, Leaf -> t
    | Node (l, k1, r, _), Node (_, k2, _, _) ->
      if k1 > k2 then merge t2 t1 (* switch merge if necessary *)
      else 
        let merged = merge r t2 in (* always merge with right *)
        let rank_left = rank l and rank_right = rank merged in
        if rank_left >= rank_right then Node (l, k1, merged, rank_right+1)
        else Node (merged, k1, l, rank_left+1) (* left becomes right due to being shorter *)

(* BITS *)
(* be (by bits)
 package version : 3
 type id : 3
 
 id : 4 => data literal: single binary value, padded to 4 bit width (nibble). be : prefixed with (1 - not last) or (0 - last)
 the value is re-packed into bytes, and may have leading zeros

 id : not 4 : operators
 
 l-id : 0 => super packet: next 15-bits are the total length of sub-packet bits
 l-id : 1 => number of sub-packets

 part 1:
 add up all the version numbers.
*)

let p16_1 () =
 let ic = open_in "16-1.txt" in
 let input = input_line ic |> Bytes.of_string |>
  Bytes.map begin fun b ->
   let bc = Char.code b in
   if bc < Char.code 'A' then (bc - Char.code '0' |> Char.chr)
   else (bc + 10 - Char.code 'A' |> Char.chr)
  end in close_in ic;
 let get_nibble idx = Bytes.get input idx |> Char.code  in
 let get_bit idx =
  let nib_idx = idx / 4 and mask_idx = 3 - (idx mod 4) in
  (get_nibble nib_idx land (1 lsl mask_idx)) lsr mask_idx in
 let get_bits idx n =
  if n > 62 then failwith "62 bit int limit" else begin
   let acc = ref 0 in
   for i = 0 to n - 1 do
    acc := (get_bit (idx+i) lsl (n-i-1)) lor !acc
   done; !acc
  end in
  let rec read_next_packet acc idx =
   let packet_ver = get_bits idx 3 in
   let type_id = get_bits (idx+3) 3 in
   if type_id = 4 then begin
    let cur = ref (idx+6) in
    while get_bit !cur <> 0 do cur := !cur + 5 done; cur := !cur + 5;
    (packet_ver,!cur) :: acc
   end else begin
    let len_id = get_bit (idx+6) in
    if len_id = 0 then begin
     let count = get_bits (idx+7) 15 and cur0 = idx + 22 in
     let subp = ref [packet_ver,cur0] and cur = ref cur0 in
     while !cur < (cur0+count) do
      subp := read_next_packet !subp !cur ;
      (match List.hd !subp with (_,c) -> cur := c)
     done ; List.append !subp acc
    end else begin
     let count = get_bits (idx+7) 11 and cur0 = idx+18 in
     let subp = ref [packet_ver,cur0] and cur = ref cur0 in
     for i = 1 to count do
      subp := read_next_packet !subp !cur ;
      (match List.hd !subp with (_,c) -> cur := c)
     done ; List.append !subp acc
    end end in
    read_next_packet [] 0 |>
    List.fold_left (fun acc (v,_) -> acc + v) 0

let p16_2 () =
 let ic = open_in "16-1.txt" in
 let input = input_line ic |> Bytes.of_string |>
  Bytes.map begin fun b ->
   let bc = Char.code b in
   if bc < Char.code 'A' then (bc - Char.code '0' |> Char.chr)
   else (bc + 10 - Char.code 'A' |> Char.chr)
  end in close_in ic;
 let get_nibble idx = Bytes.get input idx |> Char.code  in
 let get_bit idx =
  let nib_idx = idx / 4 and mask_idx = 3 - (idx mod 4) in
  (get_nibble nib_idx land (1 lsl mask_idx)) lsr mask_idx in
 let get_bits idx n =
  if n > 62 then failwith "62 bit int limit" else begin
   let acc = ref 0 in
   for i = 0 to n - 1 do
    acc := (get_bit (idx+i) lsl (n-i-1)) lor !acc
   done; !acc
  end in

  let read_literal idx =
   let rec aux (acc, idx) =
    let h = get_bit idx and next = get_bits (idx+1) 4 :: acc in
    if h = 1 then aux (next, idx+5)
    else (next, idx+5) in
   let (lit_list,idx') = aux ([], idx) in
   (lit_list |> List.mapi (fun i n -> n lsl (i*4)) |> List.fold_left (+) 0, idx') in

  let compute_sub_packet type_id subp =
   let cur' = List.hd subp |> (fun (_,_,c) -> c) in
   Printf.printf "type: %d\n" type_id;
   List.iter (fun (a,b,c) -> Printf.printf "(%d,%d,%d)\n" a b c) subp;
   match subp with
   | sps when type_id = 0 -> (*sum*)
     let v' = List.fold_left (fun acc (_,v,_) -> acc + v) 0 sps in (~- type_id, v', cur')
   | sps when type_id = 1 -> (*product*)
     let v' = List.fold_left (fun acc (_,v,_) -> acc * v) 1 sps in (~- type_id, v', cur')
   | sps when type_id = 2 -> (*minimum*)
     let v' = List.fold_left (fun acc (_,v,_) -> min acc v) Int.max_int sps in (~- type_id, v', cur')
   | sps when type_id = 3 -> (*maximum*)
     let v' = List.fold_left (fun acc (_,v,_) -> max acc v) Int.min_int sps in (~- type_id, v', cur')
   | (_,v1,_)::(_,v2,_)::[] when type_id = 5 -> (~- type_id, (if v2 > v1 then 1 else 0), cur') (*gt*)
   | (_,v1,_)::(_,v2,_)::[] when type_id = 6 -> (~- type_id, (if v2 < v1 then 1 else 0), cur') (*lt*)
   | (_,v1,_)::(_,v2,_)::[] when type_id = 7 -> (~- type_id, (if v2 = v1 then 1 else 0), cur') (*eq*)
   | _ -> failwith "invalid" in

  let rec read_next_packet acc idx =
   let packet_ver = get_bits idx 3 in
   let type_id = get_bits (idx+3) 3 in
   if type_id = 4 then begin
    let cur = idx+6 in
    let (lit,cur') = read_literal cur in
    (packet_ver,lit,cur') :: acc
   end else begin
    let len_id = get_bit (idx+6) in
    if len_id = 0 then begin
     let count = get_bits (idx+7) 15 and cur0 = idx + 22 in
     (*let subp = ref [packet_ver,~- type_id,cur0] and cur = ref cur0 in*)
     let subp = ref [] and cur = ref cur0 in
     while !cur < (cur0+count) do
      subp := read_next_packet !subp !cur ;
      (match List.hd !subp with (_,_,c) -> cur := c)
     done ; compute_sub_packet type_id !subp :: acc
    end else begin
     let count = get_bits (idx+7) 11 and cur0 = idx+18 in
     (*let subp = ref [packet_ver,- type_id,cur0] and cur = ref cur0 in*)
     let subp = ref [] and cur = ref cur0 in
     for i = 1 to count do
      subp := read_next_packet !subp !cur ;
      (match List.hd !subp with (_,_,c) -> cur := c)
     done ; compute_sub_packet type_id !subp :: acc
    end end in
    read_next_packet [] 0

let p17_1 () =
 let x0 = 0 and y0 = 0 in
 let ic = open_in "17-1.txt" in
 let (x1,x2,y1,y2) =
  input_line ic |>
  begin fun s ->
   Scanf.sscanf s " target area: x=%d..%d, y=%d..%d "
   (fun a b c d -> (a,b,c,d)) end in close_in ic;
 let rec check x y vx vy =
  if x >= x1 && x <= x2 && y >= y1 && y <= y2 then (true,'s')
  else if y < y1 then (false,'y')
  else if x > x2 then (false,'x')
  else begin
   let x' = x+vx and y' = y+vy
   and vx' = vx + (if vx > 0 then ~- 1 else if vx < 0 then 1 else 0)
   and vy' = pred vy in
   check x' y' vx' vy'
  end in
 let maxh vy =
  let rec aux my y vy =
   let y' = y + vy and vy' = pred vy in
   let my' = max my y' in
   if y' < 0 then my' else aux my' y' vy'
  in aux 0 0 vy
 in
 let mx = ref 0 and my = ref 0 in
 let ycap = 100 in (*init: 500*)
 for i = 0 to x2 do for j = 0 to ycap do (* brute force *)
  if check x0 y0 i j |> fst then if !my < j then (mx := i; my := j)
 done done;
 x1,x2,y1,y2,!mx,!my, maxh !my, check x0 y0 !mx !my

(* "intelligent" search : finds x but not y*)
(*
 let vx = ref 0 and vy = ref 0 in
 while begin match check x0 y0 !vx !vy with (_,'x') -> false | _ -> true end do incr vx done;
 while !vx > 0 do
  decr vx;
  while begin match check x0 y0 !vx !vy with (_,'x') -> false | (_,'y') -> false | _ -> true end do incr vy done;
  if check x0 y0 !vx (!vy-1) |> fst then decr vy;
  if check x0 y0 !vx !vy |> fst then if !vy > !my then (my := !vy; mx := !vx);
 done;
*)

let p17_2 () =
 let x0 = 0 and y0 = 0 in
 let ic = open_in "17-1.txt" in
 let (x1,x2,y1,y2) =
  input_line ic |>
  begin fun s ->
   Scanf.sscanf s " target area: x=%d..%d, y=%d..%d "
   (fun a b c d -> (a,b,c,d)) end in close_in ic;
 let rec check x y vx vy =
  if x >= x1 && x <= x2 && y >= y1 && y <= y2 then (true,'s')
  else if y < y1 then (false,'y')
  else if x > x2 then (false,'x')
  else begin
   let x' = x+vx and y' = y+vy
   and vx' = vx + (if vx > 0 then ~- 1 else if vx < 0 then 1 else 0)
   and vy' = pred vy in
   check x' y' vx' vy'
  end in
 let acc = ref [] in
 let ycap = 100 in
 for i = 0 to x2 do for j = y1 to ycap do (* brute force *)
  if check x0 y0 i j |> fst then acc:= (i,j)::!acc
 done done;
 x1,x2,y1,y2,List.length (!acc)

let p18_1 () =
 let ic = open_in "18-1.txt" in
 let rec read_input lines =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> lines
  | Some s -> read_input (s::lines) in
 let input = read_input [] |> List.rev in close_in ic ;
 let parse_line s = 
  let ss = String.split_on_char ',' s in
  (*List.iter (print_endline) ss ;*)
  let depth = ref 0 in
  let to_leaf s =
    String.iter begin fun c -> if c = '[' then incr depth else (if c = ']' then decr depth) end s ;
    let k = s |> String.fold_left
                (fun acc c -> if Char.code c <= Char.code '9' && Char.code c >= Char.code '0'
                              then acc ^ String.make 1 c else acc) ("") |> (fun x -> Scanf.sscanf x "%d" (Fun.id))
                              in (k,!depth)
  in List.map (to_leaf) ss in
 let add ls1 ls2 =
  let ls = List.map (fun (a,b) -> (a,succ b)) ls1 @ (List.map (fun (a,b) -> (a,succ b)) ls2) in
   List.rev ls |> List.hd |> (fun (a,_) -> List.rev ((a,0)::(List.rev ls |> List.tl))) in
 let try_explode ls =
  let rec aux acc = function
  | (ka,ia)::(kb,ib)::ls' when ia > 4 ->
    begin match (acc,ls') with
    | ((last,ic)::acc', (next,id)::ls'') -> Some (List.rev ((last+ka,ic)::acc') @ (0,ib)::(next+kb,id)::ls'')
    | ((last,ic)::acc', []) -> Some (List.rev ((0,ib)::(last+ka,ic)::acc'))
    | ([], (next,id)::ls'') -> Some ((0,ib)::(next+kb,id)::ls'')
    | ([],[]) -> Some [(0,ib)] end
  | a::bs -> aux (a::acc) bs
  | [] -> None
  in aux [] ls in
 let unparse_line ls =
  List.fold_left begin fun (last,acc) (a,ai) ->
   if ai > last then (ai,((String.make (ai-last) '[') ^ (Printf.sprintf "%d" a))::acc)
   else (ai,((Printf.sprintf "%d" a) ^ (String.make (last-ai) ']'))::acc)
  end (0,[]) ls |> snd |> List.rev |> String.concat "," in
 let try_split ls =
  let rec aux acc = function
  | (ka,ia)::ls' when ka > 9 -> begin
   let br = (match acc with [] -> 0 | (ka',ia')::_ -> ia') in
   if ia >  br then Some (List.rev acc @ (ka/2,succ ia)::(ka / 2 + (ka mod 2),ia)::ls')
   else Some (List.rev acc @ (ka/2,succ br)::(ka / 2 + (ka mod 2),ia)::ls')
  end
  | (ka,ia)::ls' -> aux ((ka,ia)::acc) ls'
  | [] -> None in aux [] ls in
 let rec process ls =
  match try_explode ls with
  | Some ls' -> print_endline (unparse_line ls') ; process ls'
  | None -> begin
   match try_split ls with
   | Some ls' -> print_endline (unparse_line ls') ; process ls'
   | None -> ls end in
 let mag ls =
  let rec aux acc = function
  | (a,ai)::(b,bi)::cs when bi <= ai -> aux ((a*3+b*2,pred ai)::acc) cs
  | (a,ai)::[] when acc = [] -> a
  | (a,ai)::bs -> aux ((a,ai)::acc) bs
  | [] -> aux [] (List.rev acc) in aux [] ls
 in
 (* List.hd input |> parse_line |> try_explode *)
 let nums = List.map (fun s -> s |> parse_line |> process) input in
 List.fold_left (fun a n -> process (add a n)) (List.hd nums) (List.tl nums) |> mag
 (*add (parse_line "") (parse_line "") |> process*)

let p18_2 () =
 let ic = open_in "18-1.txt" in
 let rec read_input lines =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> lines
  | Some s -> read_input (s::lines) in
 let input = read_input [] |> List.rev in close_in ic ;
 let parse_line s = 
  let ss = String.split_on_char ',' s in
  (*List.iter (print_endline) ss ;*)
  let depth = ref 0 in
  let to_leaf s =
    String.iter begin fun c -> if c = '[' then incr depth else (if c = ']' then decr depth) end s ;
    let k = s |> String.fold_left
                (fun acc c -> if Char.code c <= Char.code '9' && Char.code c >= Char.code '0'
                              then acc ^ String.make 1 c else acc) ("") |> (fun x -> Scanf.sscanf x "%d" (Fun.id))
                              in (k,!depth)
  in List.map (to_leaf) ss in
 let add ls1 ls2 =
  let ls = List.map (fun (a,b) -> (a,succ b)) ls1 @ (List.map (fun (a,b) -> (a,succ b)) ls2) in
   List.rev ls |> List.hd |> (fun (a,_) -> List.rev ((a,0)::(List.rev ls |> List.tl))) in
 let try_explode ls =
  let rec aux acc = function
  | (ka,ia)::(kb,ib)::ls' when ia > 4 ->
    begin match (acc,ls') with
    | ((last,ic)::acc', (next,id)::ls'') -> Some (List.rev ((last+ka,ic)::acc') @ (0,ib)::(next+kb,id)::ls'')
    | ((last,ic)::acc', []) -> Some (List.rev ((0,ib)::(last+ka,ic)::acc'))
    | ([], (next,id)::ls'') -> Some ((0,ib)::(next+kb,id)::ls'')
    | ([],[]) -> Some [(0,ib)] end
  | a::bs -> aux (a::acc) bs
  | [] -> None
  in aux [] ls in
(*
 let unparse_line ls =
  List.fold_left begin fun (last,acc) (a,ai) ->
   if ai > last then (ai,((String.make (ai-last) '[') ^ (Printf.sprintf "%d" a))::acc)
   else (ai,((Printf.sprintf "%d" a) ^ (String.make (last-ai) ']'))::acc)
  end (0,[]) ls |> snd |> List.rev |> String.concat "," in
*)
 let try_split ls =
  let rec aux acc = function
  | (ka,ia)::ls' when ka > 9 -> begin
   let br = (match acc with [] -> 0 | (ka',ia')::_ -> ia') in
   if ia >  br then Some (List.rev acc @ (ka/2,succ ia)::(ka / 2 + (ka mod 2),ia)::ls')
   else Some (List.rev acc @ (ka/2,succ br)::(ka / 2 + (ka mod 2),ia)::ls')
  end
  | (ka,ia)::ls' -> aux ((ka,ia)::acc) ls'
  | [] -> None in aux [] ls in
 let rec process ls =
  match try_explode ls with
  | Some ls' -> (*print_endline (unparse_line ls') ;*) process ls'
  | _ -> begin
   match try_split ls with
   | Some ls' -> (*print_endline (unparse_line ls') ;*) process ls'
   | _ -> ls end in
 let mag ls =
  let rec aux acc = function
  | (a,ai)::(b,bi)::cs when bi <= ai -> aux [] ((List.rev ((a*3+b*2,pred ai)::acc)) @ cs)
  | (a,ai)::[] when acc = [] -> a
  | (a,ai)::bs -> aux ((a,ai)::acc) bs
  | [] -> aux [] (List.rev acc) in aux [] ls
 in
 let nums = List.map (fun s -> s |> parse_line |> process) input in
 let max_mag = ref 0 in
 for i = 0 to pred @@ pred @@ List.length nums do for j = i to pred @@ List.length nums do
  let a = List.nth nums i and b = List.nth nums j in
  Printf.printf "a: %d, b: %d\n" i j;
   let ab = add a b |> process |> mag
   and ba = add b a |> process |> mag in
  max_mag := max ab ba |> max !max_mag;
  Printf.printf "ab: %d, ab: %d\n" ab ba;
  Printf.printf "max: %d\n" !max_mag;
 done done; !max_mag

let p21_1 () =
 let p1_init = 6 and p2_init = 2 in
 let nr = ref 0 in
 let roll seed =
  nr := !nr + 3 ;
  (* seed is always die - 1 *)
  if seed < 98 then ((seed * 3 + 6) mod 100, (seed + 3) mod 100)
  else if seed = 98 then (200, 1)
  else (103, 2) in
 let rec play (p1,p1s) (p2,p2s) is =
  let (r,s) = roll is in
  let p1' = (p1+r) mod 10 in
  let p1s' = p1s + p1' + 1 in
  if p1s' >= 1000 then Printf.printf "P1 WIN: P1: %d, P2: %d, nr: %d s: %d\n" p1s' p2s !nr s
  else begin
   let (r',s') = roll s in
   let p2' = (p2+r') mod 10 in
   let p2s' = p2s + p2' + 1 in
   if p2s' >= 1000 then Printf.printf "P2 WIN: P1: %d, P2: %d, nr: %d s:%d\n" p1s' p2s' !nr s'
   else play (p1',p1s') (p2',p2s') s'
  end in
  play (pred p1_init,0) (pred p2_init,0) 0

(* takes ~ 1 min to execute *)
let p21_2 () =
 let p1_init = 6 and p2_init = 2 in
 let wscore = 21 in
 let p1w = ref 0 and p2w = ref 0 in
 let rec play (p1,p1s) (p2,p2s) roll left mult first =
  if left then begin
   let p1' = (roll + p1) mod 10 in
   let p1s' = p1s + p1' + 1 in
   if p1s' >= wscore then p1w := !p1w + mult else begin
    play (p1',p1s') (p2,p2s) 3 false (mult*1) false ;
    play (p1',p1s') (p2,p2s) 4 false (mult*3) false ;
    play (p1',p1s') (p2,p2s) 5 false (mult*6) false ;
    play (p1',p1s') (p2,p2s) 6 false (mult*7) false ;
    play (p1',p1s') (p2,p2s) 7 false (mult*6) false ;
    play (p1',p1s') (p2,p2s) 8 false (mult*3) false ;
    play (p1',p1s') (p2,p2s) 9 false (mult*1) false end end
  else begin
   let p2' = (roll + p2) mod 10 in
   let p2s' = if first then 0 else p2s + p2' + 1 in
   if p2s' >= wscore then p2w := !p2w + mult else begin
    play (p1,p1s) (p2',p2s') 3 true (mult*1) false ;
    play (p1,p1s) (p2',p2s') 4 true (mult*3) false ;
    play (p1,p1s) (p2',p2s') 5 true (mult*6) false ;
    play (p1,p1s) (p2',p2s') 6 true (mult*7) false ;
    play (p1,p1s) (p2',p2s') 7 true (mult*6) false ;
    play (p1,p1s) (p2',p2s') 8 true (mult*3) false ;
    play (p1,p1s) (p2',p2s') 9 true (mult*1) false end end
  in play (pred p1_init,0) (pred p2_init,0) 0 false 1 true;
     Printf.printf "p1w: %d p2w: %d diff: %d\n" !p1w !p2w (!p1w - !p2w)

(* requires a very small global map *)
let p22_1 () =
 let reactor = Array.init 101 (fun _ -> Array.init 101 (fun _ -> Array.make 101 false)) in
 let reactor_set v z0 z1 y0 y1 x0 x1 =
  for k=(z0+50) to (z1+50) do for j=(y0+50) to (y1+50) do for i=(x0+50) to (x1+50) do
   reactor.(k).(j).(i) <- v
  done done done in
 let reactor_count () =
  let acc = ref 0 in
  for k=0 to 100 do for j=0 to 100 do for i=0 to 100 do
   if reactor.(k).(j).(i) then incr acc
  done done done; !acc in
 let ic = open_in "22-1.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> List.rev acc
  | Some s ->
    let t = if s.[1] = 'n' then true else false in
    let skip = if t then 3 else 4 in
    let s' = String.sub s skip (String.length s - skip) in
    let (x0,x1,y0,y1,z0,z1) = Scanf.sscanf s' "x=%d..%d,y=%d..%d,z=%d..%d"
    begin fun x x' y y' z z' ->
     (min x x', max x x', min y y', max y y', min z z', max z z')
    end in read_input ((t,x0,x1,y0,y1,z0,z1)::acc) in
  let input = read_input [] in
  Printf.printf "%d\n" (reactor_count ());
  List.iter begin fun (t,x0,x1,y0,y1,z0,z1) ->
   reactor_set t z0 z1 y0 y1 x0 x1
  end input ;
  reactor_count ()

(* very fast *)
let p22_2 () =
 (* find the intersection of two cubes *)
 let intersection (z0,z1,y0,y1,x0,x1) (z0',z1',y0',y1',x0',x1') =
  let bz0' = if z0' <= z1 && z0' >= z0 || z0 <= z1' && z0 >= z0' then true else false
  and bz1' = if z1' <= z1 && z1' >= z0 || z1 <= z1' && z1 >= z0' then true else false
  and by0' = if y0' <= y1 && y0' >= y0 || y0 <= y1' && y0 >= y0' then true else false
  and by1' = if y1' <= y1 && y1' >= y0 || y1 <= y1' && y1 >= y0' then true else false
  and bx0' = if x0' <= x1 && x0' >= x0 || x0 <= x1' && x0 >= x0' then true else false
  and bx1' = if x1' <= x1 && x1' >= x0 || x1 <= x1' && x1 >= x0' then true else false in
  let z0'' = max z0 z0' and z1'' = min z1 z1'
  and y0'' = max y0 y0' and y1'' = min y1 y1'
  and x0'' = max x0 x0' and x1'' = min x1 x1' in
  if not ((bz0' || bz1') && (by0' || by1') && (bx0' || bx1')) then None
  else Some (z0'', z1'', y0'', y1'', x0'', x1'') in
 (* intersection function with on/off passthrough *)
 let tintersection (t,z0,z1,y0,y1,x0,x1) (z0',z1',y0',y1',x0',x1') =
  match intersection (z0,z1,y0,y1,x0,x1) (z0',z1',y0',y1',x0',x1') with
  | None -> None
  | Some (z0'',z1'',y0'',y1'',x0'',x1'') -> Some (t,z0'',z1'',y0'',y1'',x0'',x1'') in
 (* calculate cube volume with inclusive edges *)
 let volume (z0,z1,y0,y1,x0,x1) = (z1 - z0 + 1) * (y1 - y0 + 1) * (x1 - x0 + 1) in
 let ic = open_in "22-2.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> List.rev acc
  | Some s ->
    let t = if s.[1] = 'n' then true else false in
    let skip = if t then 3 else 4 in
    let s' = String.sub s skip (String.length s - skip) in
    let (x0,x1,y0,y1,z0,z1) = Scanf.sscanf s' "x=%d..%d,y=%d..%d,z=%d..%d"
    begin fun x x' y y' z z' ->
     (min x x', max x x', min y y', max y y', min z z', max z z')
    end in read_input ((t,z0,z1,y0,y1,x0,x1)::acc) in
  let input = read_input [] in
  let rec count_plus cs acc = function
  | (true,z0,z1,y0,y1,x0,x1)::tail ->
    (* locate all "on" intersections below cursor *)
    let sub = List.map begin fun (t,z0',z1',y0',y1',x0',x1') ->
     if t then tintersection (true,z0',z1',y0',y1',x0',x1') (z0,z1,y0,y1,x0,x1) else None
    end acc |> List.filter (Option.is_some) |> List.map (Option.get) in
    (* add volume, acc count and recurse through nested intersections *)
    let cs' = (volume (z0,z1,y0,y1,x0,x1)) + cs - (count_plus 0 [] sub) in
    count_plus cs' ((true,z0,z1,y0,y1,x0,x1)::acc) tail
  | (false,z0,z1,y0,y1,x0,x1)::tail -> count_plus cs acc tail (* ignore off cmds *)
  | [] -> cs in
  let rec count cs acc = function
  | (true,z0,z1,y0,y1,x0,x1)::tail ->
    (* calculate volume of ALL (on/off) masked (intersecting) regions above the cursor *)
    let sub = List.map begin fun (t,z0',z1',y0',y1',x0',x1') ->
     tintersection (true,z0,z1,y0,y1,x0,x1) (z0',z1',y0',y1',x0',x1')
    end tail |> List.filter (Option.is_some) |> List.map (Option.get) |> count_plus 0 [] |> (~-) in
    (* cs' = unmasked volume of "on" command *)
    let cs' = (volume (z0,z1,y0,y1,x0,x1)) + cs + sub in
    count cs' ((true,z0,z1,y0,y1,x0,x1)::acc) tail
  | (false,z0,z1,y0,y1,x0,x1)::tail -> count cs acc tail (* ignore off *)
  | [] -> cs in
  count 0 [] input

(* works but is incredibly slow O(n^3) for large areas *)
let p22_3 () =
 let intersection0 (x0,x1,y0,y1,z0,z1) (x0',y0',z0') =
  z0' <= z1 && z0' >= z0 &&
  y0' <= y1 && y0' >= y0 &&
  x0' <= x1 && x0' >= x0 in
 let rec interaction vs last (t',x0',y0',z0') =
  match vs with
  | (t,x0,x1,y0,y1,z0,z1)::vs' ->
    if not last && t && intersection0 (x0,x1,y0,y1,z0,z1) (x0',y0',z0') then
     interaction vs' true (t',x0',y0',z0')
    else if last && not t && intersection0 (x0,x1,y0,y1,z0,z1) (x0',y0',z0') then
     interaction vs' false (t',x0',y0',z0')
    else interaction vs' last (t',x0',y0',z0') (* last does not change *)
  | [] ->
   if last && t' || not (last || t') then 0
   else if not last && t' then 1
   else ~- 1 in
 let ic = open_in "22-2s.txt" in
 let rec read_input acc =
  match try Some (input_line ic) with End_of_file -> close_in ic ; None with
  | None -> List.rev acc
  | Some s ->
    let t = if s.[1] = 'n' then true else false in
    let skip = if t then 3 else 4 in
    let s' = String.sub s skip (String.length s - skip) in
    let (x0,x1,y0,y1,z0,z1) = Scanf.sscanf s' "x=%d..%d,y=%d..%d,z=%d..%d"
    begin fun x x' y y' z z' ->
     (min x x', max x x', min y y', max y y', min z z', max z z')
    end in read_input ((t,x0,x1,y0,y1,z0,z1)::acc) in
  let input = read_input [] in
  let rec count cs acc = function
  | (t,x0,x1,y0,y1,z0,z1)::vs' ->
    let cs' = ref cs in
    for k = z0 to z1 do for j = y0 to y1 do for i = x0 to x1 do
     cs' := !cs' + interaction acc false (t,i,j,k)
    done done done; Printf.printf "Count Update: %d\n" !cs' ; flush_all ();
    count !cs' (acc @ [t,x0,x1,y0,y1,z0,z1]) vs'
  | [] -> cs in
  input,
  count 0 [] input
