(* aoc.ml *)
(* advent of code 2023 *)

(* find first and last digit of every line, then sum *)
let problem_01a () =
 let input = In_channel.(with_open_bin "01.txt" input_all) |> String.split_on_char '\n' in
 let decode_line s = String.to_seq s |>
  Seq.filter_map begin fun c ->
   match Char.code c with c when c >= 0x30 && c <= 0x39 -> Some (c - 0x30) | _ -> None
  end |> (fun seq -> match seq () with  Nil -> 0 | Cons (hd, tl) -> hd * 10 + (Seq.fold_left (fun a x -> x) 0 (seq))) in
 (*List.map decode_line input |> List.iter (fun x -> Printf.printf "%d\n" x) ;*)
 List.map decode_line input |> List.fold_left (+) 0

(* alternative file reading method *)
let problem_01a2 () =
 (* let ( let* ) o f = match o with | None -> None | Some x -> f x and return x = Some x in *)
 (* let lines = In_channel.(with_open_bin "01.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> List.of_seq)) in *)
 let lines_once f = In_channel.(with_open_bin "01.txt" (fun ic -> Seq.of_dispenser (fun () -> In_channel.input_line ic) |> f)) in
 let decode_line s =
  let first = ref 0 and last = ref 0 in
  String.iter begin fun c ->
   (match Char.code c with c when c >= 0x30 && c <= 0x39 -> Some (c - 0x30) | _ -> None) |>
   Option.iter (fun n -> last := n ; if !first = 0 then first := n)
  end s ; !first * 10 + !last in
 lines_once (fun line_seq -> Seq.map decode_line (line_seq) |> Seq.fold_left (+) 0)

let problem_01b () =
 let numbers = [| "one" ; "two" ; "three" ; "four" ; "five" ; "six" ; "seven" ; "eight" ; "nine" |] in
 let lines = In_channel.(with_open_bin "01.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> List.of_seq)) in
 let convert_line s =
  let s_len = Bytes.length s in
  for i = 0 to s_len - 3 do for j = 0 to 8 do
   let n_len = String.length numbers.(j) in
   if i+n_len <= s_len && String.equal (Bytes.sub_string s i (n_len)) numbers.(j) then Bytes.set s i (0x31 + j |> Char.chr)
  done done in
 List.iter (fun s -> Bytes.unsafe_of_string s |> convert_line) lines;
 let decode_line s =
  let first = ref 0 and last = ref 0 in
  String.iter begin fun c ->
   (match Char.code c with c when c >= 0x30 && c <= 0x39 -> Some (c - 0x30) | _ -> None) |>
   Option.iter (fun n -> last := n ; if !first = 0 then first := n)
  end s ; !first * 10 + !last in
 List.map decode_line lines |> List.fold_left (+) 0

(* cube game *)
let problem_02a () =
 let lines = In_channel.(with_open_bin "02.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> List.of_seq)) in
 let parse s =
  match String.split_on_char ':' s with
   [ id_s ; data ] -> 
    let id = Scanf.sscanf id_s " Game %u " Fun.id in
    let r = ref 0 and g = ref 0 and b = ref 0 in
    String.split_on_char ';' data |> List.map (fun s -> String.split_on_char ',' s) |> List.flatten |>
    List.iter (fun s ->
      let x, s = Scanf.sscanf s " %d %s " (fun a b -> a,b) in
      if String.equal s "red"   then r := max !r x else
      if String.equal s "green" then g := max !g x else
      if String.equal s "blue"  then b := max !b x else raise_notrace @@ Invalid_argument "Invalid Color" );
      (id, !r, !g, !b)
  | _ -> raise_notrace (Invalid_argument "Invalid Input") in
 let test r g b = r <= 12 && g <= 13 && b <= 14 in
 (*List.map parse lines |> List.iter (fun (a,b,c,d) -> Printf.printf "%d %d %d %d\n" a b c d)*)
 List.map parse lines |> List.fold_left (fun acc (id,r,g,b) -> if test r g b then acc + id else acc) 0

let problem_02b () =
 let lines = In_channel.(with_open_bin "02.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> List.of_seq)) in
 let parse s =
  match String.split_on_char ':' s with
   [ id_s ; data ] -> 
    let id = Scanf.sscanf id_s " Game %u " Fun.id in
    let r = ref 0 and g = ref 0 and b = ref 0 in
    String.split_on_char ';' data |> List.map (fun s -> String.split_on_char ',' s) |> List.flatten |>
    List.iter (fun s ->
      let x, s = Scanf.sscanf s " %d %s " (fun a b -> a,b) in
      if String.equal s "red"   then r := max !r x else
      if String.equal s "green" then g := max !g x else
      if String.equal s "blue"  then b := max !b x else raise_notrace @@ Invalid_argument "Invalid Color" );
      (id, !r, !g, !b)
  | _ -> raise_notrace (Invalid_argument "Invalid Input") in
 (*let test r g b = r <= 12 && g <= 13 && b <= 14 in *)
 (*List.map parse lines |> List.iter (fun (a,b,c,d) -> Printf.printf "%d %d %d %d\n" a b c d)*)
 List.map parse lines |> List.fold_left (fun acc (_,r,g,b) -> acc + r * g * b) 0

(* find numbers next to symbols *)
let problem_03a () =
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let lines = In_channel.(with_open_bin "03.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length lines and w = String.length lines.(0) in
 let is_digit y x =
  y < h && y >= 0 && x < w && x >= 0 &&
  ( let c = Char.code lines.(y).[x] in c >= 0x30 && c <= 0x39 ) in
 let is_symbol y x = not (is_digit y x) && lines.(y).[x] <> '.' in
 let addr_of_number y x =
  if not (is_digit y x) then None else
  let rec loop x =
   if not (is_digit y x) then Some (y,x+1) else loop (x-1)
  in loop (x-1) in
 (* treat as null terminated string *)
 let number_of_addr (y, x) =
  let rec loop n = if is_digit y (x+n) then loop (n+1) else n in
  int_of_string (String.sub lines.(y) x (loop 1)) in
 let yx_set = ref (YXSet.empty) in
 let try_add_number y x =
  match addr_of_number y x with | Some addr -> yx_set := YXSet.add addr !yx_set | None -> () in
 for i = 0 to pred h do for j = 0 to pred w do
  (* try_add_number i j *) (* for all numbers : debug *)
  if is_symbol i j then for y = -1 to 1 do for x = -1 to 1 do try_add_number (i+y) (j+x) done done
 done done ;
 YXSet.to_seq !yx_set |> Seq.map number_of_addr |> Seq.fold_left (+) 0
 (* YXSet.to_seq !yx_set |> Seq.map number_of_addr |> Seq.iter (fun x -> print_int x; print_newline ()) *)
 (* YXSet.to_seq !yx_set |> Seq.iter (fun (y,x) -> Printf.printf "(%d,%d)\n" y x) *)

let problem_03b () =
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let lines = In_channel.(with_open_bin "03.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length lines and w = String.length lines.(0) in
 let is_digit y x =
  y < h && y >= 0 && x < w && x >= 0 &&
  ( let c = Char.code lines.(y).[x] in c >= 0x30 && c <= 0x39 ) in
 (* let is_symbol y x = not (is_digit y x) && lines.(y).[x] <> '.' in *)
 let is_star y x = not (is_digit y x) && lines.(y).[x] = '*' in
 let addr_of_number y x =
  if not (is_digit y x) then None else
  let rec loop x =
   if not (is_digit y x) then Some (y,x+1) else loop (x-1)
  in loop (x-1) in
 (* treat as null terminated string *)
 let number_of_addr (y, x) =
  let rec loop n = if is_digit y (x+n) then loop (n+1) else n in
  int_of_string (String.sub lines.(y) x (loop 1)) in
 let yx_set = ref (YXSet.empty) in
 let acc = ref 0 in
 let try_add_number y x =
  match addr_of_number y x with | Some addr -> yx_set := YXSet.add addr !yx_set | None -> () in
 for i = 0 to pred h do for j = 0 to pred w do
  if is_star i j then for y = -1 to 1 do for x = -1 to 1 do try_add_number (i+y) (j+x) done done ;
  if YXSet.cardinal !yx_set <> 2 then yx_set := YXSet.empty else
   ( acc := !acc + (YXSet.to_seq !yx_set |> Seq.map number_of_addr |> Seq.fold_left ( * ) 1) ; yx_set := YXSet.empty )
 done done ;
 !acc

let problem_04a () =
 let module IntSet = Set.Make(struct type t = int let compare = compare end) in
 let parse_line s = s |>
  String.split_on_char ':' |> (function [_; data] -> data | _ -> assert false) |> String.split_on_char '|' |>
  List.map (fun s -> String.split_on_char ' ' s |> List.filter (fun s -> s <> String.empty) |> List.map (int_of_string)) |>
  (function [x; y] -> (IntSet.of_list x, IntSet.of_list y) | _ -> raise_notrace @@ Invalid_argument "Invalid Input") in
 let cards = In_channel.(with_open_bin "04.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map parse_line |> Array.of_seq)) in
 let wins = Array.map (fun (x,y) -> IntSet.inter x y |> IntSet.cardinal) cards in
 Array.fold_left (fun a w -> ((1 lsl w) lsr 1) + a) 0 wins

let problem_04b () =
 let module IntSet = Set.Make(struct type t = int let compare = compare end) in
 let parse_line s = s |>
  String.split_on_char ':' |> (function [_; data] -> data | _ -> assert false) |> String.split_on_char '|' |>
  List.map (fun s -> String.split_on_char ' ' s |> List.filter (fun s -> s <> String.empty) |> List.map (int_of_string)) |>
  (function [x; y] -> (IntSet.of_list x,IntSet.of_list y) | _ -> raise_notrace @@ Invalid_argument "Invalid Input") in
 (* combine unnecessary intermediate values into one call *)
 let wins = In_channel.(
  with_open_bin "04.txt" ( fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map parse_line |>
   Seq.map (fun (x,y) -> IntSet.inter x y |> IntSet.cardinal) |> Array.of_seq )) in
 let len = Array.length wins in
 let cards = Array.make (len) 1 in
 for i = 0 to len - 2 do
  for j = i + 1 to min (len - 1) (i + wins.(i)) do
   cards.(j) <- cards.(j) + cards.(i)
  done
 done ;
 Array.fold_left (+) 0 cards
 
(*
 Seed Mapping:
 seeds: []
 seed-to-soil:
 soil-to-fertilizer:
 fertilizer-to-water:
 water-to-light:
 light-to-temperature:
 temperature-to-humidity:
 humidity-to-location:
 [dst src len]
 unmapped values equal each other
 part1 res: lowest location number from any initial seed
*)

let problem_05a () =
 let rec map_value_with_book input book =
  match book with
  | [] -> input
  | (dst,src,len)::book' when input >= src && input < src+len -> input-src+dst
  | _::book' -> map_value_with_book input book' in
 (* parse with fold_left *)
 let parse acc line =
  match acc with
  | ([], _, _) ->
    let seeds =
     String.split_on_char ':' line |>
     List.tl |> List.hd |> String.split_on_char ' ' |>
     List.filter ((<>)"") |> List.map int_of_string in
     (seeds, [], [])
  | _ when line = "" -> acc
  | (seeds, [], almanac) when String.contains line ':' -> (seeds, [], almanac)
  | (seeds, book, almanac) when String.contains line ':' -> (seeds, [], book::almanac)
  | (seeds, book, almanac) ->
    let entry =
     (match String.split_on_char ' ' line |> List.filter((<>)"") |> List.map int_of_string with
      [a;b;c] -> (a,b,c) | _ -> raise @@ Invalid_argument "invalid input: expected 3 numbers per map entry") in
    (seeds, (entry::book), almanac) in
 let lines = In_channel.(with_open_bin "05.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let (seeds, last_book, almanac) = Array.fold_left parse ([],[],[]) lines in
 let almanac = last_book::almanac |> List.rev in
 List.map (fun seed -> List.fold_left (map_value_with_book) (seed) almanac) seeds |>
 List.fold_left (min) Int.max_int

(* 112s in REPL, 4s opt *)
let problem_05b () =
(*
 let rec map_value_with_book input book =
  match book with
  | [] -> input
  | (dst,src,len)::book' when input >= src && input < src+len -> input-src+dst
  | _::book' -> map_value_with_book input book' in
*)
 let rec map_value_with_book_inverted input book =
  match book with
  | [] -> input
  | (src,dst,len)::book' when input >= src && input < src+len -> input-src+dst
  | _::book' -> map_value_with_book_inverted input book' in
 let rec is_valid_seed s seed_ranges =
  match seed_ranges with
  | [] -> false
  | (src,len)::srs when s >= src && s < src+len -> true
  | _::srs -> is_valid_seed s srs in
 (* parse with fold_left *)
 let parse acc line =
  match acc with
  | ([], _, _) ->
    let seeds =
     String.split_on_char ':' line |>
     List.tl |> List.hd |> String.split_on_char ' ' |>
     List.filter ((<>)"") |> List.map int_of_string in
     (seeds, [], [])
  | _ when line = "" -> acc
  | (seeds, [], almanac) when String.contains line ':' -> (seeds, [], almanac)
  | (seeds, book, almanac) when String.contains line ':' -> (seeds, [], book::almanac)
  | (seeds, book, almanac) ->
    let entry =
     (match String.split_on_char ' ' line |> List.filter((<>)"") |> List.map int_of_string with
      [a;b;c] -> (a,b,c) | _ -> raise @@ Invalid_argument "invalid input: expected 3 numbers per map entry") in
    (seeds, (entry::book), almanac) in
 let seeds_to_seed_ranges acc seed =
  match acc with
  | (Some seed_start, ranges) -> (None, (seed_start, seed)::ranges)
  | (None, ranges) -> (Some seed, ranges) in
 let lines = In_channel.(with_open_bin "05.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let (seeds, last_book, almanac) = Array.fold_left parse ([],[],[]) lines in
 let almanac_reversed = last_book::almanac in
 let (_,seed_ranges) = List.fold_left (seeds_to_seed_ranges) (None,[]) seeds in
 let rec find_valid_seed idx =
  if is_valid_seed (List.fold_left (map_value_with_book_inverted) idx almanac_reversed) seed_ranges then idx
  else find_valid_seed @@ succ idx in
 find_valid_seed 0

let problem_05a_opt () =
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
 let get_new_layer layer1 layer2 =
  let top_bounds = List.fold_left (fun acc (_,src,len) -> src::src+len::acc) [] layer1 |> List.sort_uniq (compare)
  and mid_bounds = List.fold_left (fun acc (_,src,len) -> src::src+len::acc) [] layer2 |> List.sort_uniq (compare) in
  let new_mid_bounds = List.map (Fun.flip map_value_with_book_inverted layer1) mid_bounds in
  let new_srcs = new_mid_bounds @ top_bounds |> List.sort_uniq (compare) |> List.to_seq in
  let new_dsts = Seq.map (fun pt -> List.fold_left (map_value_with_book) pt [layer1;layer2]) new_srcs in
  let new_lens = Seq.map2 (-) (Seq.drop 1 new_srcs) new_srcs in
  let new_layer = Seq.map2 (fun (a,b) c -> (a,b,c)) (Seq.zip new_dsts new_srcs) (new_lens) in
  List.of_seq new_layer in
 (* parse with fold_left *)
 let parse acc line =
  match acc with
  | ([], _, _) ->
    let seeds =
     String.split_on_char ':' line |>
     List.tl |> List.hd |> String.split_on_char ' ' |>
     List.filter ((<>)"") |> List.map int_of_string in
     (seeds, [], [])
  | _ when line = "" -> acc
  | (seeds, [], almanac) when String.contains line ':' -> (seeds, [], almanac)
  | (seeds, book, almanac) when String.contains line ':' -> (seeds, [], book::almanac)
  | (seeds, book, almanac) ->
    let entry =
     (match String.split_on_char ' ' line |> List.filter((<>)"") |> List.map int_of_string with
      [a;b;c] -> (a,b,c) | _ -> raise @@ Invalid_argument "invalid input: expected 3 numbers per map entry") in
    (seeds, (entry::book), almanac) in
 let lines = In_channel.(with_open_bin "05.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let (seeds, last_book, almanac) = Array.fold_left parse ([],[],[]) lines in
 let almanac = last_book::almanac |> List.rev in
 (* let almanac_opt = get_new_layer (List.hd almanac) (List.hd @@ List.tl almanac) in *)
 (* almanac_opt *)
 let almanac_opt = List.fold_left (get_new_layer) (List.hd almanac) (List.tl almanac) in
 List.map (fun seed -> List.fold_left (map_value_with_book) (seed) [almanac_opt]) seeds
 |> List.fold_left (min) Int.max_int

let problem_05b_opt () =
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
 let get_new_layer layer1 layer2 =
  let top_bounds = List.fold_left (fun acc (_,src,len) -> src::src+len::acc) [] layer1 |> List.sort_uniq (compare)
  and mid_bounds = List.fold_left (fun acc (_,src,len) -> src::src+len::acc) [] layer2 |> List.sort_uniq (compare) in
  let new_mid_bounds = List.map (Fun.flip map_value_with_book_inverted layer1) mid_bounds in
  let new_srcs = new_mid_bounds @ top_bounds |> List.sort_uniq (compare) |> List.to_seq in
  let new_dsts = Seq.map (fun pt -> List.fold_left (map_value_with_book) pt [layer1;layer2]) new_srcs in
  let new_lens = Seq.map2 (-) (Seq.drop 1 new_srcs) new_srcs in
  let new_layer = Seq.map2 (fun (a,b) c -> (a,b,c)) (Seq.zip new_dsts new_srcs) (new_lens) in
  List.of_seq new_layer in
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
 let parse acc line =
  match acc with
  | ([], _, _) ->
    let seeds =
     String.split_on_char ':' line |>
     List.tl |> List.hd |> String.split_on_char ' ' |>
     List.filter ((<>)"") |> List.map int_of_string in
     (seeds, [], [])
  | _ when line = "" -> acc
  | (seeds, [], almanac) when String.contains line ':' -> (seeds, [], almanac)
  | (seeds, book, almanac) when String.contains line ':' -> (seeds, [], book::almanac)
  | (seeds, book, almanac) ->
    let entry =
     (match String.split_on_char ' ' line |> List.filter((<>)"") |> List.map int_of_string with
      [a;b;c] -> (a,b,c) | _ -> raise @@ Invalid_argument "invalid input: expected 3 numbers per map entry") in
    (seeds, (entry::book), almanac) in
 let lines = In_channel.(with_open_bin "05.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let (seeds, last_book, almanac) = Array.fold_left parse ([],[],[]) lines in
 let almanac = last_book::almanac |> List.rev in
 let (_,seed_ranges) = List.fold_left (seeds_to_seed_ranges) (None,[]) seeds in
 (* let almanac_opt = get_new_layer (List.hd almanac) (List.hd @@ List.tl almanac) in *)
 (* almanac_opt *)
 let almanac_opt = List.fold_left (get_new_layer) (List.hd almanac) (List.tl almanac) in
 (* almanac_opt *)
 (* minimum can only exist on lower bounds of inflection points! *)
 let candidates_type1 = List.map (fun (start,len) -> map_value_with_book start almanac_opt) seed_ranges in
 let candidates_type2 = List.filter_map (fun (dst,src,len) -> if is_valid_seed src seed_ranges then Some dst else None) almanac_opt in
 let min_type1 = List.fold_left (min) Int.max_int candidates_type1 in 
 let min_type2 = List.fold_left (min) Int.max_int candidates_type2 in 
 min min_type1 min_type2

let problem_06a () =
 let parse s =
  String.split_on_char ':' s |> List.tl |> List.hd |> String.split_on_char ' ' |> List.filter ((<>)"") |> List.map (int_of_string) in
 let test time distance press =
  press * press - press * time + distance < 0 in
 let wins time distance =
  let t = Float.of_int time
  and d = Float.of_int distance in
  let det = t *. t *. 0.25 -. d |> Float.sqrt in
  (* let's be extra careful about rounding errors *)
  let low = t *. 0.5 -. det |> Float.floor |> Float.to_int
  and high = t *. 0.5 +. det |> Float.ceil |> Float.to_int in
  let low = if test time distance low then low else low+1 in
  let high = if test time distance high then high else high-1 in
  (high-low+1) in
 let (times,distances) =
  (match In_channel.(with_open_bin "06.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map parse |> List.of_seq)) with
  | times::distances::_ -> (times,distances) | _ -> assert false) in
 List.map2 (wins) times distances |> List.fold_left ( * ) 1

let problem_06b () =
 let parse s =
  String.split_on_char ':' s |> List.tl |> List.hd |> String.split_on_char ' ' |> List.filter ((<>)"") in
 let test time distance press =
  press * press - press * time + distance < 0 in
 let wins time distance =
  let t = Float.of_int time
  and d = Float.of_int distance in
  let det = t *. t *. 0.25 -. d |> Float.sqrt in
  (* let's be extra careful about rounding errors *)
  let low = t *. 0.5 -. det |> Float.floor |> Float.to_int
  and high = t *. 0.5 +. det |> Float.ceil |> Float.to_int in
  let low = if test time distance low then low else low+1 in
  let high = if test time distance high then high else high-1 in
  (high-low+1) in
 let (times,distances) =
  (match In_channel.(with_open_bin "06.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map parse |> List.of_seq)) with
  | times::distances::_ -> (times,distances) | _ -> assert false) in
 let time = List.fold_left (^) "" times |> int_of_string
 and distance = List.fold_left (^) "" distances |> int_of_string in
 wins time distance

let problem_07a () =
 let card_to_number card =
  match card with
  | ('2'..'9' as card) -> Char.code card - 0x32
  | 'T' -> 8 | 'J' -> 9 | 'Q' -> 10 | 'K' -> 11 | 'A' -> 12
  | _ -> assert false in
 let hand_rank hand =
  let set = Array.make 13 0 in
  String.iter (fun c -> let n = card_to_number c in set.(n) <- set.(n) + 1) hand;
  Array.sort (Fun.flip compare) set;
  if set.(0) = 5 then 6 else
  if set.(0) = 4 then 5 else
  if set.(0) = 3 && set.(1) = 2 then 4 else
  if set.(0) = 3 then 3 else
  if set.(0) = 2 && set.(1) = 2 then 2 else
  if set.(0) = 2 then 1 else 0 in
 let compare_hand hand1 hand2 =
  match compare (hand_rank hand1) (hand_rank hand2) with
  | 0 -> Seq.compare (fun x y -> compare (card_to_number x) (card_to_number y)) (String.to_seq hand1) (String.to_seq hand2)
  | res -> res in
 let parse_line line =
  match String.split_on_char ' ' line |> List.filter ((<>)"") with
  | [hand; bid] -> (hand, int_of_string bid)
  | _ -> assert false in
 let input = In_channel.(with_open_bin "07.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.filter ((<>)"") |> Seq.map parse_line |> Array.of_seq)) in
 Array.sort (fun (h1,_) (h2,_) -> compare_hand h1 h2) input;
 let (res,_) = Array.fold_left (fun (acc,i) (_,x) -> (acc + i * x, succ i)) (0,1) input in
 res

let problem_07b () =
 (* function differs from part a!! *)
 let card_to_number card =
  match card with
  | ('2'..'9' as card) -> Char.code card - 0x31
  | 'T' -> 9 | 'J' -> 0 | 'Q' -> 10 | 'K' -> 11 | 'A' -> 12
  | _ -> assert false in
 let hand_rank hand =
  let set = Array.make 13 0 in
  String.iter (fun c -> let n = card_to_number c in set.(n) <- set.(n) + 1) hand;
  let jokers = set.(0) in
  set.(0) <- 0; (* reset joker count *)
  Array.sort (Fun.flip compare) set;
  if set.(0) + jokers = 5 then 6 else
  if set.(0) + jokers = 4 then 5 else
  if set.(0) + jokers = 3 && set.(1) = 2 then 4 else
  if set.(0) + jokers = 3 then 3 else
  if set.(0) + jokers = 2 && set.(1) = 2 then 2 else
  if set.(0) + jokers = 2 then 1 else 0 in
 let compare_hand hand1 hand2 =
  match compare (hand_rank hand1) (hand_rank hand2) with
  | 0 -> Seq.compare (fun x y -> compare (card_to_number x) (card_to_number y))(String.to_seq hand1) (String.to_seq hand2)
  | res -> res in
 let parse_line line =
  match String.split_on_char ' ' line |> List.filter ((<>)"") with
  | [hand; bid] -> (hand, int_of_string bid)
  | _ -> assert false in
 let input = In_channel.(with_open_bin "07.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.filter ((<>)"") |> Seq.map parse_line |> Array.of_seq)) in
 Array.sort (fun (h1,_) (h2,_) -> compare_hand h1 h2) input;
 let (res,_) = Array.fold_left (fun (acc,i) (_,x) -> (acc + i * x, succ i)) (0,1) input in
 res

(* pathway finding: AAA -> ZZZ *)
let problem_08a () =
 let parse_mapline s =
  (* AAA = (BBB, CCC) *)
  let node  = String.sub s  0 3
  and left  = String.sub s  7 3
  and right = String.sub s 12 3 in (node,left,right) in
 let (dir, map) = In_channel.(with_open_bin "08.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |>
   (fun seq -> match Seq.uncons seq with None -> assert false | Some (hd,tl) -> (hd, Seq.drop 1 tl |> Seq.map parse_mapline |> Array.of_seq)))) in
 let rec step acc i j =
  let (here, left, right) = map.(j) in
   if String.equal here "ZZZ" then acc else
   if dir.[i] = 'L' then step (succ acc) (succ i mod String.length dir) (Array.find_index (fun (h,_,_) -> String.equal h left) map |> Option.get)
   else step (succ acc) (succ i mod String.length dir) (Array.find_index (fun (h,_,_) -> String.equal h right) map |> Option.get) in
 step 0 0 (Array.find_index (fun (h,_,_) -> String.equal h "AAA") map |> Option.get)

(* pathway finding: AAA -> ZZZ *)
let problem_08a_opt () =
 let c_to_n = function
 | ('A'..'Z' as c) -> Char.code c - 0x41
 | ('0'..'9' as c) -> Char.code c - 0x30 + 26
 | _ -> assert false in
 let addr_of_s s = (c_to_n s.[0] * 36 * 36 + c_to_n s.[1] * 36 + c_to_n s.[2]) in
 let parse_mapline s =
  (* AAA = (BBB, CCC) *)
  let node  = String.sub s  0 3
  and left  = String.sub s  7 3
  and right = String.sub s 12 3 in (node,left,right) in
 let (dir, map) = In_channel.(with_open_bin "08.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |>
   (fun seq -> match Seq.uncons seq with None -> assert false | Some (hd,tl) -> (hd, Seq.drop 1 tl |> Seq.map parse_mapline |> Array.of_seq)))) in
 (* force an out_of_bounds error if you fall into a trap! *)
 let map_opt = Array.make (36*36*36) (36*36*36,36*36*36) in
 Array.iter (fun (h,l,r) -> let (h',l',r') = (addr_of_s h, addr_of_s l, addr_of_s r) in if h' <> l' || h' <> r' then map_opt.(h') <- (l',r')) map;
 let breakpoint = addr_of_s "ZZZ" in
 let rec step acc i ptr =
  if ptr = breakpoint then acc else
  step (succ acc) (succ i mod String.length dir) ((if dir.[i] = 'L' then fst else snd ) map_opt.(ptr)) in
 step 0 0 (addr_of_s "AAA")

(* optimize based on input and debug analysis! *)
let problem_08b () =
 let c_to_n = function
 | ('A'..'Z' as c) -> Char.code c - 0x41
 | ('0'..'9' as c) -> Char.code c - 0x30 + 26
 | _ -> assert false in
 let addr_of_s s = (c_to_n s.[0] * 36 * 36 + c_to_n s.[1] * 36 + c_to_n s.[2]) in
 let parse_mapline s =
  (* AAA = (BBB, CCC) *)
  let node  = String.sub s  0 3
  and left  = String.sub s  7 3
  and right = String.sub s 12 3 in (node,left,right) in
 let (dir, map) = In_channel.(with_open_bin "08.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |>
   (fun seq -> match Seq.uncons seq with None -> assert false | Some (hd,tl) -> (hd, Seq.drop 1 tl |> Seq.map parse_mapline |> Array.of_seq)))) in
 Array.sort (fun (x,_,_) (y,_,_) -> compare x.[2] y.[2]) map;
 (* find length of As *)
 let start_len = (let rec loop acc = let (h,_,_) = map.(acc) in if h.[2] <> 'A' then acc else loop (succ acc) in loop 0) in
 (* create ptr array *)
 let ptr = Array.init start_len (fun i -> let (h,_,_) = map.(i) in addr_of_s h) in
 (* in this problem, all As are some variation (flipped or equal) of their Z destination, so offset is always 0 : accounting for offset is tedious, but possible *)
 (* let ptr_cycle_offset = Array.make start_len Some 0 in *)
 (* in this problem, all cycles are prime numbers *)
 let ptr_cycles = Array.make start_len None in
 (* optimize map *)
 let map_opt = Array.make (36*36*36) (36*36*36,36*36*36) in
 (*Array.iter (fun (h,l,r) -> map_opt.(addr_of_s h) <- (addr_of_s l, addr_of_s r)) map;*)
 (* force an out_of_bounds error if you fall into a trap! *)
 Array.iter (fun (h,l,r) -> let (h',l',r') = (addr_of_s h, addr_of_s l, addr_of_s r) in if h' = l' && h' = r' then () else map_opt.(h') <- (l',r')) map;
 (* let tst_ptr () = Array.for_all (fun idx -> idx mod 36 = c_to_n 'Z') ptr in *)
 let tst_ptr acc idx =
  let all = ref true in
  for i = 0 to Array.length ptr |> pred do
   let t = ptr.(i) mod 36 = c_to_n 'Z' in
   if t then (
    Printf.printf "zf[%d] @ %d = %d\n" i idx (acc / (String.length dir));
    (* offset for mod may be adjusted if necessary if cycles do not fall on (@ 0)! look at debug outputs! *)
    if (acc mod String.length dir = 0) && (Option.is_none ptr_cycles.(i)) then ptr_cycles.(i) <- Some (acc / (String.length dir))
   );
    all := !all && t
  done; !all in
 let rec step acc i =
  if tst_ptr acc i then acc else
  if Array.for_all (Option.is_some) ptr_cycles then ( Array.fold_left ( fun a x -> a * (Option.get x)) 1 ptr_cycles * (String.length dir)) else
  (Array.map_inplace (fun idx -> (if dir.[i] = 'L' then fst else snd) map_opt.(idx)) ptr; step (succ acc) (succ i mod String.length dir)) in
 step 0 0

(* value interpolation : predict the next value with difference equations *)
let problem_09a () =
 let next_value data =
  let len = List.length data in
  let mat = Array.make_matrix (len+1) (len+1) 0 in
  List.iteri (fun i x -> mat.(0).(i) <- x) data;
  for i = 1 to len - 1 do for j = 0 to len - 1 - i do
   mat.(i).(j) <- mat.(i-1).(j+1) - mat.(i-1).(j)
  done done;
  let rec loop acc =
   if Array.for_all ((=)0) mat.(acc) then acc - 1 else
   loop (succ acc) in
  let lowest_layer = loop 1 in
  let rec loop layer =
   mat.(layer).(len-layer) <- mat.(layer+1).(len-layer-1) + mat.(layer).(len-layer-1);
   if layer = 0 then () else loop (pred layer)
  in loop lowest_layer;
  mat.(0).(len) in
 let input =
  In_channel.(with_open_bin "09.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.map (fun s -> s |> String.split_on_char ' '  |> List.map int_of_string) |> Array.of_seq)) in
 Array.fold_left (fun acc data -> next_value data + acc) 0 input

(* the only difference for b is to reverse each line of input *)
let problem_09b () =
 let next_value data =
  let len = List.length data in
  let mat = Array.make_matrix (len+1) (len+1) 0 in
  List.iteri (fun i x -> mat.(0).(i) <- x) data;
  for i = 1 to len - 1 do for j = 0 to len - 1 - i do
   mat.(i).(j) <- mat.(i-1).(j+1) - mat.(i-1).(j)
  done done;
  let rec loop acc =
   if Array.for_all ((=)0) mat.(acc) then acc - 1 else
   loop (succ acc) in
  let lowest_layer = loop 1 in
  let rec loop layer =
   mat.(layer).(len-layer) <- mat.(layer+1).(len-layer-1) + mat.(layer).(len-layer-1);
   if layer = 0 then () else loop (pred layer)
  in loop lowest_layer;
  mat.(0).(len) in
 let input =
  In_channel.(with_open_bin "09.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.map (fun s -> s |> String.split_on_char ' '  |> List.map int_of_string |> List.rev) |> Array.of_seq)) in
 Array.fold_left (fun acc data -> next_value data + acc) 0 input

(* pipe map w/o T sections *)
let problem_10a () =
 let debug = false in
 let map =
  In_channel.(with_open_bin "10.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let (sy,sx) =
  (let rec loop y x = if map.(y).[x] = 'S' then (y,x) else if succ x >= String.length map.(y) then loop (succ y) 0 else loop y (succ x) in loop 0 0) in
 let get_opt (y,x) = if y >= 0 && y < Array.length map && x >= 0 && x < String.length map.(y) then Some map.(y).[x] else None in
 let next_of_S (y,x) =
  let valids = [|"|F7";"-LF";"-7J";"|LJ"|] in
  List.map (fun (y,x) -> (y, x, get_opt (y,x))) [y-1,x;y,x-1;y,x+1;y+1,x] |>
  List.filteri (fun i (y,x,v) -> Option.is_some v && String.exists (fun c -> Option.get v = c) valids.(i)) |>
  List.hd |> (fun (y',x',v) -> (y,x,y',x',v)) in
 let next_of_loc (y,x,y',x',v) =
  let dy = y'-y and dx = x'-x in
  match v with
  | Some '|' | Some '-' -> (y',x',y'+dy,x'+dx,get_opt (y'+dy,x'+dx))
  | Some '7' | Some 'L' -> (y',x',y'+dx,x'+dy,get_opt (y'+dx,x'+dy))
  | Some 'J' | Some 'F' -> (y',x',y'-dx,x'-dy,get_opt (y'-dx,x'-dy))
  | Some 'S' -> (y',x',y',x',get_opt (y',x'))
  | _ -> assert false in
 let print_ptr (y,x,y',x',v) =
  Printf.printf "(%d,%d) -> (%d,%d): %c\n" y x y' x' @@ Option.get v in
 let is_S (_,_,_,_,v) = Option.get v = 'S' in
 let ptr = ref @@ next_of_S (sy,sx) in
 let acc = ref 0 in
 while is_S !ptr |> not do
  incr acc;
  if debug then print_ptr !ptr;
  ptr := next_of_loc !ptr;
 done;
 if debug then Printf.printf "acc: %d\n" !acc;
 (!acc+1)/2

(* pipe map w/o T sections *)
let problem_10b () =
 let debug = false in
 let map =
  In_channel.(with_open_bin "10.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let (sy,sx) =
  (let rec loop y x = if map.(y).[x] = 'S' then (y,x) else if succ x >= String.length map.(y) then loop (succ y) 0 else loop y (succ x) in loop 0 0) in
 let get_opt (y,x) = if y >= 0 && y < Array.length map && x >= 0 && x < String.length map.(y) then Some map.(y).[x] else None in
 let next_of_S (y,x) =
  let valids = [|"|F7";"-LF";"-7J";"|LJ"|] in
  List.map (fun (y,x) -> (y, x, get_opt (y,x))) [y-1,x;y,x-1;y,x+1;y+1,x] |>
  List.filteri (fun i (y,x,v) -> Option.is_some v && String.exists (fun c -> Option.get v = c) valids.(i)) |>
  List.hd |> (fun (y',x',v) -> (y,x,y',x',v)) in
 let next_of_loc (y,x,y',x',v) =
  let dy = y'-y and dx = x'-x in
  match v with
  | Some '|' | Some '-' -> (y',x',y'+dy,x'+dx,get_opt (y'+dy,x'+dx))
  | Some '7' | Some 'L' -> (y',x',y'+dx,x'+dy,get_opt (y'+dx,x'+dy))
  | Some 'J' | Some 'F' -> (y',x',y'-dx,x'-dy,get_opt (y'-dx,x'-dy))
  | Some 'S' -> (y',x',y',x',get_opt (y',x'))
  | _ -> assert false in
(*
 let print_ptr (y,x,y',x',v) =
  Printf.printf "(%d,%d) -> (%d,%d): %c\n" y x y' x' @@ Option.get v in
*)
 let is_S (_,_,_,_,v) = Option.get v = 'S' in
 (* redo part 1 but mark pipes without counting steps *)
 let is_main_loop = Array.make_matrix (Array.length map) (String.length map.(0)) false in
 is_main_loop.(sy).(sx) <- true;
 let mark_ptr (_,_,y',x',_) = is_main_loop.(y').(x') <- true in
 let ptr = ref @@ next_of_S (sy,sx) in
 while is_S !ptr |> not do
  mark_ptr !ptr;
  ptr := next_of_loc !ptr;
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
  | _ -> assert false in
 (* generate sub-pixel map, ignoring sections of pipe not in main loop *)
 let sub_map = Array.make_matrix (Array.length map * 2 + 1) (String.length map.(0) * 2 + 1) '.' in
 let get_opt_sub (y,x) = if y >= 0 && y < Array.length sub_map && x >= 0 && x < Array.length sub_map.(y) then Some sub_map.(y).(x) else None in
 if debug then Printf.printf "sub_map: h = %d, w = %d\n" (Array.length sub_map) (Array.length sub_map.(0));
 for y = 1 to Array.length sub_map - 2 do for x = 1 to Array.length sub_map.(0) - 2 do
  if y mod 2 = 1 && x mod 2 == 1 && is_main_loop.(y/2).(x/2) then sub_map.(y).(x) <- map.(y/2).[x/2] else
  if y mod 2 = 1 then (
   match get_opt (y/2,(x-1)/2), get_opt (y/2,(x+1)/2), is_main_loop.(y/2).((x-1)/2) && is_main_loop.(y/2).((x+1)/2) with
   | Some w, Some e, true ->
    if (w = 'F' || w = 'L' || w = '-') && (e = '7' || e = 'J' || e = '-') then sub_map.(y).(x) <- '-'
   | _ -> ()
  ) else
  if x mod 2 = 1 then (
   match get_opt ((y-1)/2,x/2), get_opt ((y+1)/2,x/2), is_main_loop.((y-1)/2).(x/2) && is_main_loop.((y+1)/2).(x/2) with
   | Some n, Some s, true ->
    if (n = 'F' || n = '7' || n = '|') && (s = 'L' || s = 'J' || s = '|') then sub_map.(y).(x) <- '|'
   | _ -> ()
  )
 done done;
 (* fill in S + surrounding pipe on sub-pixel map *)
 sub_map.(2*sy+1).(2*sx+1) <- shape_of_S;
 (* starting point may differ depending on input: guess on S's acute angle *)
 (* iy,ix *can* be (0,0), but if we guess right, the recursion should be more efficient *)
 let (iy,ix) =
  if shape_of_S = '|' then ( sub_map.(2*sy+0).(2*sx+1) <- '|'; sub_map.(2*sy+2).(2*sx+1) <- '|' ; (2*sy+1,2*sx+0) ) else
  if shape_of_S = 'L' then ( sub_map.(2*sy+0).(2*sx+1) <- '|'; sub_map.(2*sy+1).(2*sx+2) <- '-' ; (2*sy+0,2*sx+2) ) else
  if shape_of_S = 'J' then ( sub_map.(2*sy+0).(2*sx+1) <- '|'; sub_map.(2*sy+1).(2*sx+0) <- '-' ; (2*sy+0,2*sx+0) ) else
  if shape_of_S = '-' then ( sub_map.(2*sy+1).(2*sx+0) <- '-'; sub_map.(2*sy+1).(2*sx+2) <- '-' ; (2*sy+0,2*sx+1) ) else
  if shape_of_S = '7' then ( sub_map.(2*sy+1).(2*sx+0) <- '-'; sub_map.(2*sy+2).(2*sx+1) <- '|' ; (2*sy+2,2*sx+0) ) else
  if shape_of_S = 'F' then ( sub_map.(2*sy+1).(2*sx+2) <- '-'; sub_map.(2*sy+2).(2*sx+1) <- '|' ; (2*sy+2,2*sx+2) ) else assert false in
 if debug then for i = 0 to Array.length sub_map - 1 do for j = 0 to Array.length sub_map.(0) - 1 do print_char sub_map.(i).(j) done; print_newline () done;
 if debug then print_endline "##########" ;
 (* recursive fill *)
 let rec mark_I_sub (y,x) =
  match get_opt_sub (y,x) with
  | Some '.' ->
   sub_map.(y).(x) <- 'I';
   mark_I_sub (y-1,x);
   mark_I_sub (y,x+1);
   mark_I_sub (y+1,x);
   mark_I_sub (y,x-1)
  | _ -> () in
 mark_I_sub (iy,ix);
 if debug then for i = 0 to Array.length sub_map - 1 do for j = 0 to Array.length sub_map.(0) - 1 do print_char sub_map.(i).(j) done; print_newline () done;
 let acc = ref 0 in
 (* if our choice of iy,ix was wrong, check for empty spaces instead! *)
 let inner_marker = if sub_map.(0).(0) = 'I' then '.' else 'I' in
 for i = 0 to Array.length map - 1 do for j = 0 to String.length map.(0) - 1 do
  if sub_map.(2*i+1).(2*j+1) = inner_marker then incr acc
 done done;
 !acc

(* calculate the sum of the length of the shortest path between galaxies *)
(* any rows or columns that contain no galaxies should all actually be twice as big *)
(* it is okay for a path to pass through another galaxy *)
(* '.' + 1 = '/' + 2 = '0' *)
let problem_11a () =
 let debug = false in
 let manhattan_distance (y,x) (y',x') = abs (y'-y) + (abs (x'-x)) in
 let label_expansion map =
  let h = Array.length map and w = Bytes.length map.(0) in
  Array.iter (fun bs -> if Bytes.for_all ((=)'.') bs then Bytes.fill bs 0 w '/') map ;
  for col = 0 to Bytes.length map.(0) |> pred do
   if ( let rec loop = function | false,_ -> false
      | true, i when i >= h -> true
      | a, i -> loop (a && (Bytes.get map.(i) col <> '#'),succ i) in loop (true, 0) )
   then ( for i = 0 to h - 1 do Bytes.set map.(i) col (Bytes.get map.(i) col |> Char.code |> succ |> Char.chr) done )
  done in
 let night_sky_bytes =
  In_channel.(with_open_bin "11.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map (Bytes.unsafe_of_string) |> Array.of_seq)) in
 label_expansion night_sky_bytes;
 let night_sky = Array.map (Bytes.unsafe_to_string) night_sky_bytes in
 if debug then Array.iter (print_endline) night_sky;
 (* count galaxies *)
 let galaxy_list = ref [] in
 let x = ref 0 and y = ref 0 in
 for i = 0 to Array.length night_sky |> pred do
  if night_sky.(i).[0] = '/' || night_sky.(i).[0] = '0' then incr y;
  x := 0;
  for j = 0 to String.length night_sky.(0) |> pred do
   (match night_sky.(i).[j] with
    | '/'..'0' -> incr x
    | '#' -> galaxy_list := (!y,!x)::!galaxy_list
    | _ -> ()
   ); incr x
  done;
  incr y;
 done;
 let galaxy_list = !galaxy_list in
 (* print galaxies *)
 if debug then List.iter (fun (y,x) -> Printf.printf "(%d,%d)\n" y x) galaxy_list;
 (* map each galaxy to a minimum distance *)
 let min_distances =
  List.map ( fun (y,x) ->
    List.map (manhattan_distance (y,x)) (List.filter ((<>)(y,x)) galaxy_list) |> List.fold_left (+) 0
  ) galaxy_list in
 List.fold_left (+) 0 min_distances |> Fun.flip Int.div 2

(* use specified (1_000_000) scaling factor instead of implied scaling_factor = 2 from part a *)
(* '.' + 1 = '/' + 2 = '0' *)
let problem_11b () =
 (* use custom bytes operators *)
 let (.%[]<-) = Bytes.set and (.%[]) = Bytes.get in
 (* byte+= *)
 let (.+[]<-) = Char.(fun b idx i -> b.%[idx] <- chr @@ (+) i @@ code b.%[idx]) in
 let debug = false in
 let scaling_factor = 1_000_000 in 
 let manhattan_distance (y,x) (y',x') = abs (y'-y) + (abs (x'-x)) in
 let label_expansion map =
  let h = Array.length map and w = Bytes.length map.(0) in
  Array.iter (fun bs -> if Bytes.for_all ((=)'.') bs then Bytes.fill bs 0 w '/') map ;
  for col = 0 to Bytes.length map.(0) |> pred do
   if ( let rec loop = function i when i >= h -> true
      | i when (map.(i).%[col] <> '#') -> loop (succ i)
      | _ -> false in loop 0 )
   then for i = 0 to h - 1 do map.(i).+[col] <- ~+1 done
  done in
 let night_sky_bytes =
  In_channel.(with_open_bin "11.txt"
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map (Bytes.unsafe_of_string) |> Array.of_seq)) in
 label_expansion night_sky_bytes;
 let night_sky = Array.map (Bytes.unsafe_to_string) night_sky_bytes in
 if debug then Array.iter (print_endline) night_sky;
 (* count galaxies *)
 let galaxy_list = ref [] in
 let x = ref 0 and y = ref 0 in
 for i = 0 to Array.length night_sky |> pred do
  if night_sky.(i).[0] = '/' || night_sky.(i).[0] = '0' then y := !y + (scaling_factor-1);
  x := 0;
  for j = 0 to String.length night_sky.(0) |> pred do
   (match night_sky.(i).[j] with
    | '/'..'0' -> x := !x+(scaling_factor-1)
    | '#' -> galaxy_list := (!y,!x)::!galaxy_list
    | _ -> ()
   ); incr x
  done;
  incr y;
 done;
 let galaxy_list = !galaxy_list in
 (* print galaxies *)
 if debug then List.iter (fun (y,x) -> Printf.printf "(%d,%d)\n" y x) galaxy_list;
 (* map each galaxy to a minimum distance *)
 let distances =
  let rec loop = function [] -> 0
  | g1::g2s -> (+) (List.map (manhattan_distance g1) g2s |> List.fold_left (+) 0) @@ loop g2s
  in loop galaxy_list in
 distances

(* operational: '.', damanged: '#', unknown: '?' ; lists of continguous damageed springs *)
(* maximum number of '?'s is 19 *)
(* brute force solution *)
let problem_12a () =
 let convert_data fmt1 =
  let rec convert_data' (alist, a, last) fmt1 =
   match (last,Seq.uncons fmt1) with
   | '#', None -> a::alist |> List.rev
   | '.', None -> alist |> List.rev
   | '.', Some ('.',tl) -> convert_data' (alist, 0, '.') tl
   | '#', Some ('#',tl) -> convert_data' (alist, succ a, '#') tl
   | '.', Some ('#',tl) -> convert_data' (alist, 1, '#') tl
   | '#', Some ('.',tl) -> convert_data' (a::alist, 0, '.') tl
   | _ -> assert false in convert_data' ([],0,'.') (String.to_seq fmt1) in
 let check_combo n fmt1 fmt2 =
  let i = ref n in fmt2 = (String.map
   (function '?' when !i land 1 = 1 -> ( i := !i lsr 1; '#' ) | '?' -> ( i := !i lsr 1; '.' ) | c -> c) fmt1 |> convert_data) in
 let count_valid fmt1 fmt2 =
  let max_n = ref 1 and acc = ref 0 in
  String.iter (function '?' -> max_n := !max_n lsl 1 | _ -> ()) fmt1;
  for i = 0 to !max_n |> pred do
   if check_combo i fmt1 fmt2 then incr acc
  done; !acc in
 let parse s =
  match String.split_on_char ' ' s with
  | [fmt1;fmt2_raw] -> (fmt1, String.split_on_char ',' fmt2_raw |> List.map (int_of_string))
  | _ -> failwith "Invalid Line!" in
 let input = 
  In_channel.(with_open_bin "12.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map (parse) |> List.of_seq)) in
 input |> List.map (fun (fmt1,fmt2) -> count_valid fmt1 fmt2)
  |> List.fold_left (+) 0

(* use (bot-up) dynamic programming (in three dimensions) to solve 12b *)
let problem_12b () =
 let debug = false in
 let solve fmt1 fmt2 =
  let fmt1 = fmt1 ^ "." in 
  let fmt2 = fmt2 @ [0] in
  let fmt1_len = String.length fmt1
  and fmt2_len = List.length fmt2
  (* fmt3 is cursor length (accumulator prior to folding into fmt2) *)
  and fmt3_len = succ @@ List.fold_left (max) Int.min_int fmt2 in
  (* dynamic programming table *)
  let dp = Array.init (fmt1_len) (fun _ -> Array.make_matrix (fmt2_len) (fmt3_len) 0) in
  (* set base case *)
  if fmt1.[0] <> '.' then dp.(0).(0).(1) <- 1 ;
  if fmt1.[0] <> '#' then dp.(0).(0).(0) <- 1 ;
  for i = 1 to pred fmt1_len do for j = 0 to pred fmt2_len do for k = 0 to pred fmt3_len do
   (* adding a dot to a hash resets k (from fmt2[last-j]) and increments j; adding a dot to a dot retains j and keeps k reset *)
   let if_dot = if k = 0 then (if j <> 0 then dp.(i-1).(j-1).(List.nth fmt2 (j-1)) else 0) + dp.(i-1).(j).(k) else 0 in
   (* adding a hash increments k *)
   let if_hash = if k <> 0 then dp.(i-1).(j).(k-1) else 0 in
   (* adding a ? takes both cases into account *)
   if fmt1.[i] = '.' then dp.(i).(j).(k) <- if_dot else
   if fmt1.[i] = '#' then dp.(i).(j).(k) <- if_hash else
   if fmt1.[i] = '?' then dp.(i).(j).(k) <- if_hash + if_dot
  done done done;
  if debug then begin
   print_endline "+++++";
   for j = 0 to pred fmt2_len do for i = 0 to pred fmt1_len do for k = 0 to pred fmt3_len do
    print_int dp.(i).(j).(k) ; print_char ' '
   done; print_newline () done; print_endline "=====" done
  end;
  (* our answer resides at dp.(fmt1_len-1).(fmt2_len-1).(0), because we added a '.' to force a "finish" the sequence *)
  dp.(fmt1_len-1).(fmt2_len-1).(0) in
 let parse s =
  match String.split_on_char ' ' s with
  | [fmt1;fmt2_raw] -> (fmt1, String.split_on_char ',' fmt2_raw |> List.map (int_of_string))
  | _ -> failwith "Invalid Line!" in
 let input = 
  In_channel.(with_open_bin "12.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map (parse) |> List.of_seq)) in
 let input =
  List.map (fun (fmt1, fmt2) -> (fmt1 ^ "?" ^ fmt1 ^ "?" ^ fmt1 ^ "?" ^ fmt1 ^ "?" ^ fmt1, fmt2 @ fmt2 @ fmt2 @ fmt2 @ fmt2)) input in
  List.fold_left (fun a (fmt1,fmt2) -> a + solve fmt1 fmt2) 0 input

(* one reflection per pattern block *)
let problem_13a () =
 let find_h_ref_line map =
  let h = List.length map in
  (* pre-reversed *)
  let reverse_seq = List.to_seq map
  and forward_seq = List.rev map |> List.to_seq in
  let rec loop i =
   if i >= h  then None else
   if Seq.for_all2 (=) (Seq.drop i forward_seq) (Seq.drop (h-i) reverse_seq) then Some i else
   loop (i+1)
  in loop 1
 in
 let find_v_ref_line map =
  let forward_seq = List.map (String.to_seq) map |> List.to_seq |> Seq.transpose in
  let reverse_seq = List.of_seq forward_seq |> List.rev |> List.to_seq in
  let h = Seq.length forward_seq in
  let rec loop i =
   if i >= h  then None else
   if Seq.for_all2 (Seq.for_all2 (=)) (Seq.drop i forward_seq) (Seq.drop (h-i) reverse_seq) then Some i else
   loop (i+1)
  in loop 1
 in
 let rec parse (maps,last_map) = function
  | "" -> (last_map :: maps, [])
  | s -> (maps, s::last_map) in
 let (maps,last_map) =
  In_channel.(with_open_bin "13.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.fold_left parse ([],[]))) in
 let maps = last_map::maps in
 List.fold_left (fun a m ->
  a + Option.value ~default:0 (find_v_ref_line m)
  + Option.value ~default:0 (find_h_ref_line m) * 100) 0 maps

(* one reflection per pattern block *)
(* change equality test to allow exactly ONE discrepancy *)
let problem_13b () =
 let diff seq1 seq2 =
  Seq.fold_left2 (fun a c1 c2 -> a + (compare c1 c2 |> abs)) 0 seq1 seq2 in
 (* let diff_str s1 s2 = diff (String.to_seq s1) (String.to_seq s2) in *)
 let diff_for_all r_ctr seq1 seq2 =
  let d = diff seq1 seq2 in
  if d > 1 then false else (r_ctr := !r_ctr + d ; true) in
 let diff_for_all_str r_ctr s1 s2 =
  diff_for_all r_ctr (String.to_seq s1) (String.to_seq s2) in
 let find_h_ref_line map =
  let h = List.length map in
  (* pre-reversed *)
  let reverse_seq = List.to_seq map
  and forward_seq = List.rev map |> List.to_seq
  and ctr = ref 0 in
  let rec loop i =
   ctr := 0; 
   if i >= h  then None else
   if Seq.for_all2 (diff_for_all_str ctr) (Seq.drop i forward_seq) (Seq.drop (h-i) reverse_seq) && (!ctr = 1) then Some i else
   loop (i+1)
  in loop 1
 in
 let find_v_ref_line map =
  let forward_seq = List.map (String.to_seq) map |> List.to_seq |> Seq.transpose in
  let reverse_seq = List.of_seq forward_seq |> List.rev |> List.to_seq in
  let h = Seq.length forward_seq in
  let ctr = ref 0 in
  let rec loop i =
   ctr := 0;
   if i >= h  then None else
   if Seq.for_all2 (diff_for_all ctr) (Seq.drop i forward_seq) (Seq.drop (h-i) reverse_seq) && (!ctr = 1) then Some i else
   loop (i+1)
  in loop 1
 in
 let rec parse (maps,last_map) = function
  | "" -> (last_map :: maps, [])
  | s -> (maps, s::last_map) in
 let (maps,last_map) =
  In_channel.(with_open_bin "13.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.fold_left parse ([],[]))) in
 let maps = last_map::maps in
 (* List.hd maps |> find_h_ref_line *)
 List.fold_left (fun a m ->
  a + Option.value ~default:0 (find_v_ref_line m)
  + Option.value ~default:0 (find_h_ref_line m) * 100) 0 maps

(* rolling rocks *)
(* roll rocks north and then calculate weight *)
(*
let problem_14a () =
 let debug = false in
 let roll_left seq =
  Seq.group (fun a b -> a <> '#' && b <> '#') seq |>
  Seq.map (fun s -> s |> Seq.partition ((=)'O') |> (fun (a,b) -> Seq.append a b)) |>
  Seq.concat in
 let roll_north seq_mat =
  Seq.transpose seq_mat |> Seq.map (roll_left) |> Seq.transpose in
 let print_map seq_mat =
  Seq.iter (fun s -> Seq.iter (print_char) s ; print_newline ()) seq_mat in 
 let input =
  In_channel.(with_open_bin "14.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> List.of_seq |> List.map (String.to_seq))) in
 let h = List.length input in
 let res = input |> List.to_seq |> roll_north in
 if debug then print_map res;
 Seq.fold_lefti (fun a i s -> a + Seq.fold_left (fun a c -> if c = 'O' then a + h - i else a) 0 s) 0 res
*)

(* transposing with Seq is expensive, so let's use pointers instead *)
let problem_14a () =
 let open Bigarray in
 let debug = false in
 let map_of_input input = (*string list*)
  let h = List.length input and w = List.hd input |> String.length in
  let map = Array2.create Char C_layout h w in
  List.iteri (fun i s -> String.iteri (fun j c -> map.{i,j} <- c) s) input ;
  map in
 (* let seq_of_row map y = Seq.init (Array2.dim2 map) (fun x -> map.{y,x}) in *)
 let seq_of_col map x = Seq.init (Array2.dim1 map) (fun y -> map.{y,x}) in
 (* conversion to list (eager memoization) is necessary prior to blitting because otherwise the Seq length may change due to look ahead *)
 (* let blit_row map y seq = seq |> List.of_seq |> List.iteri (fun x c -> map.{y,x} <- c) in *)
 let blit_col map x seq = seq |> List.of_seq |> List.iteri (fun y c -> map.{y,x} <- c) in
 let roll_north map =
  for i = 0 to Array2.dim2 map - 1 do
   seq_of_col map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append rocks spaces) |>
   Seq.concat |>
   blit_col map i
  done in
 let weights map =
  let res = ref 0 in
  for y = 0 to Array2.dim1 map - 1 do for x = 0 to Array2.dim2 map - 1 do
   if map.{y,x} = 'O' then res := !res + (Array2.dim1 map - y);
  done done; !res in
 let print_map map =
  for y = 0 to  Array2.dim1 map - 1 do for x = 0 to Array2.dim2 map - 1 do
   print_char map.{y,x}
  done; print_newline () done in
 let input =
  In_channel.(with_open_bin "14.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> List.of_seq)) in
 let map = map_of_input input in
 roll_north map;
 if debug then print_map map;
 weights map

(* cycle length is 42, stablizes at 118; 1x10^3 coincides with 1x10^9 b/c 1x10^9-1x10^3 mod 42 is 0 *)
let problem_14b () =
 let open Bigarray in
 let debug = false in
 let map_of_input input = (*string list*)
  let h = List.length input and w = List.hd input |> String.length in
  let map = Array2.create Char C_layout h w in
  List.iteri (fun i s -> String.iteri (fun j c -> map.{i,j} <- c) s) input ;
  map in
 let seq_of_row map y = Seq.init (Array2.dim2 map) (fun x -> map.{y,x}) in
 let seq_of_col map x = Seq.init (Array2.dim1 map) (fun y -> map.{y,x}) in
 (* conversion to list (eager memoization) is necessary prior to blitting because otherwise the Seq length may change due to look ahead *)
 let blit_row map y seq = seq |> List.of_seq |> List.iteri (fun x c -> map.{y,x} <- c) in
 let blit_col map x seq = seq |> List.of_seq |> List.iteri (fun y c -> map.{y,x} <- c) in
 let roll_north map =
  for i = 0 to Array2.dim2 map - 1 do
   seq_of_col map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append rocks spaces) |>
   Seq.concat |>
   blit_col map i
  done in
 let roll_south map =
  for i = 0 to Array2.dim2 map - 1 do
   seq_of_col map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append spaces rocks) |>
   Seq.concat |>
   blit_col map i
  done in
 let roll_west map =
  for i = 0 to Array2.dim1 map - 1 do
   seq_of_row map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append rocks spaces) |>
   Seq.concat |>
   blit_row map i
  done in
 let roll_east map =
  for i = 0 to Array2.dim1 map - 1 do
   seq_of_row map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append spaces rocks) |>
   Seq.concat |>
   blit_row map i
  done in
 let cycle map = roll_north map; roll_west map; roll_south map; roll_east map in
 let weights map =
  let res = ref 0 in
  for y = 0 to Array2.dim1 map - 1 do for x = 0 to Array2.dim2 map - 1 do
   if map.{y,x} = 'O' then res := !res + (Array2.dim1 map - y);
  done done; !res in
 let print_map map =
  for y = 0 to  Array2.dim1 map - 1 do for x = 0 to Array2.dim2 map - 1 do
   print_char map.{y,x}
  done; print_newline () done in
 let input =
  In_channel.(with_open_bin "14.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> List.of_seq)) in
 let map = map_of_input input in
 (*roll_north map;*)
 if debug then print_map map;
 for i = 1 to 1000 do cycle map; if debug then Printf.printf "%04d: %d\n" i (weights map) done;
 weights map

(* cycle length is 42, stablizes at 118; 1x10^3 coincides with 1x10^9 b/c 1x10^9-1x10^3 mod 42 is 0 *)
let problem_14b_smart () =
 let open Bigarray in
 let debug = false in
 let map_of_input input = (*string list*)
  let h = List.length input and w = List.hd input |> String.length in
  let map = Array2.create Char C_layout h w in
  List.iteri (fun i s -> String.iteri (fun j c -> map.{i,j} <- c) s) input ;
  map in
 let seq_of_row map y = Seq.init (Array2.dim2 map) (fun x -> map.{y,x}) in
 let seq_of_col map x = Seq.init (Array2.dim1 map) (fun y -> map.{y,x}) in
 (* conversion to list (eager memoization) is necessary prior to blitting because otherwise the Seq length may change due to look ahead *)
 let blit_row map y seq = seq |> List.of_seq |> List.iteri (fun x c -> map.{y,x} <- c) in
 let blit_col map x seq = seq |> List.of_seq |> List.iteri (fun y c -> map.{y,x} <- c) in
 let roll_north map =
  for i = 0 to Array2.dim2 map - 1 do
   seq_of_col map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append rocks spaces) |>
   Seq.concat |>
   blit_col map i
  done in
 let roll_south map =
  for i = 0 to Array2.dim2 map - 1 do
   seq_of_col map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append spaces rocks) |>
   Seq.concat |>
   blit_col map i
  done in
 let roll_west map =
  for i = 0 to Array2.dim1 map - 1 do
   seq_of_row map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append rocks spaces) |>
   Seq.concat |>
   blit_row map i
  done in
 let roll_east map =
  for i = 0 to Array2.dim1 map - 1 do
   seq_of_row map i |>
   Seq.group (fun a b -> a <> '#' && b <> '#') |>
   Seq.map (fun grp -> let (rocks, spaces) = Seq.partition ((=)'O') grp in Seq.append spaces rocks) |>
   Seq.concat |>
   blit_row map i
  done in
 let cycle map = roll_north map; roll_west map; roll_south map; roll_east map in
 let weights map =
  let res = ref 0 in
  for y = 0 to Array2.dim1 map - 1 do for x = 0 to Array2.dim2 map - 1 do
   if map.{y,x} = 'O' then res := !res + (Array2.dim1 map - y);
  done done; !res in
 let print_map map =
  for y = 0 to  Array2.dim1 map - 1 do for x = 0 to Array2.dim2 map - 1 do
   print_char map.{y,x}
  done; print_newline () done in
 let input =
  In_channel.(with_open_bin "14.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> List.of_seq)) in
 let map = map_of_input input in
 (*roll_north map;*)
 if debug then print_map map;
 let weight_stack = Stack.create () in
 let cycle_length = ref None in
 let stability_rating = ref 0 in
 let calculate_cycle () =
  let top = Stack.top weight_stack in
  let guess = Stack.to_seq weight_stack |> Seq.drop (Option.value ~default:1 !cycle_length) |> Seq.uncons |> Option.map (fst) in
  if Option.value ~default:(-1) guess = top then incr stability_rating else (
  match (!cycle_length, Stack.to_seq weight_stack |> Seq.drop 1 |> Seq.find_index ((=)top) |> Option.map ((+)1)) with
  | (Some y, Some x) when x = y -> incr stability_rating
  | (_, x) -> (cycle_length := x ; stability_rating := 0))
 in
 (
  let i = ref 0 in
  while !i < 1000 && !stability_rating < (max 20 (Option.value ~default:(500) !cycle_length)) do
   cycle map;
   Stack.push (weights map) weight_stack;
   calculate_cycle ();
   if debug then (print_int (!cycle_length |> Option.value ~default:(-1)); print_newline ());
   incr i
  done;
  let cycle_length = Option.get !cycle_length in
  let offset = ((1_000_000_000 - !i) mod cycle_length) in
  let res = Stack.to_seq weight_stack |> Seq.drop (cycle_length - offset) |> Seq.uncons |> Option.get |> fst in
  if debug then Printf.printf "cycle: %d\ni: %d\n10^9 - i mod %d = %d -> %d\n" (cycle_length) !i (cycle_length) (offset) (res);
  res
 )

let problem_15a () =
 let rec hash res a seq =
  match Seq.uncons seq with
  | None -> res+a
  | Some (hd,tl) when hd = '\n' -> hash res a tl
  | Some (hd,tl) when hd = ',' -> hash (res+a) 0 tl
  | Some (hd,tl) ->
    hash res ((Char.code hd + a) * 17 mod 256) tl in
 let ic = In_channel.open_bin "15.txt" in
 let res = Seq.of_dispenser (fun () -> In_channel.input_char ic) |> hash 0 0 in
 close_in ic ; res

let problem_15b () =
 let (.+[]) = Buffer.nth in
 let instr = Buffer.create 16
 and boxes = Array.init 256 (fun _ -> Queue.create ())
 and proc_q = Queue.create () in
 (* hash is not of full running total but of current label only! *)
 let digest () =
  let rec hash seed seq = Seq.uncons seq |> function
   | Some ('a'..'z' as hd,tl) -> hash ((Char.code hd + seed) * 17 mod 256) tl | _ -> seed
  in Buffer.to_seq instr |> hash 0 in
 let rec process seq = Seq.uncons seq |> function
  | None -> ()
  | Some ('\n', tl) -> process @@ Seq.return ',' (* EOL -> final compute *)
  | Some (',', tl) when instr.+[Buffer.length instr - 1] = '-' ->
    let box = digest () in
    let label = Buffer.sub instr 0 (Buffer.length instr - 1) in
    let found = boxes.(box) |> Queue.to_seq |> Seq.exists (fun (lbl,_) -> lbl = label) in
    if found then Queue.(
     (* Queue.filter_"inplace" *)
     transfer boxes.(box) proc_q;
     to_seq proc_q |> Seq.filter (fun (lbl,_) -> lbl<>label) |> add_seq boxes.(box);
     clear proc_q
    ); Buffer.clear instr ; process tl
  | Some (',', tl) (* when instr.+[len - 2] = '=' *) -> 
    let box = digest () in
    let label = Buffer.sub instr 0 (Buffer.length instr - 2) in
    let focal = (instr.+[Buffer.length instr - 1] |> Char.code) - 0x30 in
    let found = boxes.(box) |> Queue.to_seq |> Seq.exists (fun (lbl,_) -> lbl = label) in
    if not found then Queue.add (label,focal) boxes.(box) else Queue.(
     (* Queue.map_"inplace" *)
     transfer boxes.(box) proc_q;
     to_seq proc_q |> Seq.map (fun (lbl,f) -> if lbl = label then (label,focal) else (lbl,f)) |> add_seq boxes.(box);
     clear proc_q
    ); Buffer.clear instr ; process tl
  | Some (hd, tl) -> Buffer.add_char instr hd ; process tl in
 (let ic = In_channel.open_bin "15.txt" in Seq.of_dispenser (fun () -> In_channel.input_char ic) |> process ; close_in ic) ;
 Array.to_seq boxes |> Seq.fold_lefti (fun a i box -> a + (Queue.to_seq box |> Seq.fold_lefti (fun a j (lbl,f) -> a + (i+1)*(j+1)*f) 0)) 0

(* due to the potential for loops, memoize the shit out of this! *)
let problem_16a () =
 let debug = false in
 let module Beam = struct
  type direction = North | South | East | West
  type t = {y : int; x : int; dir : direction}
  let compare = compare
  let print b =
   Printf.printf "(y=%02d,x=%02d) dir=" b.y b.x ;
   print_char (match b.dir with North -> 'N' | South -> 'S' | East -> 'E' | West -> 'W') ;
   print_newline ()
 end in
 let module BeamSet = Set.Make(Beam) in
 let memo = ref BeamSet.empty in
 let map = In_channel.(with_open_bin "16.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let visited = Array.make_matrix (Array.length map) (String.length map.(0)) false in
 let single_step (beam : Beam.t) =
  let oob (b : Beam.t) = b.y < 0 || b.y >= Array.length visited || b.x < 0 || b.x >= Array.length visited.(0) in
  let incr_beam (b : Beam.t) =
   match b.dir with
   | North -> let b' = {b with y = b.y - 1} in if oob b' then None else Some b'
   | South -> let b' = {b with y = b.y + 1} in if oob b' then None else Some b'
   | East  -> let b' = {b with x = b.x + 1} in if oob b' then None else Some b'
   | West  -> let b' = {b with x = b.x - 1} in if oob b' then None else Some b' in
  if oob beam then (None,None) else
  match map.(beam.y).[beam.x] with
  | '.' -> (incr_beam beam, None)
  | '|'  when beam.dir = North || beam.dir = South -> (incr_beam beam, None)
  | '-'  when beam.dir = East  || beam.dir = West  -> (incr_beam beam, None)
  | '/'  when beam.dir = North -> (incr_beam {beam with dir = East}, None)
  | '\\' when beam.dir = South -> (incr_beam {beam with dir = East}, None)
  | '/'  when beam.dir = South -> (incr_beam {beam with dir = West}, None)
  | '\\' when beam.dir = North -> (incr_beam {beam with dir = West}, None)
  | '/'  when beam.dir = East  -> (incr_beam {beam with dir = North}, None)
  | '\\' when beam.dir = West  -> (incr_beam {beam with dir = North}, None)
  | '/'  when beam.dir = West  -> (incr_beam {beam with dir = South}, None)
  | '\\' when beam.dir = East  -> (incr_beam {beam with dir = South}, None)
  | '|'  when beam.dir = East || beam.dir = West ->
    (incr_beam {beam with dir = North}, incr_beam {beam with dir = South})
  | '-'  when beam.dir = North || beam.dir = South ->
    (incr_beam {beam with dir = East}, incr_beam {beam with dir = West})
  | _ -> failwith (Printf.sprintf "Invalid Map Tile @ (y=%02d,x=%02d)" beam.y beam.x)
 in
 let rec next_step = function
 | Some beam when not @@ BeamSet.mem beam !memo ->
   memo := BeamSet.add beam !memo;
   visited.(beam.y).(beam.x) <- true;
   let (b1,b2) = single_step beam in
   next_step b2;
   next_step b1
 | _ -> () in
 let print_visited () =
  for y = 0 to Array.length visited - 1 do for x = 0 to Array.length visited.(0) - 1 do
   print_char (if visited.(y).(x) then '#' else '.')
  done; print_newline () ; done in
 let count_visited () =
  let acc = ref 0 in
  for y = 0 to Array.length visited - 1 do for x = 0 to Array.length visited.(0) - 1 do
   if visited.(y).(x) then incr acc
  done done; !acc in
 next_step (Some {y = 0; x = 0; dir = East});
 if debug then print_visited ();
 count_visited ()
(* single_step {y = 0; x = 0; dir = East} |> (fun (d1, d2) -> Option.map print_beam d1 |> ignore ; Option.map print_beam d2 |> ignore) *)

(* due to the potential for loops, memoize the shit out of this! *)
let problem_16b () =
 let module Beam = struct
  type direction = North | South | East | West
  type t = {y : int; x : int; dir : direction}
  let compare = compare
  let print b =
   Printf.printf "(y=%02d,x=%02d) dir=" b.y b.x ;
   print_char (match b.dir with North -> 'N' | South -> 'S' | East -> 'E' | West -> 'W') ;
   print_newline ()
 end in
 let module BeamSet = Set.Make(Beam) in
 let memo = ref BeamSet.empty in
 let map = In_channel.(with_open_bin "16.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 (* future optitmization: instead of using BeamSet, use a 4-bit mask instead of a bool for visited! *)
 let visited = Array.make_matrix (Array.length map) (String.length map.(0)) false in
 let single_step (beam : Beam.t) =
  let oob (b : Beam.t) = b.y < 0 || b.y >= Array.length visited || b.x < 0 || b.x >= Array.length visited.(0) in
  let incr_beam (b : Beam.t) =
   match b.dir with
   | North -> let b' = {b with y = b.y - 1} in if oob b' then None else Some b'
   | South -> let b' = {b with y = b.y + 1} in if oob b' then None else Some b'
   | East  -> let b' = {b with x = b.x + 1} in if oob b' then None else Some b'
   | West  -> let b' = {b with x = b.x - 1} in if oob b' then None else Some b' in
  if oob beam then (None,None) else
  match map.(beam.y).[beam.x] with
  | '.' -> (incr_beam beam, None)
  | '|'  when beam.dir = North || beam.dir = South -> (incr_beam beam, None)
  | '-'  when beam.dir = East  || beam.dir = West  -> (incr_beam beam, None)
  | '/'  when beam.dir = North -> (incr_beam {beam with dir = East}, None)
  | '\\' when beam.dir = South -> (incr_beam {beam with dir = East}, None)
  | '/'  when beam.dir = South -> (incr_beam {beam with dir = West}, None)
  | '\\' when beam.dir = North -> (incr_beam {beam with dir = West}, None)
  | '/'  when beam.dir = East  -> (incr_beam {beam with dir = North}, None)
  | '\\' when beam.dir = West  -> (incr_beam {beam with dir = North}, None)
  | '/'  when beam.dir = West  -> (incr_beam {beam with dir = South}, None)
  | '\\' when beam.dir = East  -> (incr_beam {beam with dir = South}, None)
  | '|'  when beam.dir = East || beam.dir = West ->
    (incr_beam {beam with dir = North}, incr_beam {beam with dir = South})
  | '-'  when beam.dir = North || beam.dir = South ->
    (incr_beam {beam with dir = East}, incr_beam {beam with dir = West})
  | _ -> failwith (Printf.sprintf "Invalid Map Tile @ (y=%02d,x=%02d)" beam.y beam.x)
 in
 let rec next_step = function
 | Some beam when not @@ BeamSet.mem beam !memo ->
   memo := BeamSet.add beam !memo;
   visited.(beam.y).(beam.x) <- true;
   let (b1,b2) = single_step beam in
   next_step b2;
   next_step b1
 | _ -> () in
(*
 let print_visited () =
  for y = 0 to Array.length visited - 1 do for x = 0 to Array.length visited.(0) - 1 do
   print_char (if visited.(y).(x) then '#' else '.')
  done; print_newline () ; done in
*)
 let count_visited () =
  let acc = ref 0 in
  for y = 0 to Array.length visited - 1 do for x = 0 to Array.length visited.(0) - 1 do
   if visited.(y).(x) then incr acc
  done done; !acc in
 let clear_visited () = Array.iter (fun arr -> Array.fill arr 0 (Array.length arr) false) visited in
 let max_val = ref 0 in
 (* brute force works because of earlier optimizations *)
 for y = 0 to Array.length visited - 1 do
  next_step (Some {y; x = 0; dir = East});
  max_val := max !max_val (count_visited ());
  memo := BeamSet.empty;
  clear_visited ();
  next_step (Some {y; x = (Array.length visited.(0) - 1); dir = West});
  max_val := max !max_val (count_visited ());
  memo := BeamSet.empty;
  clear_visited ();
 done;
 for x = 0 to Array.length visited.(0) - 1 do
  next_step (Some {x; y = 0; dir = South});
  max_val := max !max_val (count_visited ());
  memo := BeamSet.empty;
  clear_visited ();
  next_step (Some {x; y = (Array.length visited.(0) - 1); dir = North});
  max_val := max !max_val (count_visited ());
  memo := BeamSet.empty;
  clear_visited ();
 done;
 !max_val
 (* next_step (Some {y = 0; x = 0; dir = East}); *)
(* single_step {y = 0; x = 0; dir = East} |> (fun (d1, d2) -> Option.map print_beam d1 |> ignore ; Option.map print_beam d2 |> ignore) *)

(* playing with lava - heat loss *)
(* start: NW, end: SE *)
(* maximum straight line = 3 blocks *)
(* minimum path, but with an extra parameter *)
(* review djikstra's algorithm *)
(* optimize later! *)
let problem_17a () =
 let (>>=) = Option.bind in
 let n_of_c c = Char.code c - 0x30 in
 let module Crucible = struct
  type direction = North | South | East | West
  type t = {y: int; x: int; dir : direction; wob: int; loss: int}
  let print c = 
   Printf.printf "(y=%02d,x=%02d) wob=%d loss=%d dir=" c.y c.x c.wob c.loss;
   print_char (match c.dir with North -> 'N' | South -> 'S' | East -> 'E' | West -> 'W') ;
   print_newline ()
  end in
 let input = In_channel.(with_open_bin "17.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length input and w = String.length input.(0) in
 let valid_next (cur : Crucible.t) =
  (match cur.dir with
  | East   -> {cur with x = cur.x + 1; wob = cur.wob - 1}
  | South  -> {cur with y = cur.y + 1; wob = cur.wob - 1}
  | West   -> {cur with x = cur.x - 1; wob = cur.wob - 1}
  | North  -> {cur with y = cur.y - 1; wob = cur.wob - 1})
  ::
  (match cur.dir with
  | East  | West  -> [{cur with y = cur.y - 1; wob = 2; dir = North}; {cur with y = cur.y + 1; wob = 2; dir = South}]
  | North | South -> [{cur with x = cur.x - 1; wob = 2; dir = West}; {cur with x = cur.x + 1; wob = 2; dir = East}])
  |> List.filter Crucible.(fun cur -> cur.wob >= 0 && cur.y >= 0 && cur.y < h && cur.x >= 0 && cur.x < w)
 in
 let djk_q = Queue.create () in
 (* mins have to be lists of cursors *)
 let wob_size = 3 in
 let mins = Array.init h (fun _ -> Array.init w (fun _ -> Array.init wob_size (fun _ -> []))) in
 let start = Crucible.{y = 0; x = 0; dir = East; wob = 3; loss = 0} in
 (*valid_next start |> List.iter (Crucible.print) ; *)
 Queue.add_seq djk_q (List.to_seq @@ valid_next start);
 let walk () =
  Queue.take_opt djk_q >>=
  (fun cur ->
   let cur = Crucible.{cur with loss = cur.loss + (n_of_c input.(cur.y).[cur.x])} in
   match List.filter Crucible.(fun c -> c.dir = cur.dir) mins.(cur.y).(cur.x).(cur.wob) with
   | [c] when c.loss <= cur.loss -> Some ()
   | _ -> mins.(cur.y).(cur.x).(cur.wob) <- cur :: (List.filter Crucible.(fun c -> cur.dir <> c.dir) mins.(cur.y).(cur.x).(cur.wob)); 
     if cur.y <> (h-1) || cur.x <> (w-1)
     then Queue.add_seq djk_q (List.to_seq @@ valid_next cur)|> Option.some
     else Some ()
  )
 in
 while walk () |> Option.is_some do () done;
 for i = 0 to 2 do List.iter (Crucible.print) mins.(h-1).(w-1).(i) done;
 Array.fold_left (List.fold_left Crucible.(fun a cur -> min a cur.loss)) Int.max_int mins.(h-1).(w-1)

let problem_17b () =
 let (>>=) = Option.bind in
 let n_of_c c = Char.code c - 0x30 in
 let module UCrucible = struct
  type direction = North | South | East | West
  type t = {y: int; x: int; dir : direction; wob: int; loss: int}
  let init_wob = 9
  let turn_wob = 6
  let can_turn c = c.wob <= turn_wob
  let will_crash c limit_y limit_x = (not @@ can_turn c) &&
   (match c.dir with
    | North -> c.y - (c.wob - turn_wob) < 0
    | East  -> c.x + (c.wob - turn_wob) >= limit_x
    | South -> c.y + (c.wob - turn_wob) >= limit_y
    | West  -> c.x - (c.wob - turn_wob) < 0)
  let print c = 
   Printf.printf "(y=%02d,x=%02d) wob=%d loss=%d dir=" c.y c.x c.wob c.loss;
   print_char (match c.dir with North -> 'N' | South -> 'S' | East -> 'E' | West -> 'W') ;
   print_newline ()
  end in
 let input = In_channel.(with_open_bin "17.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length input and w = String.length input.(0) in
 let valid_next (cur : UCrucible.t) =
  (match cur.dir with
  | East   -> {cur with x = cur.x + 1; wob = cur.wob - 1}
  | South  -> {cur with y = cur.y + 1; wob = cur.wob - 1}
  | West   -> {cur with x = cur.x - 1; wob = cur.wob - 1}
  | North  -> {cur with y = cur.y - 1; wob = cur.wob - 1})
  ::
  (match cur.dir with
  | _ when not @@ UCrucible.can_turn cur -> []
  | East  | West  -> [{cur with y = cur.y - 1; wob = UCrucible.init_wob; dir = North}; {cur with y = cur.y + 1; wob = UCrucible.init_wob; dir = South}]
  | North | South -> [{cur with x = cur.x - 1; wob = UCrucible.init_wob; dir = West}; {cur with x = cur.x + 1; wob = UCrucible.init_wob; dir = East}])
  |> List.filter UCrucible.(fun cur -> (not @@ UCrucible.will_crash cur h w) && cur.wob >= 0 && cur.y >= 0 && cur.y < h && cur.x >= 0 && cur.x < w)
 in
 let djk_q = Queue.create () in
 (* mins are lists of cursors *)
 let wob_size = 10 in
 let mins = Array.init h (fun _ -> Array.init w (fun _ -> Array.init wob_size (fun _ -> []))) in
 let starts =
  UCrucible.[|
   {y = 0; x = 0; dir = East;  wob = UCrucible.init_wob + 1; loss = 0};
   {y = 0; x = 0; dir = South; wob = UCrucible.init_wob + 1; loss = 0}|]  in
 (*valid_next start |> List.iter (Crucible.print) ; *)
 Queue.add_seq djk_q (List.to_seq @@ valid_next starts.(0));
 Queue.add_seq djk_q (List.to_seq @@ valid_next starts.(1));
 let walk () =
  Queue.take_opt djk_q >>=
  (fun cur ->
   let cur = UCrucible.{cur with loss = cur.loss + (n_of_c input.(cur.y).[cur.x])} in
   match List.filter UCrucible.(fun c -> c.dir = cur.dir) mins.(cur.y).(cur.x).(cur.wob) with
   | [c] when c.loss <= cur.loss -> Some ()
   | _ -> mins.(cur.y).(cur.x).(cur.wob) <- cur :: (List.filter UCrucible.(fun c -> cur.dir <> c.dir) mins.(cur.y).(cur.x).(cur.wob)); 
     if cur.y <> (h-1) || cur.x <> (w-1)
     then Queue.add_seq djk_q (List.to_seq @@ valid_next cur)|> Option.some
     else Some ()
  )
 in
 while walk () |> Option.is_some do () done;
 (* any wob > 6 is impossible because the crucible will crash *)
 for i = 0 to 6 do List.iter (UCrucible.print) mins.(h-1).(w-1).(i) done;
 Array.fold_left (List.fold_left UCrucible.(fun a cur -> min a cur.loss)) Int.max_int mins.(h-1).(w-1)

let problem_17b_opt () =
 let (>>=) = Option.bind in
 let n_of_c c = Char.code c - 0x30 in
 let module UCrucible = struct
  type direction = North | South | East | West
  type t = {y: int; x: int; dir : direction; wob: int; loss: int}
  let to_int c = c.x lor (c.y lsl 8) lor (c.wob lsl 16) lor (Obj.magic c.dir lsl 20) lor (c.loss lsl 22)
  let of_int n = {x = n land 0xFF; y = (n lsr 8) land 0xFF; wob = (n lsr 16) land 0xF ; dir = (n lsr 20) land 0x3 |> Obj.magic; loss = n lsr 22}
  let init_wob = 9
  let turn_wob = 6
  let can_turn c = c.wob <= turn_wob
  let will_crash c limit_y limit_x = (not @@ can_turn c) &&
   (match c.dir with
    | North -> c.y - (c.wob - turn_wob) < 0
    | East  -> c.x + (c.wob - turn_wob) >= limit_x
    | South -> c.y + (c.wob - turn_wob) >= limit_y
    | West  -> c.x - (c.wob - turn_wob) < 0)
  let print c = 
   Printf.printf "(y=%02d,x=%02d) wob=%d loss=%d dir=" c.y c.x c.wob c.loss;
   print_char (match c.dir with North -> 'N' | South -> 'S' | East -> 'E' | West -> 'W') ;
   print_newline ()
  end in
 let input = In_channel.(with_open_bin "17.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length input and w = String.length input.(0) in
 let valid_next (cur : UCrucible.t) =
  (match cur.dir with
  | East   -> {cur with x = cur.x + 1; wob = cur.wob - 1}
  | South  -> {cur with y = cur.y + 1; wob = cur.wob - 1}
  | West   -> {cur with x = cur.x - 1; wob = cur.wob - 1}
  | North  -> {cur with y = cur.y - 1; wob = cur.wob - 1})
  ::
  (match cur.dir with
  | _ when not @@ UCrucible.can_turn cur -> []
  | East  | West  -> [{cur with y = cur.y - 1; wob = UCrucible.init_wob; dir = North}; {cur with y = cur.y + 1; wob = UCrucible.init_wob; dir = South}]
  | North | South -> [{cur with x = cur.x - 1; wob = UCrucible.init_wob; dir = West}; {cur with x = cur.x + 1; wob = UCrucible.init_wob; dir = East}])
  |> List.filter UCrucible.(fun cur -> (not @@ UCrucible.will_crash cur h w) && cur.wob >= 0 && cur.y >= 0 && cur.y < h && cur.x >= 0 && cur.x < w)
 in
 let djk_q = Queue.create () in
 (* mins are lists of cursors *)
 let wob_size = 10 in
 let prunable (c : UCrucible.t) = c.loss > c.x * 5 + c.y * 5 in
 (*let mins = Array.init h (fun _ -> Array.init w (fun _ -> Array.init wob_size (fun _ -> []))) in*)
 let open Bigarray in
 (* ~-1 |> UCrucible.of_int |> UCrucible.print: (y=255,x=255,) wob=15 loss=219902325551 dir=W *)
 (* I do not need to store the entire cursor structure, but for convenience, it is nice *)
 let mins = Array.init 4 (fun _ -> Array3.init Int C_layout h w wob_size (fun _ _ _ -> ~-1)) in
 let starts =
  UCrucible.[|
   {y = 0; x = 0; dir = East;  wob = UCrucible.init_wob + 1; loss = 0};
   {y = 0; x = 0; dir = South; wob = UCrucible.init_wob + 1; loss = 0}|]  in
 (*valid_next start |> List.iter (Crucible.print) ; *)
 Queue.add_seq djk_q (List.to_seq @@ valid_next starts.(0));
 Queue.add_seq djk_q (List.to_seq @@ valid_next starts.(1));
 let walk () =
  Queue.take_opt djk_q >>=
  (fun cur ->
   let cur = UCrucible.{cur with loss = cur.loss + (n_of_c input.(cur.y).[cur.x])} in
   match UCrucible.of_int mins.(cur.dir |> Obj.magic).{cur.y,cur.x,cur.wob} with
   | c when c.loss <= cur.loss || prunable cur -> Some ()
   | _ -> mins.(cur.dir |> Obj.magic).{cur.y,cur.x,cur.wob} <- UCrucible.to_int cur; 
     if cur.y <> (h-1) || cur.x <> (w-1)
     then Queue.add_seq djk_q (List.to_seq @@ valid_next cur)|> Option.some
     else Some ()
  )
 in
 while walk () |> Option.is_some do () done;
 (* any wob > 6 is impossible because the crucible will crash *)
 let res = ref Int.max_int in
 for i = 0 to 6 do UCrucible.of_int mins.(1).{h-1,w-1,i} |> UCrucible.(fun cur -> if cur.loss < !res then res := cur.loss; print cur) done;
 for i = 0 to 6 do UCrucible.of_int mins.(2).{h-1,w-1,i} |> UCrucible.(fun cur -> if cur.loss < !res then res := cur.loss; print cur) done;
 !res

(* for part 1 ignore the color part of the input *)
let problem_18a () =
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module Dir = struct
  type t = North | South | East | West
  let of_char = function 'U' -> North | 'D' -> South | 'L' -> West | 'R' -> East | _ -> failwith "invalid direction"
  let to_string = function North -> "North" | South -> "South" | West -> "West" | East -> "East"
  let move_yx (y,x) = function North -> (y-1,x) | South -> (y+1,x) | West -> (y,x-1) | East -> (y,x+1)
 end in
 let parse s =
  match String.split_on_char ' ' s with
  | [dir; len; color] -> (Dir.of_char dir.[0], int_of_string len)
  | _ -> failwith "invalid input" in
 let input =
  In_channel.(with_open_bin "18.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map parse |> Array.of_seq)) in
 let cur = ref (0,0) in
 let walls = ref YXSet.empty in
 let walk_line (dir, len) =
  for i = 1 to len do
   cur := Dir.move_yx !cur dir;
   walls := YXSet.add !cur !walls
  done in
 Array.iter (walk_line) input;
 (* Array.iter Dir.(fun (d,l) -> Printf.printf "%s: %d\n" (to_string d) l) input; *)
 let min_y = pred @@ YXSet.fold (fun (y,x) a -> min a y) !walls Int.max_int in
 let min_x = pred @@ YXSet.fold (fun (y,x) a -> min a x) !walls Int.max_int in
 let max_y = succ @@ YXSet.fold (fun (y,x) a -> max a y) !walls Int.min_int in
 let max_x = succ @@ YXSet.fold (fun (y,x) a -> max a x) !walls Int.min_int in
 Printf.printf "Box Size: (%d,%d) -> (%d,%d)\n"  min_y min_x max_y max_x;
 (* map shows that there is always at least one space between vertical walls! *)
(*
 let map = Array.make_matrix (max_y - min_y) (max_x - min_x) '.' in
 YXSet.iter (fun (y,x) -> map.(y - min_y).(x - min_x) <- '#') !walls;
 Array.iter (fun arr -> Array.iter (print_char) arr; print_newline ()) map
*)
 let inside_integral = ref 0 in
 let inside = ref false in
 let on_wall_diag_up = ref false in
 let on_wall_diag_down = ref false in
 for y = min_y to max_y do
  inside := false ;
  for x = pred min_x to succ max_x do
   if YXSet.mem (y,x) !walls then
   match (YXSet.mem (y-1,x) !walls, YXSet.mem (y+1,x) !walls, YXSet.mem (y,x+1) !walls) with
   | (true , true , false) -> inside := not !inside
   (* handle corner cases *)
   | (false, true , true ) -> on_wall_diag_up   := true
   | (true , false, true ) -> on_wall_diag_down := true
   | (false, true , false ) when !on_wall_diag_down -> inside := not !inside
   | (true , false, false ) when !on_wall_diag_up   -> inside := not !inside
   | _ -> ()
   else ( on_wall_diag_up := false; on_wall_diag_down := false ; if !inside then incr inside_integral )
  done;
  if !inside then Printf.printf "Failure at line %d\n" y
 done;
 !inside_integral + YXSet.cardinal !walls

(* for part 2, optimally store line segments, *not* points *)
let problem_18b () =
 (*let (>>=) = Option.bind in*)
 (*let ( let* ) = Option.bind in*)
 let debug = false in
 let module IntSet = Set.Make(Int) in
 let module Dir = struct
  type t = North | South | East | West
  let of_char = function '3' -> North | '1' -> South | '2' -> West | '0' -> East | _ -> failwith "invalid direction"
  let to_string = function North -> "North" | South -> "South" | West -> "West" | East -> "East"
  let move_yx (y,x) = function North -> (y-1,x) | South -> (y+1,x) | West -> (y,x-1) | East -> (y,x+1)
  let move_yx_len (y,x) len = function North -> (y-len,x) | South -> (y+len,x) | West -> (y,x-len) | East -> (y,x+len)
 end in
 let parse s =
  match String.split_on_char ' ' s with
  | [_; _; color] -> (Dir.of_char color.[7], Scanf.sscanf (String.sub color 2 5) "%x" (Fun.id))
  | _ -> failwith "invalid input" in
 let input =
  In_channel.(with_open_bin "18.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map parse |> Array.of_seq)) in
 if debug then Array.iter (fun (d,l) -> Printf.printf "%s: %d\n" (Dir.to_string d) l) input;
 (* store set of vertcal lines only, then scan left to right *)
 let cur = ref (0,0) in
 let v_lines = ref []
 and h_lines = ref [] in
 Array.iter Dir.(fun (d,l) -> 
  let (y,x) = !cur
  and (y',x') = Dir.move_yx_len !cur l d in
  if d = North || d = South then v_lines := (min y y', max y y', x) :: !v_lines
  else h_lines := (min x x', max x x', y) :: !h_lines ;
  cur := (y',x')
 ) input ;
 let box_size (y, xset) (y', _) =
  let (w,_) = List.fold_left (fun (a,sign) x -> if sign then (a - x, false) else (a + x + 1, true)) (0,true) xset in
  w * (y' - y + 1)
 in
 let intersection_size (_,xset) (_,xset') =
  let inter_size (l0,u0) (l1,u1) = max 0 (min u0 u1 - max l0 l1 + 1) in
  let pairify seq =
   seq |> Seq.fold_left
    (fun (plist, last_x) x -> match last_x with None -> (plist, Some x) | Some last_x -> ((last_x, x)::plist, None))
    ([],None) |> fst in
  let ps1 = pairify (List.to_seq xset)
  and ps2 = pairify (List.to_seq xset') in
  List.fold_left (fun a interval0 -> List.fold_left (fun a interval1 -> a + inter_size interval0 interval1) a ps2) 0 ps1 in
 (*List.rev !v_lines*)
 let points = List.fold_left (fun a (y,y',_) -> IntSet.add y' @@ IntSet.add y a) IntSet.empty !v_lines in
 if debug then IntSet.iter (Printf.printf "%d\n") points ;
 let intervals = IntSet.to_seq points |> Seq.map
  (fun pt ->
    List.map (fun (y,y',x) -> if pt >= y && pt < y' then Some x else None) !v_lines |>
    List.filter Option.is_some |> List.fold_left (fun a x -> IntSet.add (Option.get x) a) IntSet.empty |> IntSet.to_seq |> List.of_seq
  ) |>
  Seq.zip (IntSet.to_seq points) |> List.of_seq in
  List.fold_left
  (fun (a, last_interval) interval ->
   let box = box_size last_interval interval in
   let inter = intersection_size last_interval interval in
   (a + box - inter, interval)
  )
  (0, List.hd intervals) (List.tl intervals) |> fst
 (*intervals*)

 (* by hand solution of the sample problem based on "intervals" output: *)
 (*
    [(0     , [0; 461937]);
     (56407 , [0; 818608]);
     (356353, [0; 497056; 609066; 818608]);
     (500254, [5411; 497056; 609066; 818608]);
     (919647, [5411; 497056; 609066; 1186328]);
     (1186328, [])]
    
    (~- 0     + 56407   + 1) * (461937 + 1) +
    (~-56407  + 356353  + 1) * (818608 + 1) +
    (~-356353 + 500254  + 1) * (497056 + 1          - 609066 + 818608  + 1) +
    (~-500254 + 919647  + 1) * (~-5411 + 497056 + 1 - 609066 + 818608  + 1) +
    (~-919647 + 1186328 + 1) * (~-5411 + 497056 + 1 - 609066 + 1186328 + 1)
    - (~-0      + 461937 + 1)
    - (~-0      + 497056 + 1 - 609066 + 818608 + 1)
    - (~-5411   + 497056 + 1 - 609066 + 818608 + 1)
    - (~-5411   + 497056 + 1 - 609066 + 818608 + 1);;
 *)
 (* the solution is to add all overlapping blocks (all intervals + 1, both horizontally, and vertically) *)
 (* then we subtract the length of intersecting ranges for every single y point *)
 (* calculate every meaningful y point *)
 (* iterate through vertical line set at each y point to get a sorted list of x values, and add every other interval; list length should always be even *)

let problem_19a () =
 let module Cond = struct
  type t = {lhs : char; compare_test : int; rhs : int; dest : string}
  (* x|m|a|s = '.' *)
  let print cond = Printf.printf "%c,%d,%d,%s\n" cond.lhs cond.compare_test cond.rhs cond.dest
 end in
 let module Gear = struct
  type t = {x : int; m : int; a : int; s : int}
  let print g = Printf.printf "{x=%d,m=%d,a=%d,s=%d}\n" g.x g.m g.a g.s
  let idx_by_char g = function 'x' -> g.x | 'm' -> g.m | 'a' -> g.a | 's' -> g.s | _ -> assert false
  let sum g = g.x + g.m + g.a + g.s
 end in
 let parse_workflow s =
  match String.split_on_char '{' s with
  | [label; cond_list_raw] ->
    let conds = String.split_on_char ',' cond_list_raw |>
    List.map (fun s ->
     if s.[String.length s - 1] = '}' then Cond.{lhs = '.'; compare_test = 0; rhs = 0; dest = String.sub s 0 (String.length s - 1)} else
     match String.split_on_char ':' s with
     | [cond; dest] when cond.[1] = '>' -> Cond.{lhs = cond.[0]; compare_test = 1; rhs = int_of_string @@ String.sub cond 2 (String.length cond - 2); dest}
     | [cond; dest] when cond.[1] = '<' -> Cond.{lhs = cond.[0]; compare_test = ~-1; rhs = int_of_string @@ String.sub cond 2 (String.length cond - 2); dest}
     | _ -> failwith "invalid input: missing ':'") in
    (label, conds)
  | _ -> failwith "invalid input: missing {" in
 let parse_gear s = Scanf.sscanf s "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> Gear.{x;m;a;s}) in
 let (workflows, gears) =
   In_channel.( with_open_bin "19.txt" (fun ic ->
    let (workflows_raw, gears_raw) = Seq.of_dispenser (fun () -> input_line ic) |> Seq.filter ((<>)"") |> Seq.memoize |> Seq.partition (fun s -> s.[0] <> '{') in
    (Seq.map parse_workflow workflows_raw |> List.of_seq, Seq.map parse_gear gears_raw |> List.of_seq)))
 in
 let process_gear_step (g : Gear.t) (wf : Cond.t list) =
  let nxt = List.to_seq wf |>
            Seq.drop_while (fun cond -> not Cond.( cond.lhs = '.' || (compare (Gear.idx_by_char g cond.lhs) (cond.rhs) = cond.compare_test))) |>
            Seq.uncons |> Option.get |> fst in
  match nxt.dest with
  | "A" -> Either.right true
  | "R" -> Either.right false
  | label -> Either.left label
 in
 let wf_table = Hashtbl.of_seq (List.to_seq workflows) in
 let process_gear (g : Gear.t) =
   let rec loop (g : Gear.t) key =
    Either.fold ~left:(loop g) ~right:(Fun.id) @@ process_gear_step g (Hashtbl.find wf_table key)
   in loop g "in"
 in
 (*List.iter Gear.print gears*)
 (*List.iter (fun (lbl, conds) -> print_endline lbl) workflows*)
 (*process_gear_step (List.hd gears) ((Hashtbl.find wf_table "in"))*)
 let results = List.map process_gear gears in
 Seq.zip (List.to_seq results) (List.to_seq gears) |> Seq.filter (fst) |> Seq.fold_left (fun a (_,g) -> Gear.sum g + a) 0

let problem_19b () =
 let module Cond = struct
  type t = {lhs : char; compare_test : int; rhs : int; dest : string}
  (* x|m|a|s = '.' *)
  let print cond = Printf.printf "%c,%d,%d,%s\n" cond.lhs cond.compare_test cond.rhs cond.dest
 end in
 let module Gear = struct
  type t = {x : int; m : int; a : int; s : int}
  let print g = Printf.printf "{x=%d,m=%d,a=%d,s=%d}\n" g.x g.m g.a g.s
  let idx_by_char g = function 'x' -> g.x | 'm' -> g.m | 'a' -> g.a | 's' -> g.s | _ -> assert false
  let sum g = g.x + g.m + g.a + g.s
 end in
 let parse_workflow s =
  match String.split_on_char '{' s with
  | [label; cond_list_raw] ->
    let conds = String.split_on_char ',' cond_list_raw |>
    List.map (fun s ->
     if s.[String.length s - 1] = '}' then Cond.{lhs = '.'; compare_test = 0; rhs = 0; dest = String.sub s 0 (String.length s - 1)} else
     match String.split_on_char ':' s with
     | [cond; dest] when cond.[1] = '>' -> Cond.{lhs = cond.[0]; compare_test = 1; rhs = int_of_string @@ String.sub cond 2 (String.length cond - 2); dest}
     | [cond; dest] when cond.[1] = '<' -> Cond.{lhs = cond.[0]; compare_test = ~-1; rhs = int_of_string @@ String.sub cond 2 (String.length cond - 2); dest}
     | _ -> failwith "invalid input: missing ':'") in
    (label, conds)
  | _ -> failwith "invalid input: missing {" in
 (*let parse_gear s = Scanf.sscanf s "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> Gear.{x;m;a;s}) in*)
 let (workflows, _) =
   In_channel.( with_open_bin "19.txt" (fun ic ->
    let (workflows_raw, gears_raw) = Seq.of_dispenser (fun () -> input_line ic) |> Seq.filter ((<>)"") |> Seq.memoize |> Seq.partition (fun s -> s.[0] <> '{') in
    (Seq.map parse_workflow workflows_raw |> List.of_seq, ())))
 in
 let wf_table = Hashtbl.of_seq (List.to_seq workflows) in

let valid_ranges var_id (lo,hi) wf =
 List.fold_left Cond.(fun (a,lo,hi) cond ->
  if var_id <> cond.lhs then ((cond.dest,lo,hi)::a,lo,hi) else
  match cond.compare_test with
  | 1 ->  ((cond.dest, max lo (cond.rhs+1), hi)::a,lo,min hi cond.rhs)
  | -1 -> ((cond.dest, lo, min hi (cond.rhs-1))::a, max lo cond.rhs, hi)
  | _ -> assert false)
  ([],lo,hi) wf |> (fun (res,_,_) -> res) in

let process_gear_range_step (glo,ghi) wf =
 let open Gear in
 let v_x = valid_ranges 'x' (glo.x,ghi.x) wf |> List.to_seq in
 let v_m = valid_ranges 'm' (glo.m,ghi.m) wf |> List.to_seq in
 let v_a = valid_ranges 'a' (glo.a,ghi.a) wf |> List.to_seq in
 let v_s = valid_ranges 's' (glo.s,ghi.s) wf |> List.to_seq in
 let v_xm = Seq.map2 (fun (lbl,xlo,xhi) (_,mlo,mhi) -> (lbl,xlo,xhi,mlo,mhi)) v_x v_m in
 let v_xma = Seq.map2 (fun (lbl,xlo,xhi,mlo,mhi) (_,alo,ahi) -> (lbl,xlo,xhi,mlo,mhi,alo,ahi)) v_xm v_a in
 let v_xmas = Seq.map2 (fun (lbl,xlo,xhi,mlo,mhi,alo,ahi)  (_,slo,shi) -> (lbl,{x=xlo;m=mlo;a=alo;s=slo},{x=xhi;m=mhi;a=ahi;s=shi})) v_xma v_s in
 v_xmas in

 let process_gear_range (lo,hi) =
  let process_queue = Queue.create () in
  let valid_count = ref 0 in
  let glo = Gear.{x=lo;m=lo;a=lo;s=lo}
  and ghi = Gear.{x=hi;m=hi;a=hi;s=hi} in
  Queue.add_seq process_queue @@ process_gear_range_step (glo,ghi) (Hashtbl.find wf_table "in" );
  while not @@ Queue.is_empty process_queue do
   let (lbl,glo,ghi) = Queue.take process_queue in
   if ghi.x < glo.x || ghi.m < glo.m || ghi.a < glo.a || ghi.s < glo.s then () else
   if lbl = "A" then valid_count := !valid_count + (ghi.x-glo.x+1)*(ghi.m-glo.m+1)*(ghi.a-glo.a+1)*(ghi.s-glo.s+1) else
   if lbl = "R" then () else
   Queue.add_seq process_queue @@ process_gear_range_step (glo,ghi) (Hashtbl.find wf_table lbl)
  done;
  !valid_count in
 process_gear_range (1,4000)

(* Rules: *)
(* 
 flip-flop (%): initially off; flips only on low-pulse
  - if flipped on,  sends a hi-pulse
  - if flipped off, sends a lo-pulse
 conjunction (&): initially off; sample-and-hold on input (NAND) on output
 broadcaster: pass (output = input)
 button: sends lo-pulse to broadcaster (does not act until all processing is done)
 processes are sequential (breadth first), not parallel
 push the button 1000 times, res = hi * low (sent)
*)
(* copy every signal output to sink queue *)
(* use hashtable to store modules % should only have one input, % should have multiple inputs *)

let problem_20a () =
 let debug = false in
 let ( let* ) = Option.bind in
 let return = Option.some in
 let unwrap = Option.iter Fun.id in
 let module Mod = struct
  (* pulses: true -> Hi; false -> Lo *)
  (* '%', '&', '.' (button+broadcast) *)
  type t = { label : string; mod_t : char
           (* not necessary because we will track with the sink queue *)
           (*; mutable activated : bool *)
           ; mutable state: bool
           (* needs to be mutable, because this is [||] until all inputs are registered *)
           ; mutable wire_states : (string * bool) array
           (* used to register wire_states for & *)
           ; mutable inputs: string list
           ; outputs : string list}
  let process sink (m,signal,from) =
   match m.mod_t with
   (* button + broadcast *)
   | '.' ->
     List.iter (fun m_lbl -> Queue.add (m_lbl, false, "broadcaster") sink) m.outputs
   (* flip-flop *)
   | '%' ->
     (* if signal is high, do nothing *)
     if signal then () else
     (* flip state and broadcast*)
     (m.state <- not m.state;
      List.iter (fun m_lbl -> Queue.add (m_lbl, m.state, m.label) sink) m.outputs)
   (* conjunction *)
   | '&' ->
     (* update wire state *)
     Array.iteri (fun i (lbl,_) -> if lbl = from then m.wire_states.(i) <- (lbl,signal)) m.wire_states;
     (* if all wire_states are hi, send lo, else send hi *)
     let out = Array.to_seq m.wire_states |> Seq.for_all snd |> not in
     (* broadcast out *)
     List.iter (fun m_lbl -> Queue.add (m_lbl, out, m.label) sink) m.outputs
   | c -> failwith @@ "invalid type: " ^ (Char.escaped c)
  let register_wires m =
   match m.mod_t with
   | '&' -> m.wire_states <- Array.of_seq Seq.(zip (List.to_seq m.inputs) (repeat false))
   | _ -> ()
  let of_string s =
   let raw_label = String.sub s 0 (String.index s ' ') in
   let (mod_t, label) = if raw_label = "broadcaster" then ('.', raw_label) else (raw_label.[0], String.(sub raw_label 1 (length raw_label - 1))) in
   let outputs = String.to_seq s |> Seq.drop (String.rindex s '>' + 1) |> Seq.filter ((<>)' ') |> String.of_seq |> String.split_on_char ',' in
   {label; mod_t; state = false; wire_states = [||]; inputs = []; outputs}
  let register_input m s = if List.mem s m.inputs then () else m.inputs <- s :: m.inputs
 end in
 let table = Hashtbl.create 100 in
 In_channel.(with_open_bin "20.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.iter (fun s -> let m = Mod.of_string s in Hashtbl.add table m.label m)));
 (* register table elements *)
 Hashtbl.to_seq_values table |> Seq.iter (fun m -> List.iter Mod.(fun lbl -> unwrap @@ let* m_dst = Hashtbl.find_opt table lbl in return @@ register_input m_dst m.label) m.outputs);
 (* register wires *)
 Hashtbl.to_seq_values table |> Seq.iter Mod.(fun m -> register_wires m) ;
 if debug then (
  Hashtbl.iter Mod.(fun lbl m -> Printf.printf "%s : %c%s " lbl m.mod_t m.label ; List.iter (fun lbl -> print_string lbl ; print_char ',') m.outputs ; print_newline ()) table;
  print_endline "======" );
 let sink = Queue.create () in
 let hi_cnt = ref 0 and lo_cnt = ref 0 in
 let press_button () =
  Queue.add ("broadcaster", false, "button") sink;
  while not @@ Queue.is_empty sink do
   unwrap @@ 
    let* (to_lbl, signal, from_lbl) = Queue.take_opt sink in 
    if signal then incr hi_cnt else incr lo_cnt;
    if debug then Printf.printf "%s -%s-> %s\n" from_lbl (if signal then "high" else "low") to_lbl;
    let* m = Hashtbl.find_opt table to_lbl in
    return @@ Mod.process sink (m,signal,from_lbl)
  done in
 for i = 1 to 1000 do press_button () done;
 (*(!lo_cnt, !hi_cnt, !lo_cnt * !hi_cnt)*)
 !lo_cnt * !hi_cnt

let problem_20b () =
 let debug = false in
 let ( let* ) = Option.bind in
 let return = Option.some in
 let unwrap = Option.iter Fun.id in
 let module Mod = struct
  (* pulses: true -> Hi; false -> Lo *)
  (* '%', '&', '.' (button+broadcast) *)
  type t = { label : string; mod_t : char
           (* not necessary because we will track with the sink queue *)
           (*; mutable activated : bool *)
           ; mutable state: bool
           (* needs to be mutable, because this is [||] until all inputs are registered *)
           ; mutable wire_states : (string * bool) array
           (* used to register wire_states for & *)
           ; mutable inputs: string list
           ; outputs : string list}
  let process sink (m,signal,from) =
   match m.mod_t with
   (* button + broadcast *)
   | '.' ->
     List.iter (fun m_lbl -> Queue.add (m_lbl, false, "broadcaster") sink) m.outputs
   (* flip-flop *)
   | '%' ->
     (* if signal is high, do nothing *)
     if signal then () else
     (* flip state and broadcast*)
     (m.state <- not m.state;
      List.iter (fun m_lbl -> Queue.add (m_lbl, m.state, m.label) sink) m.outputs)
   (* conjunction *)
   | '&' ->
     (* update wire state *)
     Array.iteri (fun i (lbl,_) -> if lbl = from then m.wire_states.(i) <- (lbl,signal)) m.wire_states;
     (* if all wire_states are hi, send lo, else send hi *)
     let out = Array.to_seq m.wire_states |> Seq.for_all snd |> not in
     (* broadcast out *)
     List.iter (fun m_lbl -> Queue.add (m_lbl, out, m.label) sink) m.outputs
   | c -> failwith @@ "invalid type: " ^ (Char.escaped c)
  let register_wires m =
   match m.mod_t with
   | '&' -> m.wire_states <- Array.of_seq Seq.(zip (List.to_seq m.inputs) (repeat false))
   | _ -> ()
  let of_string s =
   let raw_label = String.sub s 0 (String.index s ' ') in
   let (mod_t, label) = if raw_label = "broadcaster" then ('.', raw_label) else (raw_label.[0], String.(sub raw_label 1 (length raw_label - 1))) in
   let outputs = String.to_seq s |> Seq.drop (String.rindex s '>' + 1) |> Seq.filter ((<>)' ') |> String.of_seq |> String.split_on_char ',' in
   {label; mod_t; state = false; wire_states = [||]; inputs = []; outputs}
  let register_input m s = if List.mem s m.inputs then () else m.inputs <- s :: m.inputs
 end in
 let table = Hashtbl.create 100 in
 In_channel.(with_open_bin "20.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.iter (fun s -> let m = Mod.of_string s in Hashtbl.add table m.label m)));
 (* register table elements *)
 Hashtbl.to_seq_values table |> Seq.iter (fun m -> List.iter Mod.(fun lbl -> unwrap @@ let* m_dst = Hashtbl.find_opt table lbl in return @@ register_input m_dst m.label) m.outputs);
 (* register wires *)
 Hashtbl.to_seq_values table |> Seq.iter Mod.(fun m -> register_wires m) ;
 if debug then (
  Hashtbl.iter Mod.(fun lbl m -> Printf.printf "%s : %c%s " lbl m.mod_t m.label ; List.iter (fun lbl -> print_string lbl ; print_char ',') m.outputs ; print_newline ()) table;
  print_endline "======" );
 let sink = Queue.create () in
 let hi_cnt = ref 0 and lo_cnt = ref 0 in

 (* check for trigger loops has to happen inside press_button because everything is turned off before the cycle ends! *)
 let prime_triggers = Hashtbl.to_seq_values table |> Seq.find_map Mod.(fun m -> if List.mem "rx" m.outputs then Some m.wire_states else None) |> Option.get in
 if debug then Array.iter (fun (s,_) -> print_endline s) prime_triggers;
 (* make copy *)
 let trigger_loops = Array.map (Fun.const None) prime_triggers in

 let press_buttoni i =
  Queue.add ("broadcaster", false, "button") sink;
  while not @@ Queue.is_empty sink do
   unwrap @@ 
    let* (to_lbl, signal, from_lbl) = Queue.take_opt sink in 
    if signal then incr hi_cnt else incr lo_cnt;
    if debug then Printf.printf "%s -%s-> %s\n" from_lbl (if signal then "high" else "low") to_lbl;
    let* m = Hashtbl.find_opt table to_lbl in
    Mod.process sink (m,signal,from_lbl);

    Array.iteri (fun idx (_,state) -> if state && Option.is_none trigger_loops.(idx) then trigger_loops.(idx) <- Some i) prime_triggers;

    if debug && to_lbl = "th" && (Array.to_seq m.wire_states |> Seq.exists snd)
    then (Printf.printf "%05d : " i ; Array.iter (fun (lbl,state) -> Printf.printf "%s=%c," lbl (if state then 'H' else 'L')) m.wire_states ; print_newline (); flush_all ());
    return ()
  done in

 (* this is another case of finding primes *)
 (* res = prod of 3739 3793 3923 4027 *)
 for i = 1 to 10000 do
  press_buttoni i;
 done;
 Array.fold_left (fun a x -> Option.value ~default:0 x * a) 1 trigger_loops

let problem_21a () =
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let ( let* ) = Option.bind in
 let map = In_channel.(with_open_bin "21.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length map and w = String.length map.(0) in
 let start = map |> Array.to_seq |> Seq.fold_lefti (fun a y s -> Option.fold a ~some:(Option.some) ~none:(let* x = String.index_opt s 'S' in Some (y,x))) None |> Option.get in
 let plots = YXSet.(add start empty) in
 let neighbors (y,x) = 
  [(y-1,x);(y+1,x);(y,x-1);(y,x+1)] |> List.to_seq |> Seq.filter (fun (y,x) -> y >= 0 && y < h && x >= 0 && x < w && (map.(y).[x] = '.' || map.(y).[x] = 'S')) in
 let step plots =
  YXSet.to_seq plots |>
  Seq.map neighbors |> Seq.concat |>
  YXSet.of_seq in
 Seq.iterate step plots |> Seq.drop 64 |> Seq.uncons |> Option.get |> fst |> YXSet.cardinal
 (* Seq.(iterate step plots |> drop 0 |> take 132 |> map (YXSet.cardinal)) |> List.of_seq *)

let problem_21b () =
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let ( let* ) = Option.bind in
 let map = In_channel.(with_open_bin "21.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length map and w = String.length map.(0) in
 let start = (h / 2, w / 2) in
 let neighbors (y,x) = 
  [(y-1,x);(y+1,x);(y,x-1);(y,x+1)] |> List.to_seq |> Seq.filter (fun (y,x) -> y >= 0 && y < h && x >= 0 && x < w && (map.(y).[x] = '.' || map.(y).[x] = 'S')) in
 let sq x = x * x in

 let step plots =
  YXSet.to_seq plots |>
  Seq.map neighbors |> Seq.concat |>
  YXSet.of_seq in

 (* find odd and even saturation points (after oscillation begins) *)
 let (even_sat, odd_sat) = (match Seq.(iterate step YXSet.(add start empty) |> drop 130 |> take 2 |> map (YXSet.cardinal)) |> List.of_seq with [a;b] -> (a,b) | _ -> assert false) in

 (* calculate number of saturated grids that started from S with an odd and even number of steps *)
 (* saturation can also be calculated by counting a full grid pattern and subtracting # signs depending on if they are on an even or odd diagonal *)
 (* problem number is in the form of (n*131+65) -> n = outer_radius, which is 202300; 131 = h = w; 65 = h/2 *)
 (* inner radius is the number of blocks extending from the center that will be fully saturated *)
 let inner_radius = (26501365 / w - 1) in
 let outer_radius = inner_radius + 1 in
 (* odds is always an odd power because it contains the center point, which is also odd because the center starts with an odd number of steps *)
 let (odds, evens) = (sq (inner_radius/2*2+1), sq (outer_radius/2*2)) in

 (* helper function for grabbing two idxs of an infinite sequence *)
 let step_idx2 (i1,i2) seq =
  Option.get @@
   let* (res1, tl) = seq |> Seq.drop i1 |> Seq.uncons in
   let* (res2,_) = tl |> Seq.drop (i2-i1-1) |> Seq.uncons in
   Option.some (YXSet.cardinal res1, YXSet.cardinal res2)
 in
 
 (* corner cases *)
 let n_cc = Seq.iterate step YXSet.(add (h-1,snd start) empty) |> Seq.drop (h-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let s_cc = Seq.iterate step YXSet.(add (0,  snd start) empty) |> Seq.drop (h-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let w_cc = Seq.iterate step YXSet.(add (fst start,w-1) empty) |> Seq.drop (h-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let e_cc = Seq.iterate step YXSet.(add (fst start,0  ) empty) |> Seq.drop (h-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in

 (* edge cases @ 64 && 195 *)
 (*
 let ne_ec_s = Seq.(iterate step YXSet.(add (0,  w-1) empty)) |> Seq.drop (h/2-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let nw_ec_s = Seq.(iterate step YXSet.(add (0,  0  ) empty)) |> Seq.drop (h/2-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let se_ec_s = Seq.(iterate step YXSet.(add (h-1,w-1) empty)) |> Seq.drop (h/2-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let sw_ec_s = Seq.(iterate step YXSet.(add (h-1,0  ) empty)) |> Seq.drop (h/2-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let ne_ec_l = Seq.(iterate step YXSet.(add (0,  w-1) empty)) |> Seq.drop (3*h/2-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let nw_ec_l = Seq.(iterate step YXSet.(add (0,  0  ) empty)) |> Seq.drop (3*h/2-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let se_ec_l = Seq.(iterate step YXSet.(add (h-1,w-1) empty)) |> Seq.drop (3*h/2-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 let sw_ec_l = Seq.(iterate step YXSet.(add (h-1,0  ) empty)) |> Seq.drop (3*h/2-1) |> Seq.take 1 |> Seq.map (YXSet.cardinal) |> List.of_seq |> List.hd in
 *)
 let (ne_ec_s, ne_ec_l) = Seq.(iterate step YXSet.(add (0,  w-1) empty)) |> step_idx2 (h/2-1,3*h/2-1) in
 let (nw_ec_s, nw_ec_l) = Seq.(iterate step YXSet.(add (0,  0  ) empty)) |> step_idx2 (h/2-1,3*h/2-1) in
 let (se_ec_s, se_ec_l) = Seq.(iterate step YXSet.(add (h-1,w-1) empty)) |> step_idx2 (h/2-1,3*h/2-1) in
 let (sw_ec_s, sw_ec_l) = Seq.(iterate step YXSet.(add (h-1,0  ) empty)) |> step_idx2 (h/2-1,3*h/2-1) in

 (* small edge counts = outer radius, large edge counts = outer radius *)
 (odds * odd_sat +
  evens * even_sat +
  n_cc + s_cc + w_cc + e_cc +
  outer_radius * (ne_ec_s + nw_ec_s + se_ec_s + sw_ec_s) +
  inner_radius * (ne_ec_l + nw_ec_l + se_ec_l + sw_ec_l))

(* uses 3D ranges for jenga blocks *)
let problem_22a () =
 let debug = false in
 let ( let* ) = Option.bind in
 let return = Option.some in
 let unwrap = Option.iter Fun.id in
 let module Range3D = struct
  type t = {z0 : int; z1 : int; y0 : int; y1 : int; x0 : int; x1 : int}
  let intersecting r1 r2 =
   let inter_ordered x0 x1 x0' x1' = not (x0 > x1' || x1 < x0') in
   (* z is the most likely test to fail, so put this first *)
   inter_ordered r1.z0 r1.z1 r2.z0 r2.z1 &&
   inter_ordered r1.y0 r1.y1 r2.y0 r2.y1 &&
   inter_ordered r1.x0 r1.x1 r2.x0 r2.x1
  let succ r = {r with z0 = r.z0 + 1 ; z1 = r.z1 + 1}
  let pred r = {r with z0 = r.z0 - 1 ; z1 = r.z1 - 1}
  let droppable r rset = r.z0 > 1 && not (Seq.exists (intersecting (pred r)) rset)
  let rec  drop r rset = if droppable r rset then drop (pred r) rset else r
  let drop_opt  r rset = if droppable r rset then return @@ drop (pred r) rset else None
  let to_string r = Printf.sprintf "%u,%u,%u~%u,%u,%u" r.x0 r.y0 r.z0 r.x1 r.y1 r.z1
  let of_string s =
   (* validate order when parsing *)
   let (z0,z1,y0,y1,x0,x1) = Scanf.sscanf s "%u,%u,%u~%u,%u,%u" (fun x0 y0 z0 x1 y1 z1 -> (min z0 z1, max z0 z1, min y0 y1, max y0 y1, min x0 x1, max x0 x1)) in
   {z0;z1;y0;y1;x0;x1}
 end in
 let seq_of_array_excluding i arr =
  Seq.zip (Seq.ints 0) (Array.to_seq arr) |> Seq.filter_map (fun (idx,x) -> if i = idx then None else Some x)
 in
 let drop_blocks blocks =
  (* iterate until all blocks are static *)
  let stack_changed = ref true in
  let change_count = ref 0 in
  while !stack_changed = true do
   stack_changed := false;
   Array.iteri (fun i r ->
    unwrap @@
    let rset = seq_of_array_excluding i blocks in
    let* r' = Range3D.drop_opt r rset in
    stack_changed := true;
    incr change_count;
    return @@ blocks.(i) <- r'
   ) blocks
  done;
  !change_count
 in
 (* minor optimization - early exit - assume blocks below idx are undroppable *)
 let any_droppable_blocks_from i blocks_seq =
  let idx_seq = Seq.zip (Seq.ints 0) blocks_seq in
  Seq.exists (fun (i,r) ->
   let rset = idx_seq |> Seq.filter_map (fun (idx,r) -> if i = idx then None else Some r) in
   Range3D.droppable r rset
  ) (Seq.drop (max 0 (i-1)) idx_seq)
 in
 let input = In_channel.(with_open_bin "22.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map Range3D.of_string |> Array.of_seq)) in
 Array.stable_sort compare input;
 let shifted = drop_blocks input in
 Printf.printf "%d blocks shifted!" shifted;
 Array.stable_sort compare input;
 if debug then Array.iter (fun r -> r |> Range3D.to_string |> print_endline) input;
 let unremovable = ref 0 in
 Array.iteri (fun i r -> if any_droppable_blocks_from i (seq_of_array_excluding i input) then incr unremovable) input ;
 Array.length input - !unremovable

(* tracking support structures after initial fall could optimize this, but this is much simpler *)
let problem_22b () =
 let debug = false in
 let ( let* ) = Option.bind in
 let return = Option.some in
 let unwrap = Option.iter Fun.id in
 let module Range3D = struct
  type t = {z0 : int; z1 : int; y0 : int; y1 : int; x0 : int; x1 : int}
  let intersecting r1 r2 =
   let inter_ordered x0 x1 x0' x1' = not (x0 > x1' || x1 < x0') in
   (* z is the most likely test to fail, so put this first *)
   inter_ordered r1.z0 r1.z1 r2.z0 r2.z1 &&
   inter_ordered r1.y0 r1.y1 r2.y0 r2.y1 &&
   inter_ordered r1.x0 r1.x1 r2.x0 r2.x1
  let succ r = {r with z0 = r.z0 + 1 ; z1 = r.z1 + 1}
  let pred r = {r with z0 = r.z0 - 1 ; z1 = r.z1 - 1}
  let droppable r rset = r.z0 > 1 && not (Seq.exists (intersecting (pred r)) rset)
  let rec  drop r rset = if droppable r rset then drop (pred r) rset else r
  let drop_opt  r rset = if droppable r rset then return @@ drop (pred r) rset else None
  let to_string r = Printf.sprintf "%u,%u,%u~%u,%u,%u" r.x0 r.y0 r.z0 r.x1 r.y1 r.z1
  let of_string s =
   (* validate order when parsing *)
   let (z0,z1,y0,y1,x0,x1) = Scanf.sscanf s "%u,%u,%u~%u,%u,%u" (fun x0 y0 z0 x1 y1 z1 -> (min z0 z1, max z0 z1, min y0 y1, max y0 y1, min x0 x1, max x0 x1)) in
   {z0;z1;y0;y1;x0;x1}
 end in
 let seq_of_array_excluding i arr = Seq.zip (Seq.ints 0) (Array.to_seq arr) |> Seq.filter_map (fun (idx,x) -> if i = idx then None else Some x) in
 let drop_blocks blocks =
  (* iterate until all blocks are static *)
  let stack_changed = ref true in
  let change_count = ref 0 in
  while !stack_changed = true do
   stack_changed := false;
   Array.iteri (fun i r ->
    unwrap @@
    let rset = seq_of_array_excluding i blocks in
    let* r' = Range3D.drop_opt r rset in
    stack_changed := true;
    incr change_count;
    return @@ blocks.(i) <- r'
   ) blocks
  done;
  !change_count
 in
 let input = In_channel.(with_open_bin "22.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Seq.map Range3D.of_string |> Array.of_seq)) in
 Array.stable_sort compare input;
 let shifted = drop_blocks input in
 Array.stable_sort compare input;
 if debug then Array.iter (fun r -> r |> Range3D.to_string |> print_endline) input;
 Printf.printf "%d blocks shifted!\n" shifted;
 let result = ref 0 in
 Array.iteri (fun i r -> result := !result + drop_blocks (seq_of_array_excluding i input |> Array.of_seq)) input ;
 !result

(* it does not look like it is possible to loop around, but I'm not 100% sure *)
(* if there are no neighbors, check if at end before adding result to set *)
let problem_23a () =
 let module PState = struct
  type t = {y: int; x: int; dy : int; dx : int; count : int}
  let first_state = {y = 1; x = 1; dy = 1; dx = 0; count = 1} 
  let to_string s = Printf.sprintf "(%d).[%d] |%d| <%d> :%d" s.y s.x s.dy s.dx s.count
  let on_point (y,x) s = s.y = y && s.x = x
  let neighbors map s =
   (* this goofs if you try to calculate neightbors w/o any motion *)
   assert (s.dx <> 0 || s.dy <> 0);
   (match map.(s.y).[s.x] with
   (* keep moving *)
   | '<' | '>' | 'v' | '^' -> [{s with y=s.y+s.dy; x=s.x+s.dx; count=s.count+1}]
   (* keep moving or turn *)
   | '.' ->
    {s with y=s.y+s.dy; x=s.x+s.dx; count=s.count+1} ::
    (* turn types *)
    if s.dx = 0 then [{s with dx= ~-1; dy=0; x=s.x-1; count = s.count+1};{s with dx=1; dy=0; x=s.x+1; count = s.count+1}]
    else [{s with dy= ~-1; dx=0; y=s.y-1; count = s.count+1};{s with dy=1; dx=0; y=s.y+1; count = s.count+1}]
   | _ -> assert false)
   |>
   List.filter (fun s ->
    s.y >= 0 && s.y < (Array.length map) && s.x >= 0 && s.x < (String.length map.(0)) &&
    (match map.(s.y).[s.x] with '.' -> true | '#' -> false | '>' -> s.dx = 1 | '<' -> s.dx = ~-1 | 'v' -> s.dy = 1 | '^' -> s.dy = ~-1 | _ -> false))
  let next_fork map s =
   let rec loop s =
    match neighbors map s with
    (* return singleton on dead end or dest *)
    | [] -> [s] 
    | [next] -> loop next
    | fork -> fork in loop s
 end in
 let map = In_channel.(with_open_bin "23.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length map and w = String.length map.(0) in
 let dest = (h-1,w-2) in
 let first_fork = PState.next_fork map PState.first_state in
 let res = Queue.create () in
 let pathq = Queue.of_seq (List.to_seq first_fork) in
 while not @@ Queue.is_empty pathq do
  match PState.next_fork map (Queue.take pathq) with
  | [single] -> if PState.on_point dest single then Queue.push single res
  | multi -> Queue.add_seq pathq (List.to_seq multi)
 done;
 (*Queue.to_seq res |> Seq.iter (fun s -> s |> PState.to_string |> print_endline)*)
 Queue.to_seq res |> Seq.fold_left PState.(fun a s -> if a.count > s.count then a else s) PState.first_state |> PState.to_string |> print_endline

(* this version has loops! so track set of already visited points - maybe only at forks *)
(* too slow to solve beyond the example input *)
let problem_23b () =
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module UPState = struct
  type t = {y: int; x: int; dy : int; dx : int; count : int; mset : YXSet.t}
  let first_state = {y = 1; x = 1; dy = 1; dx = 0; count = 1; mset = YXSet.empty} 
  let to_string s = Printf.sprintf "(%d).[%d] |%d| <%d> :%d" s.y s.x s.dy s.dx s.count
  let on_point (y,x) s = s.y = y && s.x = x
  (* register previous point of neighbor to no go set*)
  let register_point s = {s with mset = YXSet.add (s.y-s.dy,s.x-s.dx) s.mset}
  let neighbors map s =
   (* this goofs if you try to calculate neightbors w/o any motion *)
   assert (s.dx <> 0 || s.dy <> 0);
   (match map.(s.y).[s.x] with
   (* keep moving or turn *)
   | '<' | '>' | 'v' | '^' | '.' ->
    {s with y=s.y+s.dy; x=s.x+s.dx; count=s.count+1} ::
    (* turn types *)
    if s.dx = 0 then [{s with dx= ~-1; dy=0; x=s.x-1; count = s.count+1};{s with dx=1; dy=0; x=s.x+1; count = s.count+1}]
    else [{s with dy= ~-1; dx=0; y=s.y-1; count = s.count+1};{s with dy=1; dx=0; y=s.y+1; count = s.count+1}]
   | _ -> assert false)
   |>
   List.filter (fun s ->
    s.y >= 0 && s.y < (Array.length map) && s.x >= 0 && s.x < (String.length map.(0)) && (not @@ YXSet.mem (s.y,s.x) s.mset) &&
    (match map.(s.y).[s.x] with '.' | '>' | '<' | '^' | 'v' -> true | '#' -> false | _ -> false)) |> List.map (register_point)
  let next_fork map s =
   let rec loop s =
    match neighbors map s with
    (* return singleton on dead end or dest *)
    | [] -> [s] 
    | [next] ->loop next
    | fork -> List.map (register_point) fork in loop s
 end in
 let map = In_channel.(with_open_bin "23e.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length map and w = String.length map.(0) in
 let dest = (h-1,w-2) in
 let first_fork = UPState.next_fork map UPState.first_state in
 let res = Queue.create () in
 let pathq = Queue.of_seq (List.to_seq first_fork) in
 while not @@ Queue.is_empty pathq do
  match UPState.next_fork map (Queue.take pathq) with
  | [single] -> if UPState.on_point dest single then Queue.push single res
  | multi -> Queue.add_seq pathq (List.to_seq multi)
 done;
 (*Queue.to_seq res |> Seq.iter (fun s -> s |> UPState.to_string |> print_endline)*)
 (*Queue.to_seq res |> Seq.fold_left UPState.(fun a s -> if a.count > s.count then a else s) UPState.first_state |> UPState.to_string |> print_endline*)
 Queue.to_seq res |> Seq.fold_left UPState.(fun a s -> if a.count > s.count then a else s) UPState.first_state |> UPState.(fun s -> s.count)

(* use different strategy *)
(* create graph of junctions from map *)
(* possible speed up with global visited list/set instead of visited-per search using dfs via a stack *)
let problem_23b_opt () =
 let debug = false in
 let ( let* ) = Option.bind in
 let return = Option.some in
 let unwrap = Option.iter Fun.id in
 let module YXSet = Set.Make(struct type t = int * int let compare = compare end) in
 let module UPState = struct
  type t = {y: int; x: int; dy : int; dx : int; count : int}
  let first_state = {y = 1; x = 1; dy = 1; dx = 0; count = 1} 
  let to_string s = Printf.sprintf "(%d).[%d] |%d| <%d> :%d" s.y s.x s.dy s.dx s.count
  let on_point (y,x) s = s.y = y && s.x = x
  let neighbors map s =
   (* this goofs if you try to calculate neightbors w/o any motion *)
   assert (s.dx <> 0 || s.dy <> 0);
   (match map.(s.y).[s.x] with
   (* keep moving or turn *)
   | '#' -> []
   | '<' | '>' | 'v' | '^' | '.' ->
    {s with y=s.y+s.dy; x=s.x+s.dx; count=s.count+1} ::
    (* turn types *)
    if s.dx = 0 then [{s with dx= ~-1; dy=0; x=s.x-1; count = s.count+1};{s with dx=1; dy=0; x=s.x+1; count = s.count+1}]
    else [{s with dy= ~-1; dx=0; y=s.y-1; count = s.count+1};{s with dy=1; dx=0; y=s.y+1; count = s.count+1}]
   | _ -> assert false)
   |>
   List.filter (fun s ->
    s.y >= 0 && s.y < (Array.length map) && s.x >= 0 && s.x < (String.length map.(0)) &&
    (match map.(s.y).[s.x] with '#' -> false | _ -> true ))
  let next_junction map jset s =
   let rec loop s =
    match neighbors map s with
    | [] -> None
    | [next] -> if YXSet.mem (next.y ,next.x) jset then Some next else loop next
    | fork -> assert false (* jset is missing a fork; you done messed up! *) in loop s
 end in
 let map = In_channel.(with_open_bin "23.txt" (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |> Array.of_seq)) in
 let h = Array.length map and w = String.length map.(0) in
 let start = (0,1) in
 let dest = (h-1,w-2) in
 let is_junction (y,x) =
  map.(y).[x] <> '#' &&
   ([map.(y).[x-1]; map.(y).[x+1]; map.(y-1).[x]; map.(y+1).[x]] |>
    List.fold_left (fun a c -> if c <> '#' then a + 1 else a) 0  |> ((<) 2)) in
 (*junction points*)
 let jset =
  let acc = ref [] in
  for y = 1 to h - 2 do for x = 1 to w - 2 do
   if is_junction (y,x) then acc := (y,x) :: !acc
  done done; start :: dest :: !acc |> List.to_seq |> YXSet.of_seq in
 (*graph*)
 let graph_tbl = YXSet.to_seq jset |>
  Seq.map (fun (y,x) ->
   let walk_n = UPState.{y=y-1; x; dy = ~-1; dx=0; count=1}
   and walk_s = UPState.{y=y+1; x; dy = ~+1; dx=0; count=1}
   and walk_w = UPState.{x=x-1; y; dx = ~-1; dy=0; count=1}
   and walk_e = UPState.{x=x+1; y; dx = ~+1; dy=0; count=1} in
   [walk_n;walk_s;walk_w;walk_e] |>
   (* this bounds check handles the edge case of starting and ending nodes *)
   List.filter UPState.(fun s -> s.x >= 0 && s.x < w && s.y >= 0 && s.y < h) |>
   List.filter_map (UPState.next_junction map jset)
  ) |> Seq.zip (YXSet.to_seq jset) |> Hashtbl.of_seq in
 let steps_at_dest = ref 0 in
 let djk_q = Queue.create () in
 Queue.push (start,0,[start]) djk_q ;
 while not @@ Queue.is_empty djk_q do
  unwrap @@
   let (point,steps,visited) = Queue.take djk_q in
   if point = dest then steps_at_dest := max !steps_at_dest steps ;
   let* nbs = Hashtbl.find_opt graph_tbl point in
   let nbs = List.filter UPState.(fun s -> Hashtbl.mem graph_tbl (s.y,s.x)) nbs in
   let nbs = List.filter UPState.(fun s -> not @@ List.mem (s.y,s.x) visited) nbs in
   List.iter UPState.(fun s -> Queue.add ((s.y,s.x),s.count+steps,(s.y,s.x)::visited) djk_q) nbs;
   return ()
 done;
 if debug then 
  Hashtbl.to_seq graph_tbl |> Seq.take 20 |> Seq.iter (fun ((y,x),nbs) -> Printf.printf "(%u,%u):\n" y x; List.iter (fun s -> s |> UPState.to_string |> print_endline) nbs);
(* print_int !steps_at_dest; print_newline ()*)
 !steps_at_dest

let problem_24a () =
 let ( let* ) = Option.bind in
 let unwrap = Option.iter Fun.id in
 let return = Option.some in
 let module HailXY = struct
  type t = {x : float; y: float; vx : float; vy : float; a : float; b : float; c : float}
  let of_string s =
   let (x,y,vx,vy) = Scanf.sscanf s " %d, %d, %d @ %d, %d, %d " (fun a b _ c d _ -> Float.(of_int a, of_int b, of_int c, of_int d)) in
   let a = vy and b = ~-. vx and c = x *. vy -. y *. vx in
   {x;y;vx;vy;a;b;c}
  let to_string h = Printf.sprintf "%f, %f, @ %f, %f" h.x h.y h.vx h.vy
  let intersection h1 h2 =
   if h1.a *. h2.b = h1.b *. h2.a then None else
   let x = (h1.c *. h2.b -. h1.b *. h2.c) /. (h1.a *. h2.b -. h1.b *. h2.a) in
   let y = (h1.a *. h2.c -. h1.c *. h2.a) /. (h1.a *. h2.b -. h1.b *. h2.a) in
   Some (x,y)
 end in
 let example = false in
 let min_bound = if example then 7.  else 200000000000000.
 and max_bound = if example then 27. else 400000000000000. in
 let input =
  In_channel.(with_open_bin (if example then "24e.txt" else "24.txt")
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.map HailXY.of_string |>
   Array.of_seq)) in
 let collisions = ref 0 in
 for i = 0 to Array.length input - 2 do 
  for j = i + 1 to Array.length input - 1 do
   unwrap @@
   let* (x,y) = HailXY.intersection input.(i) input.(j) in
   if x >= min_bound && x <= max_bound && y >= min_bound && y <= max_bound &&
   ((x -. input.(i).x) *. input.(i).vx >= 0.) &&
   ((x -. input.(j).x) *. input.(j).vx >= 0.)
   then incr collisions ; return ()
 done done;
 !collisions

(* requires you to solve a 6x6 system of non-linear equations *)
(* equation structure *)
(*
 (x0-r.x0)(r.vy-vy) - (y0-r.y0)(r.vx-vx) = 0
 (x0-r.x0)(r.vz-vz) - (z0-r.z0)(r.vx-vx) = 0

 This system of equations is BILINEAR (not simply non-linear):
 y^TA[i]x = g[i]

 t may be eliminated by using a cross product on both sides of the equation (or by substitution):
 t[i]*(rv-hv) = (hp-rp)
 this works out because t is a scalar, thus, for t*(rv-hv) and (hp-rp) to be equal, (rv-hv) and (hp-rp) must be PARALLEL
 bilinear terms may be eliminated with equations from other hailstones

 lapack solution commented out because it is not part of the stdlib.
*)
let problem_24b () =
 let module HailXYZ = struct
  type t = {x : float; y: float; z : float; vx : float; vy : float; vz: float}
  let of_string s =
   let (x,y,z,vx,vy,vz) = Scanf.sscanf s " %d, %d, %d @ %d, %d, %d " (fun a b c d e f -> Float.(of_int a, of_int b, of_int c, of_int d, of_int e, of_int f)) in
   {x;y;z;vx;vy;vz}
  let to_string h = Printf.sprintf "%f, %f, %f @ %f, %f %f" h.x h.y h.z h.vx h.vy h.vz
  (* for bruteforcing the example *)
  let intersecting_line_at_t h1 h2 t1 t2 =
   let x = h1.x +. h1.vx *. t1 
   and y = h1.y +. h1.vy *. t1 
   and z = h1.z +. h1.vz *. t1 in
   let vx = h2.x +. h2.vx *. t2 -. x
   and vy = h2.y +. h2.vy *. t2 -. y
   and vz = h2.z +. h2.vz *. t2 -. z in
   {x;y;z;vx;vy;vz}
  let hail_mary_matrix h1 h2 h3 =
   let lhs =
   [|[| h1.vy -. h2.vy; h2.vx -. h1.vx; 0.; h2.y -. h1.y; h1.x -. h2.x; 0. |];
     [| h1.vy -. h3.vy; h3.vx -. h1.vx; 0.; h3.y -. h1.y; h1.x -. h3.x; 0. |];
     [| 0.; h1.vz -. h2.vz; h2.vy -. h1.vy; 0.; h2.z -. h1.z; h1.y -. h2.y |];
     [| 0.; h1.vz -. h3.vz; h3.vy -. h1.vy; 0.; h3.z -. h1.z; h1.y -. h3.y |];
     [| h1.vz -. h2.vz; 0.; h2.vx -. h1.vx; h2.z -. h1.z; 0.; h1.x -. h2.x |];
     [| h1.vz -. h3.vz; 0.; h3.vx -. h1.vx; h3.z -. h1.z; 0.; h1.x -. h3.x |]|] in
   let rhs =
     [| (h1.x *. h1.vy -. h2.x *. h2.vy) -. (h1.y *. h1.vx -. h2.y *. h2.vx)
      ; (h1.x *. h1.vy -. h3.x *. h3.vy) -. (h1.y *. h1.vx -. h3.y *. h3.vx)
      ; (h1.y *. h1.vz -. h2.y *. h2.vz) -. (h1.z *. h1.vy -. h2.z *. h2.vy)
      ; (h1.y *. h1.vz -. h3.y *. h3.vz) -. (h1.z *. h1.vy -. h3.z *. h3.vy)
      ; (h1.x *. h1.vz -. h2.x *. h2.vz) -. (h1.z *. h1.vx -. h2.z *. h2.vx)
      ; (h1.x *. h1.vz -. h3.x *. h3.vz) -. (h1.z *. h1.vx -. h3.z *. h3.vx) |] in
    (lhs, rhs)
  let intersection h1 h2 =
   (* system of equations : solve for t and s ; if t=s then we have a full solution; if t<>s we have to readjust values *)
   (* use to verify solutions / partial solutions when bruteforcing example *)
   (* [t s]
      [h1.dx, -h2.dx] = [h2.x - h1.x]
      [h1.dy, -h2.dy] = [h2.y - h1.y]
      [h1.dz, -h2.dz] = [h2.z - h1.z] *)
   (* choose an s computation that does not result in divide by zero ! *)
   let s1 () = (h1.vy *. (h2.x -. h1.x) -. h1.vx *. (h2.y -. h1.y)) /. (h1.vx *. h2.vy -. h2.vx *. h1.vy) in
   let s2 () = (h1.vz *. (h2.x -. h1.x) -. h1.vx *. (h2.z -. h1.z)) /. (h1.vx *. h2.vz -. h2.vx *. h1.vz) in
   let s3 () = (h1.vy *. (h2.z -. h1.z) -. h1.vz *. (h2.y -. h1.y)) /. (h1.vz *. h2.vy -. h2.vz *. h1.vy) in
   let s = if (h1.vx *. h2.vy -. h2.vx *. h1.vy) <> 0. then s1 () else
           if (h1.vx *. h2.vz -. h2.vx *. h1.vz) <> 0. then s2 () else
           if (h1.vz *. h2.vy -. h2.vz *. h1.vy) <> 0. then s3 () else
           Float.nan in
   let t = ((h2.x -. h1.x) +. (s *. h2.vx)) /. h1.vx in
   if Float.is_nan s then None else
   if ((h1.vx *. t -. h2.vx *. s -. (h2.x -. h1.x) |> Float.abs) < Float.epsilon) && 
      ((h1.vy *. t -. h2.vy *. s -. (h2.y -. h1.y) |> Float.abs) < Float.epsilon) &&
      ((h1.vz *. t -. h2.vz *. s -. (h2.z -. h1.z) |> Float.abs) < Float.epsilon) then Some (t,s)
   else None
 end in
(*
 let convert_mat a = Bigarray.(Array2.init Float64 Fortran_layout (Array.length a) (Array.length a) (fun y x -> a.(y-1).(x-1))) in
 let convert_vec b = Bigarray.(Array2.init Float64 Fortran_layout (Array.length b) 1 (fun y x -> b.(y-1))) in
 let solve a b =
  let open Lacaml.D in
  gesv a b in
*)
 let example = false in
 let input =
  In_channel.(with_open_bin (if example then "24e.txt" else "24.txt")
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.map HailXYZ.of_string |>
   Array.of_seq)) in
(*
  let (a,b) = HailXYZ.hail_mary_matrix input.(0) input.(1) input.(2) in
  let (a_mat, b_vec) = (convert_mat a, convert_vec b) in
  solve a_mat b_vec;
  for i = 1 to Bigarray.Array2.dim1 b_vec |> pred do Printf.printf "%20.0f" b_vec.{i,1} ; print_newline () done;
  Printf.printf "%20.0f\n" @@ b_vec.{1,1} +. b_vec.{2,1} +. b_vec.{3,1}
*)
 (*HailXYZ.intersection HailXYZ.({x = 270392223533307. ; y = 463714142194110. ; z = 273041846062208. ; vx = 26. ; vy= ~-. 331. ; vz = 53.}) input.(1)*)
 270392223533307 + 463714142194110 + 273041846062208

(* requires lacaml / lapack *)
(* use spectral graph analysis to determine best cut! *)
(*
let problem_25a () =
 let example = false
 and idxer = Array.make (26 * 26 * 26) ~-1
 and mat_size = ref 0 in
 let set_idxer s = idxer.(26 * 26 * (Char.code s.[0] - 97) + 26 * (Char.code s.[1] - 97) + (Char.code s.[2] - 97)) <- 0 in
 let get_idx s = idxer.(26 * 26 * (Char.code s.[0] - 97) + 26 * (Char.code s.[1] - 97) + (Char.code s.[2] - 97)) in
 let finalize_idxer () =
  for i = 0 to 26 * 26 * 26 - 1 do
   if idxer.(i) = 0 then (incr mat_size ; idxer.(i) <- !mat_size)
  done in

 let input = In_channel.(with_open_bin (if example then "25e.txt" else "25.txt") input_lines) in
 List.iter (fun s -> s |> String.split_on_char ' ' |> List.iter set_idxer) input ;
 finalize_idxer () ;

 let adj_mat = (* laplacian/adjacency matrix *)
  Bigarray.(Array2.init Float64 Fortran_layout (!mat_size) (!mat_size) (fun _ _ -> Float.zero)) in

 List.iter (fun s ->
  let nodes = s |> String.split_on_char ' ' |> List.map get_idx in
  let a = List.hd nodes and bs = List.tl nodes in
  List.iter (fun b -> adj_mat.{a,b} <- Float.minus_one ; adj_mat.{b,a} <- Float.minus_one) bs
 ) input ;

 let reset_degree () = for i = 1 to !mat_size do adj_mat.{i,i} <- Float.zero done in
 let calculate_degree () =
  let deg = ref Float.zero in
  for i = 1 to !mat_size do
   deg := Float.zero ;
   for j = 1 to !mat_size do
    if adj_mat.{i,j} <> Float.zero then deg := Float.add Float.one !deg
   done ;
   adj_mat.{i,i} <- !deg
  done in calculate_degree ();
 let restore_adj () =
  reset_degree ();
  for i = 1 to !mat_size - 1 do
   for j = i+1 to !mat_size do
    adj_mat.{i,j} <- adj_mat.{j,i}
  done done;
  calculate_degree () in

 let best_cut z =
  let search_neg = ref (0, ~-. Float.max_float)
  and search_pos = ref (0, Float.max_float) in
  (* find lowest abs +/- *)
  for i = 1 to !mat_size do
   if Float.sign_bit z.{i,2} then (if Float.compare z.{i,2} (snd !search_neg) > 0 then search_neg := (i,z.{i,2}) else ())
   else (if Float.compare z.{i,2} (snd !search_pos) < 0 then search_pos := (i,z.{i,2}) else ())
  done ;
  let lowest = if Float.compare (Float.abs (snd !search_neg)) (snd !search_pos) < 0 then !search_neg else !search_pos in
  Printf.printf "using border node: %i\n" (fst lowest) ;
  (* be careful, the upper triangle and diagonal of adj_mat is destroyed during syevr! so use lower only! *)
  let neighbors = Bigarray.(Array1.init Float64 Fortran_layout (!mat_size) (fun i -> if adj_mat.{max (fst lowest) i, min (fst lowest) i} = Float.zero then Float.zero else z.{i,2})) in
  (* for i = 1 to !mat_size do print_float neighbors.{i} ; print_newline () done; *)
  (* highest abs flipped sign of lowest *)
  let best_neighbor =
   if Float.sign_bit (snd lowest) then (
    search_pos := (0, ~-. Float.max_float) ;
    for i = 1 to !mat_size do if Float.compare neighbors.{i} (snd !search_pos) > 0 then search_pos := (i,neighbors.{i}) else () done ;
    !search_pos
   ) else (
    search_neg := (0, Float.max_float) ;
    for i = 1 to !mat_size do if Float.compare neighbors.{i} (snd !search_neg) < 0 then search_neg := (i,neighbors.{i}) else () done ;
    !search_neg
   ) in
  Printf.printf "making cut @ %i-%i :: (%f,%f)\n" (fst lowest) (fst best_neighbor) (snd lowest) (snd best_neighbor);
  (* make cut at lower triangle anticipating a call to restore_adj *)
  adj_mat.{max (fst lowest) (fst best_neighbor), min (fst lowest) (fst best_neighbor)} <- Float.zero
 in

 (* use syevr/syevx w / fiedler eigenvectors + iterate 3 times *)
 let (m, w, z, isuppz) = Lacaml.D.(syevr ~n:(!mat_size) ~vectors:(true) ~range:(`I (1,2)) adj_mat) in
 best_cut z ; restore_adj ();
 let (m, w, z, isuppz) = Lacaml.D.(syevr ~n:(!mat_size) ~vectors:(true) ~range:(`I (1,2)) adj_mat) in
 best_cut z ; restore_adj ();
 let (m, w, z, isuppz) = Lacaml.D.(syevr ~n:(!mat_size) ~vectors:(true) ~range:(`I (1,2)) adj_mat) in
 best_cut z ; restore_adj ();

 (* count group sizes using eigenvectors where λ = 0 ; since there are only two groups, you only need the first (zero v. non-zero) *)
 let (m, w, z, isuppz) = Lacaml.D.(syevr ~n:(!mat_size) ~vectors:(true) ~range:(`I (1,2)) adj_mat) in
 let abstol = 1e-5 in
 let counter = ref 0 in
 for j = 1 to !mat_size do if Float.abs (Float.sub z.{j,1} z.{1,1}) < abstol then incr counter done ;
 Printf.printf "sizes : %i,%i\nans: %i\n" !counter (!mat_size - !counter) ((!mat_size - !counter) * !counter)
*)

let problem_25a_dummy () = 745 * 782
