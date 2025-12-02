(* aoc_extra.ml *)
(* attempts at optimizing previous problems *)

let problem_05a () =
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

let problem_05b () =
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
 (* almanac_opt *)
 let almanac_opt = List.fold_left (get_new_layer) (List.hd almanac) (List.tl almanac) in
 (* minimum can only exist on lower bounds of inflection points! *)
 let candidates_type1 = List.map (fun (start,len) -> map_value_with_book start almanac_opt) seed_ranges in
 let candidates_type2 = List.filter_map (fun (dst,src,len) -> if is_valid_seed src seed_ranges then Some dst else None) almanac_opt in
 let min_type1 = List.fold_left (min) Int.max_int candidates_type1 in 
 let min_type2 = List.fold_left (min) Int.max_int candidates_type2 in 
 min min_type1 min_type2

let () =
 print_string "Problem 05b: " ;
 print_int @@ problem_05b () ;
 print_newline ()
