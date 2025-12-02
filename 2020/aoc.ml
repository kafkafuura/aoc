(* aoc.ml *)
(* 2020 Advent of Code *)

let problem_01a () =
 let sum_match = 2020 in
 let input = In_channel.(with_open_bin "01.txt" input_lines) |> List.map (int_of_string) in
 let rec loop = function
 | [], _ | _::[], _ -> raise_notrace (Invalid_argument "Match Not Found")
 | h::t, [] -> loop (t, List.tl t)
 | h1::_,h2::_ when h1 + h2 = sum_match -> h1 * h2
 | l1,l2 -> loop (l1, List.tl l2) in loop (input, List.tl input)

let problem_01b () =
 let sum_match = 2020 in
 let input = In_channel.(with_open_bin "01.txt" input_lines) |> List.map (int_of_string) |> Array.of_list in
 let len = Array.length input in
 let rec loop i j k =
  if i > len - 3 then raise_notrace (Invalid_argument "Match Not Found") else
  if j > len - 2 then loop (i+1) (i+2) (i+3) else
  if k > len - 1 then loop i (j+1) (j+2) else
  if input.(i) + input.(j) + input.(k) = sum_match then input.(i) * input.(j) * input.(k) else
  loop i j (k+1) in loop 0 1 2

(* low-high c: pass *)
let problem_02a () =
 let parse s =
  let sep1 = String.index_from s 0 '-' in
  let sep2 = String.index_from s sep1 ' ' in
  let sep3 = String.index_from s sep2 ':' in
  (int_of_string (String.sub s 0 sep1),
   int_of_string (String.sub s (sep1+1) (sep2-sep1-1)),
   s.[sep3-1],
   String.sub s (sep3+2) (String.length s - sep3 - 2)) in
 let valid (min',max',c',s) =
  let count = String.fold_left (fun a c -> if c=c' then a+1 else a) 0 s in
  count >= min' && count <= max' in
 let input = In_channel.(with_open_bin "02.txt" input_lines) |> List.map parse in
 input |> List.filter valid |> List.length
 
let problem_02b () =
 let parse s =
  let sep1 = String.index_from s 0 '-' in
  let sep2 = String.index_from s sep1 ' ' in
  let sep3 = String.index_from s sep2 ':' in
  (int_of_string (String.sub s 0 sep1),
   int_of_string (String.sub s (sep1+1) (sep2-sep1-1)),
   s.[sep3-1],
   String.sub s (sep3+2) (String.length s - sep3 - 2)) in
 let valid (min',max',c',s) =
  try ((s.[min'-1] = c') <> (s.[max'-1] = c')) with _ -> false in
 let input = In_channel.(with_open_bin "02.txt" input_lines) |> List.map parse in
 input |> List.filter valid |> List.length
 
let problem_03a () =
 let map = In_channel.(with_open_bin "03.txt" input_lines) |> Array.of_list in
 let w = String.length map.(0)
 and h = Array.length map in
 let dx = 3 in
 (*let dy = 1 in*)
 let count = ref 0 in
 let x = ref 0 in
 for i = 0 to h - 1 do
  if map.(i).[!x] = '#' then incr count ;
  x := (!x + dx) mod w
 done ; !count
 
let problem_03b () =
 let map = In_channel.(with_open_bin "03.txt" input_lines) |> Array.of_list in
 let w = String.length map.(0)
 and h = Array.length map in
 let count_trees (dx,dy) =
  let count = ref 0 in
  let x = ref 0
  and y = ref 0 in
  while !y < h do
   if map.(!y).[!x] = '#' then incr count ;
   x := (!x + dx) mod w ;
   y := (!y + dy)
  done ; !count
 in List.map count_trees [1,1;3,1;5,1;7,1;1,2] |> List.fold_left ( * ) 1

(* passport validation *)
(* byr, iyr, eyr, hgt, hcl, ecl, pid, cid *)
(* fields separated by spaces **or** newlines *)
let problem_04a () =
 let module SSet = Set.Make(String) in
 let required_fields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl" ; "pid"] |> SSet.of_list in
 let slice s i j = String.sub s i (j-i) in
 let indexes s c' = String.fold_left (fun (i,a) c -> i+1, if c = c' then i::a else a) (0,[]) s |> snd |> List.rev in
 let fields_of_record s =
  indexes s ':' |> List.map (fun i -> slice s (i-3) i) |> SSet.of_list in
 let valid fields = SSet.subset required_fields fields in

 let input = In_channel.(with_open_bin "04.txt" input_all) in
 let breaks =
  Seq.fold_left2
   (fun (i,a) x y -> i+1, if x = '\n' && y = '\n' then (i+2)::i::a else a)
   (0, [0])
   (String.to_seq input)
   (String.to_seq input |> Seq.drop 1) |>
  (fun (_,lst) -> String.length input - 1 :: lst) in
 let rec split acc = function
 | h1::h2::t -> split (slice input h2 h1 :: acc) t
 | _ -> acc in
 let raw_records = split [] breaks in
 List.fold_left (fun a r -> if valid (fields_of_record r) then a+1 else a) 0 raw_records

let problem_04b () =
 let module SSet = Set.Make(String) in
 let required_fields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl" ; "pid"] |> SSet.of_list in
 let slice s i j = String.sub s i (j-i) in
 let indexes s c' = String.fold_left (fun (i,a) c -> i+1, if c = c' then i::a else a) (0,[]) s |> snd |> List.rev in
 let fields_of_record s =
  indexes s ':' |> List.map (fun i -> slice s (i-3) i) |> SSet.of_list in
 let fields_and_values_of_record s =
  let idxs = indexes s ':' in
  let idxs2 = List.append (List.map (fun n -> n - 4) idxs |> List.tl)  [String.length s] in
  List.map2 (fun i j -> slice s (i-3) i, slice s (i+1) j) idxs idxs2 in
 let valid fields = SSet.subset required_fields fields in
 let validate (field,value) =
  match field with
  | "byr" -> (try let n = int_of_string value in n >= 1920 && n <= 2002 with _ -> false)
  | "iyr" -> (try let n = int_of_string value in n >= 2010 && n <= 2020 with _ -> false)
  | "eyr" -> (try let n = int_of_string value in n >= 2020 && n <= 2030 with _ -> false)
  | "hgt" when value.[String.length value - 1] = 'm' && value.[String.length value - 2] = 'c' ->
          (try let n = int_of_string (String.sub value 0 (String.length value - 2)) in n >= 150 && n <= 193 with _ -> false)
  | "hgt" when value.[String.length value - 1] = 'n' && value.[String.length value - 2] = 'i' -> 
          (try let n = int_of_string (String.sub value 0 (String.length value - 2)) in n >= 59 && n <= 76 with _ -> false)
  | "hgt" -> false
  | "hcl" -> String.length value = 7 && value.[0] = '#' && String.for_all (function '0'..'9' -> true | 'a'..'f' -> true | _ -> false) (String.sub value 1 6)
  | "ecl" -> List.mem value ["amb";"blu";"brn";"gry";"grn";"hzl";"oth"]
  | "pid" -> String.length value = 9 && String.for_all (function '0'..'9' -> true | _ -> false) value
  | _ -> true in

 let input = In_channel.(with_open_bin "04.txt" input_all) in
 let breaks =
  Seq.fold_left2
   (fun (i,a) x y -> i+1, if x = '\n' && y = '\n' then (i+2)::i::a else a)
   (0, [0])
   (String.to_seq input)
   (String.to_seq input |> Seq.drop 1) |>
  (fun (_,lst) -> String.length input - 1 :: lst) in
 let rec split acc = function
 | h1::h2::t -> split (slice input h2 h1 :: acc) t
 | _ -> acc in
 let raw_records = split [] breaks in
 List.filter (fun r -> valid (fields_of_record r) && (List.for_all validate (fields_and_values_of_record r))) raw_records |>
 List.length

(* FBLR 0101 : 0-127, 0-7 10-bit (BE) Binary Partitioning *)
(* Unique ID : full number *)
let problem_05a () = 
 In_channel.(with_open_bin "05.txt" input_lines) |>
  List.map (String.map (function 'B' | 'R' -> '1' | _ -> '0')) |>
  List.map (fun s -> Scanf.sscanf ("0b" ^ s) "%i" (Fun.id)) |>
  List.fold_left (max) ~-1

let problem_05b () = 
 let read_FBLR_binary = String.fold_left (fun a c -> match c with 'B' | 'R' -> (a lsl 1) + 1 | _ -> a lsl 1) 0 in
 let input = In_channel.(with_open_bin "05.txt" input_lines) |>
  List.map (read_FBLR_binary) |> Array.of_list in
 Array.sort (compare) input ;
 let rec loop i =
  if i >= Array.length input - 1 then raise_notrace (Invalid_argument "ID Gap Not Found") else
  if input.(i+1) - input.(i) = 2 then input.(i) + 1 else loop (i+1)
 in loop 0

module BitSet : sig
 type t = int
 val empty : t
 val full: t
 val add : int -> t -> t
 val remove : int -> t -> t
 val cardinal : t -> int
 val singleton : int -> t
 val mem : int -> t -> bool
 val equal : t -> t -> bool
 val intersection: t -> t -> t
 val union: t -> t -> t
 val diff: t -> t -> t
end = struct
 type t = int
 let empty = 0
 let full = ~-1
 let singleton x = (1 lsl x)
 let add x s = (1 lsl x) lor s
 let remove x s = s land lnot (1 lsl x)
 let cardinal s =
  let rec fold a s =
   if s = 0 then a else
   fold (if s land 1 = 1 then succ a else a) (s lsr 1) in
  fold 0 s
 let mem x s = ((1 lsl x) land s) <> 0
 let equal = (=)
 let intersection = (land)
 let union = (lor)
 let diff s1 s2 = s1 land lnot s2
end

let problem_06a () =
 let slice s i j = String.sub s i (j-i) in
 let input = In_channel.(with_open_bin "06.txt" input_all) in
 let rec breaks i a =
  if i = String.length input - 1 then i::a else
  breaks (i+1) (if input.[i] = '\n' && input.[i+1] = '\n' then (i+2)::i::a else a) in
 let rec split acc = function
 | h1::h2::t -> split (slice input h2 h1 :: acc) t
 | _ -> acc in
 breaks 0 [0] |> split [] |>
 List.map (String.fold_left (fun a c -> match c with 'a'..'z' -> BitSet.add (Char.code c) a | _ -> a) BitSet.empty) |>
 List.map (BitSet.cardinal) |>
 List.fold_left (+) 0

let problem_06b () =
 In_channel.(with_open_bin "06.txt" input_lines) |>
 List.map (String.fold_left (fun a c -> match c with 'a'..'z' -> BitSet.add (Char.code c) a | _ -> a) BitSet.empty) |>
 List.fold_left (fun (cur,a) s -> if s = BitSet.empty then ([],cur::a) else (s::cur,a)) ([],[]) |>
 (fun (cur,a) -> List.rev (if List.is_empty cur then a else cur::a)) |>
 List.map (List.fold_left (BitSet.intersection) BitSet.full) |>
 List.map (BitSet.cardinal) |>
 List.fold_left ( + ) 0
 
(* [qualifier] [color] bags contain ["no other bags" | # qualifier color bag(s), ...*) 

let problem_07a () =
 let module SSet = Set.Make(String) in
 let module SMap = Map.Make(String) in
 let input = In_channel.(with_open_bin "07.txt" input_lines) |> List.map (String.split_on_char ' ') in
 let useful = input |> List.filter (fun ws -> List.nth ws 4 <> "no") in
 let lookup = 
  useful |>
  List.fold_left
   (fun acc ws ->
    let container = List.hd ws ^ " " ^ List.nth ws 1 in
    List.to_seq ws |> Seq.drop 4 |>
    Seq.fold_lefti (fun (last,a) i w ->
     if i mod 4 <> 2 then (w,a) else
     (w, SMap.update (last ^ " " ^ w) (fun v -> v |> Option.value ~default:SSet.empty |> SSet.add container |> Option.some) a)) ("", acc) |> snd) SMap.empty in
  let bagset =
   let rec loop acc =
    let acc' =
     SSet.fold (fun el a -> SSet.union (SMap.find_opt el lookup |> Option.value ~default:SSet.empty) a) acc acc in
    if SSet.cardinal acc' = SSet.cardinal acc then acc else loop acc' in
   loop (SSet.add "shiny gold" SSet.empty) in
  (*SSet.elements bagset*)
  SSet.cardinal bagset - 1

(* use breadth first search for this one! *)
let problem_07b () =
 (* no need for fancy data structures this time *)
 (*let module SSet = Set.Make(String) in*)
 (*let module SMap = Map.Make(String) in*)
 let input = In_channel.(with_open_bin "07.txt" input_lines) |> List.map (String.split_on_char ' ') in
 let useful = input |> List.filter (fun ws -> List.nth ws 4 <> "no") in
 (* assoc forward lookup *)
 let lookup = 
  useful |>
  List.map
   (fun ws ->
    let container = List.hd ws ^ " " ^ List.nth ws 1 in
    (container, 
     List.to_seq ws |> Seq.drop 4 |>
     Seq.fold_lefti (fun (prev,last,a) i w ->
      if i mod 4 <> 2 then (last,w,a) else
      (last, w, (int_of_string prev, last ^ " " ^ w)::a)) ("","", []) |>
     (fun (_,_,c) -> c))) in
  (* don't count the original gold bag, so start at -1 *)
  let res = ref ~-1 in 
  let q = Queue.create () in
  Queue.add (1, "shiny gold") q;
  while Queue.is_empty q |> not do
   let (multiplier, key) = Queue.take q in
   match List.assoc_opt key lookup with
   | None -> res := !res + multiplier
   | Some vs ->
     res := !res + multiplier ;
     (*List.iter (fun (n,k) -> Printf.printf "%d %d : %s\n" multiplier n k) vs ;*)
     Queue.add_seq q (List.map (fun (n,k) -> (n*multiplier,k)) vs |> List.to_seq)
  done ;
  !res
 
(* acc, jmp, nop *)
(* find the infinite loop *)
(* use IntSet to track visited instructions *)
let problem_08a () =
 let module IntSet = Set.Make(Int) in
 let parse s = (String.sub s 0 3, String.sub s 4 (String.length s - 4) |> int_of_string) in
 let prog = In_channel.(with_open_bin "08.txt" input_lines) |> List.map parse |> Array.of_list in
 let rec loop (visited, a) ip =
  if IntSet.mem ip visited then a else
  let instr, off = prog.(ip) in
  match instr with
  | "nop" -> loop (IntSet.add ip visited, a) (succ ip)
  | "acc" -> loop (IntSet.add ip visited, a+off) (succ ip)
  | "jmp" -> loop (IntSet.add ip visited, a) (ip + off)
  | _ -> raise_notrace (Invalid_argument "Invalid Instruction")
 in loop (IntSet.empty, 0) 0

(* find the corrupt instruction *)
let problem_08b () =
 let module IntSet = Set.Make(Int) in
 let parse s = (String.sub s 0 3, String.sub s 4 (String.length s - 4) |> int_of_string) in
 let prog = In_channel.(with_open_bin "08.txt" input_lines) |> List.map parse |> Array.of_list in
 let altered = ref ~-1 in
 let alter_program () =
  let rec loop () =
   incr altered ;
   if !altered >= Array.length prog then raise_notrace (Invalid_argument "No more instructions alterable!") else
   match prog.(!altered) |> fst with
   | "nop" | "jmp" -> ()
   | _ -> loop () in loop () in
 let rec loop (visited, a) ip =
  if IntSet.mem ip visited then (alter_program () ; loop (IntSet.empty, 0) 0) else
  if ip = Array.length prog then (print_endline "Successfully Terminated!" ; a) else
  let instr, off = prog.(ip) in
  match instr with
  | "acc" -> loop (IntSet.add ip visited, a+off) (succ ip)
  | "nop" when !altered <> ip -> loop (IntSet.add ip visited, a) (succ ip)
  | "jmp" when !altered = ip  -> loop (IntSet.add ip visited, a) (succ ip)
  | "jmp" | "nop" -> loop (IntSet.add ip visited, a) (ip + off)
  | _ -> raise_notrace (Invalid_argument "Invalid Instruction")
 in loop (IntSet.empty, 0) 0

(* use ring buffer to reuse values efficiently *)
let problem_09a () =
 let example = false in
 let debug = true in
 let preamble_len = if example then 5 else 25 in
 let dim = preamble_len - 1 in
 (* 2d (triangular) ring buffer *)
 let sumbuf = Array.make (dim * dim) 0 in
 let q = Queue.create () in
 let clear_row n = Array.fill sumbuf (dim * n) dim 0 in
 let fill_diag n x =
  Queue.take q |> ignore ;
  Seq.zip (Seq.ints 1) (Queue.to_seq q) |>
  Seq.iter (fun (i,y) ->
   let idx = (n+i)*dim + (dim-i) in
   sumbuf.(idx mod Array.length sumbuf) <- x + y) ;
  Queue.push x q in
 (* returns remainder of input *)
 let init input =
  let init_seq = Seq.take preamble_len input in
  Queue.add_seq q init_seq ;
  let init_arr = Array.of_seq init_seq in
  for i = 0 to preamble_len - 1 do
   for j = i+1 to preamble_len - 1 do
    sumbuf.(i*dim+j-(i+1)) <- init_arr.(i) + init_arr.(j)
  done done ;
  Seq.drop preamble_len input in
 let test n x =
  let res = Array.mem x sumbuf in
  clear_row (n mod dim) ;
  fill_diag (n mod dim) x ;
  res in
 let print_state () =
  for i = 0 to dim - 1 do
   for j = 0 to dim - 1 do
    print_int sumbuf.(i*dim+j) ; print_char ' '
   done ; print_newline () ; done ; print_endline "-----" in
 let input_list =
  In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_lines) |>
  List.map (int_of_string) in
 let input = List.to_seq input_list in
 input |> init |>
 Seq.zip (Seq.ints 0) |>
 Seq.fold_left
  (fun acc (i,el) ->
   if Option.is_some acc then acc else (
   if debug then print_state () ;
   if test i el then None else Some el)) None

let problem_09b () =
 let example = false in
 let debug = false in
 let preamble_len = if example then 5 else 25 in
 let dim = preamble_len - 1 in
 (* 2d (triangular) ring buffer *)
 let sumbuf = Array.make (dim * dim) 0 in
 let q = Queue.create () in
 let clear_row n = Array.fill sumbuf (dim * n) dim 0 in
 let fill_diag n x =
  Queue.take q |> ignore ;
  Seq.zip (Seq.ints 1) (Queue.to_seq q) |>
  Seq.iter (fun (i,y) ->
   let idx = (n+i)*dim + (dim-i) in
   sumbuf.(idx mod Array.length sumbuf) <- x + y) ;
  Queue.push x q in
 (* returns remainder of input *)
 let init input =
  let init_seq = Seq.take preamble_len input in
  Queue.add_seq q init_seq ;
  let init_arr = Array.of_seq init_seq in
  for i = 0 to preamble_len - 1 do
   for j = i+1 to preamble_len - 1 do
    sumbuf.(i*dim+j-(i+1)) <- init_arr.(i) + init_arr.(j)
  done done ;
  Seq.drop preamble_len input in
 let test n x =
  let res = Array.mem x sumbuf in
  clear_row (n mod dim) ;
  fill_diag (n mod dim) x ;
  res in
 let print_state () =
  for i = 0 to dim - 1 do
   for j = 0 to dim - 1 do
    print_int sumbuf.(i*dim+j) ; print_char ' '
   done ; print_newline () ; done ; print_endline "-----" in
 let input_list =
  In_channel.(with_open_bin (if example then "09e.txt" else "09.txt") input_lines) |>
  List.map (int_of_string) in
 let invalid_num =
  List.to_seq input_list |>
  init |>
  Seq.zip (Seq.ints 0) |>
  Seq.fold_left
   (fun acc (i,el) ->
    if Option.is_some acc then acc else (
    if debug then print_state () ;
    if test i el then None else Some el)) None |> Option.get in
 Queue.clear q ;
 input_list |>
 List.fold_left (fun a n ->
  if Option.is_some a then a else (
   while Queue.to_seq q |> Seq.fold_left ( + ) 0 > invalid_num do Queue.take q |> ignore done ;
   let sum = Queue.to_seq q |> Seq.fold_left ( + ) 0 in
   if sum = invalid_num then
    let min' = Queue.to_seq q |> Seq.fold_left (min) Int.max_int in
    let max' = Queue.to_seq q |> Seq.fold_left (max) ~-1 in
    Some (min',max') else
   if sum + n = invalid_num then (
    Queue.add n q ;
    let min' = Queue.to_seq q |> Seq.fold_left (min) Int.max_int in
    let max' = Queue.to_seq q |> Seq.fold_left (max) ~-1 in
    Some (min',max')
  ) else (Queue.add n q; None))) None |>
  Option.map (fun (a,b) -> a+b)

let problem_10a () =
 let input = In_channel.(with_open_bin "10.txt" input_lines) |> List.map (int_of_string) |> Array.of_list in
 Array.sort (compare) input ;
 let j1 = if input.(0) = 1 then 1 else 0 in
 let j3 = if input.(0) = 3 then 2 else 1 in
 Seq.zip (Array.to_seq input) (Array.to_seq input |> Seq.drop 1) |>
 Seq.fold_left
  (fun (j1,j3) (x,y) ->
   if y - x = 1 then (succ j1, j3) else
   if y - x = 3 then (j1, succ j3) else (j1,j3)) (j1,j3) |>
 (fun (a,b) -> a*b)

(* use dynamic programming to solve *)
let problem_10b () =
 let module IntSet = Set.Make(Int) in
 let input = In_channel.(with_open_bin "10.txt" input_lines) |> List.map (int_of_string) in
 let adapters = IntSet.of_list input in
 let final = IntSet.max_elt adapters + 3 in
 let adapters = IntSet.add final adapters in
 let dyn = Array.make (succ final) 0 in
 dyn.(0) <- 1 ;
 for i = 0 to final - 3 do
  dyn.(i+1) <- dyn.(i+1) + if IntSet.mem (i+1) adapters then dyn.(i) else 0 ;
  dyn.(i+2) <- dyn.(i+2) + if IntSet.mem (i+2) adapters then dyn.(i) else 0 ;
  dyn.(i+3) <- dyn.(i+3) + if IntSet.mem (i+3) adapters then dyn.(i) else 0 ;
 done ;
 (* dyn.(final-3) is also valid and does not require the masking of adapters *)
 (* then you can use "to final - 4", but as-is is more readable *)
 dyn.(final)

(* life simulation *)
let problem_11a () =
 let debug = false in
 let map = In_channel.(with_open_bin "11.txt" input_lines) |> Array.of_list in
 let w = String.length map.(0) in
 let h = Array.length map in
 let z = ref 0 in
 let bmaps = Array.init 2 (fun _ -> Bytes.create (w*h)) in
 (* local bmap operators *)
 let (.%[;..]<-) = (fun bs idx x -> Bytes.set bs (idx.(0) * w + idx.(1)) x) in
 let (.%[;..]) = (fun bs idx -> Bytes.get bs (idx.(0) * w + idx.(1))) in
 (* load map into bmaps.(0) *)
 for i = 0 to h - 1 do for j = 0 to w - 1 do bmaps.(0).%[i;j] <- map.(i).[j] done done;
 (* copy first to optimize out floor copying later *)
 Bytes.blit bmaps.(0) 0 bmaps.(1) 0 (Bytes.length bmaps.(0)) ;
 let range =
  let r = Seq.(ints ~-1 |> take 3 |> memoize) in
  Seq.product r r |> Seq.filter (fun (y,x) -> x <> 0 || y <> 0) |>
  Seq.memoize in
 let neighbors map y x =
  range |>
  Seq.map (fun (y',x') -> (y+y',x+x')) |>
  Seq.filter (fun (y,x) -> y >= 0 && y < h && x >= 0 && x < w && map.%[y;x] = '#') |>
  Seq.length in
 let count_seats map = Bytes.fold_left (fun a c -> if c = '#' then succ a else a) 0 map in
 (* debug printing *)
 let print_map map =
  for i = 0 to h - 1 do print_endline (Bytes.sub_string map (i*w) w) done in
 let iterate () =
  let z' = !z lxor 1 in
  for i = 0 to h - 1 do for j = 0 to w - 1 do
   if bmaps.(!z).%[i;j] = '.' then () else
    let n = neighbors bmaps.(!z) i j in
    if n >= 4 && bmaps.(!z).%[i;j] = '#' then bmaps.(z').%[i;j] <- 'L' else
    if n = 0  && bmaps.(!z).%[i;j] = 'L' then bmaps.(z').%[i;j] <- '#' else
    bmaps.(z').%[i;j] <- bmaps.(!z).%[i;j]
   done done ;
  z := z' in
 iterate ();
 if debug then (
  print_map bmaps.(0) ;
  print_endline "-----" ;
  print_map bmaps.(1) ;
  print_endline "-----" ;
  iterate ();
  print_map bmaps.(0)) ;
 while bmaps.(0) <> bmaps.(1) do
  iterate ()
 done ;
 count_seats bmaps.(0)

let problem_11b () =
 let debug = false in
 let map = In_channel.(with_open_bin "11.txt" input_lines) |> Array.of_list in
 let w = String.length map.(0) in
 let h = Array.length map in
 let z = ref 0 in
 let bmaps = Array.init 2 (fun _ -> Bytes.create (w*h)) in
 (* local bmap operators *)
 let (.%[;..]<-) = (fun bs idx x -> Bytes.set bs (idx.(0) * w + idx.(1)) x) in
 let (.%[;..]) = (fun bs idx -> Bytes.get bs (idx.(0) * w + idx.(1))) in
 (* load map into bmaps.(0) *)
 for i = 0 to h - 1 do for j = 0 to w - 1 do bmaps.(0).%[i;j] <- map.(i).[j] done done;
 (* copy first to optimize out floor copying later *)
 Bytes.blit bmaps.(0) 0 bmaps.(1) 0 (Bytes.length bmaps.(0)) ;
 let range =
  let r = Seq.(ints ~-1 |> take 3 |> memoize) in
  Seq.product r r |> Seq.filter (fun (y,x) -> x <> 0 || y <> 0) |>
  Seq.memoize in
 let neighbors map y x =
  range |>
  Seq.filter_map
   (fun (dy,dx) ->
    let rec loop y x =
     if y < 0 || y >= h || x < 0 || x >= w then None else
     if map.%[y;x] = '#' then Some (y,x) else
     if map.%[y;x] = 'L' then None else
     loop (y+dy) (x+dx) in
    loop (y+dy) (x+dx)) |>
  Seq.length in
 let count_seats map = Bytes.fold_left (fun a c -> if c = '#' then succ a else a) 0 map in
 (* debug printing *)
 let print_map map =
  for i = 0 to h - 1 do print_endline (Bytes.sub_string map (i*w) w) done in
 let iterate () =
  let z' = !z lxor 1 in
  for i = 0 to h - 1 do for j = 0 to w - 1 do
   if bmaps.(!z).%[i;j] = '.' then () else
    let n = neighbors bmaps.(!z) i j in
    if n >= 5 && bmaps.(!z).%[i;j] = '#' then bmaps.(z').%[i;j] <- 'L' else
    if n = 0  && bmaps.(!z).%[i;j] = 'L' then bmaps.(z').%[i;j] <- '#' else
    bmaps.(z').%[i;j] <- bmaps.(!z).%[i;j]
   done done ;
  z := z' in
 iterate ();
 if debug then (
  print_map bmaps.(0) ;
  print_endline "-----" ;
  print_map bmaps.(1) ;
  print_endline "-----" ;
  iterate ();
  print_map bmaps.(0)) ;
 while bmaps.(0) <> bmaps.(1) do
  iterate ()
 done ;
 count_seats bmaps.(0)

(* degrees can only come in 90, 180, or 270 *)
let problem_12a () =
 let module Ship = struct
  type t = {mutable x : int; mutable y : int; mutable dx : int; mutable dy: int}
  (* use matrix algebra : with inverted sin terms *)
  let rot_left (s:t) n =
   let deg' = n mod 360 in
   let xdx', xdy' = [|(1,0);(0,1);(~-1,0);(0,~-1)|].(deg'/90) in
   let ydx', ydy' = [|(0,1);(~-1,0);(0,~-1);(1,0)|].(deg'/90) in
   let dx' = xdx' * s.dx + xdy' * s.dy in
   let dy' = ydx' * s.dx + ydy' * s.dy in
   s.dx <- dx'; s.dy <- dy'
  let rot_right (s:t) n = rot_left s (360-n)
 end in
 let ship : Ship.t = {x = 0; y = 0; dx = 1; dy = 0} in
 let move (ship : Ship.t) instr =
  let arg = (String.sub instr 1 (String.length instr - 1) |> int_of_string) in
  match instr.[0] with
  | 'N' -> ship.y <- ship.y - arg
  | 'S' -> ship.y <- ship.y + arg
  | 'E' -> ship.x <- ship.x + arg
  | 'W' -> ship.x <- ship.x - arg
  | 'L' -> Ship.rot_left  ship arg
  | 'R' -> Ship.rot_right ship arg
  | 'F' -> (ship.x <- ship.x + arg * ship.dx ; ship.y <- ship.y + arg * ship.dy)
  | _ -> () in
 In_channel.(with_open_bin "12.txt" input_lines) |>
 (* List.iter (fun s -> move ship s; Printf.printf "x: %d; y: %d; dx: %d; dy: %d\n" ship.x ship.y ship.dx ship.dy) ; *)
 List.iter (fun s -> move ship s) ;
 ship.x + ship.y

(* degrees can only come in 90, 180, or 270 *)
let problem_12b () =
 let debug = false in
 let module Ship = struct
  (* use dx dy for waypoint *)
  type t = {mutable x : int; mutable y : int; mutable dx : int; mutable dy: int}
  let rot_left (s:t) n =
   let deg' = n mod 360 in
   let xdx', xdy' = [|(1,0);(0,1);(~-1,0);(0,~-1)|].(deg'/90) in
   let ydx', ydy' = [|(0,1);(~-1,0);(0,~-1);(1,0)|].(deg'/90) in
   let dx' = xdx' * s.dx + xdy' * s.dy in
   let dy' = ydx' * s.dx + ydy' * s.dy in
   s.dx <- dx'; s.dy <- dy'
  let rot_right (s:t) n = rot_left s (360-n)
 end in
 let ship : Ship.t = {x = 0; y = 0; dx = 10; dy = ~-1} in
 let move (ship : Ship.t) instr =
  let arg = (String.sub instr 1 (String.length instr - 1) |> int_of_string) in
  match instr.[0] with
  | 'N' -> ship.dy <- ship.dy - arg
  | 'S' -> ship.dy <- ship.dy + arg
  | 'E' -> ship.dx <- ship.dx + arg
  | 'W' -> ship.dx <- ship.dx - arg
  | 'L' -> Ship.rot_left  ship arg
  | 'R' -> Ship.rot_right ship arg
  | 'F' -> (ship.x <- ship.x + arg * ship.dx ; ship.y <- ship.y + arg * ship.dy)
  | _ -> () in
 In_channel.(with_open_bin "12.txt" input_lines) |>
 if debug then
  (List.iter (fun s -> move ship s; Printf.printf "x: %d; y: %d; dx: %d; dy: %d\n" ship.x ship.y ship.dx ship.dy))
 else List.iter (fun s -> move ship s) ;
 (*(ship.x, ship.y)*)
 abs ship.x + abs ship.y

let problem_13a () =
 let raw_input = In_channel.(with_open_bin "13.txt" input_lines) |> Array.of_list in
 let t0 = int_of_string raw_input.(0) in
 let buses = raw_input.(1) |> String.split_on_char ',' |> List.filter ((<>)"x") |> List.map (int_of_string) in
 let waits = List.map (fun n -> n - (if t0 mod n = 0 then n else t0 mod n)) buses in
 Seq.zip (List.to_seq buses) (List.to_seq waits) |>
 Seq.fold_left (fun (b,w) (b',w') -> if w' < w then (b',w') else (b,w)) (0,Int.max_int) |>
 (fun (a,b) -> a*b)

let problem_13b () =
 let raw_input = In_channel.(with_open_bin "13.txt" input_lines) |> Array.of_list in
 let buses =
  raw_input.(1) |> String.split_on_char ',' |>
  List.to_seq |> Seq.zip (Seq.ints 0) |>
  Seq.filter_map (fun (i,s) -> if s = "x" then None else Some (i,int_of_string s)) |>
  List.of_seq in
 (* x0 = offset at last pair, p0 = periodicity of last pair *)
 buses |>
 List.fold_left
  (fun (x0, p0) (i,n) ->
   let x = ref x0 in 
   while (!x + i) mod n <> 0 do x := !x + p0 done ;
   (!x,p0*n))
  (1,1) |> fst

let problem_14a () =
 let module Op = struct
  type t =  [`Mask of int * int | `Write of int * int]
  type mt = [`Mask of int * int ]
  type wt = [`Write of int * int ]
  let of_string s =
   (* case: "mask" *)
   if s.[1] = 'a' then
    let m = String.sub s 7 36 in
    let zm = String.fold_left (fun a c -> if c = '0' then (a lsl 1) + 1 else (a lsl 1)) 0 m in
    let om = String.fold_left (fun a c -> if c = '1' then (a lsl 1) + 1 else (a lsl 1)) 0 m in
    `Mask (zm, om)
   else 
    let args = String.split_on_char ' ' s in
    let addr = (let s = List.hd args in String.sub s 4 (String.length s - 4 - 1) |> int_of_string) in
    let value = int_of_string (List.nth args 2) in
    `Write (addr, value)
   let unwrap (m : mt) (w : wt) =
    let (zm,om) = (match m with `Mask (a,b) -> (a,b)) in
    let (addr,value) = (match w with `Write (a,b) -> (a,b)) in
    (addr, (value lor om) land (lnot zm))
 end in
 let module IntMap = Map.Make(Int) in
 let input = In_channel.(with_open_bin "14.txt" input_lines) |> List.map (Op.of_string) in
 let memory =
  input |>
  List.fold_left (fun (mem,mask) op ->
   match op with
   | (`Mask _) as m -> (mem, m)
   | (`Write _) as w ->
     let (addr,value) = Op.unwrap mask w in
     (IntMap.add addr value mem, mask)) (IntMap.empty, `Mask (0,0)) |> fst in
  IntMap.fold (fun _ v a -> v+a) memory 0

let problem_14b () =
 let module Op = struct
  type t =  [`Mask of int * int | `Floating of int * int | `Write of int * int]
  type mt = [`Mask of int * int ]
  type ft = [`Floating of int * int ]
  type wt = [`Write of int * int ]
  let of_string s =
   (* case: "mask" *)
   if s.[1] = 'a' then
    let m = String.sub s 7 36 in
    (* floating mask *)
    let fm = String.fold_left (fun a c -> if c = 'X' then (a lsl 1) + 1 else (a lsl 1)) 0 m in
    let om = String.fold_left (fun a c -> if c = '1' then (a lsl 1) + 1 else (a lsl 1)) 0 m in
    `Floating (fm, om)
   else 
    let args = String.split_on_char ' ' s in
    let addr = (let s = List.hd args in String.sub s 4 (String.length s - 4 - 1) |> int_of_string) in
    let value = int_of_string (List.nth args 2) in
    `Write (addr, value)
   let convert_f2m (m : ft) (idx : int) : mt =
    let (fm, om) = (match m with `Floating (a,b) -> (a,b)) in
    let zm' = ref 0
    and om' = ref 0
    and idx = ref idx
    and cur = ref 1 in
    while !cur < (1 lsl 36) do
     if (!cur land fm) <> 0 then
      (if !idx land 1 = 1 then om' := !om' lor !cur else zm' := !zm' lor !cur ;
       idx := !idx lsr 1) ;
     cur := !cur lsl 1
    done ; `Mask (!zm', om lor !om')
   let unwrap (m : mt) (w : wt) =
    let (zm,om) = (match m with `Mask (a,b) -> (a,b)) in
    let (addr,value) = (match w with `Write (a,b) -> (a,b)) in
    ((addr lor om) land (lnot zm), value)
   let floating_width (m : ft) : int =
    let (fm,_) = (match m with `Floating (a,b) -> (a,b)) in
    let rec loop acc n =
     if n = 0 then acc else
     loop (if n land 1 = 1 then succ acc else acc) (n lsr 1)
    in loop 0 fm
 end in
 let module IntMap = Map.Make(Int) in
 let input = In_channel.(with_open_bin "14.txt" input_lines) |> List.map (Op.of_string) in
 let memory =
  input |>
  List.fold_left (fun (mem,mask) op ->
   match op with
   | (`Floating _) as m -> (mem, m)
   | (`Write _) as w ->
     let width = Op.floating_width mask in
     let mem' = ref mem in
     for i = 0 to (1 lsl width) - 1 do
      let (addr, value) = Op.unwrap (Op.convert_f2m mask i) w in
      mem' := IntMap.add addr value !mem'
     done ;
     (!mem', mask)) (IntMap.empty, `Floating (0,0)) |> fst in
  IntMap.fold (fun _ v a -> v+a) memory 0

let problem_15a () =
 let module IntMap = Map.Make(Int) in
 let input = In_channel.(with_open_bin "15.txt" input_line) |> Option.get |> String.split_on_char ',' in
 let set0 = input |> List.to_seq |> Seq.take (List.length input - 1) |> Seq.fold_lefti (fun a i s -> IntMap.add (int_of_string s) (i+1) a) IntMap.empty in
 let last0 = List.nth input (List.length input - 1) |> int_of_string in
 Seq.ints (List.length input) |> Seq.take (2020 - List.length input) |>
 Seq.fold_left
  (fun (set,last) i ->
   match IntMap.find_opt last set with
   | None -> (IntMap.add last i set, 0)
   | Some n -> (IntMap.add last i set, i - n)) (set0,last0)
 |> snd
 
(* slow, but it works! *)
(* opt: loops? hashtbls? *)
let problem_15b () =
 let module IntMap = Map.Make(Int) in
 let input = In_channel.(with_open_bin "15.txt" input_line) |> Option.get |> String.split_on_char ',' in
 let set0 = input |> List.to_seq |> Seq.take (List.length input - 1) |> Seq.fold_lefti (fun a i s -> IntMap.add (int_of_string s) (i+1) a) IntMap.empty in
 let last0 = List.nth input (List.length input - 1) |> int_of_string in
 Seq.ints (List.length input) |> Seq.take (30000000 - List.length input) |>
 Seq.fold_left
  (fun (set,last) i ->
   match IntMap.find_opt last set with
   | None -> (IntMap.add last i set, 0)
   | Some n -> (IntMap.add last i set, i - n)) (set0,last0)
 |> snd
 
let problem_16a () =
 let log = Queue.create () in
 let section_idx lines = lines |> List.to_seq |> Seq.fold_lefti (fun a i s -> if s <> "" then a else i::a) [] |> List.rev in
 let parse_params s =
  let idx = (String.index_from s 0 ':' + 2) in
  String.sub s idx (String.length s - idx) |> String.split_on_char ' ' |> List.to_seq |> Seq.zip (Seq.ints 0) |>
  Seq.filter_map (fun (i,s) -> if i land 1 = 1 then None else Some (String.split_on_char '-' s |> List.map int_of_string |> Array.of_list)) |>
  List.of_seq in
 let within range x = range.(0) <= x && x <= range.(1) in
 (* passthrough logger *)
 let log_if_invalid x valid =
  if not valid then Queue.add x log ;
  valid in
  
 let validate_ticket params xs =
  xs |>
  List.map (fun x ->
   params |>
   List.map (fun paramset -> 
    paramset |>
    List.map (fun param -> within param x) |>
    List.exists (Fun.id)) |>
   List.exists (Fun.id) |>
   log_if_invalid x) |>
  List.for_all (Fun.id) in

 let input = In_channel.(with_open_bin "16.txt" input_lines) in
 let idxs = input |> section_idx |> Array.of_list in
 let tickets = input |> List.to_seq |> Seq.drop (idxs.(1) + 2) |>
  Seq.map (fun s -> s |> String.split_on_char ',' |> List.map int_of_string) |> List.of_seq in
 let params = input |> List.to_seq |> Seq.take idxs.(0) |> Seq.map parse_params |> List.of_seq in
 let _ = List.map (validate_ticket params) tickets in
 Queue.to_seq log |> Seq.fold_left (+) 0

let problem_16b () =
 let debug = false in
 let section_idx lines = lines |> List.to_seq |> Seq.fold_lefti (fun a i s -> if s <> "" then a else i::a) [] |> List.rev in
 let parse_params s =
  let idx = (String.index_from s 0 ':' + 2) in
  String.sub s idx (String.length s - idx) |> String.split_on_char ' ' |> List.to_seq |> Seq.zip (Seq.ints 0) |>
  Seq.filter_map (fun (i,s) -> if i land 1 = 1 then None else Some (String.split_on_char '-' s |> List.map int_of_string |> Array.of_list)) |>
  List.of_seq in
 let within range x = range.(0) <= x && x <= range.(1) in
  
 let validate_ticket params xs =
  xs |>
  List.map (fun x ->
   params |>
   List.map (fun paramset -> 
    paramset |>
    List.map (fun param -> within param x) |>
    List.exists (Fun.id)) |>
   List.exists (Fun.id)) |>
  List.for_all (Fun.id) in

 let validate_field paramset tickets field_idx =
  tickets |> List.to_seq |> Seq.map (fun t -> List.nth t field_idx) |>
  Seq.map (fun x ->
   paramset |>
   List.map (fun param -> within param x) |>
   List.exists (Fun.id)) |>
  Seq.for_all (Fun.id) in

 let input = In_channel.(with_open_bin "16.txt" input_lines) in
 let idxs = input |> section_idx |> Array.of_list in
 let your_ticket = input |> List.to_seq |> Seq.drop (idxs.(1) - 1) |> Seq.uncons |> Option.get |> fst |>
  String.split_on_char ',' |> List.map int_of_string in
 let tickets = input |> List.to_seq |> Seq.drop (idxs.(1) + 2) |>
  Seq.map (fun s -> s |> String.split_on_char ',' |> List.map int_of_string) |> List.of_seq in
 let params = input |> List.to_seq |> Seq.take idxs.(0) |> Seq.map parse_params |> List.of_seq in
 let valid_tickets = List.filter (validate_ticket params) tickets in
 let field_length = List.length params in
 let validation_matrix =
  params |>
  List.map (fun paramset ->
   Seq.ints 0 |> Seq.take field_length |>
   Seq.map (validate_field (paramset) (your_ticket::valid_tickets)) |> List.of_seq) |>
  List.flatten |>
  Array.of_list in
 let mappings = Array.make field_length ~-1 in
 let row_iterators = Array.init field_length (fun i -> Seq.ints (i*field_length) |> Seq.take field_length) in
 let col_iterators = Array.init field_length (fun i -> Seq.ints 0 |> Seq.map (fun j -> j * field_length + i) |> Seq.take field_length) in
 let uniqify mat =
  for i = 0 to Array.length row_iterators - 1 do
   let unique =
    (row_iterators.(i) |> Seq.map (fun i -> if mat.(i) then 1 else 0) |> Seq.fold_left ( + ) 0) = 1 in
   if unique then (
    if debug then (print_int i ; print_newline ()) ;
    match (row_iterators.(i) |> Seq.zip (Seq.ints 0) |> Seq.fold_left (fun a (col,i) -> if mat.(i) then Some col else a) None) with
    | None -> ()
      (* col corresponds to ticket position; row/i corresponds to field/paramset position; therefore *)
      (* USE mappings.(col) <- i ; NOT mappings.(i) <- col *)
    | Some col -> mappings.(col) <- i; col_iterators.(col) |> Seq.iter (fun i -> mat.(i) <- false)
  );
  done in
 (* maximum number of iterations, less may be needed;  iterate until matrix is 100% false *)
 for i = 0 to field_length - 1 do uniqify validation_matrix done ;
 (* destination fills the first six param slots *)
 your_ticket |> List.to_seq |> Seq.fold_lefti (fun a i n -> if mappings.(i) < 6 then n * a else a) 1
 (* mappings *)

(* 3D Life *)
let problem_17a () =
 let module XYZSet = Set.Make(struct type t = int * int * int let compare = compare end) in
 let initial_state =
  In_channel.(with_open_bin "17.txt" input_lines) |>
  List.to_seq |>
  Seq.fold_lefti (fun a y s ->
   s |> String.to_seq |>
   Seq.fold_lefti (fun a x c -> if c = '#' then XYZSet.add (x,y,0) a else a) XYZSet.empty |>
   XYZSet.union a) XYZSet.empty in
 let n_iter =
  let d1 = Seq.(ints ~-1 |> take 3) in
   Seq.product (Seq.product d1 d1) d1 |>
   Seq.map (fun ((x,y),z) -> (x,y,z)) |>
   Seq.filter ((<>)(0,0,0)) |> Seq.memoize in
 let active_neighbors (x,y,z) set =
  n_iter |> Seq.filter_map (fun (dx,dy,dz) -> XYZSet.find_opt (x+dx,y+dy,z+dz) set) in
 let inactive_neighbors (x,y,z) set =
  n_iter |> Seq.filter_map (fun (dx,dy,dz) -> if XYZSet.mem (x+dx,y+dy,z+dz) set then None else Some (x+dx,y+dy,z+dz)) in
 let iterate set =
  (* remove dying *)
  let set' =
   XYZSet.filter (fun pt -> let n = active_neighbors pt set |> Seq.length in n = 2 || n = 3) set in
  (* add born *)
  let set'' =
   XYZSet.fold (fun pt a -> XYZSet.add_seq (inactive_neighbors pt set) a) set XYZSet.empty |>
   XYZSet.filter (fun pt -> let n = active_neighbors pt set |> Seq.length in n = 3) in
  XYZSet.union set' set'' in
 
 (* iterate 6 cycles *)
 (* for the example, remember: the frame of view follows the active cells! *)
 Seq.(ints 1 |> take 6) |> Seq.fold_left (fun a _ -> iterate a) initial_state |> XYZSet.elements |> List.length

(* 4D Life *)
let problem_17b () =
 let module XYZWSet = Set.Make(struct type t = int * int * int * int let compare = compare end) in
 let initial_state =
  In_channel.(with_open_bin "17.txt" input_lines) |>
  List.to_seq |>
  Seq.fold_lefti (fun a y s ->
   s |> String.to_seq |>
   Seq.fold_lefti (fun a x c -> if c = '#' then XYZWSet.add (x,y,0,0) a else a) XYZWSet.empty |>
   XYZWSet.union a) XYZWSet.empty in
 let n_iter =
  let d1 = Seq.(ints ~-1 |> take 3) in
   Seq.product (Seq.product (Seq.product d1 d1) d1) d1 |>
   Seq.map (fun (((x,y),z),w) -> (x,y,z,w)) |>
   Seq.filter ((<>)(0,0,0,0)) |> Seq.memoize in
 let active_neighbors (x,y,z,w) set =
  n_iter |>
  Seq.filter_map (fun (dx,dy,dz,dw) -> XYZWSet.find_opt (x+dx,y+dy,z+dz,w+dw) set) in
 let inactive_neighbors (x,y,z,w) set =
  n_iter |>
  Seq.filter_map (fun (dx,dy,dz,dw) -> if XYZWSet.mem (x+dx,y+dy,z+dz,w+dw) set then None else Some (x+dx,y+dy,z+dz,w+dw)) in
 let iterate set =
  (* remove dying *)
  let set' =
   XYZWSet.filter (fun pt -> let n = active_neighbors pt set |> Seq.length in n = 2 || n = 3) set in
  (* add born *)
  let set'' =
   XYZWSet.fold (fun pt a -> XYZWSet.add_seq (inactive_neighbors pt set) a) set XYZWSet.empty |>
   XYZWSet.filter (fun pt -> let n = active_neighbors pt set |> Seq.length in n = 3) in
  XYZWSet.union set' set'' in
 
 (* iterate 6 cycles *)
 Seq.(ints 1 |> take 6) |> Seq.fold_left (fun a _ -> iterate a) initial_state |> XYZWSet.elements |> List.length

(* Infix Calculator *)
(* no numbers > 9, no numbers < 0, add/mul only *)
let problem_18a () =
 let is_digit c = (0x30 <= Char.code c) && (Char.code c <= 0x39) in
 let module Op = struct
  (* change to LP and RP, will probably be easier than tracking depth! *)
  type t = Add | Mul | LP | RP | Num of int
  let of_char = function
  | '+' -> Add | '*' -> Mul | '(' -> LP | ')' -> RP
  | c when is_digit c -> Num (Char.code c - 0x30)
  | _ -> raise_notrace @@ Invalid_argument "Invalid Character"
  let to_string = function
  | Add -> "+" | Mul -> "*" | LP -> "(" | RP -> ")"
  | Num n -> Int.to_string n
 end in
 let lex s =
  String.to_seq s |>
  Seq.filter ((<>)' ') |>
  Seq.map (Op.of_char) |> List.of_seq in

 let rec iterate acc eq =
  Op.( match eq with
  | [] -> iterate [] (List.rev acc)
  | Num n :: [] when acc = [] -> n
  | Num n1 :: Add :: Num n2 :: tl -> iterate [] (List.rev_append acc (Num (n1+n2)::tl))
  | Num n1 :: Mul :: Num n2 :: tl -> iterate [] (List.rev_append acc (Num (n1*n2)::tl))
  | LP :: Num n :: RP :: tl -> iterate [] (List.rev_append acc (Num n::tl))
  | hd :: tl -> iterate (hd::acc) tl ) in
  
 let input_raw = In_channel.(with_open_bin "18.txt" input_lines) in
 input_raw |> List.filter ((<>)"") |>
 List.map (fun s -> s |> lex |> iterate []) |>
 List.fold_left ( + ) 0

(* TODO : Watch 0DE5 video on infix expressions *)
let problem_18b () =
 let is_digit c = (0x30 <= Char.code c) && (Char.code c <= 0x39) in
 let module Op = struct
  type t = Add | Mul | LP | RP | Num of int | Expr of t list
  let of_char = function
  | '+' -> Add | '*' -> Mul | '(' -> LP | ')' -> RP
  | c when is_digit c -> Num (Char.code c - 0x30)
  | _ -> raise_notrace @@ Invalid_argument "Invalid Character"
  let rec to_string = function
  | Add -> " + " | Mul -> " * " | LP -> "(" | RP -> ")"
  | Expr expr -> Fun.flip (^) ")" @@ (^) "(" (List.fold_left (fun a e -> a ^ to_string e) "" expr)
  | Num n -> Int.to_string n
 end in

 let lex s =
  String.to_seq s |>
  Seq.filter ((<>)' ') |>
  Seq.map (Op.of_char) |> List.of_seq in

 let parse exp =
  let rec parse_expr acc = Op.(function
  | [] -> (List.rev acc), []
  | RP :: tl -> (List.rev acc), tl
  | LP :: tl -> let (exp, next) = parse_expr [] tl in parse_expr ((Expr exp)::acc) next
  | hd :: tl -> parse_expr (hd::acc) tl) in parse_expr [] exp |> fst in

 (* exp is an unwrapped Expr: i.e., Op.t list *)
 let solve exp =
  let rec iterate_add acc eq =
   (* precedence 1 *)
   Op.(match eq with
    | [] -> iterate_mul [] (List.rev acc)
    | Num n1 :: Add :: Num n2 :: tl -> iterate_add acc (Num (n1+n2)::tl)
    | hd :: tl -> iterate_add (hd::acc) tl)
   (* precedence 2 *)
  and iterate_mul acc eq =
   Op.(match eq with
    | [] -> assert false (* everything should have collapsed by this point *)
    | Num n :: [] when acc = [] -> Num n
    | Num n1 :: Mul :: Num n2 :: tl -> iterate_mul acc (Num (n1*n2)::tl)
    | hd :: tl -> iterate_mul (hd::acc) tl)
  (* precedence 0 *)
  and iterate_expr acc eq =
   Op.(match eq with
    | [] -> iterate_add [] (List.rev acc)
    | Expr exp :: tl -> iterate_expr acc (iterate_expr [] exp::tl)
    | hd :: tl -> iterate_expr (hd::acc) tl) in
  iterate_expr [] exp |> (function Num n -> n | _ -> raise_notrace (Invalid_argument "Solve Error")) in

 let input_raw = In_channel.(with_open_bin "18.txt" input_lines) in
 input_raw |> List.filter ((<>)"") |>
 List.map (fun s -> s |> lex |> parse |> solve) |>
 List.fold_left ( + ) 0

(* message validation : regex compilation simulator with concatenation and branching *)
(* only possible characters are "a" and "b" *)
(* each rule has no more than 2 branches *)
(* use automata to construct grammars *)
(* rules should have a compiled flag if fully defined *)
(* 134 rules in total; all are described in full input *)
(* can use an ordered set, but we'll use arrays instead *)
let problem_19a () =
 let module Rule : sig
  (* all rules should end in a single final node *)
  (* nodes with None are "epsilon nodes" *)
  type t = Final of char option | Single of char option * t | Double of char option * t * t
  val singleton : char -> t
  val epsilon : t
  val attach : t -> t -> t
  val cons : t -> t -> t
  val test_string_from : t -> string -> int -> bool
 end = struct
  type t = Final of char option | Single of char option * t | Double of char option * t * t
  let singleton c = Final (Some c)
  let epsilon = Final None
  let attach n1 n2 =
   match n1 with
   | Final i -> Single (i, n2)
   | Single (i,o) -> Double (i, o, n2)
   | Double (i,o1,o2) -> Double (i, o1, Double (None, o2, n2))
  let rec cons n1 n2 =
   match n1 with
   | Final a -> Single (a, n2)
   | Single (a, b) -> Single (a, cons b n2)
   | Double (a, b, c) -> Double (a, cons b n2, cons c n2)
  let rec test_string_from n s idx =
   try (* catch out of bounds errors *)
    match n with
    | Final  None when idx = String.length s -> true
    | Final  (Some c) when s.[idx] = c && idx + 1 = String.length s -> true
    | Single (None, o) -> test_string_from o s idx
    | Single (Some c, o) when s.[idx] = c -> test_string_from o s (idx+1)
    | Double (None, o1, o2) -> test_string_from o1 s idx || test_string_from o2 s idx
    | Double (Some c, o1, o2) when s.[idx] = c -> test_string_from o1 s (idx+1) || test_string_from o2 s (idx+1)
    | _ -> false
   with Invalid_argument _ -> false
 end in
 let raw_input = In_channel.(with_open_bin "19.txt" input_lines) in
 let rules_len = raw_input |> List.find_index ((=)"") |> Option.get in
 let rule_definitions = Array.make rules_len "" in
 let rules = Array.make rules_len None in
 raw_input |>
 List.to_seq |> Seq.take rules_len |>
 Seq.iter (fun s ->
  let brk = String.index_from s 0 ':' in
  let idx = String.sub s 0 brk |> int_of_string in
  rule_definitions.(idx) <- (String.sub s (brk+2) (String.length s - brk - 2))) ;
 Array.iteri (fun i s -> if s.[0] = '"' then rules.(i) <- Some (Rule.singleton s.[1])) rule_definitions ;
 let is_double_def s = String.contains s '|' in
 let parse_nums s = String.split_on_char ' ' s |> List.filter ((<>)"|") |> List.map (int_of_string) in
 let ready_to_compile s = parse_nums s |> List.to_seq |> Seq.for_all (fun i -> rules.(i) |> Option.is_some) in
 let compile s =
  let rs = s |> parse_nums |> List.map (fun n -> rules.(n) |> Option.get) in
  if is_double_def s then
    let left_len = s |> String.split_on_char ' ' |> List.find_index ((=)"|") |> Option.get in
    let left = List.to_seq rs |> Seq.take left_len |> List.of_seq in
    let right = List.to_seq rs |> Seq.drop left_len |> List.of_seq in
    Rule.Double (None, List.fold_right Rule.cons left Rule.epsilon, List.fold_right Rule.cons right Rule.epsilon)
  else List.fold_right Rule.cons rs Rule.epsilon in
 while rules.(0) = None do
  Array.iteri (fun i s -> if rules.(i) = None && ready_to_compile s then rules.(i) <- Some (compile s)) rule_definitions
 done ;
 let r0 = rules.(0) |> Option.get in
 let grammars = raw_input |> List.to_seq |> Seq.drop (rules_len+1) in
 Seq.fold_left (fun a s -> if Rule.test_string_from r0 s 0 then succ a else a) 0 grammars
 
 (* by-hand example *)
(*
 let r4 = Rule.singleton 'a' in
 let r5 = Rule.singleton 'b' in
 let r2 = Rule.Double (None, Rule.cons r4 r4, Rule.cons r5 r5) in
 let r3 = Rule.Double (None, Rule.cons r4 r5, Rule.cons r5 r4) in
 let r1 = Rule.Double (None, Rule.cons r2 r3, Rule.cons r3 r2) in
 let r0 = Rule.cons r4 (Rule.cons r1 r5) in
 ["ababbb"; "bababa"; "abbbab"; "aaabbb"; "aaaabbb"] |>
 List.map (fun s -> Rule.test_string_from r0 s 0)
*)

let problem_19b () =
 let module Rule : sig
  (* all rules should end in a single final node *)
  (* nodes with None are "epsilon nodes" *)
  (* Rule.epsilon is the zero monoid *)
  (* Star (a, exit) can be implemented with Plus : Double (None, exit, Plus (a, exit)) *)
  type t = Final of char option | Single of char option * t | Double of char option * t * t | Plus of t * t | Wings of t * t * t
  val singleton : char -> t
  val epsilon : t
  val attach : t -> t -> t
  val cons : t -> t -> t
  val test_string_from : t -> string -> int -> bool
 end = struct
  type t = Final of char option | Single of char option * t | Double of char option * t * t | Plus of t * t | Wings of t * t * t
  let singleton c = Final (Some c)
  let epsilon = Final None
  let rec attach n1 n2 =
   match n1 with
   | Final i -> Single (i, n2)
   | Single (i,o) -> Double (i, o, n2)
   | Double (i,o1,o2) -> Double (i, o1, Double (None, o2, n2))
   | Plus (a, exit) -> Plus (a, attach exit n2)
   | Wings (l, r, exit) -> Wings (l, r, attach exit n2)
  let rec cons n1 n2 =
   match n1 with
   | Final a -> Single (a, n2)
   | Single (a, b) -> Single (a, cons b n2)
   | Double (a, b, c) -> Double (a, cons b n2, cons c n2)
   | Plus (a, exit) -> Plus (a, cons exit n2)
   | Wings (l, r, exit) -> Wings (l, r, cons exit n2)
  let rec test_string_from n s idx =
   try (* catch out of bounds errors *)
    match n with
    | Final  None when idx = String.length s -> true
    | Final  (Some c) when s.[idx] = c && idx + 1 = String.length s -> true
    | Single (None, o) -> test_string_from o s idx
    | Single (Some c, o) when s.[idx] = c -> test_string_from o s (idx+1)
    | Double (None, o1, o2) -> test_string_from o1 s idx || test_string_from o2 s idx
    | Double (Some c, o1, o2) when s.[idx] = c -> test_string_from o1 s (idx+1) || test_string_from o2 s (idx+1)
    | Plus (a, exit) -> test_string_from (cons a exit) s idx || test_string_from (cons a n) s idx
    | Wings (l, r, exit) -> test_string_from (cons l (cons r exit)) s idx || test_string_from (cons l (cons (Wings (l, r, r)) exit)) s idx
    | _ -> false
   with Invalid_argument _ -> false
 end in
 let raw_input = In_channel.(with_open_bin "19.txt" input_lines) in
 let rules_len = raw_input |> List.find_index ((=)"") |> Option.get in
 let rule_definitions = Array.make rules_len "" in
 let rules = Array.make rules_len None in
 raw_input |>
 List.to_seq |> Seq.take rules_len |>
 Seq.iter (fun s ->
  let brk = String.index_from s 0 ':' in
  let idx = String.sub s 0 brk |> int_of_string in
  rule_definitions.(idx) <- (String.sub s (brk+2) (String.length s - brk - 2))) ;
 Array.iteri (fun i s -> if s.[0] = '"' then rules.(i) <- Some (Rule.singleton s.[1])) rule_definitions ;
 let is_double_def s = String.contains s '|' in
 let parse_nums s = String.split_on_char ' ' s |> List.filter ((<>)"|") |> List.map (int_of_string) in
 let ready_to_compile s = parse_nums s |> List.to_seq |> Seq.for_all (fun i -> rules.(i) |> Option.is_some) in
 let compile s =
  let rs = s |> parse_nums |> List.map (fun n -> rules.(n) |> Option.get) in
  if is_double_def s then
    let left_len = s |> String.split_on_char ' ' |> List.find_index ((=)"|") |> Option.get in
    let left = List.to_seq rs |> Seq.take left_len |> List.of_seq in
    let right = List.to_seq rs |> Seq.drop left_len |> List.of_seq in
    Rule.Double (None, List.fold_right Rule.cons left Rule.epsilon, List.fold_right Rule.cons right Rule.epsilon)
  else List.fold_right Rule.cons rs Rule.epsilon in
 rule_definitions.(8) <- "42 | 42 8" ;
 rule_definitions.(11) <- "42 31 | 42 11 31" ;
 (* 8 and 11 must be specially compiled after the rest *)
 while rules.(42) = None || rules.(31) = None do
  Array.iteri (fun i s -> if rules.(i) = None && ready_to_compile s then rules.(i) <- Some (compile s)) rule_definitions
 done ;
 rules.(8) <- Some (Rule.Plus (Option.get rules.(42), Rule.epsilon));
 rules.(11) <- Some (Rule.Wings (Option.get rules.(42), Option.get rules.(31), Rule.epsilon));
 rules.(0) <- Some (compile rule_definitions.(0)) ;
 
 let r0 = rules.(0) |> Option.get in
 let grammars = raw_input |> List.to_seq |> Seq.drop (rules_len+1) in
 Seq.fold_left (fun a s -> if Rule.test_string_from r0 s 0 then succ a else a) 0 grammars
 
 (* by-hand example *)
(*
 let r4 = Rule.singleton 'a' in
 let r5 = Rule.singleton 'b' in
 let r2 = Rule.Double (None, Rule.cons r4 r4, Rule.cons r5 r5) in
 let r3 = Rule.Double (None, Rule.cons r4 r5, Rule.cons r5 r4) in
 let r1 = Rule.Double (None, Rule.cons r2 r3, Rule.cons r3 r2) in
 let r0 = Rule.cons r4 (Rule.cons r1 r5) in
 ["ababbb"; "bababa"; "abbbab"; "aaabbb"; "aaaabbb"] |>
 List.map (fun s -> Rule.test_string_from r0 s 0)
*)

(* problem 20a *)
(* test if corner by how many edges cannot line up with any other edge *)
let problem_20a () =
 let debug = false in
 let raw_input = In_channel.(with_open_bin "20.txt" input_lines) |> Array.of_list in
 let q = Queue.create () in
 Array.iteri (fun i s -> if s = "" then Queue.add (i+1) q) raw_input;
 let dim = Queue.peek q - 2 in
 let parse_id s =
  let brk = String.index_from s 0 ' ' in
  String.sub s (brk+1) (String.length s - brk - 2) |>
  int_of_string in
 let ids = Seq.cons 0 (Queue.to_seq q) |> Seq.map (fun i -> (i, parse_id raw_input.(i))) |> Array.of_seq in
 Queue.clear q ;
 let iterators_of_id (i,_) =
  let top = raw_input.(i+1) |> String.to_seq in
  let bot = raw_input.(i+dim) |> String.to_seq in
  let col = Array.to_seq raw_input |> Seq.drop (i+1) |> Seq.take dim in
  let left = col |> Seq.map (fun s -> s.[0]) in
  let right = col |> Seq.map (fun s -> s.[dim-1]) in
  [|top;bot;left;right|] in
 let count_matching_edges count id1 id2 =
  let reverse seq = List.(seq |> of_seq |> rev |> to_seq) in
  let set1 = iterators_of_id id1 in
  let set2 = iterators_of_id id2 in
  (* this can be simplified in loops, but this separation is useful if flipping is banned *)
  (* handle rotated cases *)
  if Seq.equal (=) set1.(0) set2.(1) then count.(0) <- count.(0) + 1; (* TB -> rot 90*)
  if Seq.equal (=) set1.(0) set2.(2) then count.(0) <- count.(0) + 1;
  if Seq.equal (=) set1.(0) (reverse set2.(0)) then count.(0) <- count.(0) + 1;
  if Seq.equal (=) set1.(0) (reverse set2.(3)) then count.(0) <- count.(0) + 1;
  if Seq.equal (=) set1.(3) set2.(2) then count.(3) <- count.(3) + 1; (* RL -> rot 90*)
  if Seq.equal (=) set1.(3) (reverse set2.(0)) then count.(3) <- count.(3) + 1;
  if Seq.equal (=) set1.(3) (reverse set2.(3)) then count.(3) <- count.(3) + 1;
  if Seq.equal (=) set1.(3) set2.(1) then count.(3) <- count.(3) + 1;
  if Seq.equal (=) set1.(1) set2.(0) then count.(1) <- count.(1) + 1; (* BT -> rot 90*)
  if Seq.equal (=) set1.(1) set2.(3) then count.(1) <- count.(1) + 1;
  if Seq.equal (=) set1.(1) (reverse set2.(1)) then count.(1) <- count.(1) + 1;
  if Seq.equal (=) set1.(1) (reverse set2.(2)) then count.(1) <- count.(1) + 1;
  if Seq.equal (=) set1.(2) set2.(3) then count.(2) <- count.(2) + 1; (* LR -> rot 90*)
  if Seq.equal (=) set1.(2) (reverse set2.(1)) then count.(2) <- count.(2) + 1;
  if Seq.equal (=) set1.(2) (reverse set2.(2)) then count.(2) <- count.(2) + 1;
  if Seq.equal (=) set1.(2) set2.(0) then count.(2) <- count.(2) + 1 ;
  (* handle flipped cases *)
  if Seq.equal (=) set1.(0) (reverse set2.(1)) then count.(0) <- count.(0) + 1; (* TB -> rot 90*)
  if Seq.equal (=) set1.(0) (reverse set2.(2)) then count.(0) <- count.(0) + 1;
  if Seq.equal (=) set1.(0) set2.(0) then count.(0) <- count.(0) + 1;
  if Seq.equal (=) set1.(0) set2.(3) then count.(0) <- count.(0) + 1;
  if Seq.equal (=) set1.(3) (reverse set2.(2)) then count.(3) <- count.(3) + 1; (* RL -> rot 90*)
  if Seq.equal (=) set1.(3) set2.(0) then count.(3) <- count.(3) + 1;
  if Seq.equal (=) set1.(3) set2.(3) then count.(3) <- count.(3) + 1;
  if Seq.equal (=) set1.(3) (reverse set2.(1)) then count.(3) <- count.(3) + 1;
  if Seq.equal (=) set1.(1) (reverse set2.(0)) then count.(1) <- count.(1) + 1; (* BT -> rot 90*)
  if Seq.equal (=) set1.(1) (reverse set2.(3)) then count.(1) <- count.(1) + 1;
  if Seq.equal (=) set1.(1) set2.(1) then count.(1) <- count.(1) + 1;
  if Seq.equal (=) set1.(1) set2.(2) then count.(1) <- count.(1) + 1;
  if Seq.equal (=) set1.(2) (reverse set2.(3)) then count.(2) <- count.(2) + 1; (* LR -> rot 90*)
  if Seq.equal (=) set1.(2) set2.(1) then count.(2) <- count.(2) + 1;
  if Seq.equal (=) set1.(2) set2.(2) then count.(2) <- count.(2) + 1;
  if Seq.equal (=) set1.(2) (reverse set2.(0)) then count.(2) <- count.(2) + 1 in
 (*let q = Queue.create () in*)
 let count = Array.make 4 0 in
 let corners = ref [] in
 for i = 0 to Array.length ids - 1 do
  for j = 0 to Array.length ids - 1 do if i <> j then count_matching_edges count ids.(i) ids.(j) else () done ;
  if debug then Printf.printf "%d: %d %d %d %d\n" (ids.(i) |> snd) count.(0) count.(1) count.(2) count.(3) ;
  if Array.fold_left ( + ) 0 count = 2 then corners := (snd ids.(i)) :: !corners ;
  Array.fill count 0 4 0
 done;
 List.fold_left ( * ) 1 !corners

(* This solution is not very optimized, but it works! *)
(* every puzzle piece edge matches ONLY one configuration *)
let problem_20b () =
 let debug = true in
 let example = false in
 let raw_input = In_channel.(with_open_bin (if example then "20e.txt" else "20.txt") input_lines) |> Array.of_list in
 let q = Queue.create () in
 Array.iteri (fun i s -> if s = "" then Queue.add (i+1) q) raw_input;
 let dim = Queue.peek q - 2 in
 let parse_id s =
  let brk = String.index_from s 0 ' ' in
  String.sub s (brk+1) (String.length s - brk - 2) |>
  int_of_string in
 let ids = Seq.cons 0 (Queue.to_seq q) |> Seq.map (fun i -> (i, parse_id raw_input.(i))) |> Array.of_seq in
 Queue.clear q ;

 let iterators_of_id (i,_) =
  let top = raw_input.(i+1) |> String.to_seq in
  let bot = raw_input.(i+dim) |> String.to_seq in
  let col = Array.to_seq raw_input |> Seq.drop (i+1) |> Seq.take dim in
  let left = col |> Seq.map (fun s -> s.[0]) in
  let right = col |> Seq.map (fun s -> s.[dim-1]) in
  [|top;bot;left;right|] in

 let link_matching_edges link id1 id2 =
  let reverse seq = List.(seq |> of_seq |> rev |> to_seq) in
  let set1 = iterators_of_id id1 in
  let set2 = iterators_of_id id2 in
  for i = 0 to 3 do for j = 0 to 3 do
   if Seq.equal (=) set1.(i) set2.(j) then link.(i) <- Some (snd id2, j, false) else () done done ;
  for i = 0 to 3 do for j = 0 to 3 do
   if Seq.equal (=) set1.(i) (reverse set2.(j)) then link.(i) <- Some (snd id2, j, true) else () done done in

 let corners = ref [] in
 let linkset =
  Seq.(ints 0 |> take (Array.length ids)) |>
  Seq.map (fun i ->
   let link = Array.make 4 None in
   for j = 0 to Array.length ids - 1 do if i <> j then link_matching_edges link ids.(i) ids.(j) else () done ;
   if Array.fold_left (fun a el -> if Option.is_none el then succ a else a) 0 link = 2 then corners := i :: !corners ;
   link) |> Array.of_seq in

 (* Midway : Show Corner Information *)
 (* List.map (fun idx -> idx, ids.(idx), linkset.(idx)) !corners *)
 (* [(99, (1188, 1823),
     [|Some (3221, 0, false); None; None; Some (1553, 2, false)|]);
    (83, (996, 3391),
     [|None; Some (1103, 1, true); Some (3373, 1, true); None|]);
    (55, (660, 3571),
     [|Some (3823, 2, false); None; Some (1031, 3, false); None|]);
    (48, (576, 1327),
     [|None; Some (1663, 3, true); Some (2591, 2, false); None|])] *)

  (* State Explanation *)
  (* lowest two bits reflect position of (0,0) *)
  (* third bit denotes whether an increase of x (0) or y (1) moves "right" *)
  (* state 0: (0,0) is Top Left *)
  (* state 1: (0,0) is Top Right *)
  (* state 2: (0,0) is Bottom Right *)
  (* state 3: (0,0) is Bottom Left *)

 let corner_state links =
  (* Edges: 0 = Top, 1 = Bottom, 2 = Left, 3 = Right *)
  if links.(0) = None && links.(2) = None then 0 else
  if links.(0) = None && links.(3) = None then 1 else
  if links.(1) = None && links.(3) = None then 2 else
  if links.(1) = None && links.(2) = None then 3 else ~-1 in

 let state_to_right state links =
  (* Edges: 0 = Top, 1 = Bottom, 2 = Left, 3 = Right *)
  let edge =
   (match state with
    | 0 -> links.(3) | 1 -> links.(2) | 2 -> links.(2) | 3 -> links.(3)
    | 4 -> links.(1) | 5 -> links.(1) | 6 -> links.(0) | 7 -> links.(0)
    | _ -> assert false) in
  (* alter edge to enable matching with initial states other than 0 *)
  let edge =
  (function
   | Some (id,side,flip) ->
     if state = 2 || state = 3 || state = 5 || state = 6
     then Some (id, side, not flip) else Some (id, side, flip)
   | None -> None) edge in
  match edge with
  (* complicated mapping, tested by hand *)
  | None -> None
  | Some (id, 2, false) -> Some (id, 0) (* attaches to the left *)
  | Some (id, 2, true)  -> Some (id, 3)
  | Some (id, 0, true)  -> Some (id, 5) (* attaches to the top *)
  | Some (id, 0, false) -> Some (id, 4)
  | Some (id, 3, true)  -> Some (id, 2) (* attaches to right *)
  | Some (id, 3, false) -> Some (id, 1)
  | Some (id, 1, false) -> Some (id, 7) (* attaches to bottom *)
  | Some (id, 1, true)  -> Some (id, 6)
  | _ -> assert false in

 let origin_idx = List.hd !corners in
 let origin_state = Some(ids.(origin_idx) |> snd, corner_state linkset.(origin_idx)) in

 let rec build_row acc state =
  match state with
   | None -> List.rev acc
   | Some (id, s) ->
     let idx = Array.find_index (fun (_,i) -> i = id) ids |> Option.get in
     build_row (state::acc) (state_to_right s (linkset.(idx))) in

 let build_col acc state =
  match state with
  | None -> List.rev acc
  | Some (id, s) ->
    build_row acc (Some (id, s lxor 0x4)) |>
    List.map (function Some (id, s) -> Some (id, s lxor 0x4) | _ -> None) in

 let puzzle = build_col [] origin_state |> List.map (fun state -> build_row [] state) in
 let puzzle_dim = List.length puzzle in
 let offset_mat = puzzle |>
  List.map (fun row ->
   row |>
   List.filter_map (function None -> None | Some (id, state) -> Some (Array.find_opt (fun (off,i) -> i = id) ids |> Option.get |> fst, state)) |>
   Array.of_list) |> Array.of_list in
  
 let sub_idx (offset, state) y x =
  (* 0, 0 is... *)
  match state with
  | 0 (* top-left *) -> raw_input.(offset + 1 + y).[x]
  | 1 (* top-right *) -> raw_input.(offset + 1 + y).[dim - 1 - x]
  | 2 (* bottom-right*) -> raw_input.(offset + dim - y).[dim - 1 - x]
  | 3 (* bottom-left*) -> raw_input.(offset + dim - y).[x]
      (* swap x and y *)
  | 4 (* top-left *) -> raw_input.(offset + 1 + x).[y]
  | 5 (* top-right *) -> raw_input.(offset + 1 + x).[dim - 1 - y]
  | 6 (* bottom-right*) -> raw_input.(offset + dim - x).[dim - 1 - y]
  | 7 (* bottom-left*) -> raw_input.(offset + dim - x).[y]
  | _ -> assert false in

 let puzzle_idx y x =
  sub_idx (offset_mat.(y / dim).(x / dim)) (y mod dim) (x mod dim) in

 let buf = Buffer.create (((dim-2) * puzzle_dim + 1) * (dim-2) * puzzle_dim) in
 for y = 0 to dim * puzzle_dim - 1 do
  if y mod dim <> 0 &&
     y mod dim <> (dim -1)
  then (
   for x = 0 to dim * puzzle_dim - 1 do
    if x mod dim <> 0 &&
       x mod dim <> (dim-1) then 
    Buffer.add_char buf @@ puzzle_idx y x;
   done; Buffer.add_char buf '\n');
 done;
 if debug then print_endline (Buffer.contents buf) ;

 let image_dim = (dim-2) * puzzle_dim in

 let image_get image state y x =
  match state with
  | 0 (* top-left *) -> Bytes.get image (y * (image_dim + 1) + x)
  | 1 (* top-right *) -> Bytes.get image (y * (image_dim + 1) + image_dim - 1 - x)
  | 2 (* bottom-right*) -> Bytes.get image ((image_dim - y - 1) * (image_dim + 1) + image_dim - 1 - x)
  | 3 (* bottom-left*) -> Bytes.get image ((image_dim - y - 1) * (image_dim + 1) + x)
      (* swap x and y *)
  | 4 (* top-left *) -> Bytes.get image (x * (image_dim + 1) + y)
  | 5 (* top-right *) -> Bytes.get image (x * (image_dim + 1) + image_dim - 1 - y)
  | 6 (* bottom-right*) -> Bytes.get image ((image_dim - x - 1) * (image_dim + 1) + image_dim - 1 - y)
  | 7 (* bottom-left*) -> Bytes.get image ((image_dim - x - 1) * (image_dim + 1) + y)
  | _ -> assert false in

 let image_set image state y x v =
  match state with
  | 0 (* top-left *) -> Bytes.set image (y * (image_dim + 1) + x) v
  | 1 (* top-right *) -> Bytes.set image (y * (image_dim + 1) + image_dim - 1 - x) v
  | 2 (* bottom-right*) -> Bytes.set image ((image_dim - y - 1) * (image_dim + 1) + image_dim - 1 - x) v
  | 3 (* bottom-left*) -> Bytes.set image ((image_dim - y - 1) * (image_dim + 1) + x) v
      (* swap x and y *)
  | 4 (* top-left *) -> Bytes.set image (x * (image_dim + 1) + y) v
  | 5 (* top-right *) -> Bytes.set image (x * (image_dim + 1) + image_dim - 1 - y) v
  | 6 (* bottom-right*) -> Bytes.set image ((image_dim - x - 1) * (image_dim + 1) + image_dim - 1 - y) v
  | 7 (* bottom-left*) -> Bytes.set image ((image_dim - x - 1) * (image_dim + 1) + y) v
  | _ -> assert false in

 (* Sea Monster Mask *)
 (* ..................#. *)
 (* #....##....##....### *)
 (* .#..#..#..#..#..#... *)
 let get_monster image state y x =
  try
   image_get image state (y+0) (x+18) = '#' &&
   image_get image state (y+1) (x+ 0) = '#' &&
   image_get image state (y+1) (x+ 5) = '#' &&
   image_get image state (y+1) (x+ 6) = '#' &&
   image_get image state (y+1) (x+11) = '#' &&
   image_get image state (y+1) (x+12) = '#' &&
   image_get image state (y+1) (x+17) = '#' &&
   image_get image state (y+1) (x+18) = '#' &&
   image_get image state (y+1) (x+19) = '#' &&
   image_get image state (y+2) (x+ 1) = '#' &&
   image_get image state (y+2) (x+ 4) = '#' &&
   image_get image state (y+2) (x+ 7) = '#' &&
   image_get image state (y+2) (x+10) = '#' &&
   image_get image state (y+2) (x+13) = '#' &&
   image_get image state (y+2) (x+16) = '#'
  with Invalid_argument _ -> false in

 let mark_monster image state y x =
  try
   image_set image state (y+0) (x+18) 'O';
   image_set image state (y+1) (x+ 0) 'O';
   image_set image state (y+1) (x+ 5) 'O';
   image_set image state (y+1) (x+ 6) 'O';
   image_set image state (y+1) (x+11) 'O';
   image_set image state (y+1) (x+12) 'O';
   image_set image state (y+1) (x+17) 'O';
   image_set image state (y+1) (x+18) 'O';
   image_set image state (y+1) (x+19) 'O';
   image_set image state (y+2) (x+ 1) 'O';
   image_set image state (y+2) (x+ 4) 'O';
   image_set image state (y+2) (x+ 7) 'O';
   image_set image state (y+2) (x+10) 'O';
   image_set image state (y+2) (x+13) 'O';
   image_set image state (y+2) (x+16) 'O'
  with Invalid_argument _ -> () in

 let image = Buffer.to_bytes buf in
 for i = 0 to 7 do
  for y = 0 to image_dim - 3 do
   for x = 0 to image_dim - 20 do
    if get_monster image i y x then (
     if debug then Printf.printf "Monster Found! %d %d %d\n" i y x;
     mark_monster image i y x)
  done done done ;
 if debug then print_bytes image;
 Bytes.fold_left (fun a c -> if c = '#' then succ a else a) 0 image

let problem_21a () =
 let debug = true in
 let module SSet = Set.Make(String) in
 let parse s =
  let brk = String.index_from s 0 '(' in
  let ings = String.sub s 0 brk |> String.split_on_char ' ' in
  let alls = String.sub s (brk+10) (String.length s - brk - 11) |> String.split_on_char ',' |> List.map (String.trim) in
  (ings,alls) in
 let input = In_channel.(with_open_bin "21.txt" input_lines) |> List.map parse in
 let ingredients, allergens =
  List.fold_left (fun (ai,aa) (ings,alls) -> SSet.add_seq (List.to_seq ings) ai, SSet.add_seq (List.to_seq alls) aa)
  SSet.(empty, empty) input in
 let candidates_of_allergen allergen =
  List.fold_left (fun a (ings,alls) -> if List.mem allergen alls then SSet.inter (ings |> List.to_seq |> SSet.of_seq) a else a) ingredients input in
 let allergen_risk = SSet.to_seq allergens |> Seq.map candidates_of_allergen |> Seq.fold_left (fun a set -> SSet.union set a) SSet.empty in
 let safe = SSet.diff ingredients allergen_risk in
 if debug then SSet.iter print_endline safe ;
 List.fold_left (fun a (ings,_) -> List.fold_left (fun a ing -> if SSet.mem ing safe then succ a else a) 0 ings + a) 0 input

let problem_21b () =
 let debug = true in
 let module SSet = Set.Make(String) in
 let module SMap = Map.Make(String) in
 let parse s =
  let brk = String.index_from s 0 '(' in
  let ings = String.sub s 0 (brk-1) |> String.split_on_char ' ' in
  let alls = String.sub s (brk+10) (String.length s - brk - 11) |> String.split_on_char ',' |> List.map (String.trim) in
  (ings,alls) in
 let input = In_channel.(with_open_bin "21.txt" input_lines) |> List.map parse in
 let ingredients, allergens =
  List.fold_left (fun (ai,aa) (ings,alls) -> SSet.add_seq (List.to_seq ings) ai, SSet.add_seq (List.to_seq alls) aa)
  SSet.(empty, empty) input in
 let candidates_of_allergen allergen =
  List.fold_left (fun a (ings,alls) -> if List.mem allergen alls then SSet.inter (ings |> List.to_seq |> SSet.of_seq) a else a) ingredients input in
 let allergen_risk = SSet.to_seq allergens |> Seq.map candidates_of_allergen |> Seq.fold_left (fun a set -> SSet.union set a) SSet.empty in
 let safe = SSet.diff ingredients allergen_risk in
 if debug then SSet.iter print_endline safe ;
 let allergen_risk_assoc = ref (SSet.to_seq allergens |> Seq.map (fun al -> (al, candidates_of_allergen al)) |> List.of_seq) in
 let allergen_dict = ref SMap.empty in
 while SMap.cardinal !allergen_dict < SSet.cardinal allergens do
  !allergen_risk_assoc |>
  List.iter
   (fun (al, candidates) ->
    if SSet.cardinal candidates = 1
    then SSet.iter (fun ing -> allergen_dict := SMap.add al ing !allergen_dict) candidates; ()) ;
  let removable = SMap.to_seq !allergen_dict |> Seq.map snd |> SSet.of_seq in
  allergen_risk_assoc := List.map (fun (al,candidates) -> (al, SSet.diff candidates removable)) !allergen_risk_assoc
 done ;
 if debug then (print_endline "-----" ; SMap.to_seq !allergen_dict |> Seq.iter (fun (a,ing) -> Printf.printf "%s: %s\n" a ing) ; print_endline "-----") ;
 let res = Buffer.create 80 in
 SMap.to_seq !allergen_dict |> Seq.iter (fun (_,ing) -> Buffer.add_string res ing ; Buffer.add_char res ',') ;
 Buffer.truncate res (Buffer.length res - 1) ;
 Buffer.contents res
 
let problem_22a () =
 let input = In_channel.(with_open_bin "22.txt" input_lines) in
 let brk = List.find_index ((=)"") input |> Option.get in
 let p1 = List.to_seq input |> Seq.drop 1 |> Seq.take (brk - 1) |> Seq.map int_of_string |> Queue.of_seq in
 let p2 = List.to_seq input |> Seq.drop (brk+2) |> Seq.map int_of_string |> Queue.of_seq in
 while not (Queue.is_empty p1 || Queue.is_empty p2) do
  let c1 = Queue.take p1 and c2 = Queue.take p2 in
  if c1 > c2 then (Queue.add c1 p1 ; Queue.add c2 p1)
  else (Queue.add c2 p2 ; Queue.add c1 p2)
 done ;
 let len = max (Queue.length p1) (Queue.length p2) in
 Seq.append (Queue.to_seq p1) (Queue.to_seq p2) |>
 Seq.zip (Seq.ints 0 |> Seq.map ((-)len)) |> Seq.fold_left (fun a (b,c) -> a + b * c) 0

(* slow, but works (5s compiled) *)
(* alternative infinite loop protection, or *)
(* additional caching / memoization may help... *)
(* ilp is invoked *very* often (5213 times) *)
let problem_22b () =
 let debug = false in
 let input = In_channel.(with_open_bin "22.txt" input_lines) in
 let brk = List.find_index ((=)"") input |> Option.get in
 let p1 = List.to_seq input |> Seq.drop 1 |> Seq.take (brk - 1) |> Seq.map int_of_string |> Queue.of_seq in
 let p2 = List.to_seq input |> Seq.drop (brk+2) |> Seq.map int_of_string |> Queue.of_seq in
 let module SSet = Set.Make(String) in
 let stringify_buf = Buffer.create 80 in
 let stringify q1 q2 =
  Buffer.clear stringify_buf ;
  Queue.to_seq q1 |> Seq.iter (fun i -> Buffer.add_string stringify_buf (Int.to_string i) ; Buffer.add_char stringify_buf ',') ;
  if Buffer.length stringify_buf <> 0 then Buffer.truncate stringify_buf (Buffer.length stringify_buf - 1) ;
  Buffer.add_char stringify_buf '|';
  Queue.to_seq q2 |> Seq.iter (fun i -> Buffer.add_string stringify_buf (Int.to_string i) ; Buffer.add_char stringify_buf ',') ;
  if Buffer.length stringify_buf <> 0 then Buffer.truncate stringify_buf (Buffer.length stringify_buf - 1) ;
  Buffer.contents stringify_buf in
 (* returns true if player 1 wins *)
 let rec play infset q1 q2 =
  let break = ref false in
  while not (Queue.is_empty q1 || Queue.is_empty q2 || !break) do
   let c1 = Queue.take q1 and c2 = Queue.take q2 in
   let res =
    if Queue.length q1 >= c1 && Queue.length q2 >= c2
    then play (ref SSet.empty)
          (Queue.to_seq q1 |> Seq.take c1 |> Queue.of_seq)
          (Queue.to_seq q2 |> Seq.take c2 |> Queue.of_seq)
    else c1 > c2 in
   if res then
    (Queue.add c1 q1 ; Queue.add c2 q1)
   else
    (Queue.add c2 q2 ; Queue.add c1 q2) ;
   let played = stringify q1 q2 in
   if debug then print_endline played ;
   if SSet.mem played !infset then break := true ;
   if not !break then infset := SSet.add played !infset;
  done ;
  !break || Queue.is_empty q2 in
 let _ = play (ref SSet.empty) p1 p2 in
 let len = max (Queue.length p1) (Queue.length p2) in
 Seq.append (Queue.to_seq p1) (Queue.to_seq p2) |>
 Seq.zip (Seq.ints 0 |> Seq.map ((-)len)) |> Seq.fold_left (fun a (b,c) -> a + b * c) 0

(* crab shuffle *)
let problem_23a () =
 let example = false in
 let ring = In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_line) |> Option.get |>
  String.to_seq |>
  Seq.map (fun c -> Char.code c - 0x30) |>
  Queue.of_seq in
 let pick = Array.make 3 0 in
 let move () =
  let top = Queue.take ring in
  for i = 0 to 2 do pick.(i) <- Queue.take ring done;
  Queue.add top ring ;
  let rec adjust = function | 0 -> adjust 9 | n when Array.mem n pick -> adjust (pred n) | n -> n in
  let dest = adjust (top-1) in
  let next = Queue.peek ring in
  while Queue.peek ring <> dest do Queue.add (Queue.take ring) ring done ;
  Queue.add (Queue.take ring) ring; 
  for i = 0 to 2 do Queue.add pick.(i) ring done ;
  while Queue.peek ring <> next do Queue.add (Queue.take ring) ring done in
 for i = 1 to 100 do
  move ()
 done ;
 while Queue.peek ring <> 1 do Queue.add (Queue.take ring) ring done ;
 let _ = Queue.take ring in
 Queue.iter (print_int) ring ;
 print_newline ()

(* crab shuffle *)
(* use simulated dequeue / doubly linked list *)
(*
let problem_23b_alt () =
 let example = false in
 let debug = true in
 let input = In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_line) |> Option.get |>
  String.to_seq |>
  Seq.map (fun c -> Char.code c - 0x30) |> Array.of_seq in
 let deque = Array.init 1_000_001 (fun i -> ((i-1) lsl 24) + (i+1)) in

 (* nullify extra cell *)
 deque.(0) <- 0 ;

 (* helper functions *)
 (*let prev_get n = (n lsr 24) in*)
 let next_get n = (n land 0xFFFFFF) in
 let prev_set n n' = (n land 0xFFFFFF) + (n' lsl 24) in
 let next_set n n' = (n land 0xFFFFFF000000) + n' in
 let np_set p n = (p lsl 24) + n in

 (* set up ring deque *)
 deque.(1_000_000) <- next_set deque.(1_000_000) input.(0) ;
 deque.(input.(0)) <- np_set 1_000_000 input.(1) ;
 let input_len = Array.length input in
 for i = 1 to input_len - 2 do
  deque.(input.(i)) <- np_set input.(i-1) input.(i+1)
 done ;
 deque.(input.(input_len - 1)) <- np_set input.(input_len - 2) (input_len + 1) ;

 let head = ref input.(0) in
 let len = 1_000_000 in
 let pick = Array.make 3 0 in

 let move () =
  (* load pick array *)
  pick.(0) <- deque.(!head)    |> next_get ;
  pick.(1) <- deque.(pick.(0)) |> next_get ;
  pick.(2) <- deque.(pick.(1)) |> next_get ;
  let next_head = deque.(pick.(2)) |> next_get in

  (* detach picks from deque *)
  deque.(!head)     <- next_set deque.(!head) next_head ;
  deque.(next_head) <- prev_set deque.(next_head) !head ;

  let rec adjust = function | 0 -> adjust len | n when Array.mem n pick -> adjust (pred n) | n -> n in
  let dest = adjust (!head-1) in

  (* insert picks after dest *)
  let after_dest = deque.(dest) |> next_get in
  deque.(dest)       <- next_set deque.(dest) pick.(0) ;
  deque.(pick.(0))   <- prev_set deque.(pick.(0)) dest ;
  deque.(pick.(2))   <- next_set deque.(pick.(2)) after_dest ;
  deque.(after_dest) <- prev_set deque.(after_dest) pick.(2) ;
 
  (* advance head *)
  head := next_head in

 for i = 1 to 10_000_000 do
  move () ;
  if debug && i mod 1_000_000 = 0 then (Printf.printf "Progress: %d%%\n" (i / 100_000) ; flush stdout)
 done ;
 let a = deque.(1) |> next_get in
 let b = deque.(a) |> next_get in
 (a,b,a*b)
*)

(* crab shuffle *)
(* use simulated single linked list *)
(* prev_get is not necessary! *)
let problem_23b () =
 let example = false in
 let debug = true in
 let input = In_channel.(with_open_bin (if example then "23e.txt" else "23.txt") input_line) |> Option.get |>
  String.to_seq |>
  Seq.map (fun c -> Char.code c - 0x30) |> Array.of_seq in
 let ll = Array.init 1_000_001 (fun i -> i+1) in

 (* set up ring ll *)
 ll.(1_000_000) <- input.(0) ;
 let input_len = Array.length input in
 for i = 0 to input_len - 2 do ll.(input.(i)) <- input.(i+1) done ;
 ll.(input.(input_len - 1)) <- (input_len + 1) ;

 (* use extra cell to store head *)
 ll.(0) <- input.(0) ;

 let len = 1_000_000 in
 let pick = Array.make 3 0 in

 let move () =
  (* load pick array *)
  pick.(0) <- ll.(ll.(0)) ;
  pick.(1) <- ll.(pick.(0)) ;
  pick.(2) <- ll.(pick.(1)) ;

  (* detach picks from ll *)
  ll.(ll.(0)) <- ll.(pick.(2));

  let rec adjust = function | 0 -> adjust len | n when Array.mem n pick -> adjust (pred n) | n -> n in
  let dest = adjust (ll.(0)-1) in

  (* insert picks after dest *)
  ll.(pick.(2)) <- ll.(dest) ;
  ll.(dest)     <- pick.(0) ;
 
  (* advance head *)
  ll.(0) <- ll.(ll.(0)) in

 for i = 1 to 10_000_000 do
  move () ;
  if debug && i mod 1_000_000 = 0 then (Printf.printf "Progress: %d%%\n" (i / 100_000) ; flush stdout)
 done ;
 let a = ll.(1) in
 let b = ll.(a) in
 (a,b,a*b)

(* remap hexagonal coordinate system to cartesian w/ *)
(* ne = (1, 0), sw = (-1, 0), nw = (0, 1), se = (0. -1) *)
(* e = ne + se = (1, -1) ; w = nw + sw = (-1, 1) *)

let problem_24a () =
 let module XYSet = Set.Make(struct type t = int * int let compare = compare end) in
 let xy_add (x,y) (x',y') = (x+x',y+y') in
 let rec parse a = function
 | [] -> a
 | 'e'::tl -> parse (xy_add a (1,~-1)) tl
 | 'w'::tl -> parse (xy_add a (~-1,1)) tl
 | 'n'::'e'::tl -> parse (xy_add a (1,0)) tl
 | 'n'::'w'::tl -> parse (xy_add a (0,1)) tl
 | 's'::'w'::tl -> parse (xy_add a (~-1,0)) tl
 | 's'::'e'::tl -> parse (xy_add a (0,~-1)) tl
 | c::tl -> raise_notrace @@ Invalid_argument (Printf.sprintf "Invalid Character @ %c" c) in
 let parse_line s = parse (0,0) (s |> String.to_seq |> List.of_seq) in
 let input = In_channel.(with_open_bin "24.txt" input_lines) |> List.map parse_line in
 let black_tiles = List.fold_left (fun a xy -> if XYSet.mem xy a then XYSet.remove xy a else XYSet.add xy a) XYSet.empty input in
 XYSet.cardinal black_tiles
 
let problem_24b () =
 let module XYSet = Set.Make(struct type t = int * int let compare = compare end) in
 let xy_add (x,y) (x',y') = (x+x',y+y') in
 let rec parse a = function
 | [] -> a
 | 'e'::tl -> parse (xy_add a (1,~-1)) tl
 | 'w'::tl -> parse (xy_add a (~-1,1)) tl
 | 'n'::'e'::tl -> parse (xy_add a (1,0)) tl
 | 'n'::'w'::tl -> parse (xy_add a (0,1)) tl
 | 's'::'w'::tl -> parse (xy_add a (~-1,0)) tl
 | 's'::'e'::tl -> parse (xy_add a (0,~-1)) tl
 | c::tl -> raise_notrace @@ Invalid_argument (Printf.sprintf "Invalid Character @ %c" c) in
 let parse_line s = parse (0,0) (s |> String.to_seq |> List.of_seq) in
 let adjacents xy = List.to_seq [1,~-1;~-1,1;1,0;0,1;~-1,0;0,~-1] |> Seq.map (xy_add xy) in
 let adjacent_whites xyset =
  xyset |> XYSet.to_seq |>
  Seq.map adjacents |> Seq.concat |>
  XYSet.of_seq |> Fun.flip XYSet.diff xyset in
 let flip_from_black xyset xy =
  match adjacents xy |> Seq.fold_left (fun a xy -> if XYSet.mem xy xyset then succ a else a) 0 with
  | 1 | 2 -> false | _ -> true in
 let flip_from_white xyset xy =
  if adjacents xy |> Seq.fold_left (fun a xy -> if XYSet.mem xy xyset then succ a else a) 0 = 2 then true else false in
 let input = In_channel.(with_open_bin "24.txt" input_lines) |> List.map parse_line in
 (* initial state is the same as 24a *)
 let black_tiles = List.fold_left (fun a xy -> if XYSet.mem xy a then XYSet.remove xy a else XYSet.add xy a) XYSet.empty input in
 let next_day xyset =
  let whites' =
   XYSet.to_seq xyset |> Seq.filter (flip_from_black xyset) in
  let blacks' =
   adjacent_whites xyset |> XYSet.to_seq |> Seq.filter (flip_from_white xyset) in
  XYSet.diff xyset (XYSet.of_seq whites') |>
  XYSet.union (XYSet.of_seq blacks') in
 let n = 100 in
 (* alternate iterator styles *)
 (* speed is measured w/o any optimization *)
(*
  (* for debugging *)
  let final_tiles = Seq.iterate next_day black_tiles |> Seq.map XYSet.cardinal |> Seq.take (n+1) in
  List.of_seq final_tiles

  (* medium speed : ~4s *)
  let iterator = Seq.repeat () |> Seq.take n in
  let final_tiles = Seq.fold_left (fun a _ -> next_day a) black_tiles iterator in
  XYSet.cardinal final_tiles

  (* slightly slower speed : ~4.2s *)
  let final_tiles = Seq.iterate next_day black_tiles |> Seq.map XYSet.cardinal |> Seq.drop n in
  Seq.uncons final_tiles |> Option.get |> fst
*)

 (* fastest: ~3.5s *)
 let final = ref black_tiles in
 for i=1 to n do final := next_day !final done ;
 XYSet.cardinal !final

(* cryptographic handshake *)

let problem_25a () =
 let step n a = (a * n) mod 20201227 in
 let calc sub loop_size =
  let res = ref 1 in for i = 1 to loop_size do res := step sub !res done ; !res in
 let find_ls (pk1, pk2) =
  let res = ref 1 and count = ref 0 in
  while pk1 <> !res && pk2 <> !res do res := step 7 !res; incr count done;
  (pk1 = !res, !count) in
 let pub_keys = In_channel.(with_open_bin "25.txt" input_lines) |> List.map int_of_string |> Array.of_list in
 let first, ls = find_ls (pub_keys.(0), pub_keys.(1)) in
 if first then 
  calc pub_keys.(1) ls
 else
  calc pub_keys.(0) ls
