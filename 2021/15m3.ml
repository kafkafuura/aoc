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

let () =
 Printf.printf "%d\n" @@ p15_3 ()
