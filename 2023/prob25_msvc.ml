(* use spectral graph analysis to determine best cut! *)
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

 (*let input = In_channel.(with_open_bin (if example then "25e.txt" else "25.txt") input_lines) in*) (* input_lines unavailable in ocaml4 *)
 (* split on char creates a 0-length final line we must remove! *)
 let input = In_channel.(with_open_bin (if example then "25e.txt" else "25.txt") input_all) |> String.split_on_char '\n' |> List.filter ((<>)"") in

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

 (* use direct method *)
 (* Array0 Helpers *)
 (* let (~!) = Bigarray.Array0.get in *)
 (* let (<~) = Bigarray.Array0.set in *)

 (* initialize parameters *)
 let work  = Bigarray.Array1.create Bigarray.Float64 Bigarray.fortran_layout 1
 and iwork = Bigarray.Array1.create Bigarray.Int32   Bigarray.fortran_layout 1
 and m     = Bigarray.Array0.create Bigarray.Int32   Bigarray.fortran_layout
 and w     = Bigarray.Array1.create Bigarray.Float64 Bigarray.fortran_layout 2
 and z     = Bigarray.Array2.create Bigarray.Float64 Bigarray.fortran_layout !mat_size 2
 and isuppz= Bigarray.Array1.create Bigarray.Int32   Bigarray.fortran_layout (!mat_size * 2)
 and info  = Bigarray.Array0.create Bigarray.Int32   Bigarray.fortran_layout in

 (* find optimal work_sizes *)
 Lapack.dsyevr_unsafe
  ~jobz:'V' ~range:'I' ~uplo:'U'
  ~n:(!mat_size) ~a:(adj_mat) ~lda:(!mat_size) ~vl:0. ~vu:0. ~il:1 ~iu:2 ~abstol:0.
  ~m ~w ~z ~ldz:(!mat_size) ~isuppz ~work ~lwork:(~-1) ~iwork ~liwork:(~-1) ~info ;
 let lwork = int_of_float work.{1}
 and liwork = Int32.to_int iwork.{1} in
 (* reallocate work vectors *)
 let work  = Bigarray.Array1.create Bigarray.Float64 Bigarray.fortran_layout lwork
 and iwork = Bigarray.Array1.create Bigarray.Int32   Bigarray.fortran_layout liwork in

 (* use syevr/syevx w / fiedler eigenvectors + iterate 3 times *)
 for i = 1 to 3 do
 Lapack.dsyevr_unsafe
  ~jobz:'V' ~range:'I' ~uplo:'U'
  ~n:(!mat_size) ~a:(adj_mat) ~lda:(!mat_size) ~vl:0. ~vu:0. ~il:1 ~iu:2 ~abstol:0.
  ~m ~w ~z ~ldz:(!mat_size) ~isuppz ~work ~lwork ~iwork ~liwork ~info ;
 best_cut z ; restore_adj ()
 done ;

 (* final iteration to find group sizes *)
 Lapack.dsyevr_unsafe
  ~jobz:'V' ~range:'I' ~uplo:'U'
  ~n:(!mat_size) ~a:(adj_mat) ~lda:(!mat_size) ~vl:0. ~vu:0. ~il:1 ~iu:2 ~abstol:0.
  ~m ~w ~z ~ldz:(!mat_size) ~isuppz ~work ~lwork ~iwork ~liwork ~info ;

 (* lacaml (simplified) version *)
(*
 let (m, w, z, isuppz) = Lacaml.D.(syevr ~n:(!mat_size) ~vectors:(true) ~range:(`I (1,2)) adj_mat) in
 best_cut z ; restore_adj ();
 let (m, w, z, isuppz) = Lacaml.D.(syevr ~n:(!mat_size) ~vectors:(true) ~range:(`I (1,2)) adj_mat) in
 best_cut z ; restore_adj ();
 let (m, w, z, isuppz) = Lacaml.D.(syevr ~n:(!mat_size) ~vectors:(true) ~range:(`I (1,2)) adj_mat) in
 best_cut z ; restore_adj ();
*)

 (* count group sizes using eigenvectors where λ = 0 ; since there are only two groups, you only need the first (zero v. non-zero) *)
 (* let (m, w, z, isuppz) = Lacaml.D.(syevr ~n:(!mat_size) ~vectors:(true) ~range:(`I (1,2)) adj_mat) in *)
 let abstol = 1e-5 in
 let counter = ref 0 in
 for j = 1 to !mat_size do if Float.abs (Float.sub z.{j,1} z.{1,1}) < abstol then incr counter done ;
 Printf.printf "sizes : %i,%i\nans: %i\n" !counter (!mat_size - !counter) ((!mat_size - !counter) * !counter)

let _ = problem_25a ()
