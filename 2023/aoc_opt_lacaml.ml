(* packages: lacaml *)

let problem_24b () =
 let module HailXYZ = struct
  type t = {x : float; y: float; z : float; vx : float; vy : float; vz: float}
  let of_string s =
   let (x,y,z,vx,vy,vz) = Scanf.sscanf s " %d, %d, %d @ %d, %d, %d " (fun a b c d e f -> Float.(of_int a, of_int b, of_int c, of_int d, of_int e, of_int f)) in
   {x;y;z;vx;vy;vz}
  let to_string h = Printf.sprintf "%f, %f, %f @ %f, %f %f" h.x h.y h.z h.vx h.vy h.vz
  (* for testing *)
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
   (* system of equations : solve for t and s ; if t=s then we have a solution *)
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
  let error_of_intersection_at_t line h1 t =
   let x = h1.x +. h1.vx *. t
   and y = h1.y +. h1.vy *. t
   and z = h1.z +. h1.vz *. t in
   Float.abs (line.vy *. line.vz *. (x -. line.x) -.
              line.vx *. line.vz *. (y -. line.y)) +.
   Float.abs (line.vy *. line.vz *. (x -. line.x) -.
              line.vx *. line.vy *. (z -. line.z))
  let parallel h1 h2 = (h2.vy *. h1.vx = h1.vy *. h2.vx) && (h2.vz *. h1.vx = h1.vz *. h2.vx)
 end in
 let convert_mat a = Bigarray.(Array2.init Float64 Fortran_layout (Array.length a) (Array.length a) (fun y x -> a.(y-1).(x-1))) in
 let convert_vec b = Bigarray.(Array2.init Float64 Fortran_layout (Array.length b) 1 (fun y x -> b.(y-1))) in
 let solve a b =
  let open Lacaml.D in
  gesv a b in
 let example = false in
 let input =
  In_channel.(with_open_bin (if example then "24e.txt" else "24.txt")
  (fun ic -> Seq.of_dispenser (fun () -> input_line ic) |>
   Seq.map HailXYZ.of_string |>
   Array.of_seq)) in
  let (a,b) = HailXYZ.hail_mary_matrix input.(0) input.(1) input.(2) in
  let (a_mat, b_vec) = (convert_mat a, convert_vec b) in
  solve a_mat b_vec;
  for i = 1 to Bigarray.Array2.dim1 b_vec do Printf.printf "%20.0f" b_vec.{i,1} ; print_newline () done;
  Printf.printf "%20.0f\n" @@ b_vec.{1,1} +. b_vec.{2,1} +. b_vec.{3,1}
  
(*
 let _ = HailXYZ.intersection HailXYZ.{x=24.;y=13.;z=10.;vx= ~-. 3.; vy = 1.; vz = 2.} input.(0) in
 let line = HailXYZ.intersecting_line_at_t input.(0) input.(1) 5. 3. in
 Printf.printf "%s\n" HailXYZ.(to_string line);
 HailXYZ.intersection line input.(2)
*)
 (*HailXYZ.hail_mary_matrix input.(0) input.(1) input.(2)*)

(*  270392223533307 463714142194110 273041846062208 26 -331 53 *) (* using R *)

 (*HailXYZ.intersection HailXYZ.({x = 270392223533307. ; y = 463714142194110. ; z = 273041846062208. ; vx = 26. ; vy= ~-. 331. ; vz = 53.}) input.(1)*)

let () = problem_24b ()
