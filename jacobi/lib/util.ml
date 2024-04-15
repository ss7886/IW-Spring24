let listlist_to_arrarr (list :  float list list) : floatarray array =
  Array.of_list (List.map (Float.Array.of_list) list)

let print_vector (vec : floatarray) : unit = 
  Float.Array.iter (fun x -> (Printf.printf "%f " x)) vec;
  print_newline ()

let print_ints (arr : int array) : unit =
  Array.iter (fun x -> (Printf.printf "%d " x)) arr;
  print_newline ()

(* Checks whether v1 and v2 are element-wise equal (1e-9 precision). *)
let vec_eq (v1 : floatarray) (v2 : floatarray) : bool =
  let delta = 0.000000001 in
  if Float.Array.length v1 != Float.Array.length v2 then false else
  let rec aux (i : int) : bool =
    if i = Float.Array.length(v1) then true else
      let f1 = Float.Array.get v1 i in
      let f2 = Float.Array.get v2 i in
      if abs_float (f1 -. f2) < delta then aux (i + 1) else (
      print_float f1;
      print_string " =/= ";
      print_float f2;
      print_newline ();
      false
    )
  in aux 0

let vec_close (v1 : floatarray) (v2 : floatarray) (delta : float) : bool = 
  if Float.Array.length v1 != Float.Array.length v2 then false else
  let rec aux (i : int) : bool =
    if i = Float.Array.length(v1) then true else
      let f1 = Float.Array.get v1 i in
      let f2 = Float.Array.get v2 i in
      if abs_float (f1 -. f2) < delta then aux (i + 1) else (
      print_float f1;
      print_string " =/= ";
      print_float f2;
      print_newline ();
      false
    )
  in aux 0

let arr_to_floatarr (arr : float array) : floatarray = 
  Float.Array.map_from_array Fun.id arr

let dot_product (vec1 : floatarray) (vec2 : floatarray) : float = 
  Float.Array.fold_left (+.) 0. (Float.Array.map2 ( *.) vec1 vec2)

(* Computes a partial dot product of n elements starting at start1 in vec1 and
 * start2 in vec2. *)
let partial_dot_product (vec1 : floatarray) (start1 : int) (vec2 : floatarray)
    (start2 : int) (n : int) : float = 
  let _ = assert (start1 >= 0 && start1 + n <= Float.Array.length vec1) in
  let _ = assert (start2 >= 0 && start2 + n <= Float.Array.length vec2) in
  let rec aux (i1 : int) (i2 : int) : float = 
    if i1 = start1 + n then 0. else
      let x1 = Float.Array.get vec1 i1 in
      let x2 = Float.Array.get vec2 i2 in
      x1 *. x2 +. aux (i1 + 1) (i2 + 1)
  in
  aux start1 start2

let dot_product2 (vec1 : floatarray) (vec2 : floatarray) : float =
  let n = Float.Array.length vec1 in
  let _ = assert (Float.Array.length vec2 = n) in
  partial_dot_product vec1 0 vec2 0 n

let timer (f: unit -> 'a) (iters: int) : 'a = 
  let t = Unix.gettimeofday () in
  let rec aux (i : int) : unit =
    if i = iters - 1 then () else (
      f ();
      aux (i + 1)
    )
  in (
    aux 0;
    print_string "Time to run ";
    print_int iters;
    print_string " iters: ";
    print_float (Unix.gettimeofday () -. t);
    print_endline " seconds.";
    f ()
  )
