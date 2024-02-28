let listlist_to_arrarr (list :  float list list) : floatarray array =
  Array.of_list (List.map (Float.Array.of_list) list)

let print_vector (vec : floatarray) : unit = 
  Float.Array.iter (fun x' -> (print_float x'; print_string " ")) vec;
  print_newline ()

let vec_eq (v1 : floatarray) (v2 : floatarray) : bool =
  let rec aux (i : int) : bool =
    if i = Float.Array.length(v1) then true else
      let f1 = Float.Array.get v1 i in
      let f2 = Float.Array.get v2 i in
      if f1 = f2 then aux (i + 1) else (
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

let mat_vec_mult (matrix : floatarray array) (vec : floatarray) : floatarray = 
  arr_to_floatarr (Array.map (dot_product vec) matrix)
