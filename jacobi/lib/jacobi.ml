exception TooManyIters

let dot_product (vec1 : floatarray) (vec2 : floatarray) = 
  Float.Array.fold_left ( +. ) 0. (Float.Array.map2 ( *. ) vec1 vec2)

let calc_xi (b : floatarray) (x: floatarray) (i : int) (row : floatarray) : float =
  (Float.Array.get b i -. dot_product row x) /. Float.Array.get row i +. Float.Array.get x i

let calc_x (matrix : floatarray array) (b : floatarray) (x : floatarray) = 
  let x_arr = Array.mapi (calc_xi b x) matrix in
  Float.Array.map_from_array (Fun.id) x_arr

let jacobi_aux (matrix : floatarray array) (b: floatarray) (x : floatarray)
  (iters : int) : floatarray = 
  if iters > 1000 then raise TooManyIters else
  let new_x = calc_x matrix b x in
  new_x


let jacobi_seq (matrix : floatarray array) (b : floatarray) : floatarray =
  let n = Array.length matrix in
  let _ = (
    assert (Float.Array.length b = n);
    Array.iter (fun row -> assert(Float.Array.length row = n)) matrix
  ) in
  let init = Float.Array.make n 0. in
  jacobi_aux matrix b init 0


let _ = print_endline "Hello World!"
