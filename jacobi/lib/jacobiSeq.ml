open Sparse
open Util

exception TooManyIters

let calc_xi (b : floatarray) (x: floatarray) (i : int) (row : floatarray) : float =
  (Float.Array.get b i -. dot_product row x) /. Float.Array.get row i +. Float.Array.get x i

let calc_x (matrix : floatarray array) (b : floatarray) (x : floatarray) = 
  let x_arr = Array.mapi (calc_xi b x) matrix in
  arr_to_floatarr x_arr

let rec jacobi_aux (matrix : floatarray array) (b: floatarray) (x : floatarray)
      (iters : int) : floatarray = 
  if iters > 1000 then raise TooManyIters else
  let new_x = calc_x matrix b x in
  let diff = Float.Array.map2 (-.) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq < 0.0001 then (
    (* print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations.");  *)
    new_x
  ) else jacobi_aux matrix b new_x (iters + 1)

let jacobi_seq (matrix : floatarray array) (b : floatarray) : floatarray =
  let n = Array.length matrix in
  let _ = (
    assert (Float.Array.length b = n);
    Array.iter (fun row -> assert(Float.Array.length row = n)) matrix
  ) in
  let init = Float.Array.make n 0. in
  let x = jacobi_aux matrix b init 0 in
  let res = Float.Array.map2 (-.) (mat_vec_mult matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    (* print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq)); *)
    x
  )

let rec jacobi_sparse_aux (matrix : Square.t) (b: floatarray) 
      (x : floatarray) (iters : int) : floatarray = 
  if iters > 1000 then raise TooManyIters else
  let ax = Square.mult_vec matrix x in
  let b_minus_ax = Float.Array.map2 (-.) b ax in
  let div_D = Float.Array.map2 (/.) b_minus_ax matrix.diag in
  let new_x = Float.Array.map2 (+.) x div_D in
  let diff = Float.Array.map2 (-.) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq < 0.0001 then (
    (* print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations.");  *)
    new_x
  ) else jacobi_sparse_aux matrix b new_x (iters + 1)

let jacobi_sparse (matrix : Square.t) (b : floatarray) : floatarray =
  let n = matrix.n in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let x = jacobi_sparse_aux matrix b init 0 in
  let res = Float.Array.map2 (-.) (Square.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    (* print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq)); *)
    x
  )
