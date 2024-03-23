open Util

exception TooManyIters

let calc_xi (b : floatarray) (x: floatarray) (i : int) (row : floatarray) : float =
  (Float.Array.get b i -. dot_product row x) /. Float.Array.get row i +. Float.Array.get x i

let calc_x (matrix : Dense.t) (b : floatarray) (x : floatarray) = 
  let x_arr = Array.mapi (calc_xi b x) matrix.vals in
  arr_to_floatarr x_arr

let rec jacobi_aux (matrix : Dense.t) (b: floatarray) (x : floatarray)
      (iters : int) : floatarray = 
  if iters > 1000 then raise TooManyIters else
  let new_x = calc_x matrix b x in
  let diff = Float.Array.map2 (-.) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq < 0.0001 then (
    (* print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations.");  *)
    new_x
  ) else jacobi_aux matrix b new_x (iters + 1)

let jacobi_seq (matrix : Dense.t) (b : floatarray) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let init = Float.Array.make n 0. in
  let x = jacobi_aux matrix b init 0 in
  let res = Float.Array.map2 ( -. ) (Dense.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    (* print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq)); *)
    x
  )

let rec jacobi_sparse_aux (matrix : Sparse.t) (b: floatarray) 
      (d_inv : floatarray) (x : floatarray) (iters : int) : floatarray = 
  if iters > 1000 then raise TooManyIters else
  let lu_x = Sparse.mult_LU matrix x 1 in
  let b_minus = Float.Array.map2 ( -. ) b lu_x in
  let new_x = Float.Array.map2 ( *. ) d_inv b_minus in 
  let diff = Float.Array.map2 ( -. ) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq < 0.0001 then (
    (* print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations.");  *)
    new_x
  ) else jacobi_sparse_aux matrix b d_inv new_x (iters + 1)

let jacobi_sparse (matrix : Sparse.t) (b : floatarray) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let d_inv = Float.Array.map (fun x -> 1. /. x) (Sparse.diag matrix) in
  let x = jacobi_sparse_aux matrix b d_inv init 0 in
  let res = Float.Array.map2 ( -. ) (Sparse.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    (* print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq)); *)
    x
  )
