open SparsePar
open Util

exception TooManyIters
let _ = par_mult_vec

let rec naive_aux (matrix : Sparse.t) (b: floatarray) (d_inv : floatarray) 
      (x : floatarray) (iters : int) (p : int) : floatarray = 
  if iters > 1000 then raise TooManyIters else
  let lu_x = par_mult_LU matrix x 1 p in
  let b_minus = Float.Array.map2 ( -. ) b lu_x in
  let new_x = Float.Array.map2 ( *. ) d_inv b_minus in 
  let diff = Float.Array.map2 (-.) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq < 0.000001 then (
    (* print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations.");  *)
    new_x
  ) else naive_aux matrix b d_inv new_x (iters + 1) p

let jacobi_par_naive (matrix : Sparse.t) (b : floatarray) (p : int) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let d_inv = Float.Array.map (fun x -> 1. /. x) (Sparse.diag matrix) in
  let x = naive_aux matrix b d_inv init 0 p in
  let res = Float.Array.map2 (-.) (Sparse.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    (* print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq)); *)
    x
  )