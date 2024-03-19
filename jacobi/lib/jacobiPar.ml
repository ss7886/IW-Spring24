open Sparse
open SparsePar
open Util

exception TooManyIters
let _ = par_mult_vec

let naive_aux (p : int) (matrix : Matrix.t) (b: floatarray) 
      (x : floatarray) (iters : int) : floatarray = 
  let _ = p, matrix, b, x, iters in
  if iters > 1000 then raise TooManyIters else
  Float.Array.make matrix.num_cols 0.
  (* if iters > 1000 then raise TooManyIters else
  let ax = par_mult_vec p matrix x in
  let b_minus_ax = Float.Array.map2 (-.) b ax in
  let div_D = Float.Array.map2 (/.) b_minus_ax matrix.diag in
  let new_x = Float.Array.map2 (+.) x div_D in
  let diff = Float.Array.map2 (-.) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq < 0.0001 then (
    (* print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations.");  *)
    new_x
  ) else naive_aux p matrix b new_x (iters + 1) *)

let jacobi_par_naive (p : int) (matrix : Matrix.t) (b : floatarray) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let x = naive_aux p matrix b init 0 in
  let res = Float.Array.map2 (-.) (Matrix.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    (* print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq)); *)
    x
  )