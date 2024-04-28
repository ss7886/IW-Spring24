open SparsePar
open Util
open UtilPar

(* exception TooManyIters *)
exception DoesNotConverge

let rec naive_aux (matrix : Sparse.t) (b: floatarray) (d_inv : floatarray) 
      (x : floatarray) (iters : int) (old_diff_sq : float) (p : int) : floatarray = 
  if iters > 1000 then (
    print_endline "maximum iterations reached";
    x
  ) else
  let lu_x = par_mult_LU matrix x 1 p in
  let b_minus = Float.Array.map2 ( -. ) b lu_x in
  let new_x = Float.Array.map2 ( *. ) d_inv b_minus in 
  let diff = Float.Array.map2 (-.) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq > old_diff_sq then raise DoesNotConverge else ();
  if diff_sq < 0.000001 then (
    print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations."); 
    new_x
  ) else naive_aux matrix b d_inv new_x (iters + 1) diff_sq p

let jacobi_par_naive (matrix : Sparse.t) (b : floatarray) (p : int) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let d_inv = Float.Array.map (fun x -> 1. /. x) (Sparse.diag matrix) in
  let x = naive_aux matrix b d_inv init 0 Float.max_float p in
  let res = Float.Array.map2 (-.) (Sparse.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq));
    x
  )

let rec naive_aux_2 (matrix : Sparse.t) (b: floatarray) (d_inv : floatarray) 
      (x : floatarray) (iters : int) (old_diff_sq : float) (p : int) : floatarray = 
  if iters > 1000 then (
    print_endline "maximum iterations reached";
    x
  ) else
  let lu_x = par_mult_LU matrix x 1 p in
  let b_minus = par_float_map2 ( -. ) b lu_x p in
  let new_x = par_float_map2 ( *. ) d_inv b_minus p in 
  let diff = par_float_map2 (-.) new_x x p in
  let diff_sq = dot_product diff diff in
  if diff_sq > old_diff_sq then raise DoesNotConverge else ();
  if diff_sq < 0.000001 then (
    print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations."); 
    new_x
  ) else naive_aux_2 matrix b d_inv new_x (iters + 1) diff_sq p

let jacobi_par_naive_2 (matrix : Sparse.t) (b : floatarray) (p : int) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let d_inv = Float.Array.map (fun x -> 1. /. x) (Sparse.diag matrix) in
  let x = naive_aux_2 matrix b d_inv init 0 Float.max_float p in
  let res = par_float_map2 (-.) (Sparse.mult_vec matrix x) b p in
  let res_sq = dot_product res res in (
  let _ = res_sq in ();
    print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq));
    x
  )

let rec block_aux (matrix : Sparse.t) (b: floatarray) 
      (d_LU : (Dense.t * Dense.t) array) (x : floatarray) (block_size : int) 
      (iters : int) (old_diff_sq : float) (p : int) : floatarray = 
  if iters > 1000 then (
    print_endline "maximum iterations reached";
    x
  ) else
  let n = matrix.num_rows in
  let num_blocks = Array.length d_LU in
  let overflow = n mod block_size in
  let lu_x = par_mult_LU matrix x block_size p in
  let b_minus = Float.Array.map2 ( -. ) b lu_x in
  let solve_block (i : int) (mat_LU : Dense.t * Dense.t) : floatarray = 
    let b_sub = Float.Array.sub b_minus (i * block_size) 
      (if i = num_blocks - 1 && overflow > 0 then overflow else block_size) in
    let mat_L, mat_U = mat_LU in
    Dense.solve_LU mat_L mat_U b_sub
  in
  let x_subs = par_mapi solve_block d_LU p in
  let new_x = Float.Array.concat (Array.to_list x_subs) in
  let diff = Float.Array.map2 ( -. ) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq > old_diff_sq then raise DoesNotConverge else ();
  if diff_sq < 0.000001 then (
    print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations."); 
    new_x
  ) else block_aux matrix b d_LU new_x block_size (iters + 1) diff_sq p

let block_jacobi_par (matrix : Sparse.t) (b : floatarray) (block_size : int)
      (p : int) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let d_block = Sparse.diag_block matrix block_size in
  let d_LU = par_map Dense.decomp_LU d_block p in
  let x = block_aux matrix b d_LU init block_size 0 Float.max_float p in
  let res = Float.Array.map2 ( -. ) (Sparse.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq));
    x
  )
