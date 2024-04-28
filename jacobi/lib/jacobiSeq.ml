open Util

(* exception TooManyIters *)
exception DoesNotConverge

let calc_xi (b : floatarray) (x: floatarray) (i : int) (row : floatarray) : float =
  (Float.Array.get b i -. dot_product row x) /. Float.Array.get row i +. Float.Array.get x i

let calc_x (matrix : Dense.t) (b : floatarray) (x : floatarray) = 
  let x_arr = Array.mapi (calc_xi b x) matrix.vals in
  arr_to_floatarr x_arr

let rec jacobi_aux (matrix : Dense.t) (b: floatarray) (x : floatarray)
      (iters : int) (old_diff_sq : float): floatarray = 
  if iters > 1000 then (
    print_endline "maximum iterations reached";
    x
  ) else
  let new_x = calc_x matrix b x in
  let diff = Float.Array.map2 (-.) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq > old_diff_sq then raise DoesNotConverge else
  if diff_sq < 0.000001 then (
    (* print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations.");  *)
    new_x
  ) else jacobi_aux matrix b new_x (iters + 1) diff_sq

let jacobi_seq (matrix : Dense.t) (b : floatarray) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let init = Float.Array.make n 0. in
  let x = jacobi_aux matrix b init 0 Float.max_float in
  let res = Float.Array.map2 ( -. ) (Dense.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    (* print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq)); *)
    x
  )

let rec jacobi_sparse_aux (matrix : Sparse.t) (b: floatarray) (d_inv : floatarray)
      (x : floatarray) (iters : int) (old_diff_sq : float) : floatarray = 
  if iters > 1000 then (
    print_endline "maximum iterations reached";
    x
  ) else
  let lu_x = Sparse.mult_LU matrix x 1 in
  let b_minus = Float.Array.map2 ( -. ) b lu_x in
  let new_x = Float.Array.map2 ( *. ) d_inv b_minus in 
  let diff = Float.Array.map2 ( -. ) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq > old_diff_sq then raise DoesNotConverge else ();
  if diff_sq < 0.000001 then (
    print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations."); 
    new_x
  ) else jacobi_sparse_aux matrix b d_inv new_x (iters + 1) diff_sq

let jacobi_sparse (matrix : Sparse.t) (b : floatarray) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let d_inv = Float.Array.map (fun x -> 1. /. x) (Sparse.diag matrix) in
  let x = jacobi_sparse_aux matrix b d_inv init 0 Float.max_float in
  let res = Float.Array.map2 ( -. ) (Sparse.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    Printf.printf "Squared Error (||Ax - b||^2): %f" res_sq; print_newline ();
    x
  )

let rec block_jacobi_aux (matrix : Sparse.t) (b: floatarray) 
      (d_LU : (Dense.t * Dense.t) array) (x : floatarray) (block_size : int) 
      (iters : int) (old_diff_sq : float) : floatarray = 
  if iters > 1000 then (
    print_endline "maximum iterations reached";
    x
  ) else
  let n = matrix.num_rows in
  let num_blocks = Array.length d_LU in
  let overflow = n mod block_size in
  let lu_x = Sparse.mult_LU matrix x block_size in
  let b_minus = Float.Array.map2 ( -. ) b lu_x in
  let solve_block (i : int) (mat_LU : Dense.t * Dense.t) : floatarray = 
    let b_sub = Float.Array.sub b_minus (i * block_size) 
      (if i = num_blocks - 1 && overflow > 0 then overflow else block_size) in
    let mat_L, mat_U = mat_LU in
    Dense.solve_LU mat_L mat_U b_sub
  in
  let x_subs = Array.mapi solve_block d_LU in
  let new_x = Float.Array.concat (Array.to_list x_subs) in
  let diff = Float.Array.map2 ( -. ) new_x x in
  let diff_sq = dot_product diff diff in
  if diff_sq > old_diff_sq then raise DoesNotConverge else ();
  if diff_sq < 0.000001 then (
    print_endline ("Converged in " ^ (string_of_int iters) ^ " iterations."); 
    new_x
  ) else block_jacobi_aux matrix b d_LU new_x block_size (iters + 1) diff_sq

let block_jacobi (matrix : Sparse.t) (b : floatarray) (block_size : int) : floatarray =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let _ = assert (Float.Array.length b = n) in
  let init = Float.Array.make n 0. in
  let d_block = Sparse.diag_block matrix block_size in
  let d_LU = Array.map Dense.decomp_LU d_block in
  let x = block_jacobi_aux matrix b d_LU init block_size 0 Float.max_float in
  let res = Float.Array.map2 ( -. ) (Sparse.mult_vec matrix x) b in
  let res_sq = dot_product res res in (
    let _ = res_sq in ();
    print_endline ("Squared Error (||Ax - b||^2): " ^ (string_of_float res_sq));
    x
  )