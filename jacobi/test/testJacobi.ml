open JacobiLib
open JacobiSeq
open Util

let matrix1 = Dense.from_list [
  [3.; 1.; 0.; 0.];
  [2.; 2.5; 0.5; 0.];
  [0.; 0.; 1.; -0.5];
  [0.; 0.; 1.2; 2.0]
]

let matrix2 = Dense.from_list [
  [1.; 0.; 0.; 0.];
  [0.; 1.; 0.; 0.];
  [0.; 0.; 1.; 0.];
  [0.; 0.; 0.; 1.]
]

let matrix3 = Dense.from_list [
  [1.0; 0.24; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.];
  [0.; 1.0; 0.; 0.; -0.2; 0.; 0.; 0.; 0.; 0.];
  [0.; -0.5; 1.0; 0.; 0.05; 0.; 0.; 0.; 0.; 0.];
  [0.; 0.; 0.; 1.0; 0.1; 0.; 0.; 0.; 0.; 0.];
  [0.; 0.78; 0.; 0.; 1.0; 0.; 0.; 0.; 0.; 0.];
  [0.; 0.; 0.; -0.2; 0.; 1.0; 0.; 0.; 0.; 0.];
  [0.; 0.; 0.; 0.6; 0.; 0.; 1.0; -0.423; 0.; 0.];
  [0.; 0.; 0.; 0.; 0.05; 0.; 0.; 1.0; 0.; 0.];
  [0.; 0.; 0.; 0.; 0.; 0.34; 0.; 0.; 1.0; 0.95];
  [0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.95; 1.0];
]

let _ = print_endline "Testing Jacobi method in sequence on dense and sparse matrices"

let sparse1 = Sparse.dense_to_sparse matrix1
let sparse2 = Sparse.dense_to_sparse matrix2
let sparse3 = Sparse.dense_to_sparse matrix3

let b1 = Float.Array.of_list [4.3; 3.2; 2.2; 3.1]
let b2 = Float.Array.of_list [-2.2; 12.5; -0.2; 3.1]
let b3 = Float.Array.of_list [0.0; 1.0; 2.0; 4.0]
let b4 = Float.Array.of_list [-9.9; 8.8; -7.7; 6.6; -5.5; 4.4; -3.3; 2.2; -1.1; 0.05]

let mat_L1, mat_U1 = Dense.decomp_LU matrix1
let mat_L2, mat_U2 = Dense.decomp_LU matrix2
let mat_L3, mat_U3 = Dense.decomp_LU matrix3

let x1 = jacobi_seq matrix1 b1
(* let _ = (print_vector x1; print_newline ()) *)
let x1_sparse = jacobi_sparse sparse1 b1
(* let _ = (print_vector x1_sparse; print_newline ()) *)
let _ = assert (vec_eq x1 x1_sparse)
let x1_exact = Dense.solve_LU mat_L1 mat_U1 b1
(* let _ = (print_vector x1_exact; print_newline ()) *)
let _ = assert (vec_close x1 x1_exact 0.01)
let x1_block = block_jacobi sparse1 b1 2 
let _ = assert (vec_close x1 x1_block 0.01)

let x2 = jacobi_seq matrix1 b2
(* let _ = (print_vector x2; print_newline ()) *)
let x2_sparse = jacobi_sparse sparse1 b2
let _ = assert (vec_eq x2 x2_sparse)
(* let x2_exact = Dense.solve_LU mat_L1 mat_U1 b2
let _ = assert (vec_close x2 x2_exact 0.001) *)

let x3 = jacobi_seq matrix1 b3
(* let _ = (print_vector x3; print_newline ()) *)
let x3_sparse = jacobi_sparse sparse1 b3
let _ = assert (vec_eq x3 x3_sparse)
(* let x3_exact = Dense.solve_LU mat_L1 mat_U1 b3
let _ = assert (vec_close x3 x3_exact 0.001) *)

let x4 = jacobi_seq matrix2 b1
(* let _ = (print_vector x4; print_newline ()) *)
let x4_sparse = jacobi_sparse sparse2 b1
(* let _ = (print_vector x4_sparse; print_newline ()) *)
let _ = assert (vec_eq x4 x4_sparse)
let x4_exact = Dense.solve_LU mat_L2 mat_U2 b1
let _ = assert (vec_close x4 x4_exact 0.000000001)

let x5 = jacobi_seq matrix2 b2
(* let _ = (print_vector x5; print_newline ()) *)
let x5_sparse = jacobi_sparse sparse2 b2
let _ = assert (vec_eq x5 x5_sparse)
let x5_exact = Dense.solve_LU mat_L2 mat_U2 b2
let _ = assert (vec_close x5 x5_exact 0.000000001)

let x6 = jacobi_seq matrix2 b3
(* let _ = (print_vector x6; print_newline ()) *)
let x6_sparse = jacobi_sparse sparse2 b3
let _ = assert (vec_eq x6 x6_sparse)
let x6_exact = Dense.solve_LU mat_L2 mat_U2 b3
let _ = assert (vec_close x6 x6_exact 0.000000001)

let x7 = jacobi_seq matrix3 b4
(* let _ = (print_vector x7; print_newline ()) *)

let x7_sparse = jacobi_sparse sparse3 b4
(* let _ = (print_vector x7_sparse; print_newline ()) *)
let _ = assert (vec_eq x7 x7_sparse)
let x7_exact = Dense.solve_LU mat_L3 mat_U3 b4
(* let _ = (print_vector (Dense.solve_L mat_L3 b4); print_newline ()) *)
(* let _ = (print_vector x7_exact; print_newline ()) *)
let _ = assert (vec_close x7 x7_exact 0.02)
let x7_block2 = block_jacobi sparse3 b4 2
let _ = assert (vec_close x7_exact x7_block2 0.001)
