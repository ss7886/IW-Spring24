open JacobiLib
open CsvUtil
open Util

let _ = print_endline "Testing Sparse Matrices"

let dense1 = Dense.from_list [
  [3.; 1.; 0.; 0.];
  [2.; 2.5; 0.5; 0.];
  [0.; 0.; 1.; -0.5];
  [0.; 0.; 1.2; 2.0]
]
let dense2 = Dense.from_list [
  [1.; 0.; 0.; 0.];
  [0.; 1.; 0.; 0.];
  [0.; 0.; 1.; 0.];
  [0.; 0.; 0.; 1.]
]

let b1 = Float.Array.of_list [4.3; 3.2; 2.2; 3.1]
let b2 = Float.Array.of_list [-2.2; 12.5; -0.2; 3.1]
let b3 = Float.Array.of_list [0.0; 1.0; 2.0; 4.0]

let sparse1 = Sparse.dense_to_sparse dense1
let sparse2 = Sparse.dense_to_sparse dense2

let test_sparse (dense : Dense.t) (sparse : Sparse.t) : unit =
  let check_val (row : int) (col : int) (x : float) : unit = 
    assert (x = Sparse.get_val sparse row col)
  in
  Array.iteri (fun row_num row -> Float.Array.iteri (check_val row_num) row) dense.vals

(* let _ = print_vector sparse1.vals
let _ = print_vector sparse2.vals *)

let _ = test_sparse dense1 sparse1
let _ = test_sparse dense2 sparse2

let csv1 = read_sparse_from_csv "data/A-test1.csv"
let csv2 = read_sparse_from_csv "data/A-test2.csv"

let _ = assert (csv1 = sparse1)
let _ = assert (csv2 = sparse2)

let csv3 = build_sparse_from_csv "data/A-test1.csv"
let csv4 = build_sparse_from_csv "data/A-test2.csv"

let _ = assert (csv1 = csv3)
let _ = assert (csv2 = csv4)

(* let _ = print_vector (mat_vec_mult dense1 b1)
let _ = print_vector (Matrix.mult_vec sparse1 b1) *)

let _ = assert (vec_eq (Dense.mult_vec dense1 b1) (Sparse.mult_vec sparse1 b1))
let _ = assert (vec_eq (Dense.mult_vec dense1 b2) (Sparse.mult_vec sparse1 b2))
let _ = assert (vec_eq (Dense.mult_vec dense1 b3) (Sparse.mult_vec sparse1 b3))
let _ = assert (vec_eq (Dense.mult_vec dense2 b1) (Sparse.mult_vec sparse2 b1))
let _ = assert (vec_eq (Dense.mult_vec dense2 b2) (Sparse.mult_vec sparse2 b2))
let _ = assert (vec_eq (Dense.mult_vec dense2 b3) (Sparse.mult_vec sparse2 b3))
