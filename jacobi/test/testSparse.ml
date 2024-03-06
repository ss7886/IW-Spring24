open JacobiLib
open Sparse
open CsvUtil
open Util

exception MismatchedVals

let dense1 = listlist_to_arrarr [
  [3.; 1.; 0.; 0.];
  [2.; 2.5; 0.5; 0.];
  [0.; 0.; 1.; -0.5];
  [0.; 0.; 1.2; 2.0]
]
let dense2 = listlist_to_arrarr [
  [1.; 0.; 0.; 0.];
  [0.; 1.; 0.; 0.];
  [0.; 0.; 1.; 0.];
  [0.; 0.; 0.; 1.]
]

let b1 = Float.Array.of_list [4.3; 3.2; 2.2; 3.1]
let b2 = Float.Array.of_list [-2.2; 12.5; -0.2; 3.1]
let b3 = Float.Array.of_list [0.0; 1.0; 2.0; 4.0]

let sparse1 = Square.dense_to_sparse dense1
let sparse2 = Square.dense_to_sparse dense2

let test_sparse (dense : floatarray array) (sparse : Square.t) : unit =
  let check_val (row : int) (col : int) (x : float) : unit = 
    if x = Square.get_val sparse row col then () else raise MismatchedVals
  in
  Array.iteri (fun row_num row -> Float.Array.iteri (check_val row_num) row) dense

let _ = print_endline "Testing Sparse Matrices"

(* let _ = print_vector sparse1.vals
let _ = print_vector sparse2.vals *)

let _ = test_sparse dense1 sparse1
let _ = test_sparse dense2 sparse2

let csv1 = read_sparse_from_csv "data/A-test1.csv"
let csv2 = read_sparse_from_csv "data/A-test2.csv"

let _ = assert (csv1 = sparse1)
let _ = assert (csv2 = sparse2)

(* let _ = print_vector (mat_vec_mult dense1 b1)
let _ = print_vector (SparseMatrix.mult_vec sparse1 b1) *)

(* let _ = assert (vec_eq (mat_vec_mult dense1 b1) (SparseMatrix.mult_vec sparse1 b1)) *)
let _ = assert (vec_eq (mat_vec_mult dense1 b2) (Square.mult_vec sparse1 b2))
let _ = assert (vec_eq (mat_vec_mult dense1 b3) (Square.mult_vec sparse1 b3))
let _ = assert (vec_eq (mat_vec_mult dense2 b1) (Square.mult_vec sparse2 b1))
let _ = assert (vec_eq (mat_vec_mult dense2 b2) (Square.mult_vec sparse2 b2))
let _ = assert (vec_eq (mat_vec_mult dense2 b3) (Square.mult_vec sparse2 b3))
