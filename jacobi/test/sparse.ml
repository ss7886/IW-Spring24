open JacobiLib

exception MismatchedVals

let listlist_to_arrarr (list :  float list list) : floatarray array =
  Array.of_list (List.map (Float.Array.of_list) list)

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

let sparse1 = Sparse.SparseMatrix.dense_to_sparse dense1
let sparse2 = Sparse.SparseMatrix.dense_to_sparse dense2

let test_sparse (dense : floatarray array) (sparse : Sparse.SparseMatrix.matrix) : unit =
  let check_val (row : int) (col : int) (x : float) : unit = 
    if x = Sparse.SparseMatrix.get_val sparse row col then () else raise MismatchedVals
  in
  Array.iteri (fun row_num row -> Float.Array.iteri (check_val row_num) row) dense

let _ = print_endline "Testing Sparse Matrices"

let _ = test_sparse dense1 sparse1
let _ = test_sparse dense2 sparse2
