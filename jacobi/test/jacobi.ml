open JacobiLib
open JacobiSeq
open Sparse

let listlist_to_arrarr (list :  float list list) : floatarray array =
  Array.of_list (List.map (Float.Array.of_list) list)

let print_vector (vec : floatarray) : unit = 
  Float.Array.iter (fun x' -> (print_float x'; print_string " ")) vec;
  print_newline ()

(* let vec_eq (v1 : floatarray) (v2 : floatarray) : bool =
  let rec aux (i : int) : bool =
    if i = Float.Array.length(v1) then true else
      let f1 = Float.Array.get v1 i in
      let f2 = Float.Array.get v2 i in
      if f1 = f2 then aux (i + 1) else (
      print_float f1;
      print_string " =/= ";
      print_float f2;
      print_newline ();
      false
    )
  in aux 0 *)

let matrix1 = listlist_to_arrarr [
  [3.; 1.; 0.; 0.];
  [2.; 2.5; 0.5; 0.];
  [0.; 0.; 1.; -0.5];
  [0.; 0.; 1.2; 2.0]
]

let matrix2 = listlist_to_arrarr [
  [1.; 0.; 0.; 0.];
  [0.; 1.; 0.; 0.];
  [0.; 0.; 1.; 0.];
  [0.; 0.; 0.; 1.]
]

let matrix3 = listlist_to_arrarr [
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

let sparse1 = SparseMatrix.dense_to_sparse matrix1
let sparse2 = SparseMatrix.dense_to_sparse matrix2
let sparse3 = SparseMatrix.dense_to_sparse matrix3

let b1 = Float.Array.of_list [4.3; 3.2; 2.2; 3.1]
let b2 = Float.Array.of_list [-2.2; 12.5; -0.2; 3.1]
let b3 = Float.Array.of_list [0.0; 1.0; 2.0; 4.0]
let b4 = Float.Array.of_list [-9.9; 8.8; -7.7; 6.6; -5.5; 4.4; -3.3; 2.2; -1.1; 0.05]

let x1 = jacobi_seq matrix1 b1
let _ = (print_vector x1; print_newline ())

let x1_sparse = jacobi_sparse sparse1 b1
let _ = (print_vector x1_sparse; print_newline ())
(* let _ = assert (vec_eq x1 x1_sparse) *)

let x2 = jacobi_seq matrix1 b2
let _ = (print_vector x2; print_newline ())

let x3 = jacobi_seq matrix1 b3
let _ = (print_vector x3; print_newline ())

let x4 = jacobi_seq matrix2 b1
let _ = (print_vector x4; print_newline ())

let x4_sparse = jacobi_sparse sparse2 b1
let _ = (print_vector x4_sparse; print_newline ())
(* let _ = assert (vec_eq x4 x4_sparse) *)

let x5 = jacobi_seq matrix2 b2
let _ = (print_vector x5; print_newline ())

let x6 = jacobi_seq matrix2 b3
let _ = (print_vector x6; print_newline ())

let x7 = jacobi_seq matrix3 b4
let _ = (print_vector x7; print_newline ())

let x7_sparse = jacobi_sparse sparse3 b4
let _ = (print_vector x7_sparse; print_newline ())
(* let _ = assert (vec_eq x7 x7_sparse) *)
