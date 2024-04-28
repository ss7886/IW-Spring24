open JacobiLib
open CsvUtil
open Util
open UtilPar
open SparsePar
open JacobiPar

let num_processors = [1; 2; 4; 8; 16; 32; 64]

(* Load in matrices *)

let _ = print_string "Loading test matrices..."

let bcsstk08 = build_sparse_from_mm "data/bcsstk08.mtx" true
let bcsstk12 = build_sparse_from_mm "data/bcsstk12.mtx" true
let bcsstk17 = build_sparse_from_mm "data/bcsstk17.mtx" true
let bcsstk36 = build_sparse_from_mm "data/bcsstk36.mtx" true
let ct20stif = build_sparse_from_mm "data/ct20stif.mtx" true
let msc23052 = build_sparse_from_mm "data/msc23052.mtx" true
let pwtk = build_sparse_from_mm "data/pwtk.mtx" true
let cage12 = build_sparse_from_mm "data/cage12.mtx" false
let cage13 = build_sparse_from_mm "data/cage13.mtx" false
let cage14 = build_sparse_from_mm "data/cage14.mtx" false

let _ = print_endline "Done!"

(* Checking Diagonal Dominance *)
let _ = print_endline "Checking Diagonal Dominance of matrices:"

let _ = if Sparse.is_diag_dominant bcsstk08 then print_endline "bcsstk08: T" else print_endline "bcsstk08: F"
let _ = if Sparse.is_diag_dominant bcsstk12 then print_endline "bcsstk12: T" else print_endline "bcsstk12: F"
let _ = if Sparse.is_diag_dominant bcsstk17 then print_endline "bcsstk17: T" else print_endline "bcsstk17: F"
let _ = if Sparse.is_diag_dominant bcsstk36 then print_endline "bcsstk36: T" else print_endline "bcsstk36: F"
let _ = if Sparse.is_diag_dominant ct20stif then print_endline "ct20stif: T" else print_endline "ct20stif: F"
let _ = if Sparse.is_diag_dominant msc23052 then print_endline "msc23052: T" else print_endline "msc23052: F"
let _ = if Sparse.is_diag_dominant pwtk then print_endline "pwtk: T" else print_endline "pwtk: F"
let _ = if Sparse.is_diag_dominant cage12 then print_endline "cage12: T" else print_endline "cage12: F"
let _ = if Sparse.is_diag_dominant cage13 then print_endline "cage13: T" else print_endline "cage13: F"
let _ = if Sparse.is_diag_dominant cage14 then print_endline "cage14: T" else print_endline "cage14: F"

(* Test Vector Operations *)
let float_100_000 = Float.Array.init 100_000 float_of_int
let float_1_000_000 = Float.Array.init 1_000_000 float_of_int
let float_10_000_000 = Float.Array.init 10_000_000 float_of_int

let testVectorOps (x : floatarray) (iters : int) (p : int) : unit =
  Printf.printf "%d processors - " p;
  timer (fun _ -> let _ = par_dot_product x x p in ()) iters;
  timer (fun _ -> let _ = par_float_map2 ( +. ) x x p in ()) iters

let _ = print_endline "Testing floatarray - n: 100,000"
let _ = List.iter (testVectorOps float_100_000 100) num_processors

let _ = print_endline "Testing floatarray - n: 1,000,000"
let _ = List.iter (testVectorOps float_1_000_000 10) num_processors

let _ = print_endline "Testing floatarray - n: 10,000,000"
let _ = List.iter (testVectorOps float_10_000_000 1) num_processors

(* Test Matrix Multiply *)
let _ = print_newline(); print_endline "Timing Matrix Multiplication:"

let bcsstk08_b = Float.Array.make bcsstk08.num_cols 1.
let bcsstk12_b = Float.Array.make bcsstk12.num_cols 1.
let bcsstk17_b = Float.Array.make bcsstk17.num_cols 1.
let bcsstk36_b = Float.Array.make bcsstk36.num_cols 1.
let ct20stif_b = Float.Array.make ct20stif.num_cols 1.
let msc23052_b = Float.Array.make msc23052.num_cols 1.
let pwtk_b = Float.Array.make pwtk.num_cols 1.
let cage12_b = Float.Array.make cage12.num_cols 1.
let cage13_b = Float.Array.make cage13.num_cols 1.
let cage14_b = Float.Array.make cage14.num_cols 1.

let testMatrixMultiply (m : Sparse.t) (x : floatarray) (iters : int) (p : int) : unit =
  Printf.printf "%d processors - " p;
  timer (fun _ -> let _ = par_mult_vec m x p in ()) iters

let _ = Printf.printf "Testing bcsstk08 - n: %d, count: %d\n" bcsstk08.num_rows bcsstk08.count
let _ = List.iter (testMatrixMultiply bcsstk08 bcsstk08_b 100) num_processors

let _ = Printf.printf "Testing bcsstk12 - n: %d, count: %d\n" bcsstk12.num_rows bcsstk12.count
let _ = List.iter (testMatrixMultiply bcsstk12 bcsstk12_b 100) num_processors

let _ = Printf.printf "Testing bcsstk17 - n: %d, count: %d\n" bcsstk17.num_rows bcsstk17.count
let _ = List.iter (testMatrixMultiply bcsstk17 bcsstk17_b 100) num_processors

let _ = Printf.printf "Testing bcsstk36 - n: %d, count: %d\n" bcsstk36.num_rows bcsstk36.count
let _ = List.iter (testMatrixMultiply bcsstk36 bcsstk36_b 100) num_processors

let _ = Printf.printf "Testing ct20stif - n: %d, count: %d\n" ct20stif.num_rows ct20stif.count
let _ = List.iter (testMatrixMultiply ct20stif ct20stif_b 100) num_processors

let _ = Printf.printf "Testing msc23052 - n: %d, count: %d\n" msc23052.num_rows msc23052.count
let _ = List.iter (testMatrixMultiply msc23052 msc23052_b 100) num_processors

let _ = Printf.printf "Testing pwtk - n: %d, count: %d\n" pwtk.num_rows pwtk.count
let _ = List.iter (testMatrixMultiply pwtk pwtk_b 100) num_processors

let _ = Printf.printf "Testing cage12 - n: %d, count: %d\n" cage12.num_rows cage12.count
let _ = List.iter (testMatrixMultiply cage12 cage12_b 100) num_processors

let _ = Printf.printf "Testing cage13 - n: %d, count: %d\n" cage13.num_rows cage13.count
let _ = List.iter (testMatrixMultiply cage13 cage13_b 100) num_processors

let _ = Printf.printf "Testing cage14 - n: %d, count: %d\n" cage14.num_rows cage14.count
let _ = List.iter (testMatrixMultiply cage14 cage14_b 100) num_processors

(* Testing Jacobi Method *)
let _ = print_newline (); print_endline "Timing Jacobi Method:"

let testJacobiPar (m : Sparse.t) (x : floatarray) (iters : int) (p : int) : unit =
  Printf.printf "%d processors:" p; print_newline();
  print_endline "Block Size 1"; timer (fun _ -> let _ = jacobi_par_naive m x p in ()) iters; print_newline ();
  print_endline "Block Size 2"; timer (fun _ -> let _ = block_jacobi_par m x 2 p in ()) iters; print_newline ();
  print_endline "Block Size 4"; timer (fun _ -> let _ = block_jacobi_par m x 4 p in ()) iters; print_newline ();
  print_endline "Block Size 8"; timer (fun _ -> let _ = block_jacobi_par m x 8 p in ()) iters; print_newline ();
  print_endline "Block Size 16"; timer (fun _ -> let _ = block_jacobi_par m x 16 p in ()) iters; print_newline ();
  print_endline "Block Size 32"; timer (fun _ -> let _ = block_jacobi_par m x 32 p in ()) iters; print_newline ()

let _ = Printf.printf "Testing cage12 - n: %d, count: %d\n" cage12.num_rows cage12.count
let _ = List.iter (testJacobiPar cage12 cage12_b 1) num_processors

let _ = Printf.printf "Testing cage13 - n: %d, count: %d\n" cage13.num_rows cage13.count
let _ = List.iter (testJacobiPar cage13 cage13_b 1) num_processors

let _ = Printf.printf "Testing cage14 - n: %d, count: %d\n" cage14.num_rows cage14.count
let _ = List.iter (testJacobiPar cage14 cage14_b 1) num_processors
