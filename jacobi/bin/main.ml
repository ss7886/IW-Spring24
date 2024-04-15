open JacobiLib
open CsvUtil
open Util
open SparsePar

let num_processors = [1; 2; 4; 8; 16; 32; 64]

let _ = print_string "Reading in test matrices... "

let bcsstk08 = build_sparse_from_mm "data/bcsstk08.mtx" true
let bcsstk12 = build_sparse_from_mm "data/bcsstk12.mtx" true
let bcsstk17 = build_sparse_from_mm "data/bcsstk17.mtx" true
let bcsstk36 = build_sparse_from_mm "data/bcsstk36.mtx" true
let ct20stif = build_sparse_from_mm "data/ct20stif.mtx" true
let msc23052 = build_sparse_from_mm "data/msc23052.mtx" true
let pwtk = build_sparse_from_mm "data/pwtk.mtx" true

let _ = print_endline "Done!"

let x1 = Float.Array.make bcsstk08.num_cols 1.
let x2 = Float.Array.make bcsstk12.num_cols 1.
let x3 = Float.Array.make bcsstk17.num_cols 1.
let x4 = Float.Array.make bcsstk36.num_cols 1.
let x5 = Float.Array.make ct20stif.num_cols 1.
let x6 = Float.Array.make msc23052.num_cols 1.
let x7 = Float.Array.make pwtk.num_cols 1.

let testMatrixMultiply (m : Sparse.t) (x : floatarray) (iters : int) (p : int) : unit =
  Printf.printf "%d processors - " p;
  timer (fun _ -> let _ = par_mult_vec m x p in ()) iters

let _ = Printf.printf "Testing bcsstk08 - n: %d, count: %d\n" bcsstk08.num_rows bcsstk08.count
let _ = List.iter (testMatrixMultiply bcsstk08 x1 100) num_processors

let _ = Printf.printf "Testing bcsstk12 - n: %d, count: %d\n" bcsstk12.num_rows bcsstk12.count
let _ = List.iter (testMatrixMultiply bcsstk12 x2 100) num_processors

let _ = Printf.printf "Testing bcsstk17 - n: %d, count: %d\n" bcsstk17.num_rows bcsstk17.count
let _ = List.iter (testMatrixMultiply bcsstk17 x3 100) num_processors

let _ = Printf.printf "Testing bcsstk36 - n: %d, count: %d\n" bcsstk36.num_rows bcsstk36.count
let _ = List.iter (testMatrixMultiply bcsstk36 x4 100) num_processors

let _ = Printf.printf "Testing ct20stif - n: %d, count: %d\n" ct20stif.num_rows ct20stif.count
let _ = List.iter (testMatrixMultiply ct20stif x5 100) num_processors

let _ = Printf.printf "Testing msc23052 - n: %d, count: %d\n" msc23052.num_rows msc23052.count
let _ = List.iter (testMatrixMultiply msc23052 x6 100) num_processors

let _ = Printf.printf "Testing pwtk - n: %d, count: %d\n" pwtk.num_rows pwtk.count
let _ = List.iter (testMatrixMultiply pwtk x7 100) num_processors
