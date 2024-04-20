open JacobiLib
open CsvUtil
open Util
open SparsePar

let num_processors = [1; 2; 4; 8; 16; 32; 64]
let _ = num_processors
let _ = par_mult_vec
let _ = timer

let _ = print_string "Reading in test matrices... "

let af_shell3 = build_sparse_from_mm "data/af_shell3.mtx" true
let bcsstk08 = build_sparse_from_mm "data/bcsstk08.mtx" true
let bcsstk12 = build_sparse_from_mm "data/bcsstk12.mtx" true
let bcsstk17 = build_sparse_from_mm "data/bcsstk17.mtx" true
let bcsstk36 = build_sparse_from_mm "data/bcsstk36.mtx" true
let cfd1 = build_sparse_from_mm "data/cfd1.mtx" true
let ct20stif = build_sparse_from_mm "data/ct20stif.mtx" true
let ecology2 = build_sparse_from_mm "data/ecology2.mtx" true
let g3_circuit = build_sparse_from_mm "data/G3_circuit.mtx" true
let msc23052 = build_sparse_from_mm "data/msc23052.mtx" true
let nasasrb = build_sparse_from_mm "data/nasasrb.mtx" true
let nd3k = build_sparse_from_mm "data/nd3k.mtx" true
let nd6k = build_sparse_from_mm "data/nd6k.mtx" true
let pwtk = build_sparse_from_mm "data/pwtk.mtx" true
let shipsec1 = build_sparse_from_mm "data/shipsec1.mtx" true

let _ = print_endline "Done!"

let _ = if Sparse.is_diag_dominant af_shell3 then print_endline "afshell3: T" else print_endline "afshell3: F"
let _ = if Sparse.is_diag_dominant bcsstk08 then print_endline "bcsstk08: T" else print_endline "bcsstk08: F"
let _ = if Sparse.is_diag_dominant bcsstk12 then print_endline "bcsstk12: T" else print_endline "bcsstk12: F"
let _ = if Sparse.is_diag_dominant bcsstk17 then print_endline "bcsstk17: T" else print_endline "bcsstk17: F"
let _ = if Sparse.is_diag_dominant bcsstk36 then print_endline "bcsstk36: T" else print_endline "bcsstk36: F"
let _ = if Sparse.is_diag_dominant cfd1 then print_endline "cfd1: T" else print_endline "cfd1: F"
let _ = if Sparse.is_diag_dominant ct20stif then print_endline "ct20stif: T" else print_endline "ct20stif: F"
let _ = if Sparse.is_diag_dominant ecology2 then print_endline "ecology2: T" else print_endline "ecology2: F"
let _ = if Sparse.is_diag_dominant g3_circuit then print_endline "g3_circuit: T" else print_endline "g3_circuit: F"
let _ = if Sparse.is_diag_dominant msc23052 then print_endline "msc23052: T" else print_endline "msc23052: F"
let _ = if Sparse.is_diag_dominant nasasrb then print_endline "nasasrb: T" else print_endline "nasasrb: F"
let _ = if Sparse.is_diag_dominant nd3k then print_endline "nd3k: T" else print_endline "nd3k: F"
let _ = if Sparse.is_diag_dominant nd6k then print_endline "nd6k: T" else print_endline "nd6k: F"
let _ = if Sparse.is_diag_dominant pwtk then print_endline "pwtk: T" else print_endline "pwtk: F"
let _ = if Sparse.is_diag_dominant shipsec1 then print_endline "shipsec1: T" else print_endline "shipsec1: F"

(* let x1 = Float.Array.make bcsstk08.num_cols 1.
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
let _ = List.iter (testMatrixMultiply pwtk x7 100) num_processors *)
