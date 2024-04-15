open JacobiLib
open CsvUtil
open Util
open SparsePar
open JacobiSeq
open JacobiPar

let _ = jacobi_sparse
let _ = jacobi_par_naive
let _ = timer
let _ = par_mult_vec

let bcsstk08 = build_sparse_from_mm "data/bcsstk08.mtx" true
let bcsstk12 = build_sparse_from_mm "data/bcsstk12.mtx" true
let bcsstk17 = build_sparse_from_mm "data/bcsstk17.mtx" true
let bcsstk36 = build_sparse_from_mm "data/bcsstk36.mtx" true
let ct20stif = build_sparse_from_mm "data/ct20stif.mtx" true
let msc23052 = build_sparse_from_mm "data/msc23052.mtx" true
let pwtk = build_sparse_from_mm "data/pwtk.mtx" true

let _ = bcsstk08, bcsstk12, bcsstk17, bcsstk36, ct20stif, msc23052, pwtk

(* let _ = if (Sparse.is_diag_dominant bcsstk08) then print_endline "T" else print_endline "F"
let _ = if (Sparse.is_diag_dominant bcsstk12) then print_endline "T" else print_endline "F"
let _ = if (Sparse.is_diag_dominant bcsstk17) then print_endline "T" else print_endline "F"
let _ = if (Sparse.is_diag_dominant bcsstk36) then print_endline "T" else print_endline "F"
let _ = if (Sparse.is_diag_dominant ct20stif) then print_endline "T" else print_endline "F"
let _ = if (Sparse.is_diag_dominant msc23052) then print_endline "T" else print_endline "F"
let _ = if (Sparse.is_diag_dominant pwtk) then print_endline "T" else print_endline "F" *)

(* let _ = timer (fun _ -> let _ = build_sparse_from_mm "data/bcsstk08.mtx" true in ()) 1
let _ = timer (fun _ -> let _ = build_sparse_from_mm "data/bcsstk12.mtx" true in ()) 1
let _ = timer (fun _ -> let _ = build_sparse_from_mm "data/bcsstk17.mtx" true in ()) 1
let _ = timer (fun _ -> let _ = build_sparse_from_mm "data/bcsstk36.mtx" true in ()) 1
let _ = timer (fun _ -> let _ = build_sparse_from_mm "data/ct20stif.mtx" true in ()) 1
let _ = timer (fun _ -> let _ = build_sparse_from_mm "data/msc23052.mtx" true in ()) 1
let _ = timer (fun _ -> let _ = build_sparse_from_mm "data/pwtk.mtx" true in ()) 1
let _ = timer (fun _ -> let _ = Sparse.mult_vec pwtk (Float.Array.make pwtk.num_cols 1.) in ()) 1
let _ = timer (fun _ -> let _ = Sparse.is_diag_dominant pwtk in ()) 1 *)

let x = Float.Array.make (pwtk.num_cols) 1.
let _ = timer (fun _ -> let _ = Sparse.mult_vec pwtk x in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec pwtk x 1 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec pwtk x 2 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec pwtk x 4 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec pwtk x 8 in ()) 100
