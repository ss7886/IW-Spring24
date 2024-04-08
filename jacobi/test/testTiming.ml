open JacobiLib
open JacobiSeq
open JacobiPar
open SparsePar
open CsvUtil
open Util

let _ = print_vector
let _ = Sparse.mult_vec
let _ = par_mult_vec

let _ = print_endline "Running timing tests"
let a50 = read_sparse_from_csv "data/A-50-0.1.csv"
let b50 = read_vec_from_csv "data/b-50-uniform.csv"
let _ = jacobi_sparse a50 b50

let a1000 = read_sparse_from_csv "data/A-1000-0.02.csv"
let b1000 = read_vec_from_csv "data/b-1000-uniform.csv"
let _ = jacobi_sparse a1000 b1000

let a10_000 = read_sparse_from_csv "data/A-10_000-0.02.csv"
let b10_000 = read_vec_from_csv "data/b-10_000-normal.csv"
let x1 = jacobi_sparse a10_000 b10_000
let x2 = jacobi_par_naive 4 a10_000 b10_000
let _ = assert (vec_eq x1 x2)

let x3 = block_jacobi a10_000 b10_000 10
let _ = assert (vec_close x1 x3 0.01)

let _ = timer (fun _ -> let _ = jacobi_sparse a10_000 b10_000 in ()) 2
let _ = timer (fun _ -> let _ = block_jacobi a10_000 b10_000 2 in ()) 2
let _ = timer (fun _ -> let _ = block_jacobi a10_000 b10_000 5 in ()) 2
let _ = timer (fun _ -> let _ = block_jacobi a10_000 b10_000 10 in ()) 2
let _ = timer (fun _ -> let _ = block_jacobi a10_000 b10_000 20 in ()) 2

(* let _ = timer (fun _ -> let _ = Sparse.mult_vec a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec 1 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec 2 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec 4 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec 8 a10_000 b10_000 in ()) 100 *)


(* let _ = timer (fun _ -> let _ = jacobi_sparse a10_000 b10_000 in ()) 20
let _ = timer (fun _ -> let _ = jacobi_par_naive 1 a10_000 b10_000 in ()) 20
let _ = timer (fun _ -> let _ = jacobi_par_naive 2 a10_000 b10_000 in ()) 20
let _ = timer (fun _ -> let _ = jacobi_par_naive 4 a10_000 b10_000 in ()) 20
let _ = timer (fun _ -> let _ = jacobi_par_naive 8 a10_000 b10_000 in ()) 20 *)
(* let _ = timer (fun _ -> let _ = jacobi_par_naive 8 a10_000 b10_000 in ()) 10 *)

(* let _ = print_int (Domain.recommended_domain_count ()); print_newline () *)
(* let _ = print_vector (Float.Array.sub (SparseMatrix.mult_vec a10_000 x) 0 10) *)
