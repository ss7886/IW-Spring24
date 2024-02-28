open JacobiLib
open JacobiSeq
open Sparse
open CsvUtil
open Util

let _ = print_vector
let _ = SparseMatrix.mult_vec

let _ = print_endline "Running timing tests"
let a50 = read_sparse_from_csv "data/A-50-0.1.csv"
let b50 = read_vec_from_csv "data/b-50-uniform.csv"
let _ = jacobi_sparse a50 b50

let a1000 = read_sparse_from_csv "data/A-1000-0.02.csv"
let b1000 = read_vec_from_csv "data/b-1000-uniform.csv"
let _ = jacobi_sparse a1000 b1000

let a10_000 = read_sparse_from_csv "data/A-10_000-0.002.csv"
let b10_000 = read_vec_from_csv "data/b-10_000-normal.csv"
let x = jacobi_sparse a10_000 b10_000
let _ = print_vector (Float.Array.sub (SparseMatrix.mult_vec a10_000 x) 0 10)
