open JacobiLib
open Sparse
open SparsePar
open CsvUtil
open Util
let _ = vec_eq
let _ = par_mult_vec


let _ = print_endline "Testing Parallel Multiplication"

let a50 = read_sparse_from_csv "data/A-1000-0.02.csv"
let b50 = read_vec_from_csv "data/b-1000-uniform.csv"

let seq = Matrix.mult_vec a50 b50
let par1 = par_mult_vec 1 a50 b50
let par2 = par_mult_vec 2 a50 b50
let par4 = par_mult_vec 4 a50 b50

let _ = assert (vec_eq seq par1)
let _ = assert (vec_eq seq par2)
let _ = assert (vec_eq seq par4)

let _ = assert (vec_eq seq par1)
