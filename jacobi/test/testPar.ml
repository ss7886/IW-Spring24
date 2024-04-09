open JacobiLib
open SparsePar
open CsvUtil
open Util
open UtilPar
let _ = vec_eq
let _ = par_mult_vec


let _ = print_endline "Testing Parallel Multiplication"

let a1000 = read_sparse_from_csv "data/A-1000-0.02.csv"
let b1000 = read_vec_from_csv "data/b-1000-uniform.csv"

let seq = Sparse.mult_vec a1000 b1000
let par1 = par_mult_vec a1000 b1000 1
let par2 = par_mult_vec a1000 b1000 2
let par4 = par_mult_vec a1000 b1000 4

let _ = assert (vec_eq seq par1)
let _ = assert (vec_eq seq par2)
let _ = assert (vec_eq seq par4)

let dot_prod = dot_product b1000 b1000
let par_dot_prod1 = par_dot_product b1000 b1000 1
let par_dot_prod2 = par_dot_product b1000 b1000 2
let par_dot_prod3 = par_dot_product b1000 b1000 6
let par_dot_prod4 = par_dot_product b1000 b1000 8

let _ = assert (abs_float (dot_prod -. par_dot_prod1) < 0.000001)
let _ = assert (abs_float (dot_prod -. par_dot_prod2) < 0.000001)
let _ = assert (abs_float (dot_prod -. par_dot_prod3) < 0.000001)
let _ = assert (abs_float (dot_prod -. par_dot_prod4) < 0.000001)

let double = Float.Array.map2 ( +. ) b1000 b1000
let par_double1 = par_add_vec b1000 b1000 1
let par_double2 = par_add_vec b1000 b1000 2
let par_double3 = par_add_vec b1000 b1000 6
let par_double4 = par_add_vec b1000 b1000 8

let _ = assert (vec_eq double par_double1)
let _ = assert (vec_eq double par_double2)
let _ = assert (vec_eq double par_double3)
let _ = assert (vec_eq double par_double4)

let diff = Float.Array.map2 ( -. ) seq b1000
let par_diff_1 = par_sub_vec seq b1000 1
let par_diff_2 = par_sub_vec seq b1000 2
let par_diff_3 = par_sub_vec seq b1000 6
let par_diff_4 = par_sub_vec seq b1000 8

let _ = assert (vec_eq diff par_diff_1)
let _ = assert (vec_eq diff par_diff_2)
let _ = assert (vec_eq diff par_diff_3)
let _ = assert (vec_eq diff par_diff_4)
