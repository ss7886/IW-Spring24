open JacobiLib
open CsvUtil
open Util
open UtilPar
open SparsePar

let _ = par_mult_vec
let _ = par_dot_product

let a10_000 = read_sparse_from_csv "data/A-10_000-0.002.csv"
let b10_000 = read_vec_from_csv "data/b-10_000-normal.csv"

let _ = a10_000

(* let _ = timer (fun _ -> let _ = dot_product b10_000 b10_000 in ()) 100000
let _ = timer (fun _ -> let _ = dot_product2 b10_000 b10_000 in ()) 100000 *)

(* let _ = timer (fun _ -> let _ = par_mult_vec a10_000 b10_000 1 in ()) 500
let _ = timer (fun _ -> let _ = par_mult_vec a10_000 b10_000 2 in ()) 500
let _ = timer (fun _ -> let _ = par_mult_vec a10_000 b10_000 4 in ()) 500
let _ = timer (fun _ -> let _ = par_mult_vec a10_000 b10_000 8 in ()) 500 *)

let _ = timer (fun _ -> let _ = par_dot_product b10_000 b10_000 1 in ()) 1000
let _ = timer (fun _ -> let _ = par_dot_product b10_000 b10_000 2 in ()) 1000
let _ = timer (fun _ -> let _ = par_dot_product b10_000 b10_000 4 in ()) 1000
let _ = timer (fun _ -> let _ = par_dot_product b10_000 b10_000 8 in ()) 1000
