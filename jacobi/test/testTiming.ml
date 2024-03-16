open JacobiLib
open JacobiSeq
open JacobiPar
open SparsePar
open Sparse
open CsvUtil
open Util

let _ = print_vector
let _ = Square.mult_vec
let _ = par_mult_vec

let timer (f: unit -> unit) (iters: int) : unit = 
  print_string "Time to run ";
  print_int iters;
  print_string " iters: ";
  let t = Sys.time() in
  let rec aux (i : int) : unit =
    if i = iters then () else (
      f ();
      aux (i + 1)
    )
  in (
    aux 0;
    print_float (Sys.time() -. t);
    print_endline " seconds."
  )

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

let _ = timer (fun _ -> let _ = Square.mult_vec a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec 1 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec 2 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec 3 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = par_mult_vec 4 a10_000 b10_000 in ()) 100

(* 
let _ = timer (fun _ -> let _ = jacobi_sparse a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = jacobi_par_naive 1 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = jacobi_par_naive 2 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = jacobi_par_naive 3 a10_000 b10_000 in ()) 100
let _ = timer (fun _ -> let _ = jacobi_par_naive 4 a10_000 b10_000 in ()) 100 *)
(* let _ = timer (fun _ -> let _ = jacobi_par_naive 8 a10_000 b10_000 in ()) 10 *)

(* let _ = print_int (Domain.recommended_domain_count ()); print_newline () *)
(* let _ = print_vector (Float.Array.sub (SparseMatrix.mult_vec a10_000 x) 0 10) *)
