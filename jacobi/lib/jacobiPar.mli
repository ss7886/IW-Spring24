(*  Computes the solution to the linear system in parallel across p 
 *  processors. Only parallelizes matrix-vector multiplications. *)
val jacobi_par_naive : Sparse.t -> floatarray -> int -> floatarray
(*  Computes the solution to the linear system in parallel across p 
 *  processors. Parallelizes matrix-vector multiplications and all vector 
 *  operations. *)
val jacobi_par_naive_2 : Sparse.t -> floatarray -> int -> floatarray
(*  block_jacobi_par mat x block_size p computes the solution to the linear
 *  system using the Block-Jacobi method in parallel across p processors. *)
val block_jacobi_par : Sparse.t -> floatarray -> int -> int -> floatarray
