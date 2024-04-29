(*  Computes the solution to the linear system Ax = b, where A is dense. *)
val jacobi_seq : Dense.t -> floatarray -> floatarray
(*  Computes the solution to the linear system Ax = b, where A is sparse. *)
val jacobi_sparse : Sparse.t -> floatarray -> floatarray
(*  Computes the solution to the linear system Ax = b, where A is sparse, using
 *  using the Block-Jacobi method. *)
val block_jacobi : Sparse.t -> floatarray -> int -> floatarray
