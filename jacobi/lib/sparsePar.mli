(*  par_mult_vec mat x p computes the matrix-vector multiplication of mat and
 *  x in parallel across p domains. *)
val par_mult_vec : Sparse.t -> floatarray -> int -> floatarray
(*  par_mult_LU mat x block_size p computes the matrix-vector multiplication of
 *  mat x, where all entries of mat along the block diagonal have been replaced
 *  with 0, in parallel across p domains. *)
val par_mult_LU : Sparse.t -> floatarray -> int -> int -> floatarray
