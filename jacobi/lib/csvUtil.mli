val read_vec_from_csv : string -> floatarray
val read_sparse_from_csv : string -> Sparse.t

val build_sparse_from_csv : string -> Sparse.t

(*  build_sparse_from_mm file isSym builds a sparse matrix from a Matrix-Market
 *  file in matrix coordinate real format. isSym represents if the matrix is
 *  in symmetric format. *)
val build_sparse_from_mm : string -> bool -> Sparse.t
