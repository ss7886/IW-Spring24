val read_vec_from_csv : string -> floatarray
val read_sparse_from_csv : string -> Sparse.t

val build_sparse_from_csv : string -> Sparse.t

val build_sparse_from_mm : string -> bool -> Sparse.t
