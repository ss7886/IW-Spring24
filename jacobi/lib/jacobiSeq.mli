open Sparse

val jacobi_seq : floatarray array -> floatarray -> floatarray
val jacobi_sparse : SparseMatrix.matrix -> floatarray -> floatarray