type t = {
    num_rows : int;
    num_cols : int;
    count : int;
    vals : floatarray;
    cols : int array;
    row_ptr : int array;
  }

type entry = int * int * float

type builder = {
    num_rows : int;
    num_cols : int;
    count : int;
    capacity : int;
    entries : entry array;
}

val dense_to_sparse : Dense.t -> t

val get_val : t -> int -> int -> float

(*  mult_row_vec mat x i returns the dot product of the ith row of mat and x. *)
val mult_row_vec : t -> floatarray -> int -> float
val mult_vec : t -> floatarray -> floatarray
(*  mult_row_LU mat x block_size i computes the dot product of the ith row of
 *  mat with x, where all entries of mat along the block diagonal have been
 *  replaced with 0. *)
val mult_row_LU : t -> floatarray -> int -> int -> float
(*  mult_LU mat x block_size computes the matrix-vector multiplication of mat x,
 *  where all entries of mat along the block diagonal have been replaced with
 *  0. *)
val mult_LU : t -> floatarray -> int -> floatarray

(*  Returns the diagonal of a matrix. *)
val diag : t -> floatarray
(*  diag_block mat block_size returns the block diagonal of mat represented as
 *  an array of dense matrices, where each entry represents a single block. *)
val diag_block : t -> int -> Dense.t array

val is_diag_dominant : t -> bool

(*  new_builder m n returns a new matrix builder with m rows and n columns. *)
val new_builder : int -> int -> builder
(*  Inserts an entry into a matrix builder. *)
val builder_insert : builder -> entry -> builder
(*  Constructs a sparse matrix from a builder. *)
val build_sparse : builder -> t
