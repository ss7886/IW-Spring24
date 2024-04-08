type t = {
    num_rows : int;
    num_cols : int;
    count : int;
    vals : floatarray;
    cols : int array;
    row_ptr : int array;
  }

val dense_to_sparse : Dense.t -> t

val get_val : t -> int -> int -> float
val mult_vec : t -> floatarray -> floatarray
val mult_row_vec : t -> floatarray -> int -> float
val mult_LU : t -> floatarray -> int -> floatarray
val mult_row_LU : t -> floatarray -> int -> int -> float
val diag : t -> floatarray
val diag_block : t -> int -> Dense.t array
