type t = {
    num_rows : int;
    num_cols : int;
    vals : floatarray array;
}

val from_arr : floatarray array -> t
val from_list : float list list -> t
val copy : t -> t
val eq : t -> t -> bool
val print : t -> unit

val get_val : t -> int -> int -> float
val set_val : t -> int -> int -> float -> unit
val get_row : t -> int -> floatarray

val mult_vec : t -> floatarray -> floatarray
val mult_row_vec : t -> floatarray -> int -> float

val decomp_LU : t -> t * t
val solve_L : t -> floatarray -> floatarray
val solve_U : t -> floatarray -> floatarray
val solve_LU : t -> t -> floatarray -> floatarray
(* val mult_LU : t -> floatarray -> int -> floatarray
val mult_row_LU : t -> floatarray -> int -> int -> float *)