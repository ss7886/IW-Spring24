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

(*  mult_row_vec mat x i computes the dot product of the ith row of mat and x
 *  using Util.dot_product. *)
val mult_row_vec : t -> floatarray -> int -> float
val mult_vec : t -> floatarray -> floatarray
(*  mult_row_vec mat x i computes the dot product of the ith row of mat and x
 *  using Util.dot_product2. *)
val mult_row_vec2 : t -> floatarray -> int -> float
val mult_vec2 : t -> floatarray -> floatarray

(*  decomp_LU mat returns the LU decomposition of mat as (L, U). *)
val decomp_LU : t -> t * t
(*  Solves the system Lx = b. *)
val solve_L : t -> floatarray -> floatarray
(*  Solves the system Ux = b. *)
val solve_U : t -> floatarray -> floatarray
(*  Solves the system LUx = b. *)
val solve_LU : t -> t -> floatarray -> floatarray
