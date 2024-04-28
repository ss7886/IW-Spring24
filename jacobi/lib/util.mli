val listlist_to_arrarr : float list list -> floatarray array
val print_vector : floatarray -> unit
val print_ints : int array -> unit
val vec_eq : floatarray -> floatarray -> bool
val vec_close : floatarray -> floatarray -> float -> bool
val arr_to_floatarr : float array -> floatarray
val dot_product : floatarray -> floatarray -> float
val partial_dot_product : floatarray -> int -> floatarray -> int -> int -> float
val dot_product2 : floatarray -> floatarray -> float

val timer : (unit -> 'a) -> int -> 'a
