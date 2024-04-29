val listlist_to_arrarr : float list list -> floatarray array
val arr_to_floatarr : float array -> floatarray

val print_vector : floatarray -> unit
val print_ints : int array -> unit

(*  Checks if two floatarrays are equal element by element. *)
val vec_eq : floatarray -> floatarray -> bool
(*  vec_close vec1 vec2 epsilon checks that each entry of vec1 and vec2 are 
 *  within a distance of epsilon. *)
val vec_close : floatarray -> floatarray -> float -> bool

(*  Calculates the dot product using map and fold_left. *)
val dot_product : floatarray -> floatarray -> float
(*  partial_dot_product vec1 start1 vec2 start2 len calculates the dot product 
 *  of subvectors of length len of vec1 and vec2, starting at start1 and start2
 *  respectively, using a recursive function. *)
val partial_dot_product : floatarray -> int -> floatarray -> int -> int -> float
(*  Calculates the dot product using partial_dot_product.*)
val dot_product2 : floatarray -> floatarray -> float

(*  timer f iters calls f iters times and reports the time it takes to 
 *  complete. *)
val timer : (unit -> 'a) -> int -> 'a
