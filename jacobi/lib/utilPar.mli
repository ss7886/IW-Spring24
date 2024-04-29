(*  par_dot_product vec1 vec2 p computes the dot product of vec1 and vec2 in
 *  parallel across p domains. *)
val par_dot_product : floatarray -> floatarray -> int -> float
(*  par_map f arr p applies f to every entry in arr in parallel across p 
 *  domains. *)
val par_map : ('a -> 'b) -> 'a array -> int -> 'b array
(*  par_mapi f arr p applies (f i) to every entry in arr in parallel across p
 *  domains, where i is the index of the entry. *)
val par_mapi : (int -> 'a -> 'b) -> 'a array -> int -> 'b array
(*  par_float_map2 f vec1 vec2 p applies f to every entry of vec1 and vec2 in
 *  parallel across p domains.*)
val par_float_map2 : (float -> float -> float) -> floatarray -> floatarray -> int -> floatarray
