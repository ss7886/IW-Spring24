open JacobiLib
open Util

let _ = print_endline "Testing Utility Functions"

let v1 = Float.Array.of_list [1.; 2.; 3.]
let v2 = Float.Array.of_list [2.; 3.; 4.]
let v3 = Float.Array.of_list [1.; 2.; 3.; 4.]
let v4 = Float.Array.of_list [2.; 3.; 4.; 5.]

let _ = assert (dot_product v1 v2 = 20.)
let _ = assert (partial_dot_product v1 0 v2 0 3 = 20.)
let _ = assert (partial_dot_product v3 0 v4 0 3 = 20.)
let _ = assert (partial_dot_product v3 0 v3 1 3 = 20.)
let _ = assert (partial_dot_product v1 0 v2 0 0 = 0.)