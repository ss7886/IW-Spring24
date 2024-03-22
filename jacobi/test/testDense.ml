open JacobiLib
open Util

let _ = print_endline "Testing Dense Matrices"

let dense1 = Dense.from_list [
  [1.; 2.; 3.];
  [4.; 5.; 6.];
  [7.; 8.; 9.]
]
let dense2 = Dense.from_list [
  [1.; 0.; 0.];
  [0.; 1.; 0.];
  [0.; 0.; 1.]
]
let b1 = Float.Array.of_list [4.3; 3.2; 2.2]
let b2 = Float.Array.of_list [-2.2; 12.5; -0.2]
let b3 = Float.Array.of_list [0.0; 1.0; 2.0]

let dense1_L, dense1_U = Dense.decomp_LU dense1
let dense2_L, dense2_U = Dense.decomp_LU dense2

let _ = assert (vec_eq (Dense.mult_vec dense1 b1) (Dense.mult_vec dense1_L (Dense.mult_vec dense1_U b1)))
let _ = assert (vec_eq (Dense.mult_vec dense1 b2) (Dense.mult_vec dense1_L (Dense.mult_vec dense1_U b2)))
let _ = assert (vec_eq (Dense.mult_vec dense1 b3) (Dense.mult_vec dense1_L (Dense.mult_vec dense1_U b3)))
let _ = assert (vec_eq (Dense.mult_vec dense2 b1) (Dense.mult_vec dense2_L (Dense.mult_vec dense2_U b1)))
let _ = assert (vec_eq (Dense.mult_vec dense2 b2) (Dense.mult_vec dense2_L (Dense.mult_vec dense2_U b2)))
let _ = assert (vec_eq (Dense.mult_vec dense2 b3) (Dense.mult_vec dense2_L (Dense.mult_vec dense2_U b3)))
