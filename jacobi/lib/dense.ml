open Util

type t = {
    num_rows : int;
    num_cols : int;
    vals : floatarray array;
}

let from_arr (vals : floatarray array) : t = 
  let n = Array.length vals in 
  let _ = assert (n > 0) in 
  let m = Float.Array.length (Array.get vals 0) in
  let _ = Array.iter (fun row -> assert (Float.Array.length row = m)) vals in
  {num_rows = n; num_cols = m; vals = vals}

let from_list (vals : float list list) : t = 
  let arr = listlist_to_arrarr vals in
  from_arr arr

let copy (matrix : t) : t =
  let vals = Array.init matrix.num_rows (fun row -> Float.Array.copy (Array.get matrix.vals row)) in
  {num_rows = matrix.num_rows; num_cols = matrix.num_cols; vals=vals}

let eq (mat1 : t) (mat2 : t) : bool = 
  if mat1.num_rows != mat2.num_rows then false else
  if mat1.num_cols != mat2.num_cols then false else
  Array.fold_left ( && ) true (Array.map2 vec_eq mat1.vals mat2.vals)

let print (matrix : t) : unit =
  print_endline "[";
  Array.iter print_vector matrix.vals;
  print_endline "]"

let get_val (matrix : t) (i : int) (j : int) : float = 
  Float.Array.get (Array.get matrix.vals i) j

let set_val (matrix : t) (i : int) (j : int) (x : float) : unit =
  Float.Array.set (Array.get matrix.vals i) j x

let get_row (matrix : t) (i : int) : floatarray =
  Array.get matrix.vals i

let mult_vec (matrix : t) (b : floatarray) : floatarray =
  let _ = assert (matrix.num_cols = Float.Array.length b) in
  arr_to_floatarr (Array.map (dot_product b) matrix.vals)

let mult_row_vec (matrix : t) (b : floatarray) (row : int) : float =
  let _ = assert (matrix.num_cols = Float.Array.length b) in
  dot_product (Array.get matrix.vals row) b

let rec decomp_aux (mat_L : t) (mat_U : t) (k : int) : unit = 
  if k = mat_U.num_rows - 1 then () else
  let a = get_val mat_U k k in
  let c = 1. /. a in
  let rec set_cv (i : int) : unit =
    if i = mat_L.num_rows then () else (
      set_val mat_L i k (c *. get_val mat_U i k);
      set_cv (i + 1)
    )
  in
  set_cv (k + 1);
  let rec set_zeroes (i : int) : unit = 
    if i = mat_U.num_rows then () else (
      set_val mat_U i k 0.;
      set_zeroes (i + 1)
    )
  in
  set_zeroes (k + 1);
  let rec sub_cvw (i : int) : unit =
    if i = mat_U.num_rows then () else
    let v = get_val mat_L i k in
    if v = 0. then () else (
      let rec sub_cvw_row (j : int) : unit = 
        if j = mat_U.num_cols then () else
        let x = get_val mat_U i j in
        let w = get_val mat_U k j in
        set_val mat_U i j (x -. v *. w);
        sub_cvw_row (j + 1)
      in
      sub_cvw_row (k + 1)
    );
    sub_cvw (i + 1)
  in
  sub_cvw (k + 1);
  decomp_aux mat_L mat_U (k + 1)

let decomp_LU (matrix : t) : t * t =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let l_vals = Array.init n (fun i -> Float.Array.init n (fun j -> if i = j then 1. else 0.)) in
  let mat_L = from_arr l_vals in
  let mat_U = copy matrix in
  decomp_aux mat_L mat_U 0;
  (mat_L, mat_U)

(* val solve_L : t -> floatarray -> floatarray
val solve_U : t -> floatarray -> floatarray
val solve_LU : t -> t -> floatarray -> floatarray *)

let solve_L (mat_L : t) (b : floatarray) : floatarray = 
  let _ = assert (mat_L.num_rows = mat_L.num_cols) in
  let n = mat_L.num_rows in
  let res = Float.Array.make n 0. in
  let rec aux (i : int) : unit = 
    if i = n then () else
    let row = get_row mat_L i in
    let a = Float.Array.get row i in
    let b_i = Float.Array.get b i in 
    let x = (b_i -. partial_dot_product row 0 res 0 i) /. a in
    Float.Array.set res i x;
    aux (i + 1)
  in
  aux 0;
  res

let solve_U (mat_U : t) (b : floatarray) : floatarray = 
  let _ = assert (mat_U.num_rows = mat_U.num_cols) in
  let n = mat_U.num_rows in
  let res = Float.Array.make n 0. in
  let rec aux (i : int) : unit = 
    if i = -1 then () else
    let row = get_row mat_U i in
    let a = Float.Array.get row i in
    let b_i = Float.Array.get b i in 
    let x = (b_i -. partial_dot_product row (i + 1) res (i + 1) (n - i - 1)) /. a in
    Float.Array.set res i x;
    aux (i - 1)
  in
  aux (n - 1);
  res

let solve_LU (mat_L : t) (mat_U : t) (b : floatarray) : floatarray = 
  let b1 = solve_L mat_L b in
  solve_U mat_U b1
