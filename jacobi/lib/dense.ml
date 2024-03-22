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

let get_val (matrix : t) (i : int) (j : int) : float = 
  Float.Array.get (Array.get matrix.vals i) j

let set_val (matrix : t) (i : int) (j : int) (x : float) : unit =
  Float.Array.set (Array.get matrix.vals i) j x

let mult_vec (matrix : t) (b : floatarray) : floatarray =
  let _ = assert (matrix.num_cols = Float.Array.length b) in
  arr_to_floatarr (Array.map (dot_product b) matrix.vals)

let mult_row_vec (matrix : t) (b : floatarray) (row : int) : float =
  let _ = assert (matrix.num_cols = Float.Array.length b) in
  dot_product (Array.get matrix.vals row) b

let rec decomp_aux (l_mat : t) (u_mat : t) (k : int) : unit = 
  if k = u_mat.num_rows - 1 then () else
  let a = get_val u_mat k k in
  let c = 1. /. a in
  let rec set_cv (i : int) : unit =
    if i = l_mat.num_rows then () else (
      set_val l_mat i k (c *. get_val u_mat i k);
      set_cv (i + 1)
    )
  in
  set_cv (k + 1);
  let rec set_zeroes (i : int) : unit = 
    if i = u_mat.num_rows then () else (
      set_val u_mat i k 0.;
      set_zeroes (i + 1)
    )
  in
  set_zeroes (k + 1);
  let rec sub_cvw (i : int) : unit =
    if i = u_mat.num_rows then () else
    let rec sub_cvw_row (j : int) : unit = 
      if j = u_mat.num_cols then () else
      let x = get_val u_mat i j in
      let v = get_val l_mat i k in
      let w = get_val u_mat k j in
      set_val u_mat i j (x -. v *. w);
      sub_cvw_row (j + 1)
    in
    sub_cvw_row (k + 1);
    sub_cvw (i + 1)
  in
  sub_cvw (k + 1);
  decomp_aux l_mat u_mat (k + 1)

let decomp_LU (matrix : t) : t * t =
  let _ = assert (matrix.num_rows = matrix.num_cols) in
  let n = matrix.num_rows in
  let l_vals = Array.init n (fun i -> Float.Array.init n (fun j -> if i = j then 1. else 0.)) in
  let l_mat = from_arr l_vals in
  let u_mat = copy matrix in
  decomp_aux l_mat u_mat 0;
  (l_mat, u_mat)