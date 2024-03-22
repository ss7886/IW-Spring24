type t = {
  num_rows : int;
  num_cols : int;
  count : int;
  vals : floatarray;
  cols : int array;
  row_ptr : int array;
}

let dense_to_sparse (dense : floatarray array) : t = 
  let num_rows = Array.length dense in
  let num_cols = Float.Array.length (Array.get dense 0) in
  let count_row (count : int) (row : floatarray) : int = 
    Float.Array.fold_left (fun count x -> if x = 0. then count else count + 1) count row
  in
  let count = Array.fold_left (count_row) 0 dense in
  let vals = Float.Array.create count in
  let cols = Array.make count (-1) in
  let row_ptr = Array.make num_rows (-1) in
  let index = ref 0 in
  let populate (col : int) (x : float) : unit = 
    if (x = 0.) then () else (
      Float.Array.set vals !index x;
      Array.set cols !index col;
      index := !index + 1
    )
  in
  let populate_row (rownum : int) (row : floatarray) : unit = (
    Array.set row_ptr rownum !index;
    Float.Array.iteri populate row
  )
  in 
  let _ = Array.iteri populate_row dense in
  {num_rows=num_rows; num_cols=num_cols; count=count; vals=vals; cols=cols;
    row_ptr=row_ptr}

let get_val (m : t) (row : int) (col : int) : float = 
  let count = m.count in
  let vals = m.vals in
  let cols = m.cols in
  let row_ptr = m.row_ptr in 
  let _ = assert (row >= 0 && col >= 0 && row < m.num_rows && col < m.num_cols) in
  let start_index = Array.get row_ptr row in
  let stop_index = if row < (m.num_rows - 1) then Array.get row_ptr (row + 1) else count in
  let rec find (index : int) (stop : int) : float =
    if index = stop then 0. else
      if Array.get cols index = col then Float.Array.get vals index else
        find (index + 1) stop
  in find start_index stop_index

let mult_row_vec (m : t) (b : floatarray) (row : int) : float = 
  let start_index = Array.get m.row_ptr row in
  let end_index = (if row = m.num_rows - 1 then m.count else Array.get m.row_ptr (row + 1)) in
  let rec aux (i : int) (stop : int) : float =
    if i = stop then 0. else
      let m_val = Float.Array.get m.vals i in
      let b_val = Float.Array.get b (Array.get m.cols i) in (
      m_val *. b_val +. aux (i + 1) stop
      )
  in aux start_index end_index

let mult_vec (m : t) (b : floatarray) : floatarray =
  Float.Array.init m.num_rows (mult_row_vec m b)

let mult_row_LU (m : t) (b : floatarray) (block_size : int) (row : int) : float = 
  let start_index = Array.get m.row_ptr row in
  let end_index = (if row = m.num_rows - 1 then m.count else Array.get m.row_ptr (row + 1)) in
  let rec aux (i : int) (stop : int) : float =
    if i = stop then 0. else
      let col = Array.get m.cols i in
      if row / block_size = col / block_size then 0. +. aux (i + 1) stop else
        let m_val = Float.Array.get m.vals i in
        let b_val = Float.Array.get b col in
        m_val *. b_val +. aux (i + 1) stop
  in aux start_index end_index

let mult_LU (m : t) (b : floatarray) (block_size : int) : floatarray = 
  Float.Array.init m.num_rows (mult_row_LU m b block_size)

let diag (m : t) : floatarray = 
  Float.Array.init (min m.num_cols m.num_rows) (fun i -> get_val m i i)

let diag_block (m : t) (block_size : int) : floatarray array array = 
  let diag_length = min m.num_cols m.num_rows in
  let init_zeroes (size : int) : floatarray array =
    Array.init size (fun _ -> Float.Array.make size 0.)
  in
  let res = Array.init (diag_length / block_size) (fun _ -> init_zeroes block_size) in
  let fill_vals (row : int) : unit =
    let start_index = Array.get m.row_ptr row in
    let end_index = (if row = m.num_rows - 1 then m.count else Array.get m.row_ptr (row + 1)) in
    let rec aux (i : int) : unit =
      let col = Array.get m.cols i in
      if col / block_size > row / block_size || i = end_index then () else
      if col / block_size = row / block_size then (
        let block = row / block_size in
        let block_col = col mod block_size in
        let block_row = row mod block_size in
        let x = Float.Array.get m.vals start_index in
        Float.Array.set (Array.get (Array.get res block) block_row) block_col x
      ) else aux (i + 1)
    in aux start_index
  in (
    for row = 0 to diag_length do
      fill_vals row
    done;
    res
  )
