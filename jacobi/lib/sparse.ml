type t = {
  num_rows : int;
  num_cols : int;
  count : int;
  vals : floatarray;
  cols : int array;
  row_ptr : int array;
}

type entry = int * int * float

type builder = {
    num_rows : int;
    num_cols : int;
    count : int;
    capacity : int;
    entries : entry array;
}

let dense_to_sparse (dense : Dense.t) : t = 
  let count_row (count : int) (row : floatarray) : int = 
    Float.Array.fold_left (fun count x -> if x = 0. then count else count + 1) count row
  in
  let count = Array.fold_left (count_row) 0 dense.vals in
  let vals = Float.Array.create count in
  let cols = Array.make count (-1) in
  let row_ptr = Array.make dense.num_rows (-1) in
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
  let _ = Array.iteri populate_row dense.vals in
  { num_rows=dense.num_rows; num_cols=dense.num_cols; count=count; vals=vals;
    cols=cols; row_ptr=row_ptr }

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

let diag_block (m : t) (block_size : int) : Dense.t array = 
  let _ = assert (m.num_cols = m.num_rows) in
  let n = m.num_cols in
  let init_zeroes (size : int) : floatarray array =
    Array.init size (fun _ -> Float.Array.make size 0.)
  in
  let overflow = n mod block_size in
  let num_blocks = n / block_size + if overflow > 0 then 1 else 0 in
  let vals = Array.init num_blocks (fun i -> 
    if i = n / block_size then init_zeroes overflow 
    else init_zeroes block_size) in
  let res = Array.map Dense.from_arr vals in
  let rec fill_vals (row : int) : unit = 
    if row = n then () else
    let start = Array.get m.row_ptr row in
    let stop = (if row = m.num_rows - 1 then m.count else Array.get m.row_ptr (row + 1)) in
    let rec aux (i : int) : unit =
      if i = stop then () else
      let col = Array.get m.cols i in
      if col / block_size > row / block_size then () else
      if col / block_size = row / block_size then (
        let block = row / block_size in
        let block_col = col mod block_size in
        let block_row = row mod block_size in
        let x = Float.Array.get m.vals i in
        Dense.set_val (Array.get res block) block_row block_col x;
        aux (i + 1)
      ) else aux (i + 1)
    in
    aux start;
    fill_vals (row + 1)
  in
  fill_vals 0;
  res

let is_diag_dominant (m : t) : bool =
  let rec aux_rows (row : int) : bool =
    if row = m.num_rows then true else (
      let start = Array.get m.row_ptr row in
      let stop = if row = m.num_rows - 1 then m.count else Array.get m.row_ptr (row + 1) in
      let rec aux (i : int) (diag : float) (off_diag : float) : bool =
        if i = stop then (diag > off_diag) else (
          let x = Float.Array.get m.vals i in
          let col = Array.get m.cols i in
          if row = col then aux (i + 1) (diag +. x) off_diag else aux (i + 1) diag (off_diag +. x)
        )
      in aux start 0. 0. && aux_rows (row + 1)
    )
  in aux_rows 0

let new_builder (num_rows : int) (num_cols : int) : builder =
  let capacity = 8 in
  let entries = Array.make capacity (0, 0, 0.) in
  {
    num_rows=num_rows; num_cols=num_cols; count=0; capacity=capacity; 
    entries=entries
  }

let resize_builder (mat_builder : builder) (capacity : int) : builder =
  let count = mat_builder.count in
  let _ = assert (capacity >= count) in
  let new_entries = Array.make capacity (0, 0, 0.) in
  let _ = Array.blit mat_builder.entries 0 new_entries 0 count in
  {
    num_rows=mat_builder.num_rows; num_cols=mat_builder.num_cols; count=count;
    capacity=capacity; entries=new_entries
  }

let builder_insert (mat_builder : builder) (new_entry : entry) : builder =
  let row, col, _ = new_entry in
  let _ = assert (row >= 0 && row < mat_builder.num_rows) in
  let _ = assert (col >= 0 && col < mat_builder.num_cols) in
  let count = mat_builder.count in
  let capacity = mat_builder.capacity in
  let mat_builder = if count < capacity then mat_builder 
    else resize_builder mat_builder (capacity * 2) in
  let _ = Array.set mat_builder.entries count new_entry in
  {
    num_rows=mat_builder.num_rows; num_cols=mat_builder.num_cols;
    count=count + 1; capacity=mat_builder.capacity; entries=mat_builder.entries
  }

let compare_entry (a : entry) (b : entry) : int =
  let row_a, col_a, val_a = a in
  let row_b, col_b, val_b = b in
  if row_a = row_b then
    if col_a = col_b then
      if val_a = 0. then -1 else if val_b = 0. then 1 else 0
    else compare col_a col_b
  else
    compare row_a row_b

let build_sparse (mat_builder : builder) : t =
  let num_rows = mat_builder.num_rows in
  let num_cols = mat_builder.num_cols in
  let count = mat_builder.count in
  let entries = mat_builder.entries in
  let _ = Array.sort compare_entry mat_builder.entries in
  let skip = mat_builder.capacity - mat_builder.count in
  let vals = Float.Array.make count 0. in
  let cols = Array.make count 0 in
  let row_ptr = Array.make num_rows 0 in
  let last_row = ref (-1) in

  let populate (index : int) (row, col, value : entry) : unit =
    let i = index - skip in 
    if i < 0 then () else (
      if row > !last_row then
        let rec update_row (index : int) : unit = 
          if index > row then () else (
            Array.set row_ptr index i;
            last_row := index;
            update_row (index + 1)
          )
        in update_row (!last_row + 1)
      else ();
      Float.Array.set vals i value;
      Array.set cols i col
    )
  in

  Array.iteri populate entries;
  {
    num_rows=num_rows; num_cols=num_cols; count=count; vals=vals; cols=cols;
    row_ptr=row_ptr;
  }
