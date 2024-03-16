module type SparseSig = sig
  type t = {
    num_rows : int;
    num_cols : int;
    count : int;
    vals : floatarray;
    cols : int array;
    row_ptr : int array;
  }

  val dense_to_sparse : floatarray array -> t

  val get_val : t -> int -> int -> float
  val mult_vec : t -> floatarray -> floatarray
  val mult_row_vec : t -> int -> floatarray -> float
end

module Matrix : SparseSig = struct
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
  
  let mult_row_vec (m : t) (row : int) (b : floatarray) : float = 
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
    let res = Float.Array.create m.num_rows in
    let rec aux (i : int) : unit =
      if i = m.num_rows then () else (
        Float.Array.set res i (mult_row_vec m i b);
        aux (i + 1)
      )
    in (
      aux 0;
      res
    )
end

module type SparseSquareSig = sig
  type t = {
    n : int;
    count : int;
    vals : floatarray;
    cols : int array;
    diag : floatarray;
    row_ptr : int array;
  }

  val dense_to_sparse : floatarray array -> t

  val get_val : t -> int -> int -> float
  val mult_vec : t -> floatarray -> floatarray
  val mult_row_vec : t -> int -> floatarray -> float
  (* val split_cols : t -> int -> Matrix.t list *)
end

module Square : SparseSquareSig = struct
  type t = {
    n : int;
    count : int;
    vals : floatarray;
    cols : int array;
    diag : floatarray;
    row_ptr : int array;
  }

  let dense_to_sparse (dense : floatarray array) : t = 
    let mat = Matrix.dense_to_sparse dense in
    let _ = assert (mat.num_rows = mat.num_cols) in
    let diag = Float.Array.init mat.num_rows (fun i -> Matrix.get_val mat i i) in
    {n=mat.num_rows; count=mat.count; vals=mat.vals; cols=mat.cols; diag=diag; 
     row_ptr=mat.row_ptr}
  
  let get_val (m : t) (row : int) (col : int) : float = 
    let n = m.n in
    let count = m.count in
    let vals = m.vals in
    let cols = m.cols in
    let row_ptr = m.row_ptr in 
    let _ = assert (row >= 0 && col >= 0 && row < n && col < n) in
    let start_index = Array.get row_ptr row in
    let stop_index = if row < (n - 1) then Array.get row_ptr (row + 1) else count in
    let rec find (index : int) (stop : int) : float =
      if index = stop then 0. else
        if Array.get cols index = col then Float.Array.get vals index else
          find (index + 1) stop
    in find start_index stop_index
  
  let mult_row_vec (m : t) (row : int) (b : floatarray) : float = 
    let start_index = Array.get m.row_ptr row in
    let end_index = (if row = m.n - 1 then m.count else Array.get m.row_ptr (row + 1)) in
    let rec aux (i : int) (stop : int) : float =
      if i = stop then 0. else
        let m_val = Float.Array.get m.vals i in
        let b_val = Float.Array.get b (Array.get m.cols i) in (
        m_val *. b_val +. aux (i + 1) stop
        )
    in aux start_index end_index
  
  let mult_vec (m : t) (b : floatarray) : floatarray =
    let res = Float.Array.create m.n in
    let rec aux (i : int) : unit =
      if i = m.n then () else (
        Float.Array.set res i (mult_row_vec m i b);
        aux (i + 1)
      )
    in (
      aux 0;
      res
    )
  
  (* let split_cols (mat: t) (k: int) : Matrix.t list =
    let counts = Array.make k 0 in
    let _ = Array.iter (fun i -> 
      let j = i * k / mat.n in
      Array.set counts j (Array.get counts j + 1)
    ) mat.cols in
    let vals = Array.init k (fun i -> Float.Array.create (Array.get counts i)) in
    let cols = Array.init k (fun i -> Array.make (Array.get counts i) 0) in
    let rows = Array.init k (fun i -> Array.make mat.n 0) in
    let indices = Array.make k 0 in
    let next_row = ref 0 in
    let row_num = ref (-1) in
    let _ = Array.iteri  (fun old_i col ->
      if old_i = !next_row then (
        
      );
      let x = Float.Array.get mat.vals old_i in
      let j = col * k / mat.n in
      let new_i = Array.get indices j in
      let new_col = (((col * k) mod mat.n) / k) in (
        Float.Array.set (Array.get vals j) new_i x;
        Array.set (Array.get cols j) new_i new_col
      )
    ) mat.cols in *)

end
