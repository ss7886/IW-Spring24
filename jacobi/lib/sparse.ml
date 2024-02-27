module type SparseSig = sig
  type matrix = {
    n : int;
    count : int;
    vals : floatarray;
    cols : int array;
    diag : floatarray;
    row_ptr : int array;
  }

  val dense_to_sparse : floatarray array -> matrix

  val get_val : matrix -> int -> int -> float
  val mult_vec : matrix -> floatarray -> floatarray
  val mult_row_vec : matrix -> int -> floatarray -> float
end

module SparseMatrix : SparseSig = struct
  type matrix = {
    n : int;
    count : int;
    vals : floatarray;
    cols : int array;
    diag : floatarray;
    row_ptr : int array;
  }

  let dense_to_sparse (dense : floatarray array) : matrix = 
    let n = Array.length dense in
    let count_row (count : int) (row : floatarray) : int = 
      Float.Array.fold_left (fun count x -> if x = 0. then count else count + 1) count row
    in
    let count = Array.fold_left (count_row) 0 dense in
    let vals = Float.Array.create count in
    let cols = Array.make count (-1) in
    let diag = Float.Array.create n in
    let row_ptr = Array.make n (-1) in
    let index = ref 0 in
    let populate (row : int) (col : int) (x : float) : unit = 
      if x = 0. then (if row = col then Float.Array.set diag row 0.) else (
      Float.Array.set vals !index x;
      if row = col then Float.Array.set diag row x else ();
      Array.set cols !index col;
      index := !index + 1
    ) in
    let populate_row (rownum : int) (row : floatarray) : unit = (
      Array.set row_ptr rownum !index;
      Float.Array.iteri (populate rownum) row
    )
    in 
    let _ = Array.iteri populate_row dense in
    {n=n; count=count; vals=vals; cols=cols; diag=diag; row_ptr=row_ptr}
  
  let get_val (m : matrix) (row : int) (col : int) : float = 
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
  
  let mult_row_vec (m : matrix) (row : int) (b : floatarray) : float = 
    let start_index = Array.get m.row_ptr row in
    let end_index = (if row = m.n - 1 then m.count else Array.get m.row_ptr (row + 1)) in
    let rec aux (i : int) (stop : int) : float =
      if i = stop then 0. else
        let m_val = Float.Array.get m.vals i in
        let b_val = Float.Array.get b (Array.get m.cols i) in (
        m_val *. b_val +. aux (i + 1) stop
        )
    in aux start_index end_index
  
  let mult_vec (m : matrix) (b : floatarray) : floatarray =
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

end
