module type SparseSig = sig
  type matrix = {
    n : int;
    count : int;
    vals : floatarray;
    cols : int array;
    row_ptr : int array;
  }

  val dense_to_sparse : floatarray array -> matrix

  val get_val : matrix -> int -> int -> float
  (* val set_val : matrix -> int -> int -> float -> unit

  val mult_vec : matrix -> floatarray -> floatarray
  val mult_row_vec : matrix -> int -> floatarray -> float *)
end

module SparseMatrix : SparseSig = struct
  type matrix = {
    n : int;
    count : int;
    vals : floatarray;
    cols : int array;
    row_ptr : int array;
  } ;;

  let dense_to_sparse (dense : floatarray array) : matrix = 
    let n = Array.length dense in
    let count_row (count : int) (row : floatarray) : int = 
      Float.Array.fold_left (fun count x -> if x = 0. then count else count + 1) count row
    in
    let count = Array.fold_left (count_row) 0 dense in
    let vals = Float.Array.create count in
    let cols = Array.make count (-1) in
    let row_ptr = Array.make n (-1) in
    let index = ref 0 in
    let populate (col : int) (x : float) : unit = 
      if x = 0. then () else (
      Float.Array.set vals !index x;
      Array.set cols !index col;
      index := !index + 1
    ) in
    let populate_row (rownum : int) (row : floatarray) : unit = (
      Array.set row_ptr rownum !index;
      Float.Array.iteri populate row
    )
    in 
    let _ = Array.iteri populate_row dense in
    {n=n; count=count; vals=vals; cols=cols; row_ptr=row_ptr}
  
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
end
