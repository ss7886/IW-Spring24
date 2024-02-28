open Sparse

exception BadFormat of string
exception BadLine of string list

let parse_int (line: string list) : int = 
  match line with
  | str :: [] -> int_of_string str
  | _ -> raise (BadLine line)

let rec parse_floats (line: string list) : float list = 
  match line with
  | [] -> []
  | x :: tl -> float_of_string x :: parse_floats tl

let read_entry (line: string list) : (int * int * float) = 
  match line with
  | row :: col :: x :: [] -> int_of_string row, int_of_string col, float_of_string x
  | _ -> raise (BadLine line)

let read_vec_from_csv (f: string) : floatarray = 
  let data = Csv.load f in
  let _, floats = (
    match data with
    | line1 :: line2 :: [] -> parse_int line1, parse_floats line2
    | _ -> raise (BadFormat f)
  ) in
  Float.Array.of_list floats

let read_sparse_from_csv (f: string) : SparseMatrix.matrix = 
  let data = Csv.load f in
  let n, entries, data' = (
    match data with
    | line1 :: line2 :: data' -> parse_int line1, parse_int line2, data'
    | _ -> raise (BadFormat f)
  ) in
  let vals = Float.Array.create entries in
  let cols = Array.make entries 0 in
  let diag = Float.Array.create n in
  let rows = Array.make n 0 in
  let diagFilled = ref false in
  let rec update_rows (prev : int) (current : int) (i : int) : unit =
    if prev = current then () else (
      if not !diagFilled && prev > 0 then (
        Float.Array.set diag prev 0.
      ) else ();
      diagFilled := false;
      Array.set rows current i;
      update_rows (prev + 1) current i
    ) 
  in
  let rec aux (data': string list list) (i: int) (prev_row: int) : unit =
    match data' with
    | [] -> ()
    | line :: tl -> 
      let row, col, x = read_entry line in (
        if row > prev_row then update_rows prev_row row i else ();
        if row = col then (
          diagFilled := true;
          Float.Array.set diag row x
        ) else ();
        if row > col && not !diagFilled then (
          diagFilled := true;
          Float.Array.set diag row x
        ) else ();
        Float.Array.set vals i x;
        Array.set cols i col;
        aux tl (i + 1) row
    )
  in 
  (
    aux data' 0 (-1);
    {n=n; count=entries; vals=vals; cols=cols; diag=diag; row_ptr=rows}
  )
