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

let read_sparse_from_csv (f: string) : Sparse.t = 
  let data = Csv.load f in
  let n, entries, data' = (
    match data with
    | line1 :: line2 :: data' -> parse_int line1, parse_int line2, data'
    | _ -> raise (BadFormat f)
  ) in
  let vals = Float.Array.create entries in
  let cols = Array.make entries 0 in
  let rows = Array.make n 0 in
  let rec update_rows (prev : int) (current : int) (i : int) : unit =
    if prev = current then () else (
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
        Float.Array.set vals i x;
        Array.set cols i col;
        aux tl (i + 1) row
    )
  in 
  (
    aux data' 0 (-1);
    {num_rows=n; num_cols=n; count=entries; vals=vals; cols=cols; row_ptr=rows}
  )

let build_sparse_from_csv (f : string) : Sparse.t =
  let data = Csv.load f in
  let n, _, data' = (
    match data with
    | line1 :: line2 :: data' -> parse_int line1, parse_int line2, data'
    | _ -> raise (BadFormat f)
  ) in
  let mat_builder = ref (Sparse.new_builder n n) in
  List.iter (fun line -> 
    let entry = read_entry line in
    mat_builder := Sparse.builder_insert (!mat_builder) entry) data';
  Sparse.build_sparse (!mat_builder)

let read_lines_from_mm (f : string) : string list list =
  let lines = Array.to_list(Arg.read_arg f) in
  let filtered = List.filter (fun s -> not (String.starts_with ~prefix:"%" s) && String.length s > 0) lines in
  List.map (String.split_on_char ' ') filtered

let build_sparse_from_mm (f : string) (symmetric : bool) : Sparse.t =
  let data = read_lines_from_mm f in
  let rows, cols, _, entries = (
    match data with
    | (str1 :: str2 :: str3 :: []) :: tl -> int_of_string str1, int_of_string str2, int_of_string str3, tl
    | _ -> raise (BadFormat f)
  ) in
  let mat_builder = ref (Sparse.new_builder rows cols) in
  List.iter (fun line ->
    let entry = read_entry line in
    let row, col, x = entry in
    let row, col = row - 1, col - 1 in
    mat_builder := Sparse.builder_insert (!mat_builder) (row, col, x);
    if symmetric && row != col then 
      mat_builder := Sparse.builder_insert (!mat_builder) (col, row, x) else ()
  ) entries;
  Sparse.build_sparse (!mat_builder)
