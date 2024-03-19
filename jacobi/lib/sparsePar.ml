open Sparse

(* let par_mult_vec (p: int) (mat: SparseMatrix.matrix) (b: floatarray) : floatarray =
  let init_domain (i: int) =
    Domain.spawn (fun _ -> (
      print_string "Spawning Domain number ";
      print_int i;
      print_newline ()
    ))
  in let domains = Array.init p (init_domain) in (
    Array.iter (fun d -> Domain.join d) domains;
    SparseMatrix.mult_vec mat b
  ) *)

let par_mult_vec (p: int) (mat: Matrix.t) (b: floatarray) : floatarray =
  let mult_block (start: int) (stop: int) : floatarray = 
    let res = Float.Array.create (stop - start) in
    let rec aux (i: int) : unit = 
      if i = stop then () else (
        Float.Array.set res (i - start) (Matrix.mult_row_vec mat b i);
        aux (i + 1)
      )
    in (
      aux start;
      res
    )
  in
  let init_domain (i: int) = 
    Domain.spawn (fun _ -> mult_block (i * mat.num_rows / p) ((i + 1) * mat.num_rows / p))
  in
  let domains = List.init p init_domain in 
  let subs = (List.map (fun d -> Domain.join d) domains) in
  Float.Array.concat subs

let par_mult_LU (p: int) (mat: Matrix.t) (b: floatarray) (block_size : int) : floatarray =
  let mult_block (start: int) (stop: int) : floatarray = 
    let res = Float.Array.create (stop - start) in
    let rec aux (i: int) : unit = 
      if i = stop then () else (
        Float.Array.set res (i - start) (Matrix.mult_row_LU mat b block_size i);
        aux (i + 1)
      )
    in (
      aux start;
      res
    )
  in
  let init_domain (i: int) = 
    Domain.spawn (fun _ -> mult_block (i * mat.num_rows / p) ((i + 1) * mat.num_rows / p))
  in
  let domains = List.init p init_domain in 
  let subs = (List.map (fun d -> Domain.join d) domains) in
  Float.Array.concat subs

let par_mult_vec2 (ver: int) (hor: int) (cols: Matrix.t list) (b: floatarray) : floatarray =
  assert (hor = List.length cols);
  let mult_block (start: int) (stop: int) (mat: Matrix.t) (b_sub: floatarray): floatarray = 
    let res = Float.Array.create (stop - start) in
    let rec aux (i: int) : unit = 
      if i = stop then () else (
        Float.Array.set res (i - start) (Matrix.mult_row_vec mat b_sub i);
        aux (i + 1)
      )
    in (
      aux start;
      res
    )
  in
  let init_domain (i: int) = 
    let row = i / ver in
    let col = i mod ver in
    let mat = List.nth cols col in
    let start = row * mat.num_rows / ver in
    let stop = (row + 1) * mat.num_rows / ver in
    let b_sub = Float.Array.sub b start stop in
    Domain.spawn (fun _ -> mult_block start stop mat b_sub)
  in 
  let domains = List.init (ver * hor) init_domain in
  let subs = List.map (fun d -> Domain.join d) domains in
  let rec aux (subs: floatarray list) (sum: floatarray) (i: int) : floatarray list =
    match subs with
    | [] -> [sum]
    | hd :: tl ->
      if i = hor then sum :: aux tl hd 1 else aux tl (Float.Array.map2 (+.) hd sum) (i + 1)
  in
  Float.Array.concat (aux subs (Float.Array.make (Float.Array.length (List.nth subs 0)) 0.) 0)
