let par_mult_vec (p: int) (mat: Sparse.t) (b: floatarray) : floatarray =
  let mult_block (start: int) (stop: int) : floatarray = 
    Float.Array.init (stop - start) (fun i -> Sparse.mult_row_vec mat b (start + i))
  in
  let init_domain (i: int) = 
    Domain.spawn (fun _ -> mult_block (i * mat.num_rows / p) ((i + 1) * mat.num_rows / p))
  in
  let domains = List.init p init_domain in 
  let subs = List.map (fun d -> Domain.join d) domains in
  Float.Array.concat subs

let par_mult_LU (p: int) (mat: Sparse.t) (b: floatarray) (block_size : int) : floatarray =
  let mult_block (start: int) (stop: int) : floatarray = 
    let res = Float.Array.create (stop - start) in
    let rec aux (i: int) : unit = 
      if i = stop then () else (
        Float.Array.set res (i - start) (Sparse.mult_row_LU mat b block_size i);
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

let par_mult_vec2 (ver: int) (hor: int) (cols: Sparse.t list) (b: floatarray) : floatarray =
  assert (hor = List.length cols);
  let mult_block (start: int) (stop: int) (mat: Sparse.t) (b_sub: floatarray): floatarray = 
    let res = Float.Array.create (stop - start) in
    let rec aux (i: int) : unit = 
      if i = stop then () else (
        Float.Array.set res (i - start) (Sparse.mult_row_vec mat b_sub i);
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
