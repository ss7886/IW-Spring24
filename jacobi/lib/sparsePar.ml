let par_mult_vec (mat: Sparse.t) (b: floatarray) (p: int) : floatarray =
  let mult_block (start: int) (stop: int) : floatarray = 
    Float.Array.init (stop - start) (fun i -> Sparse.mult_row_vec mat b (start + i))
  in
  let init_domain (i: int) = 
    Domain.spawn (fun _ -> mult_block (i * mat.num_rows / p) ((i + 1) * mat.num_rows / p))
  in
  let domains = List.init p init_domain in 
  let subs = List.map (fun d -> Domain.join d) domains in
  Float.Array.concat subs

let par_mult_LU (mat: Sparse.t) (b: floatarray) (block_size : int) (p: int) : floatarray =
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
