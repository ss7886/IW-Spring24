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

let par_mult_vec (p: int) (mat: SparseMatrix.matrix) (b: floatarray) : floatarray =
  let res = Float.Array.create mat.n in
  let mult_block (start: int) (stop: int) : unit = 
    let rec aux (i: int) : unit = 
      if i >= stop then () else (
        Float.Array.set res i (SparseMatrix.mult_row_vec mat i b);
        aux (i + 1)
      )
    in aux start
  in
  let init_domain (i: int) = 
    Domain.spawn (fun _ -> mult_block (i * mat.n / p) ((i + 1) * mat.n / p))
  in
  let domains = Array.init p (init_domain) in (
    Array.iter (fun d -> Domain.join d) domains;
    res
  )
