open Util

let par_dot_product (v1 : floatarray) (v2 : floatarray) (p : int) : float =
  let n = Float.Array.length v1 in
  let _ = assert (n = Float.Array.length v2) in
  let init_domain (i : int) =
    Domain.spawn (fun _ -> 
      let start = i * (n / p) in
      let n_block = n / p + if i = p - 1 then n mod p else 0 in
      partial_dot_product v1 start v2 start n_block
    )
  in 
  let domains = List.init p init_domain in
  let results = List.map (Domain.join) domains in
  List.fold_left ( +. ) 0. results

let par_add_vec (v1 : floatarray) (v2 : floatarray) (p : int) : floatarray = 
  let n = Float.Array.length v1 in
  let _ = assert (n = Float.Array.length v2) in
  let init_domain (i : int) =
    Domain.spawn (fun _ ->
      let start = i * (n / p) in
      let stop = if i = p - 1 then n else (i + 1) * (n / p) in
      Float.Array.init (stop - start) (fun i ->
        let index = i + start in
        let x1 = Float.Array.get v1 index in
        let x2 = Float.Array.get v2 index in
        x1 +. x2
      )
    )
  in
  let domains = List.init p init_domain in
  let results = List.map Domain.join domains in
  Float.Array.concat results

let par_sub_vec (v1 : floatarray) (v2 : floatarray) (p : int) : floatarray = 
  let n = Float.Array.length v1 in
  let _ = assert (n = Float.Array.length v2) in
  let init_domain (i : int) =
    Domain.spawn (fun _ ->
      let start = i * (n / p) in
      let stop = if i = p - 1 then n else (i + 1) * (n / p) in
      Float.Array.init (stop - start) (fun i ->
        let index = i + start in
        let x1 = Float.Array.get v1 index in
        let x2 = Float.Array.get v2 index in
        x1 -. x2
      )
    )
  in
  let domains = List.init p init_domain in
  let results = List.map Domain.join domains in
  Float.Array.concat results
  