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

let par_map (f : 'a -> 'b) (arr : 'a array) (p : int) : 'b array =
  let n = Array.length arr in
  let init_domain (i : int) =
    Domain.spawn (fun _ ->
      let start = i * (n / p) in
      let stop = if i = p - 1 then n else (i + 1) * (n / p) in
      Array.init (stop - start) (fun i ->
        let index = i + start in
        let x = Array.get arr index in
        f x
      )
    )
  in
  let domains = List.init p init_domain in
  let results = List.map (Domain.join) domains in
  Array.concat results

let par_mapi (f : int -> 'a -> 'b) (arr : 'a array) (p : int) : 'b array =
  let n = Array.length arr in
  let init_domain (i : int) =
    Domain.spawn (fun _ ->
      let start = i * (n / p) in
      let stop = if i = p - 1 then n else (i + 1) * (n / p) in
      Array.init (stop - start) (fun i ->
        let index = i + start in
        let x = Array.get arr index in
        f index x
      )
    )
  in
  let domains = List.init p init_domain in
  let results = List.map (Domain.join) domains in
  Array.concat results

let par_float_map2 (f : float -> float -> float) (vec1 : floatarray) 
      (vec2 : floatarray) (p : int) : floatarray = 
  let n = Float.Array.length vec1 in
  let _ = assert (n = Float.Array.length vec2) in
  let results = Float.Array.make n 0. in
  let init_domain (i : int) =
    Domain.spawn (fun _ ->
      let start = i * (n / p) in
      let stop = if i = p - 1 then n else (i + 1) * (n / p) in
      let sub = Float.Array.init (stop - start) (fun i ->
        let index = i + start in
        let x1 = Float.Array.get vec1 index in
        let x2 = Float.Array.get vec2 index in
        f x1 x2
      ) in
      Float.Array.blit sub 0 results start (stop - start)
    )
  in
  let domains = List.init p init_domain in
  let _ = List.iter Domain.join domains in
  results
  