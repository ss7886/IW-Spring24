open JacobiLib
open CsvUtil
open Util

let b10_000 = read_vec_from_csv "data/b-10_000-normal.csv"

let _ = timer (fun _ -> let _ = dot_product b10_000 b10_000 in ()) 100000
let _ = timer (fun _ -> let _ = dot_product2 b10_000 b10_000 in ()) 100000
