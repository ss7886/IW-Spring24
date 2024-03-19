let rec fibSeq (n: int) : int = 
  if n = 1 then 0 else
    if n = 2 then 1 else
      fibSeq (n - 1) + fibSeq (n - 2)

let fibPar (n: int) (p: int) : int = 
  if n < 20 || p = 1 then (
    let d1 = Domain.spawn (fun _ -> fibSeq n) in
    Domain.join d1
  ) else
  if p = 2 then (
    let d1 = Domain.spawn (fun _ -> fibSeq (n - 1)) in
    let d2 = Domain.spawn (fun _ -> fibSeq (n - 2)) in
    let r1 = Domain.join d1 in
    let r2 = Domain.join d2 in
    r1 + r2
  ) else
  if p = 4 then (
    let d1 = Domain.spawn (fun _ -> fibSeq (n - 4)) in
    let d2 = Domain.spawn (fun _ -> fibSeq (n - 3)) in
    let d3 = Domain.spawn (fun _ -> fibSeq (n - 3)) in
    let d4 = Domain.spawn (fun _ -> fibSeq (n - 2)) in
    (Domain.join d1) + (Domain.join d2) + (Domain.join d3) + (Domain.join d4)
  ) else 0 

let fibParN (n: int) (p: int) : int = 
  let d = List.init p (fun _ -> Domain.spawn (fun _ -> fibSeq n)) in
  let r = List.map Domain.join d in
  List.nth r 0

let fibTwice (n: int) : int = 
  let d1 = Domain.spawn (fun _ -> fibSeq n) in
  let d2 = Domain.spawn (fun _ -> fibSeq n) in
  let r1 = Domain.join d1 in
  Printf.printf "fib(%d) = %d\n%!" n r1;
  let r2 = Domain.join d2 in
  r2

let timer (f: unit -> unit) (iters: int) : unit = 
  let t = Sys.time() in
  let rec aux (i : int) : unit =
    if i = iters then () else (
      f ();
      aux (i + 1)
    )
  in (
    aux 0;
    print_string "Time to run ";
    print_int iters;
    print_string " iters: ";
    print_float (Sys.time() -. t);
    print_endline " seconds."
  )

let _ = fibPar
let _ = fibParN
let _ = fibTwice

(* let _ = timer (fun _ -> let x = fibSeq 42 in (print_int x; print_newline ())) 1
let _ = timer (fun _ -> let x = fibSeq 41 in (print_int x; print_newline ())) 1
let _ = timer (fun _ -> let x = fibSeq 40 in (print_int x; print_newline ())) 1
let _ = timer (fun _ -> let x = fibPar 42 1 in (print_int x; print_newline ())) 1
let _ = timer (fun _ -> let x = fibPar 42 2 in (print_int x; print_newline ())) 1
let _ = timer (fun _ -> let x = fibPar 42 4 in (print_int x; print_newline ())) 1

let _ = timer (fun _ -> let _ = fibParN 42 1 in ()) 1
let _ = timer (fun _ -> let _ = fibParN 42 2 in ()) 1
let _ = timer (fun _ -> let _ = fibParN 42 4 in ()) 1 *)

let _ = timer (fun _ -> let _ = fibSeq 42 in ()) 1
let _ = timer (fun _ -> let _ = fibTwice 42 in ()) 1

let _ = print_int (Domain.recommended_domain_count ())
