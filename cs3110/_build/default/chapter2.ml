let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | x -> fib (x - 1) + fib (x - 2)

(* auxilliary functions can be defined *)
let fib_fast n = 
  let rec aux iters_remaining double_prev prev = match iters_remaining with
    | iters_remaining when iters_remaining <= 0 -> 0
    | iters_remaining when iters_remaining = 1 -> prev
    | iters_remaining -> aux (iters_remaining-1) (prev) (prev + double_prev) 
  in aux n 0 1

(* infix operators can be defined *)
let ( +/. ) a b = (a +. b) /. 2.

let add x y = x + y

let () =
  assert ((fib 0) = (fib_fast 0));
  assert ((fib 1) = (fib_fast 1));
  assert ((fib 2) = (fib_fast 2));
  assert ((fib 5) = (fib_fast 5));
  assert (3. +/. 7. = 5.);
  assert ((add 5) 1  = add 5 1)
  

    
 
