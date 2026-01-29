let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | x -> fib (x - 1) + fib (x - 2)

let fib_fast n = 
  let rec aux iters_remaining prev double_prev = match iters_remaining with
    | iters_remaining when iters_remaining <= 0 -> 0
    | iters_remaining when iters_remaining = 1 -> prev
    | iters_remaining -> aux (iters_remaining-1) (prev + double_prev) (prev)
  in aux n 1 0

let () =
  print_endline (string_of_int (fib 5));
  print_endline (string_of_int (fib_fast 5));
  assert ((fib 5) = (fib_fast 5))
  

    
 
