(* order of labeled arguments should be kept consistent between calling them and their definition *)
(* because it matters when passing these functions in a higher-order context *)
(* these two functions have different types*)
let apply_to_tuple f (first,second) = f ~first ~second
let apply_to_tuple_2 f (first,second) = f ~second ~first
let divide ~first ~second = first / second

(* apply_to_tuple divide (3,4) works but not apply_to_tuple_2 divide (3,4)*)
