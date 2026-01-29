let product lst = List.fold_left ( * ) 1 lst
let concat lst = List.fold_left ( ^ ) "" lst  

let first_bigred = function
  | "bigred" :: _ -> true
  | _ -> false

let lst_len_two_four = function 
  | [_a; _b; _c; _d] -> true
  | [_a; _b] -> true
  | _ -> false
