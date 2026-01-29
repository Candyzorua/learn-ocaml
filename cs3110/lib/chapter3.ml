let product lst = List.fold_left ( * ) 1 lst
let concat lst = List.fold_left ( ^ ) "" lst  

let first_bigred = function
  | "bigred" :: _ -> true
  | _ -> false

let lst_len_two_four = function 
  | [_a; _b; _c; _d] -> true
  | [_a; _b] -> true
  | _ -> false

let fifth_elem lst = if (List.length lst >= 5) then (List.nth lst 5) else 0
let desc_sort lst = List.rev (List.sort (Stdlib.compare) lst)

(* can pattern match with 2 vars at once! *)
let take lst n =
  let rec aux acc n lst =
    match lst, n with
    | _, 0 -> acc
    | [], _ -> acc
    | hd :: tl, _ -> aux (hd :: acc) (n - 1) tl
  in
  List.rev (aux [] n lst)

