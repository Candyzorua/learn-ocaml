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

(* can combine if and pattern matching*)
  let rec drop lst n =
    if n == 0 then lst else match lst with
    | [] -> []
    | _ :: tl -> drop tl (n-1)

(* can deconstruct list so we can use the second element too, not just the head *)
let rec is_mon_inc_then_dec = 
  let rec is_mon_dec = function
  | [] | [_] -> true
  | h1 :: h2 :: t2 ->
      h1 >= h2 && is_mon_dec (h2 :: t2) in 
  function
  | [] | [_] -> true
  | h1 :: (h2 :: _ as t) as lst ->
      if h1 <= h2
      then is_mon_inc_then_dec t
      else is_mon_dec lst

let add_elem_to_powerset pwset elem = List.concat (List.map (fun set -> [set; elem::set]) pwset)

let powerset lst = List.fold_left (add_elem_to_powerset) [[]] lst 


type student = { first_name : string ; last_name : string ; gpa : float }
let k = {first_name = "kevin"; last_name = "zhang"; gpa = 4.0}

type poketype = Normal | Fire | Water
type pokemon = {name:string; hp:int; ptype:poketype}

let charizard = { name = "charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "squirtle"; hp = 44; ptype = Water }





