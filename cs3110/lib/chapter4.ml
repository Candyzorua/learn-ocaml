let rec repeat f n x = match n with
| 0 -> x
| n when n > 0 -> repeat f (n-1) (f x)
| _ -> failwith "non-negative n required"

let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j

let sum_cube_odd n = List.fold_left ( + ) 0 
(List.map (fun x -> x * x * x) (List.filter (fun x -> x mod 2 = 1) (1 -- n)))

let sum_cube_odd_pipeline n =
  (1 -- n)
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

let rec exists_rec fn = function 
| [] -> false
| hd :: tl -> if (fn hd) then true else exists_rec fn tl

let exists_fold fn =
  List.fold_left (fun acc x -> (fn x) || acc) false

let exists_lib = List.exists

let double_map f g lst = List.map (fun x -> f (g x)) lst

let keys lst =
  lst
  |> List.rev_map fst
  |> List.sort_uniq Stdlib.compare

let is_valid_matrix lst =
  let lst_lengths = List.map List.length lst in match lst_lengths with
  | [] -> false
  | hd :: tl ->
    not (List.exists (fun x -> x = 0) lst_lengths) 
    && List.for_all (fun x -> x = hd) tl

let is_valid_matrix_2 = function
  | [] -> false
  | row :: rows ->
      let n = List.length row in
      n > 0 && List.for_all (fun r -> List.length r = n) rows
