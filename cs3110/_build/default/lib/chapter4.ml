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

(* don't want the intermediate list *)
let is_valid_matrix_2 = function
  | [] -> false
  | row :: rows ->
      let n = List.length row in
      n > 0 && List.for_all (fun r -> List.length r = n) rows

let add_row_vectors vec1 vec2 =
  if (List.length vec1) <> (List.length vec2) then invalid_arg "unequal vector lengths"
  else List.map2 (+) vec1 vec2

let add_row_vectors_idiomatic vec1 vec2 =
  try List.map2 ( + ) vec1 vec2
  with Invalid_argument _ ->
    invalid_arg "unequal vector lengths"

let add_matrices mat1 mat2 =
  try List.map2 add_row_vectors mat1 mat2
  with Invalid_argument _ ->
  invalid_arg "unequal matrix dimensions"

let matrix_transpose mat =
  if not (is_valid_matrix mat) then invalid_arg "not a matrix"
  else let rec aux m =
      match m with
      | [] -> []
      | [] :: _ -> []
      | _ ->
          List.map List.hd m :: aux (List.map List.tl m)
    in
    aux mat

let row_vector_dot_product vec1 vec2 =
  try List.fold_left (+) 0 (List.map2 ( * ) vec1 vec2)
  with Invalid_argument _ ->
    invalid_arg "unequal vector lengths"

let matrix_multiply mat1 mat2 =
  let transposed_mat2 = (matrix_transpose mat2) in
  List.map (fun row_of_mat1 -> 
    List.map (row_vector_dot_product row_of_mat1) transposed_mat2) mat1

(* clearer without partial application *)
let matrix_multiply_clearer mat1 mat2 =
  let transposed_mat2 = matrix_transpose mat2 in
  List.map
    (fun row1 -> List.map (fun col2 -> row_vector_dot_product row1 col2) transposed_mat2)
    mat1
