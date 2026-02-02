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

let safe_hd lst = match lst with
| [] -> None
| hd :: _ -> Some hd

let safe_tl lst = match lst with
| [] -> None
| _ :: tl -> Some tl

let max_hp lst = match lst with
| [] -> None
| hd :: tl -> 
  Some (List.fold_left (fun acc pkmn -> if pkmn.hp > acc.hp then pkmn else acc) hd tl)

  let is_before date1 date2 =
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2)

let is_before lst = match lst with 
| [] -> None
| hd :: tl -> 
  Some (List.fold_left (fun acc dt -> if is_before dt acc then dt else acc) hd tl)

  (** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t

let dict = insert "height" 157 (insert "weight" 57 [])

type suit = Diamond | Club | Heart | Spade
type rank = Number of int | Jack | Queen | King | Ace
type card = {suit: suit; rank: rank}
let ace_of_spades = {suit=Spade; rank=Ace}

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

(* if is clearer than pattern-matching here *)
let sign x =
  if x = 0 then Zero
  else if x > 0 then Pos
  else Neg

let quadrant : int*int -> quad option = fun (x,y) ->
  match sign x, sign y with
    | Pos, Pos -> Some I
    | Neg, Pos -> Some II
    | Neg, Neg -> Some III
    | Pos, Neg -> Some IV
    | _ -> None

(* cond guards *)
let quadrant_when : int*int -> quad option = function
    | x, y when x > 0 && y > 0 -> Some I
    | x, y when x < 0 && y > 0 -> Some II
    | x, y when x < 0 && y < 0 -> Some III
    | x, y when x > 0 && y > 0 -> Some IV
    | _ -> None

(* ocaml's trees look different from python's trees! *)
(* python's trees: trees' methods do something *)
(* ocaml's trees: functions do something on the tree *)
(* should use parameterized type here *)
type 'a tree =
  | Leaf
  | Node of {
      value : 'a;
      left  : 'a tree;
      right : 'a tree;
    }
  
(* unpacking on ADTs *)
(* record fields are matched by name. for ignored record fields we can just put _ at the end *)
let rec depth = function
| Leaf -> 0
| Node { left; right; _ } ->
    1 + max (depth left) (depth right)

(* when there are two params and both curried / tuple are possible, tuple is preferred *)
let rec shape t1 t2 =
  match t1, t2 with
  | Leaf, Leaf -> true
  | Leaf, Node _ -> false
  | Node _, Leaf -> false
  | Node { left = l1; right = r1; _ }, (* can use = to assign names for record unpacking*)
    Node { left = l2; right = r2; _ } ->
      shape l1 l2 && shape r1 r2

(* sentinel value is a smell so we don't want to use max_int here *)
(* using h preserves generality *)
let list_max = function
| [] -> failwith "empty list"
| h :: t -> List.fold_left max h t

let list_max_string = function 
| [] -> "empty"
| h :: t -> string_of_int (List.fold_left max h t)

type ('a, 'b) bstree =
  | Leaf
  | BSTNode of {
      value : 'a * 'b;
      left  : ('a, 'b) bstree;
      right : ('a, 'b) bstree;
    }








