open Base

type 'a element =
  { value : 'a
  ; mutable next : 'a element option
  ; mutable prev : 'a element option
  }

type 'a t =
  { mutable first : 'a element option
  }

let create () = { first = None }
let is_empty t = Option.is_none t.first
let first t = t.first
let next elt = elt.next
let prev elt = elt.prev
let value elt = elt.value

let iter t ~f =
  let rec loop = function
    | None -> ()
    | Some elt ->
      f elt.value;
      loop elt.next
  in
  loop t.first

let find_el t ~f =
  let rec loop = function
    | None -> None
    | Some elt when f elt.value -> Some elt
    | Some elt -> loop elt.next
  in
  loop t.first

let insert_first t value =
  let new_elt = { value; prev = None; next = t.first } in
  (match t.first with
   | Some old_first -> old_first.prev <- Some new_elt
   | None -> ());
  t.first <- Some new_elt;
  new_elt

let insert_after elt value =
  let new_elt = { value; prev = Some elt; next = elt.next } in
  (match elt.next with
   | Some old_next -> old_next.prev <- Some new_elt
   | None -> ());
  elt.next <- Some new_elt;
  new_elt

let remove t elt =
  (match elt.prev with
   | Some prev -> prev.next <- elt.next
   | None -> t.first <- elt.next);
  (match elt.next with
   | Some next -> next.prev <- elt.prev
   | None -> ());
  elt.prev <- None;
  elt.next <- None
