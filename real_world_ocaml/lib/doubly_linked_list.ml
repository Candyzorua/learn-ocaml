open Base

type 'a element =
  { value : 'a
  ; mutable next : 'a element option
  ; mutable prev : 'a element option
  }

type 'a t = 'a element option ref

let insert_first t value =
  let new_elt = { prev = None; next = !t; value } in
  (match !t with
  | Some old_first -> old_first.prev <- Some new_elt
  | None -> ());
  t := Some new_elt;
  new_elt

let insert_after elt value =
  let new_elt = { value; prev = Some elt; next = elt.next } in
  (match elt.next with
  | Some old_next -> old_next.prev <- Some new_elt
  | None -> ());
  elt.next <- Some new_elt;
  new_elt

let remove t elt =
  let { prev; next; _ } = elt in 
    (match prev with 
    | Some prev -> prev.next <- next
    | None -> t := next);
    (match next with 
    | Some next -> next.prev <- prev
    | None -> ());
  elt.prev<-None;
  elt.next<-None

  
