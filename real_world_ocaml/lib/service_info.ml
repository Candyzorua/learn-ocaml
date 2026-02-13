open Core

(* new record type *)
type service_info =
  { service_name : string;
    port         : int;
    protocol     : string;
    comment      : string option;
  }

(* field punning: fields are bound to variables of the same name *)
let service_info_to_string { service_name; port; protocol; comment } =
  let base = sprintf "%s %i/%s" service_name port protocol in
  match comment with
  | None -> base
  | Some text -> base ^ " #" ^ text;;

(* combining label and field punning
  note that the service_info type is inferred *)
let create_service_info ~service_name ~port ~protocol ~comment =
  { service_name; port; protocol; comment };;

(* record types can be polymorphic *)
type 'a with_line_num = { item: 'a; line_num: int }

let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_num line ->
    { item = parse line;
      line_num = line_num + 1;
    })

(* overlapping fields of records is a big problem in ocaml.
   we deal with this using 3 things:
   1. put diff record types in diff modules
   2. reference the type via the module
   3. type annotations *)

module Log_entry = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      important: bool;
      message: string;
    }
end
module Heartbeat = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      status_message: string;
    }
end
module Logon = struct
  type t =
    { session_id: string;
      time: Time_ns.t;
      user: string;
      credentials: string;
    }
end
(* we can still use label punning! *)
(* get rid of ambiguity by using the Log_entry module name here *)
let create_log_entry ~session_id ~important message =
  { Log_entry.time = Time_ns.now ();
    Log_entry.session_id;
    Log_entry.important;
    Log_entry.message
  };;

(* only need to reference the module name once *)
let create_log_entry_concise ~session_id ~important message =
  { Log_entry.
    time = Time_ns.now (); session_id; important; message }


(* using type annotations to do type-directed constructor disambiguation *)
let message_to_string ({ important; message; _ } : Log_entry.t) =
  if important then String.uppercase message else message
let is_important (t:Log_entry.t) = t.important

type client_info =
  { addr: Core_unix.Inet_addr.t; 
    port: int;
    user: string;
    credentials: string;
    last_heartbeat_time: Time_ns.t;
}
(* functional updating with the "with" keyword *)
let register_heartbeat t hb =
  { t with last_heartbeat_time = hb.Heartbeat.time };;
