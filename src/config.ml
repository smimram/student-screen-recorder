(** Configuration. *)

open Extlib

let admin_user = ref "admin"

let admin_password = ref "admin"

(** Path where all internal data is stored. *)
let data = ref "data"

(** Path where screenshots for events are stored. *)
let events = ref "data/events"

(** Load the configuration file. *)
let load fname =
  let yaml = File.read fname |> Yaml.of_string |> Result.get_ok in
  match yaml with
  | `O l ->
     List.iter
       (function
        | "admin_user", s -> admin_user := Yaml.Util.to_string_exn s
        | "admin_password", s -> admin_password := Yaml.Util.to_string_exn s
        | k, _ -> Printf.printf "ignoring entry: %s\n%!" k
       ) l
  | `Null -> ()
  | _ -> assert false
