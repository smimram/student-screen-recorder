(** Configuration. *)

open Extlib

let admin_user = ref "admin"

let admin_password = ref "admin"

let check_events = ref false

(** Path where all internal data is stored. *)
let data = ref "data"

(** Path where screenshots are stored. *)
let screenshots = ref "data/screenshots"

(** Open events. *)
let events : string list option ref = ref None

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
