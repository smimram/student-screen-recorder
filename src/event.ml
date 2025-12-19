(** Events. *)

(* open Extlib *)

type t =
  {
    name : string;
    opening : float option; (** opening time *)
    closing : float option; (** closing time *)
  }

let name e = e.name

(** All available event names. *)
let names () =
  Sys.readdir !Config.events
  |> Array.to_list
  |> List.sort compare
  |> List.filter (fun d -> Sys.is_directory @@ Filename.concat !Config.events d)

let find_opt name =
  if not (List.mem name @@ names ()) then None else
    (* let conf = Filename.concat (Filename.concat !Config.events name) "_event.yml" in *)
    (* if not @@ Sys.file_exists conf then *)
    Some { name; opening = None; closing = None }

(** Whether current time is within the opening time of event. *)
let valid time e =
  (match e.opening with Some t -> t <= time | None -> true)
  && (match e.closing with Some t -> time <= t | None -> true)

(*
let protect f =
  let m = Mutex.create () in
  Mutex.protect m f

let events = ref [{name = "test"; opening = None; closing = None}]

let load fname =
  let yaml = File.read fname |> Yaml.of_string |> Result.get_ok in
  protect (fun () ->
      events := [];
      match yaml with
      | `O l ->
         List.iter
           (function
            | name, yaml ->
               match yaml with
               | `O ["begin", opening; "end", closing] ->
                  let to_time_opt s =
                    s
                    |> Yaml.Util.to_string_option_exn
                    |> Option.map Time.of_string
                  in
                  let opening = to_time_opt opening in
                  let closing = to_time_opt closing in
                  let e = {name; opening = opening; closing = closing} in
                  events := e :: !events
               | _ -> assert false
           ) l
      | `Null -> ()
      | _ -> assert false
    )

let store fname =
  let yaml =
    List.map
      (fun e ->
        let opening = Option.fold ~none:`Null ~some:(fun s -> `String (Time.to_string s)) e.opening in
        let closing = Option.fold ~none:`Null ~some:(fun s -> `String (Time.to_string s)) e.closing in
        e.name, `O ["begin", opening; "end", closing]
      ) (protect (fun () -> !events))
  in
  let yaml = `O yaml in
  Yaml.to_string yaml |> Result.get_ok |> File.write fname
 *)
