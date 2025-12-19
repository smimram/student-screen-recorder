(** Events. *)

open Extlib

type t =
  {
    name : string;
    opening : float option; (** opening time *)
    closing : float option; (** closing time *)
  }

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

let name e = e.name

let list () =
  protect (fun () -> List.sort compare !events)

let exists name =
  List.exists (fun e -> e.name = name) @@ list ()

let find_opt name =
  List.find_opt (fun e -> e.name = name) @@ list ()

(** Whether current time is within the opening time of event. *)
let valid time e =
  (match e.opening with Some t -> t <= time | None -> true)
  && (match e.closing with Some t -> time <= t | None -> true)
