(** Events. *)

open Extlib

type t =
  {
    name : string;
    opening : float; (** opening time *)
    closing : float; (** closing time *)
  }

let protect f =
  let m = Mutex.create () in
  Mutex.protect m f

let events = ref []

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
               | `O ["begin", `String b; "end", `String e] ->
                  let opening = Time.of_string b in
                  let closing = Time.of_string e in
                  let e = {name; opening; closing} in
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
        e.name, `O ["begin", `String (Time.to_string e.opening); "end", `String (Time.to_string e.closing)]
      ) (protect (fun () -> !events))
  in
  let yaml = `O yaml in
  Yaml.to_string yaml |> Result.get_ok |> File.write fname
