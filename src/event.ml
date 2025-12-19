(** Events. *)

(* open Extlib *)

type t =
  {
    name : string;
    opening : float option; (** opening time *)
    closing : float option; (** closing time *)
  }

(** All available events. *)
let list () =
  Sys.readdir !Config.events
  |> Array.to_list
  |> List.sort compare
  |> List.filter (fun d -> Sys.is_directory @@ Filename.concat !Config.events d)

let exists event =
  List.mem event @@ list ()

let find_opt name =
  if not (List.mem name @@ list ()) then None else
    Some { name; opening = None; closing = None }

(** Whether current time is within the opening time of event. *)
let valid time _event =
  (* TODO: parse opening and closing time *)
  (* let conf = Filename.concat (Filename.concat !Config.events name) "_event.yml" in *)
  (* if not @@ Sys.file_exists conf then *)
  let opening = None in
  let closing = None in
  (match opening with Some t -> t <= time | None -> true)
  && (match closing with Some t -> time <= t | None -> true)

let dir event = Filename.concat !Config.events event

let screenshots_rows event =
  let csv = Filename.concat (dir event) "screenshots.csv" in
  if not (Sys.file_exists csv) then [] else
    In_channel.with_open_bin csv (fun ic -> Csv.of_channel ~has_header:true ic |> Csv.Rows.input_all)

(** All students who submitted in the event. *)
let students event =
  screenshots_rows event
  |> List.map
       (fun row ->
         let find = Csv.Row.find row in
         let firstname = find "Firstname" in
         let lastname = find "Lastname" in
         let u = Student.make ~firstname ~lastname in
         u
       )
  |> List.sort_uniq Student.compare

(** All screenshots from a student. *)
let student_screenshots event s =
  let dir = dir event in
  screenshots_rows event
  |> List.filter_map
       (fun row ->
         let find = Csv.Row.find row in
         let firstname = find "Firstname" in
         let lastname = find "Lastname" in
         let filename = find "Filename" in
         if Student.make ~firstname ~lastname = s then Some filename
         else None
       )
  |> List.map (Filename.concat dir)

let () = ()

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
