(** Record last connections. *)

open Extlib

(** Last connection from a student. *)
type t =
  {
    time : float; (** last successful upload *)
    ip : string; (** IP for last connection *)
    port : int; (** port for last connection *)
    event : string; (** event for upload (which exam) *)
    filename : string; (** filename for last successful upload *)
  }

let protect f =
  let m = Mutex.create () in
  Mutex.protect m f

let table = Hashtbl.create 100

(** Set last connection. *)
let set ~time ~(student:Student.t) ~ip ~port ~event ~filename =
  let event = Event.name event in
  protect (fun () -> Hashtbl.replace table student { time; ip; port; event; filename })

let find_opt student =
  protect (fun () -> Hashtbl.find_opt table student)

(** Students alive. *)
let alive ?(since=60.) () =
  let t = Unix.time () in
  let compare (u1,l1) (u2,l2) =
    let c = Student.compare u1 u2 in
    if c <> 0 then c else compare l1 l2
  in
  protect (fun () -> Hashtbl.to_seq table) |> Seq.filter (fun (_,l) -> l.time >= t -. since) |> List.of_seq |> List.sort compare

(** Students who recently went away. *)
let gone ?since_alive ?(since_gone=3600.) ?event () =
  let gone = alive ~since:since_gone () in
  let alive = alive ?since:since_alive () in
  let gone = List.diff gone alive in
  match event with
  | Some e -> List.filter (fun (_,l) -> l.event = e) gone
  | None -> gone

(** Recent events. *)
let events ?(since=3600.) () =
  alive ~since () |> List.map snd |> List.map (fun l -> l.event) |> List.sort_uniq compare

(** Alive people by event. *)
let by_event ?since () =
  let events = events ?since () in
  let alive = alive ?since () in
  List.map (fun e -> e, List.filter (fun (_,l) -> l.event = e) alive) events

(** Alive people by IP. *)
let by_ip ?since () =
  let module M = Map.Make(String) in
  let alive = alive ?since () in
  let m = ref M.empty in
  List.iter
    (fun (u,l) ->
      let ip = l.ip in
      let uu = Option.value ~default:[] @@ M.find_opt ip !m in
      m := M.add ip (u::uu) !m
    ) alive;
  M.to_list !m
