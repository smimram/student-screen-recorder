(** Record last connections. *)

(** Last connection from a user. *)
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
let set ~time ~(user:User.t) ~ip ~port ~event ~filename =
  protect (fun () -> Hashtbl.replace table user { time; ip; port; event; filename })

let find_opt user =
  protect (fun () -> Hashtbl.find_opt table user)

(** Users alive. *)
let alive ?(since=120.) () =
  let t = Unix.time () in
  let compare (u1,l1) (u2,l2) =
    let c = User.compare u1 u2 in
    if c <> 0 then c else compare l1 l2
  in
  protect (fun () -> Hashtbl.to_seq table) |> Seq.filter (fun (_,l) -> l.time >= t -. since) |> List.of_seq |> List.sort compare

let events ?since () =
  alive ?since () |> List.map snd |> List.map (fun l -> l.event) |> List.sort_uniq compare

let by_event ?since () =
  let events = events ?since () in
  let alive = alive ?since () in
  List.map (fun e -> e, List.filter (fun (_,l) -> l.event = e) alive) events

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
