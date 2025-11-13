type t =
  {
    time : float; (** last successful upload *)
    event : string;
    filename : string; (** filename for last successful upload *)
  }

let table = Hashtbl.create 100

let set ~time ~(user:User.t) ~event ~filename =
  Hashtbl.replace table user { time; event; filename }

let alive ?(since=120.) () =
  let t = Unix.time () in
  let compare (u1,l1) (u2,l2) =
    let c = User.compare u1 u2 in
    if c <> 0 then c else compare l1 l2
  in
  Hashtbl.to_seq table |> Seq.filter (fun (_,l) -> l.time >= t -. since) |> List.of_seq |> List.sort compare

let events ?since () =
  alive ?since () |> List.map snd |> List.map (fun l -> l.event) |> List.sort_uniq compare

let by_event ?since () =
  let events = events ?since () in
  let alive = alive ?since () in
  List.map (fun e -> e, List.filter (fun (_,l) -> l.event = e) alive) events
