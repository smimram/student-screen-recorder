type t =
  {
    time : float; (** last successful upload *)
    filename : string; (** filename for last successful upload *)
  }

let table = Hashtbl.create 100

let set ~time ~(user:User.t) ~filename =
  Hashtbl.replace table user { time; filename }

let alive ?(since=120.) () =
  let t = Unix.time () in
  Hashtbl.to_seq table |> Seq.filter (fun (_,l) -> l.time >= t -. since) |> List.of_seq
