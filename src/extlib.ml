let failwith fmt = Printf.ksprintf failwith fmt

module Dream = struct
  include Dream

  let text s =
    Dream.respond ~headers:["Content-Type", "text/plain; charset=utf-8"] s
end

module List = struct
  include List

  let diff l1 l2 =
    List.filter (fun x -> not (List.mem x l2)) l1

  let filter_map_pairs f l =
    let rec aux = function
      | x::l -> (List.filter_map (f x) l)@(aux l)
      | [] -> []
    in
    aux l

  let assoc_all k l =
    List.filter_map (fun (k',v) -> if k = k' then Some v else None) l

  (** Convert an association list to a list of associations (key / values). *)
  let assocs l =
    let keys = List.map fst l |> List.sort_uniq Stdlib.compare in
    List.map (fun k -> k, assoc_all k l) keys
end

module File = struct
  let read fname =
    let ic = open_in fname in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s

  let write fname s =
    let oc = open_out fname in
    output_string oc s;
    close_out oc
end

module Time = struct
  let of_string s =
    let year, month, day, hour, min, sec = Scanf.sscanf s "%d-%d-%d %d:%d" (fun y m d h mi -> (y, m, d, h, mi, 0)) in
    let tm =
      {
        Unix.
        tm_sec = sec;
        tm_min = min;
        tm_hour = hour;
        tm_mday = day;
        tm_mon = month - 1;
        tm_year = year - 1900;
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false;
      }
    in
    fst @@ Unix.mktime tm

  let to_string t =
    let open Unix in
    let tm = Unix.gmtime t in
    Printf.sprintf "%d-%d-%d %d:%d" (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday tm.tm_hour tm.tm_min
end

module Yaml = struct
  include Yaml

  module Util = struct
    include Util

    let to_string_option_exn = function
      | `String s -> Some s
      | `Null -> None
      | _ -> assert false

    let of_string_option = function
      | Some s -> `String s
      | None -> `Null
  end
end
