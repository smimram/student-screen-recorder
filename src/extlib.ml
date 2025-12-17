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
