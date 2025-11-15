module List = struct
  include List

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
end
