module List = struct
  include List

  let filter_map_pairs f l =
    let rec aux = function
      | x::l -> (List.filter_map (f x) l)@(aux l)
      | [] -> []
    in
    aux l
end
