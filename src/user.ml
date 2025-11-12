type t =
  {
    firstname : string;
    lastname : string;
  }

let make ~firstname ~lastname =
  {
    firstname;
    lastname;
  }

let firstname u = u.firstname

let lastname u = u.lastname

(* sort by last name *)
let compare u1 u2 =
  let c = compare (String.lowercase_ascii @@ lastname u1) (String.lowercase_ascii @@ lastname u2) in
  if c <> 0 then c else compare u1 u2

let to_string u = firstname u ^ " " ^ lastname u
