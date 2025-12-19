(** Users. *)

open Extlib

(** A user. *)
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

let slug s =
  let canonize s =
    s
    |> String.trim
    |> String.lowercase_ascii
    |> String.space_to_dash
  in
  canonize s.lastname ^ "-" ^ canonize s.firstname

(* sort by last name *)
let compare u1 u2 =
  let c = compare (String.lowercase_ascii @@ lastname u1) (String.lowercase_ascii @@ lastname u2) in
  if c <> 0 then c else compare u1 u2

(** String representation of a user. *)
let to_string u = lastname u ^ " " ^ firstname u
