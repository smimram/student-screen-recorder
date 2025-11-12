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
