let html ?(head="") body =
  Printf.sprintf
{|
<!DOCTYPE html>
<html>
<head>
%s
</head>
<body>
%s
</body>
</html>
|}
    head body

let h1 s = "<h1>" ^ s ^ "</h1>\n"

let h2 s = "<h2>" ^ s ^ "</h2>\n"

let h3 s = "<h3>" ^ s ^ "</h3>\n"

let ul l = "<ul>" ^ String.concat "\n" l ^ "</ul>\n"

let ol l = "<ol>" ^ String.concat "\n" l ^ "</ol>\n"

let li s = "<li>" ^ s ^ "</li>\n"

let img ?width src =
  let width =
    match width with
    | Some w -> " width=\""^w^"\""
    | None -> ""
  in
  Printf.sprintf "<img%s src=\"%s\">" width src
