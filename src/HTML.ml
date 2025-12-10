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

let p s = "<p>" ^ s ^ "</p>\n"

let div ?style s =
  let style =
    match style with
    | Some style -> Printf.sprintf " style=\"%s\"" style
    | None -> ""
  in
  Printf.sprintf "<div%s>%s</div>" style s

let br () = "<br>"

let ul l = "<ul>" ^ String.concat "\n" l ^ "</ul>\n"

let ol l = "<ol>" ^ String.concat "\n" l ^ "</ol>\n"

let li s = "<li>" ^ s ^ "</li>\n"

let a ?target href s =
  let target =
    match target with
    | Some target -> Printf.sprintf " target=\"%s\"" target
    | None -> ""
  in
  Printf.sprintf "<a%s href=\"%s\">%s</a>" target href s

let img ?width src =
  let width =
    match width with
    | Some width -> Printf.sprintf " width=\"%s\"" width
    | None -> ""
  in
  Printf.sprintf "<img%s src=\"%s\">" width src
