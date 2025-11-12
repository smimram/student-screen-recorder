let store user screenshot =
  let filename =
    let time = Unix.localtime @@ Unix.time () in
    let space_to_dash s = String.init (String.length s) (fun i -> if s.[i] = ' ' then '-' else s.[i]) in
    let check s = assert (not (String.contains s '/' || String.contains s '\\')); s in
    let canonize s =
      s
      |> String.trim
      |> String.lowercase_ascii
      |> space_to_dash
      |> check
    in
    let firstname = canonize @@ User.firstname user in
    let lastname = canonize @@ User.lastname user in
    Printf.sprintf "%s-%s-%04d%02d%02d-%02d%02d%02d.png" firstname lastname (time.tm_year+1900) (time.tm_mon+1) time.tm_mday time.tm_hour time.tm_min time.tm_sec
  in
  let filename = Filename.concat "output" filename in
  let oc = open_out filename in
  output_string oc screenshot;
  close_out oc

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" @@ Dream.from_filesystem "." "test.html";
         Dream.post "/upload"
           (fun response ->
             Printf.printf "request from %s\n%!" @@ Dream.client response;
             match Dream.header response "content-type" with
             | Some content_type when String.starts_with ~prefix:"multipart/form-data" content_type ->
                (
                  let%lwt parts = Dream.multipart ~csrf:false response in
                  match parts with
                  | `Ok fields ->
                     print_endline ("ok multipart: '" ^ (String.concat "," @@ List.map fst fields) ^ "'");
                     let firstname = List.assoc "firstname" fields |> List.hd |> snd in
                     let lastname = List.assoc "lastname" fields |> List.hd |> snd in
                     let user = User.make ~firstname ~lastname in
                     let screenshot = List.assoc "screenshot" fields |> List.hd |> snd in
                     Printf.printf "name: %s / %s\n%!" firstname lastname;
                     store user screenshot;
                     Dream.respond "ok"
                  | _ ->
                     print_endline "invalid multipart";
                     Dream.respond "invalid"
                )
             | Some content_type when String.starts_with ~prefix:"application/x-www-form-urlencoded" content_type ->
                (
                  let%lwt form = Dream.form ~csrf:false response in
                  match form with
                  | `Ok fields ->
                     let firstname = List.assoc "firstname" fields in
                     let lastname = List.assoc "lastname "fields in
                     let user = User.make ~firstname ~lastname in
                     let screenshot = List.assoc "screenshot" fields in
                     store user screenshot;
                     Dream.respond "ok"
                  | _ ->
                     print_endline "invalid form";
                     Dream.respond "invalid"
                )
             | Some _ ->
                failwith "unhandled content type"
             | None ->
                failwith "no content type"
           )
       ]
