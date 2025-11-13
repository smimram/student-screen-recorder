let check_string ?(max_length=1024) s =
  assert (String.length s <= max_length);
  assert (not @@ String.contains s '/');
  assert (not @@ String.contains s '\\')

let store user screenshot =
  let time = Unix.time () in
  let filename =
    let tm = Unix.localtime time in
    let space_to_dash s = String.init (String.length s) (fun i -> if s.[i] = ' ' then '-' else s.[i]) in
    let canonize s =
      s
      |> String.trim
      |> String.lowercase_ascii
      |> space_to_dash
      |> (fun s -> check_string s; s)
    in
    let firstname = canonize @@ User.firstname user in
    let lastname = canonize @@ User.lastname user in
    Printf.sprintf "%s-%s-%04d%02d%02d-%02d%02d%02d.png" lastname firstname (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
  in
  let filename = Filename.concat !Config.screenshots filename in
  let oc = open_out filename in
  output_string oc screenshot;
  close_out oc;
  Dream.log "wrote: %s" filename;
  Last.set ~time ~user ~filename

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/test" @@ Dream.from_filesystem "static" "test.html";
         Dream.get "/ssr.js" @@ Dream.from_filesystem "static" "ssr.js";
         Dream.post "/upload"
           (fun response ->
             Printf.printf "request from %s\n%!" @@ Dream.client response;
             match Dream.header response "content-type" with
             | Some content_type when String.starts_with ~prefix:"multipart/form-data" content_type ->
                (
                  let%lwt parts = Dream.multipart ~csrf:false response in
                  match parts with
                  | `Ok fields ->
                     Dream.log "multipart fields: %s" @@ (String.concat ", " @@ List.map fst fields);
                     let firstname = List.assoc "firstname" fields |> List.hd |> snd in
                     let lastname = List.assoc "lastname" fields |> List.hd |> snd in
                     let user = User.make ~firstname ~lastname in
                     let screenshot = List.assoc "screenshot" fields |> List.hd |> snd in
                     Dream.log "multipart from: %s" @@ User.to_string user;
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
                     Dream.log "form fields: %s" (String.concat ", " @@ List.map fst fields);
                     let firstname = List.assoc "firstname" fields in
                     let lastname = List.assoc "lastname "fields in
                     let user = User.make ~firstname ~lastname in
                     let screenshot = List.assoc "screenshot" fields in
                     Dream.log "form from: %s" @@ User.to_string user;
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
           );
         Dream.get "/alive"
           (fun _ ->
             let title = "<h1>Students alive</h1>" in
             let list = Last.alive () |> List.map fst |> List.map User.to_string |> List.map (fun s -> "<li>"^s^"</li>\n") |> String.concat "" in
             let list = "<ul>"^list^"</ul>" in
             let body = title ^ list in
             let body = "<html><head></head><body>" ^ body ^ "</body>" in
             Dream.respond body
           )
       ]

