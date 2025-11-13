let check_string ?(max_length=1024) s =
  assert (String.length s <= max_length);
  assert (not @@ String.contains s '/');
  assert (not @@ String.contains s '\\')

let store ~user ~client ~event ~screenshot =
  let time = Unix.time () in
  assert (String.length screenshot <= 10*1024*1024);
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
  check_string event;
  let dir = Filename.concat !Config.screenshots event in
  if Sys.file_exists dir then assert (Sys.is_directory dir)
  else Sys.mkdir dir 0o755;
  let full_filename = Filename.concat dir filename in
  let oc = open_out full_filename in
  output_string oc screenshot;
  close_out oc;
  Dream.log "wrote: %s" full_filename;
  Last.set ~time ~user ~client ~event ~filename:(Filename.concat event filename)

let () =
  Arg.parse
    (Arg.align
       [
         "--admin-password",
         Arg.Set_string Config.admin_password,
         " Admin password."
       ]
    )
    (fun _ -> ())
    "ssr [options]"
  ;
  let ensure_admin handler request =
    match Dream.header request "Authorization" with
    | Some header ->
       let check_credentials user pass =
         user = "admin" && pass = !Config.admin_password
       in
       let decode_auth header =
         match String.split_on_char ' ' header with
         | ["Basic"; encoded] ->
            (
              try
                let decoded = Base64.decode_exn encoded in
                match String.split_on_char ':' decoded with
                | [username; password] -> Some (username, password)
                | _ -> None
              with _ -> None
            )
         | _ -> None
       in
       (
         match decode_auth header with
         | Some (user, pass) when check_credentials user pass -> handler request
         | _ -> Dream.respond ~status:`Unauthorized ~headers:[("WWW-Authenticate", "Basic realm=\"Restricted\"")] "Invalid credentials"
       )
    | None -> Dream.respond ~status:`Unauthorized ~headers:[("WWW-Authenticate", "Basic realm=\"Restricted\"")] "Authentication required"
  in
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" @@ (fun _ -> Dream.respond "Hi people!");
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
                     let event = List.assoc "event" fields |> List.hd |> snd in
                     let screenshot = List.assoc "screenshot" fields |> List.hd |> snd in
                     Dream.log "multipart from: %s" @@ User.to_string user;
                     store ~user ~client:(Dream.client response) ~event ~screenshot;
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
                     let event = List.assoc "event" fields in
                     let screenshot = List.assoc "screenshot" fields in
                     Dream.log "form from: %s" @@ User.to_string user;
                     store ~user ~client:(Dream.client response) ~event ~screenshot;
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
         Dream.scope "/admin" [ensure_admin] [
             Dream.get "/"
               (fun _ ->
                 let alive =
                   let now = Unix.time () in
                   List.map
                     (fun (e, uu) ->
                       let uu = uu |> List.map (fun (u,l) -> User.to_string u ^ Printf.sprintf " (since %ds, from %s)" (int_of_float @@ Float.round (now -. l.Last.time)) l.Last.client) |> List.map HTML.li |> HTML.ol in
                       HTML.h2 e ^ uu
                     ) @@ Last.by_event ()
                   |> String.concat "\n"
                 in
                 let alive = HTML.h1 "Students" ^ alive in
                 let screenshots =
                   List.map
                     (fun (e, uu) ->
                       let uu = uu |> List.map (fun (u,l) -> HTML.div (HTML.a ~target:"_blank" ("screenshots/"^l.Last.filename) (HTML.img ~width:"400" ("screenshots/"^l.Last.filename)) ^ HTML.br () ^ User.to_string u)) |> String.concat "\n" in
                       HTML.h2 e ^ HTML.div ~style:"display: flex; flex-wrap: wrap; gap: 10px;" uu
                     ) @@ Last.by_event ()
                   |> String.concat "\n"
                 in
                 let screenshots = HTML.h1 "Screenshots" ^ screenshots in
                 let head = {|<meta http-equiv="refresh" content="60">|} in
                 let body = HTML.html ~head (HTML.a "screenshots/" "All screenshots" ^ alive ^ screenshots) in
                 Dream.html body
               );
             Dream.get "/screenshots/" (fun _ ->
                 let body =
                   Sys.readdir !Config.screenshots
                   |> Array.to_list
                   |> List.filter (fun d -> Sys.is_directory @@ Filename.concat !Config.screenshots d)
                   |> List.map
                        (fun d ->
                          let s =
                            Sys.readdir (Filename.concat !Config.screenshots d)
                            |> Array.to_list
                            |> List.map (fun f -> HTML.div (HTML.a ~target:"_blank" (d^"/"^f) (HTML.img ~width:"300" (d^"/"^f) ^ HTML.br () ^ f)))
                            |> String.concat "\n"
                          in
                          let s = HTML.div ~style:"display: flex; flex-wrap: wrap; gap: 10px;" s in
                          HTML.h2 d ^ s
                        )
                   |> String.concat "\n"
                 in
                 let body = HTML.html body in
                 Dream.html body
               );
             Dream.get "/screenshots/**" @@ Dream.static !Config.screenshots
           ]
       ]
