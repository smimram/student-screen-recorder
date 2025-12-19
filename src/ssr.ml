(** Main SSR server. *)

open Extlib

module HTML = struct
  include HTML

  let html ?(head="") body =
    let head = {|<link rel="stylesheet" href="/ssr.css" />|} ^ head in
    html ~head body
end

let m = Mutex.create ()

let check_string ?(max_length=1024) s =
  assert (not @@ String.starts_with ~prefix:"." s);
  assert (String.length s <= max_length);
  assert (not @@ String.contains s '/');
  assert (not @@ String.contains s '\\')

(** Store screenshot. *)
let store ~student ~client ~event ~screenshot =
  let time = Unix.time () in
  (* Ensure that the event is valid. *)
  check_string event;
  let event =
    match Event.find_opt event with
    | Some event -> event
    | None -> failwith "Ignoring screenshot from %s, event %s does not exist." (Student.to_string student) event
  in
  if not (Event.valid time event) then failwith "Ignoring screenshot from %s, invalid time for event %s" (Student.to_string student) (Event.name event);
  (* Ensure that the screenshot is not too big. *)
  assert (String.length screenshot <= 10*1024*1024);
  (* At least 5 sec to avoid being flooded. *)
  (
    match Last.find_opt student with
    | Some c -> if time < c.time +. 5. then failwith "Ignoring screenshot from %s, last was only %.02fs ago" (Student.to_string student) (time -. c.time)
    | None -> ()
  );
  let tm = Unix.localtime time in
  let filename =
    let space_to_dash s = String.init (String.length s) (fun i -> if s.[i] = ' ' then '-' else s.[i]) in
    let canonize s =
      s
      |> String.trim
      |> String.lowercase_ascii
      |> space_to_dash
      |> (fun s -> check_string s; s)
    in
    let firstname = canonize @@ Student.firstname student in
    let lastname = canonize @@ Student.lastname student in
    Printf.sprintf "%s-%s-%04d%02d%02d-%02d%02d%02d.png" lastname firstname (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
  in
  let dir = Filename.concat !Config.screenshots (Event.name event) in
  if Sys.file_exists dir then assert (Sys.is_directory dir) else Sys.mkdir dir 0o755;
  let full_filename = Filename.concat dir filename in
  let oc = open_out full_filename in
  output_string oc screenshot;
  close_out oc;
  Dream.log "Wrote %s" full_filename;
  let ip, port =
    match String.index_from_opt client 0 ':' with
    | Some n ->
       let ip = String.sub client 0 n in
       let n = n+1 in
       let port = String.sub client n (String.length client - n) |> int_of_string_opt |> Option.value ~default:0 in
       ip, port
    | None -> client, 0
  in
  (* TODO: per-event mutex? *)
  Mutex.protect m
    (fun () ->
      let csv = Filename.concat dir "ssr.csv" in
      let header = not (Sys.file_exists csv) in
      Out_channel.with_open_gen [Open_wronly; Open_creat; Open_append] 0o644 csv
        (fun oc ->
          let csv = Csv.to_channel oc in
          if header then Csv.output_record csv ["Lastname"; "Firstname"; "Date"; "Time"; "IP"; "Port"; "Filename"];
          Csv.output_record csv
            [
              Student.lastname student;
              Student.firstname student;
              Printf.sprintf "%04d/%02d/%02d" (tm.tm_year+1900) (tm.tm_mon+1) tm.tm_mday;
              Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec;
              ip;
              string_of_int port;
              filename
            ]
        );
    );
  Last.set ~time ~student ~ip ~port ~event ~filename:(Filename.concat (Event.name event) filename)

(** Handle screenshot upload. *)
let upload response =
  (* Printf.printf "request from %s\n%!" @@ Dream.client response; *)
  let headers = ["Access-Control-Allow-Origin", "*"] in
  match Dream.header response "content-type" with
  | Some content_type when String.starts_with ~prefix:"multipart/form-data" content_type ->
     (
       let%lwt parts = Dream.multipart ~csrf:false response in
       match parts with
       | `Ok fields ->
          Dream.log "Multipart form fields: %s" @@ (String.concat ", " @@ List.map fst fields);
          let firstname = List.assoc "firstname" fields |> List.hd |> snd in
          let lastname = List.assoc "lastname" fields |> List.hd |> snd in
          let student = Student.make ~firstname ~lastname in
          let event = List.assoc "event" fields |> List.hd |> snd in
          let screenshot = List.assoc "screenshot" fields |> List.hd |> snd in
          Dream.log "Form from %s (%s)" (Student.to_string student) event;
          store ~student ~client:(Dream.client response) ~event ~screenshot;
          Dream.respond ~headers "ok"
       | _ ->
          Dream.log "Invalid multipart";
          Dream.respond ~headers "invalid"
     )
  | Some content_type when String.starts_with ~prefix:"application/x-www-form-urlencoded" content_type ->
     (
       let%lwt form = Dream.form ~csrf:false response in
       match form with
       | `Ok fields ->
          Dream.log "Form fields: %s" (String.concat ", " @@ List.map fst fields);
          let firstname = List.assoc "firstname" fields in
          let lastname = List.assoc "lastname "fields in
          let student = Student.make ~firstname ~lastname in
          let event = List.assoc "event" fields in
          let screenshot = List.assoc "screenshot" fields in
          Dream.log "Form from %s (%s)" (Student.to_string student) event;
          store ~student ~client:(Dream.client response) ~event ~screenshot;
          Dream.respond ~headers "ok"
       | _ ->
          Dream.log "Invalid form";
          Dream.respond ~headers "invalid"
     )
  | Some _ ->
     failwith "unhandled content type"
  | None ->
     failwith "no content type"

let admin _ =
  let alive =
    let now = Unix.time () in
    List.map
      (fun (e, uu) ->
        let open Last in
        let uu = uu |> List.map (fun (u,l) -> Student.to_string u ^ Printf.sprintf " (since %ds, from %s:%d)" (int_of_float @@ Float.round (now -. l.time)) l.ip l.port) |> List.map HTML.li |> HTML.ol in
        let gone =
          let gone = Last.gone ~event:e () in
          if gone = [] then ""
          else
            HTML.p "Recently gone:"
            ^ (gone |> List.map (fun (u,l) -> Student.to_string u ^ Printf.sprintf " (since %ds, from %s:%d)" (int_of_float @@ Float.round (now -. l.time)) l.ip l.port) |> List.map HTML.li |> HTML.ul)
        in
        HTML.h2 e ^ uu ^ gone
      ) @@ Last.by_event ()
    |> String.concat "\n"
  in
  let warnings =
    let byip =
      Last.by_ip ()
      |> List.filter (fun (_,uu) -> List.length uu > 1)
    in
    let warnings =
      if byip = [] then "" else
        HTML.p "Students with same IP:" ^
          (
            List.map
              (fun (ip,uu) ->
                Printf.sprintf "%s: %s" ip (String.concat ", " @@ List.map Student.to_string uu)
              ) byip
            |> List.map HTML.li
            |> HTML.ul
          )
    in
    if warnings = "" then "" else HTML.h1 "Warnings" ^ warnings
  in
  let alive = HTML.h1 "Students" ^ alive in
  let screenshots =
    List.map
      (fun (e, uu) ->
        let open Last in
        let uu = uu |> List.map (fun (u,l) -> HTML.div (HTML.a ~target:"_blank" ("screenshots/"^l.filename) (HTML.img ~width:"400" ("screenshots/"^l.filename)) ^ HTML.br () ^ Student.to_string u)) |> String.concat "\n" in
        HTML.h2 e ^ HTML.div ~style:"display: flex; flex-wrap: wrap; gap: 10px;" uu
      ) @@ Last.by_event ()
    |> String.concat "\n"
  in
  let screenshots = HTML.h1 "Screenshots" ^ screenshots in
  let events =
    let events =
      Sys.readdir !Config.screenshots
      |> Array.to_list
      |> List.filter (fun d -> Sys.is_directory @@ Filename.concat !Config.screenshots d)
      |> List.map (fun event ->
             let dir = Filename.concat !Config.screenshots event in
             let l =
               let csv = Filename.concat dir "ssr.csv" in
               if not (Sys.file_exists csv) then "" else
                 File.read csv
                 |> Csv.of_string
                 |> Csv.Rows.input_all
                 |> List.map (fun row ->
                        let find = Csv.Row.find row in
                        let firstname = find "Firstname" in
                        let lastname = find "Lastname" in
                        let u = Student.make ~firstname ~lastname in
                        u
                      )
                 |> List.sort_uniq Student.compare
                 |> List.map Student.to_string
                 |> List.map HTML.li
                 |> HTML.ol
             in
             HTML.h2 event ^ l
           )
      |> String.concat "\n"
    in
    HTML.h1 "Events" ^ events
  in
  let head = {|<meta http-equiv="refresh" content="60">|} in
  let links =
    List.map HTML.li
      [
        HTML.a "screenshots/" "All screenshots";
        HTML.a "events/" "Events";
        HTML.a "test/" "Test";
      ]
    |> HTML.ul
  in
  let body = HTML.html ~head (links ^ warnings ^ alive ^ screenshots ^ events) in
  Dream.html body

let screenshots _ =
  let body =
    Sys.readdir !Config.screenshots
    |> Array.to_list
    |> List.sort compare
    |> List.filter (fun d -> Sys.is_directory @@ Filename.concat !Config.screenshots d)
    |> List.map
         (fun d ->
           let s =
             Sys.readdir (Filename.concat !Config.screenshots d)
             |> Array.to_list
             |> List.sort compare
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

let events _ =
  let body =
    HTML.h1 "Events"
    ^ HTML.h2 "Add"
    ^ {|
       <form>
       <label>Name:</label>
       <input type="text" id="name" required><br>
       <label>Begin:</label>
       <input type="text" id="begin" required><br>
       <label>End:</label>
       <input type="text" id="end" required><br>
       <button type="submit">Add</button>
       </form>
       |}
    ^ HTML.h2 "List"
    ^ (
        Event.list ()
        |> List.map (fun e -> e.Event.name)
        |> List.map HTML.li
        |> HTML.ul
    )
  in
  Dream.html @@ HTML.html body
  
let () =
  let test = ref false in
  let conffile = "ssr.yml" in
  let eventfile = "events.yml" in
  if Sys.file_exists conffile then
    (
      Printf.printf "Loading configuration from %s... %!" conffile;
      Config.load conffile;
      Printf.printf "done.\n%!"
    );
  if Sys.file_exists eventfile then
    (
      Printf.printf "Loading events from %s... %!" eventfile;
      Event.load eventfile;
      Printf.printf "done.\n%!"
    );
  Arg.parse
    (Arg.align
       [
         "--admin-password", Arg.Set_string Config.admin_password, " Admin password.";
         "--test", Arg.Set test, " Only run tests.";
       ]
    )
    (fun _ -> ())
    "ssr [options]";

  let ensure_admin handler request =
    match Dream.header request "Authorization" with
    | Some header ->
       let check_credentials user pass =
         user = !Config.admin_user && pass = !Config.admin_password
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
  if not !test then
    Dream.run ~interface:"0.0.0.0"
    @@ Dream.logger
    @@ Dream.router
         [
           Dream.get "/" @@ (fun _ -> Dream.text "Hi SSR people!");
           Dream.get "/ssr.js" @@ Dream.from_filesystem "static" "ssr.js";
           Dream.get "/ssr.css" @@ Dream.from_filesystem "static" "ssr.css";
           Dream.post "/upload" upload;
           Dream.scope "/admin" [ensure_admin] [
               Dream.get "/" admin;
               Dream.get "/screenshots/" screenshots;
               Dream.get "/screenshots/**" @@ Dream.static !Config.screenshots;
               Dream.get "/test/" @@ Dream.from_filesystem "static" "test.html";
               Dream.get "/events/" events;
             ]
         ]
