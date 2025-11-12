let () =
  let oc = open_out "output.webm" in
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" @@ Dream.from_filesystem "." "test.html";
    Dream.post "/rec"
      (fun r ->
         Printf.printf "request from %s\n%!" @@ Dream.client r;
         let%lwt body = Dream.body r in
         output_string oc body;
         Dream.respond "ok"
      )
  ]
