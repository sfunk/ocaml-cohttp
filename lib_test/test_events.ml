open Core.Std

open Pa_event

open Async.Std
open Cohttp_async

let lifetime = 90.

let handler ~body sock req =
  Pa_event.event_start "handle_request";
  let _res = begin 
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
    | "/file" ->
      Server.respond_with_file "/tmp/file.html"
    | "/large-file" ->
      Server.respond_with_file "/tmp/large_file.html"
    | _ ->   
      Server.respond_with_string "Test"
  end in 
  upon _res (fun _ -> Pa_event.event_end "handle_request");
  _res

    

let make_net_server () =
  Server.create (Tcp.on_port 8081) handler

let _ = 
  let _server = make_net_server () in
  Clock.after (Time.Span.of_sec lifetime) >>> fun () ->
  shutdown 0

let _ = 
  Scheduler.go ()

