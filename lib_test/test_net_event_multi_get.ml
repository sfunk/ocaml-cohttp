open Core.Std
open Async.Std

let iterations = 10

let fetch uri =
  let event_name = sprintf "Fetch %s" uri in
  EVENT_DIRECT_ASYNC event_name = (
  Cohttp_async.Client.get (Uri.of_string uri)
  >>= fun (res, body) ->
  Pipe.to_list body
  >>= fun bufs ->
  let buf = String.concat ~sep:"" bufs in
  printf "%s -> %d bytes\n" uri (String.length buf);
  return ()
  )

let rec perform_get n start_time  =
  Printf.printf "%d\n%!" n;
  (List.map [ "http://www.twitter.com";
	      "http://lastminute.com";
	      "http://www.bbc.co.uk"; 
	      "http://www.google.com";
	      "http://recoil.org"; ]
     ~f:fetch ) 
  |> Deferred.all_unit  >>= fun () ->
  if (n>=iterations) then begin
    printf "Finished! Time taken: %s\n" 
      (Time.now() |> (Fn.flip Time.diff) start_time |> Time.Span.to_string );
    return ()
  end
  else perform_get (n+1) start_time

let _ =
  let _ = perform_get 0 (Time.now ()) in
  Printf.printf "start\n%!";
  Clock.run_after (Time.Span.of_sec (Float.of_int (iterations*3 + 10))) 
    (fun () -> shutdown 0) ();
  Scheduler.go ()
