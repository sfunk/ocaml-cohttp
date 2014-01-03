open Core.Std
open Async.Std
open Cohttp_async

let rec make_req n = 
  let start_time = Time.now () in
  let headers = Cohttp.Header.of_list ["connection","close"] in 
  let uri = Uri.of_string "http://www.apple.com/" in
  Client.get ~headers uri >>= fun (res,body) ->
(*  Cohttp.Header.iter (fun k v -> List.iter v ~f:(Printf.eprintf "%s: %s\n%!" k)) 
    (Cohttp.Response.headers res); *)
  Pipe.to_list body >>= fun bufs ->
  let buf = String.concat ~sep:"" bufs in
  printf "\nResponse in bytes: %d\n" (String.length buf);
  let end_time = Time.now () in
  printf "Done in %s\n" (Time.diff end_time start_time |> Time.Span.to_string);
  match n with
    | 99 -> return ()
    | n -> 
      (* This is to avoid being blocked somewhere due to mistakenly being held for
	 an attack *)
      if (n  % 10 = 0) then begin
	Clock.after (Time.Span.of_sec 1.) >>= fun () ->
	make_req (n+1)
      end
      else make_req (n+1)


let _ = 
  let _ = Monitor.try_with ~extract_exn:true (fun () -> make_req 0) >>= (function
    | Ok _ -> return (shutdown 0)
    | Error exn ->
      Printf.fprintf stderr "err %s.\n%!" (Exn.backtrace ()); return ()
  )
  in
  Scheduler.go ()
