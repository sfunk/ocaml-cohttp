open Core.Std
open Async.Std
open Cohttp_async

let iterations = 30

let alloc_array = Array.create ~len:iterations 0
let time_array = Array.create ~len:iterations 0.
let major_alloc_array = Array.create ~len:iterations 0

let rec make_req n = 
  let start_time = Time.now () in
  let start_alloc = Gc.minor_words () in
  let start_major_alloc = Gc.major_words () in
  let headers = Cohttp.Header.of_list ["connection","close"] in 
  let uri = Uri.of_string "http://www.google.com/" in
  Client.get ~headers uri >>= fun (res,body) ->
(*  Cohttp.Header.iter (fun k v -> List.iter v ~f:(Printf.eprintf "%s: %s\n%!" k)) 
    (Cohttp.Response.headers res); *)
  Pipe.to_list body >>= fun bufs ->
  let buf = String.concat ~sep:"" bufs in
  printf "\nResponse in bytes: %d\n" (String.length buf);
  let end_time = Time.now () in
  printf "Done in %s\n" (Time.diff end_time start_time |> Time.Span.to_string);
  let end_alloc = Gc.minor_words () in
  let end_major_alloc = Gc.major_words () in
  alloc_array.(n) <- (end_alloc - start_alloc);
  time_array.(n) <- (Time.diff end_time start_time |> Time.Span.to_float);
  major_alloc_array.(n) <- (end_major_alloc - start_major_alloc);
  if (n < iterations - 1) then make_req (n+1)
  else return ()



let _ = 
  let _ = Monitor.try_with ~extract_exn:true (fun () -> make_req 0) >>= (function
    | Ok _ -> Printf.printf "M Allocations:\n%s\n"
      (Array.to_list major_alloc_array |> List.to_string  ~f:Int.to_string);
      (* Printf.printf "Allocations:\n%s\n"
      (Array.to_list alloc_array |> List.to_string  ~f:Int.to_string);
      Printf.printf "Runtime:\n%s\n"
      (Array.to_list time_array |> List.to_string  ~f:(fun f -> sprintf "%.5f" (f*.1000.)) ); *)
      return (shutdown 0)
    | Error exn ->
      Printf.fprintf stderr "err %s.\n%!" (Exn.backtrace ()); return ()
  )
  in
  Scheduler.go ()
