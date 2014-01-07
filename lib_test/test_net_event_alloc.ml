open Core.Std
open Async.Std
open Cohttp_async

let iterations = 30

let minor_alloc_array = Array.create ~len:iterations 0
let major_alloc_array = Array.create ~len:iterations 0
let minor_gcs = Array.create ~len:iterations 0
let major_gcs = Array.create ~len:iterations 0
let time_array = Array.create ~len:iterations 0.

let rec make_req n = 
  let start_time = Time.now () in
  let gc1 = Gc.quick_stat () in
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
  let gc2 = Gc.quick_stat () in
  time_array.(n) <- (Time.diff end_time start_time |> Time.Span.to_float);
  minor_alloc_array.(n) <- Float.to_int (gc2.Gc.Stat.minor_words -. gc1.Gc.Stat.minor_words);
  major_alloc_array.(n) <- Float.to_int (gc2.Gc.Stat.major_words -. gc1.Gc.Stat.major_words);
  minor_gcs.(n) <- (gc2.Gc.Stat.minor_collections - gc1.Gc.Stat.minor_collections);
  major_gcs.(n) <- (gc2.Gc.Stat.major_collections - gc1.Gc.Stat.major_collections);
  if (n < iterations - 1) then make_req (n+1)
  else return ()



let _ = 
  let _ = Monitor.try_with ~extract_exn:true (fun () -> make_req 0) >>= (function
    | Ok _ -> 
      Printf.printf "minor_heap size:%d MB\n"
      (((Gc.Control.minor_heap_size (Gc.get ())) * (Sys.word_size) / 1_000_000));
      Printf.printf "minor Allocations:\n%s\n"
      (Array.to_list minor_alloc_array |> List.to_string  ~f:Int.to_string);
      Printf.printf "major Allocations:\n%s\n"
      (Array.to_list major_alloc_array |> List.to_string  ~f:Int.to_string);
      Printf.printf "minor GCs:\n%s\n"
      (Array.to_list minor_gcs |> List.to_string  ~f:Int.to_string);
      Printf.printf "major GCs:\n%s\n"
      (Array.to_list major_gcs |> List.to_string  ~f:Int.to_string);

(*      Printf.printf "Runtime:\n%s\n"
      (Array.to_list time_array |> List.to_string  ~f:(fun f -> sprintf "%.5f" (f*.1000.)) ); *)
      return (shutdown 0)
    | Error exn ->
      Printf.fprintf stderr "err %s.\n%!" (Exn.backtrace ()); return ()
  )
  in
  Scheduler.go ()
