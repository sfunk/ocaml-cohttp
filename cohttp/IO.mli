module type S = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> 'b t -> 'b t
  val upon : 'a t -> ('a -> unit) -> unit
  val return : 'a -> 'a t


  (* Added for EVENT_ASYNC support *)
  module Scheduler : sig
    val with_local : 'a Core.Std.Univ_map.Key.t -> 'a option -> f:(unit -> 'b) -> 'b
  end


  type ic
  type oc

  val iter : ('a -> unit t) -> 'a list -> unit t
  val read_line : ic -> string option t
  val read : ic -> int -> string t
  val read_exactly : ic -> int -> string option t

  val write : oc -> string -> unit t
end

