module Reader : sig
  type 'a t
  include Binary_reader.Reader with type 'a t := 'a t

  include Binary_functor.Functor with type 'a t := 'a t
  include Binary_applicative.Applicative with type 'a t := 'a t
  include Binary_monad.Monad with type 'a t := 'a t

  val run : ?default_size:int -> ?max_size:int -> 'a t -> Lwt_io.input_channel -> 'a Lwt.t
end = struct
  type 'a t = (Lwt_io.input_channel * int) -> string -> ('a * string) Lwt.t

  let read l = fun (ic, m) s ->
    let s' = if l <= String.length s then s else String.create l in
    let s'' = if l <= m then s' else s in
    Lwt.map
      (fun () -> (s', 0), s'')
      (Lwt_io.read_into_exactly ic s' 0 l)

  let map f m = fun t s ->
    Lwt.map 
      (fun (x, s') -> (f x, s'))
      (m t s)

  let pure x = fun _ s ->
    Lwt.return (x, s)
  let apply f x = fun t s ->
    let open Lwt in
    f t s >>= fun (f', s') ->
    Lwt.map
      (fun (x', s'') -> (f' x', s''))
      (x t s')

  let bind k m = fun t s ->
    Lwt.bind
      (k t s)
      (fun (r, s') -> m r t s')

  let run ?(default_size=1024) ?(max_size=4096) (t:'a t) ic =
    let s = String.create default_size in
    Lwt.map (fun (a, _) -> a) (t (ic, max_size) s)
end

module Writer : sig
  val run : Lwt_io.output_channel -> Binary_writer.t -> unit Lwt.t
end = struct
  let run oc = Binary_writer.run (Lwt_io.write oc) Lwt.bind Lwt.return
end
