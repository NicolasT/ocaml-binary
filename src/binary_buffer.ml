module Writer : sig
  val run : Buffer.t -> Binary_writer.t -> unit
end = struct
  (* The following uses a sort-of `Identity` monad and hides the in-place
   * mutation of `Buffer.t` under the carpet.
   *)
  external identity : 'a -> 'a = "%identity"
  external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"

  let return = identity
  let bind = revapply

  let run b = Binary_writer.run (Buffer.add_string b) bind return
end
