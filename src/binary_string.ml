module Reader : sig
  type 'a t
  include Binary_reader.Reader with type 'a t := 'a t

  include Binary_functor.Functor with type 'a t := 'a t
  include Binary_applicative.Applicative with type 'a t := 'a t
  include Binary_monad.Monad with type 'a t := 'a t

  val run : ?offset:int -> 'a t -> string -> ('a * int)
end = struct
  type 'a t = string -> int -> ('a * int)

  (* Could use ApplicativeOfMonad and FunctorOfApplicative,
   * but we can provide more efficient implementations manually *)

  let map f m = fun s o ->
    let (v, o') = m s o in
    (f v, o')

  let pure x = fun _ o -> x, o
  let apply f x = fun s o ->
    let (f', o') = f s o in
    let (x', o'') = x s o' in
    (f' x', o'')

  let bind k m = fun s o ->
    let (r, o') = k s o in
    (m r) s o'

  let read l = fun s o ->
    if o + l > String.length s
    then
      raise (Invalid_argument "Binary_string.Reader.read: out of bounds")
    else
      (s, o), (o + l)

  let run ?(offset=0) t s = t s offset
end

module Writer : sig
  (* This is highly inefficient, since it concatenates strings internally.
   * Consider using the `Buffer`-based version
   *)
  val run : Binary_writer.t -> string
end = struct
  (* The following uses a state monad over a string state.
   * Consider the monadic type to be
   *
   * type 'a t = string -> ('a, string)
   *)

  let return v = fun s -> (v, s)
  let bind k m = fun s ->
    let (r, s') = k s in
    m r s'

  let write s = fun s' ->
    ((), s' ^ s)

  let run t =
    let m = Binary_writer.run write bind return t in
    let ((), s) = m "" in
    s
end
