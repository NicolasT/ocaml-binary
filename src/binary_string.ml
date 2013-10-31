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

  let fmap f m = fun s o ->
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

  let read l = fun s o -> (s, o), (o + l)

  let run ?(offset=0) t s = t s offset
end