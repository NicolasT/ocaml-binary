module type Reader = sig
  type 'a t

  val read : int -> (string * int) t
end

(* Things we can do with a Functor *)
module type Functorial = sig
  type 'a t

  val char : char t
  val bytes : int -> string t

  val int8 : int t
  val uint8 : int t
  val int16le : int t
  val uint16le : int t
  val int16be : int t
  val uint16be : int t
  val int32le : int32 t
  val int32be : int32 t
  val int64le : int64 t
  val int64be : int64 t
end

module Functorial = functor(F : sig
  type 'a t
  include Reader with type 'a t := 'a t
  include Binary_functor.Functor with type 'a t := 'a t
end) -> (struct
  type 'a t = 'a F.t

  let char = F.fmap (fun (s, o) -> String.get s o) (F.read 1)
  let bytes l = F.fmap (fun (s, o) -> String.sub s o l) (F.read l)

  module LE = EndianString.LittleEndian
  module BE = EndianString.BigEndian

  let wrap cnt f = F.fmap (fun (s, o) -> f s o) (F.read cnt)

  let int8 = wrap 1 LE.get_int8
  let uint8 = wrap 1 LE.get_uint8

  let int16le = wrap 2 LE.get_int16
  let uint16le = wrap 2 LE.get_uint16
  let int16be = wrap 2 BE.get_int16
  let uint16be = wrap 2 BE.get_uint16

  let int32le = wrap 4 LE.get_int32
  let int32be = wrap 4 BE.get_int32

  let int64le = wrap 8 LE.get_int64
  let int64be = wrap 8 BE.get_int64
end : Functorial with type 'a t = 'a F.t)


(* Things we can do with an Applicative, but not with a Functor *)
module type Applicative = sig
  type 'a t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val list : 'a t -> int -> 'a list t

  include Functorial with type 'a t := 'a t
end

module Applicative = functor(A : sig
  type 'a t
  include Reader with type 'a t := 'a t
  include Binary_applicative.Applicative with type 'a t := 'a t
end) -> (struct
  type 'a t = 'a A.t

  module F = (Functorial(A) : Functorial with type 'a t := 'a A.t)
  include F

  module AU = Binary_applicative.Utils(A)
  open AU
  open AU.Infix

  let pair a b = (fun a b -> (a, b)) <$> a <*> b
  let triple a b c = (fun a b c -> (a, b, c)) <$> a <*> b <*> c
  let list t l = replicateA l t
end : Applicative with type 'a t = 'a A.t)


(* Things for which we need a Monad *)
module type Monadic = sig
  type 'a t

  include Applicative with type 'a t := 'a t

  val array : 'a array -> 'a t -> unit t
end

module Monadic = functor(M : sig
  type 'a t
  include Reader with type 'a t := 'a t
  include Binary_monad.Monad with type 'a t := 'a t
end) -> (struct
  type 'a t = 'a M.t

  module MU = Binary_monad.Utils(M)
  include MU
  include MU.Infix

  module A = (Applicative(M) : Applicative with type 'a t := 'a M.t)
  include A

  let array a t =
    let l = Array.length a in
    let rec loop = function
      | n when n = l -> M.pure ()
      | n -> M.fmap (Array.set a n) t >>= fun () -> loop (n + 1)
    in
    loop 0
end : Monadic with type 'a t = 'a M.t)
