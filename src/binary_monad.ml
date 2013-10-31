open Binary_functor
open Binary_applicative

module type Monad = sig
  type 'a t

  include Applicative with type 'a t := 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module ApplicativeOfMonad = functor(M : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end) -> (struct
  module A = struct
    type 'a t = 'a M.t

    let pure = M.return
    let apply f x =
      let (>>=) = M.bind in
      f >>= fun f' ->
      x >>= fun x' ->
      M.return (f' x')
  end

  include A

  module F = FunctorOfApplicative(A)
  let fmap = F.fmap
end : Applicative with type 'a t = 'a M.t)

module Utils (M : Monad) : sig
  module Infix : sig
    val (>>=) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
    val (=<<) : ('a -> 'b M.t) -> 'a M.t -> 'b M.t
  end

  val return : 'a -> 'a M.t
end = struct
  module Infix = struct
    let (>>=) = M.bind
    let (=<<) = fun m k -> M.bind k m
  end

  let return = M.pure
end
