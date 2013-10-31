open Binary_functor

module type Applicative = sig
  type 'a t

  include Functor with type 'a t := 'a t

  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module FunctorOfApplicative = functor (A : sig
  type 'a t
  
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end) -> (struct
  type 'a t = 'a A.t

  let fmap f t = A.apply (A.pure f) t
end : Functor with type 'a t = 'a A.t)

module Utils (A : Applicative) : sig
  module Infix : sig
    val (<$>) : ('a -> 'b) -> 'a A.t -> 'b A.t
    val (<*>) : ('a -> 'b) A.t -> 'a A.t -> 'b A.t
  end

  val replicateA : int -> 'a A.t -> 'a list A.t
end = struct
  module Infix = struct
    let (<$>) = A.fmap
    let (<*>) = A.apply
  end

  let replicateA c a =
    let rec loop = function
      | 0 -> A.pure []
      | n ->
          let (<*>) = A.apply
          and (<$>) = A.fmap in
          (fun v a -> v :: a) <$> a <*> loop (n - 1)
    in
    loop c
end
