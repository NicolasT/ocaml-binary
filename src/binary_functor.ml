module type Functor = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module Utils (F : Functor) : sig
  module Infix : sig
    val (<$>) : ('a -> 'b) -> 'a F.t -> 'b F.t
  end
end = struct
  module Infix = struct
    let (<$>) = F.fmap
  end
end
