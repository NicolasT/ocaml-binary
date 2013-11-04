module type Functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Utils (F : Functor) : sig
  module Infix : sig
    val (<$>) : ('a -> 'b) -> 'a F.t -> 'b F.t
  end
end = struct
  module Infix = struct
    let (<$>) = F.map
  end
end
