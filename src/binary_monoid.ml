module type Monoid = sig
  type t

  val empty : t
  val append : t -> t -> t
end

module Utils (M : Monoid) : sig
  module Infix : sig
    val (<>) : M.t -> M.t -> M.t
  end
end = struct
  module Infix = struct
    let (<>) = M.append
  end
end
