type t

type 'a writer = 'a -> t

include Binary_monoid.Monoid with type t := t

val concat : t list -> t

(* To understand this type, mentally change `'a` to `unit m` for some monad
 * `m`, then think how the first argument is `string -> unit m`, the second is
 * `'a m -> ('a -> 'b m) -> 'b m) (yup, bind), and the third is `'a -> 'a m`
 * (return!).
 * Finally, the return value is of type `unit m`.
 *)
val run :  (string -> 'a)
        -> ('a -> (unit -> 'a) -> 'a)
        -> (unit -> 'a)
        -> t
        -> 'a

val char : char writer
val bytes : string writer

val int8 : int writer
val int16le : int writer
val int16be : int writer
val int32le : int32 writer
val int32be : int32 writer
val int64le : int64 writer
val int64be : int64 writer

val pair : 'a writer -> 'b writer -> ('a * 'b) writer
val triple : 'a writer -> 'b writer -> 'c writer -> ('a * 'b * 'c) writer

val list : 'a writer -> 'a list writer
