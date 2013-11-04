open Core.Std
open Core_bench.Std

module A = Binary_reader.Applicative (Binary_string.Reader)
module M = Binary_reader.Monadic (Binary_string.Reader)

module AU = Binary_applicative.Utils (Binary_string.Reader)
module MU = Binary_monad.Utils (Binary_string.Reader)

let applicative =
  let open A in
  let open AU.Infix in

  (fun a _ b -> (a, b)) <$> int32le <*> int32be <*> int32le

let monadic =
  let open M in
  let open MU in
  let open MU.Infix in

  int32le >>= fun a ->
  int32be >>= fun _ ->
  int32le >>= fun b ->
  return (a, b)

let main () =
  let src = "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c" in

  let a = Bench.Test.create ~name:"Applicative" (fun () ->
    ignore (Binary_string.Reader.run applicative src))
  and m = Bench.Test.create ~name:"Monadic" (fun () ->
    ignore (Binary_string.Reader.run monadic src)) in

  Command.run (Bench.make_command [a; m;])
;;

main ()
