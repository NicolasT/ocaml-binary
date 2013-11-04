open Core.Std
open Core_bench.Std

let unsafe_sub s o l =
  let dest = String.create l in
  let () = String.unsafe_blit s o dest 0 l in
  dest

let main () =
  let src = "abcdefghijklmnop" in

  let s = Bench.Test.create ~name:"String.sub" (fun () ->
    ignore (String.sub src 4 4))
  and u = Bench.Test.create ~name:"unsafe_sub" (fun () ->
    ignore (unsafe_sub src 4 4)) in

  Command.run (Bench.make_command [s; u;])
;;

main ()
