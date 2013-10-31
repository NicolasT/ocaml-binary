let main () =
  let str () =
    let module M = Binary_monad.Utils (Binary_string.Reader) in
    let open M.Infix in
    let module MR = Binary_reader.Applicative (Binary_string.Reader) in
    let open MR in

    let s = Printf.sprintf "%cabcd" (Char.chr 4) in

    let reader = uint8 >>= list char in
    let (r, _) = Binary_string.Reader.run reader s in
    List.iter (Printf.printf "%c\n") r
  in
  str ();

  flush stdout;

  let act () =
    let open Lwt in

    let module AR = Binary_reader.Applicative (Binary_lwt.Reader) in
    let open AR in

    let reader = AR.pair (bytes 2) (bytes 2) in
    Lwt_io.with_file
      Lwt_io.Input
      "/etc/hostname"
      (Binary_lwt.Reader.run reader) >>= fun (a, b) ->
    Lwt_io.printf "%s %s\n" a b
  in
  Lwt_main.run (act ())
;;

main ()
