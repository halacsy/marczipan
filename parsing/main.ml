
	open Tokenizer

let read_whole_file chan =
  let buf = Buffer.create 4096 in
  let rec loop () =
    let newline = input_line chan in
    Buffer.add_string buf newline;
    Buffer.add_char buf '\n';
    loop ()
  in
  try
    loop ()
  with
    End_of_file -> Buffer.contents buf

let sub str start stop =
String.sub str start (stop - start)

let main () =
  let cin =
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin
  in

  let do_doc id content =
    Printf.printf "ID = %s\n" id;
    let lexbuf = Lexing.from_string content in
    let do_word (Token(s, typ, _, _)) = 
      Printf.printf "%s\t%s:%s\n" id typ s;
    in
    Tokenizer.iterate do_word lexbuf;
  in
  Mh_parser.parse cin do_doc
  
;;

  let _ = 
		Printexc.print main ()
