module Hash = Mfhash.String

let stopwords = Hash.empty ()

let _ =
	let ic = open_in_bin "/Users/hp/work/data/ir/hungarianstopword.txt" in
	try
		while(true) do
			let w = input_line ic in
			let _ = Hash.update (fun () -> w) (fun x -> x ) stopwords w in ()
		done
	with End_of_file -> ()
	
;;

let iterate f lexbuf =
	let aux (Tokenizer.Token (s, _, _, _)) =
		let term = String.lowercase s in
		try
			let _ = Hash.find stopwords term in
			()
		with Not_found -> f (Tokenizer.Token (term, "", 0, 0))
	in
	Tokenizer.iterate aux lexbuf
	
	
let iterate_ngram f lexbuf =
	let split (Tokenizer.Token (s, _, _, _)) =
	  let s = "_" ^ s ^ "_" in
	  let l = String.length s in
	  let n = l - 2 in
	  if n < 1 then
	    f (Tokenizer.Token (s, "", 0, 0))
    else
      for i = 0 to n - 1 do
        f (Tokenizer.Token ((String.sub s i 3), "", 0, 0))
      done
	in
	iterate split lexbuf

let iterate_ngram2 len f lexbuf =
  let buf = Buffer.create 1024 in
	let add_to_buf (Tokenizer.Token (s, _, _, _)) =
	  Buffer.add_string buf "_";
	  Buffer.add_string buf s
	in
	iterate add_to_buf lexbuf;
	Buffer.add_string buf "_";
	let s= Buffer.contents buf in
	let l = String.length s in
	let n = l - (len - 1) in
	if n < 1 then
	  f (Tokenizer.Token (s, "", 0, 0))
  else
    for i = 0 to n - 1 do
      f (Tokenizer.Token ((String.sub s i len), "", 0, 0))
    done


let _ =
  let aux (Tokenizer.Token (s, _, _ , _)) =
    print_endline s 
  in
  iterate_ngram2 4 aux (Lexing.from_string "csak mindent at kell alakitani igy")

  
