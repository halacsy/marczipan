# 2 "xml_tokenizer.mll"
 
  type token = StartE of string * int * int | EndE of string * int * int | Data of string * int * int | StartDoc of string * string option 

# 6 "xml_tokenizer.ml"
let __ocaml_lex_init_lexbuf lexbuf mem_size =
  let pos = lexbuf.Lexing.lex_curr_pos in
  lexbuf.Lexing.lex_mem <- Array.create mem_size (-1) ;
  lexbuf.Lexing.lex_start_pos <- pos ;
  lexbuf.Lexing.lex_last_pos <- pos ;
  lexbuf.Lexing.lex_last_action <- -1

let rec __ocaml_lex_next_char lexbuf =
  if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len then begin
    if lexbuf.Lexing.lex_eof_reached then
      256
    else begin
      lexbuf.Lexing.refill_buff lexbuf ;
      __ocaml_lex_next_char lexbuf
    end
  end else begin
    let i = lexbuf.Lexing.lex_curr_pos in
    let c = lexbuf.Lexing.lex_buffer.[i] in
    lexbuf.Lexing.lex_curr_pos <- i+1 ;
    Char.code c
  end

let rec __ocaml_lex_state0 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |eof *)
  |256 ->
    __ocaml_lex_state1 lexbuf
(* |'<' *)
  |60 ->
    __ocaml_lex_state3 lexbuf
  | _ ->
    __ocaml_lex_state2 lexbuf

and __ocaml_lex_state1 lexbuf = (* *)
  5

and __ocaml_lex_state2 lexbuf = (* *)
  lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_last_action <- 4 ;
  match __ocaml_lex_next_char lexbuf with
(* |'<'|'>'|eof *)
  |60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
  | _ ->
    __ocaml_lex_state2 lexbuf

and __ocaml_lex_state3 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |' '|'<'|'>'|eof *)
  |32|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'/' *)
  |47 ->
    __ocaml_lex_state4 lexbuf
(* |'!' *)
  |33 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state6 lexbuf
(* |'?' *)
  |63 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state7 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state4 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |' '|'/'|'<'|'>'|eof *)
  |32|47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
  | _ ->
    __ocaml_lex_state88 lexbuf

and __ocaml_lex_state5 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state6 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'D' *)
  |68 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state74 lexbuf
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state7 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |'x' *)
  |120 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state10 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state8 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'='|'>'|eof *)
  |60|61|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state9 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'>'|eof *)
  |60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'=' *)
  |61 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf
(* |' ' *)
  |32 ->
(* L=2 [2] <- [4] ;[3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_mem.(4) ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=3 [2] <- [4] ;[4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_mem.(4) ;
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |'/' *)
  |47 ->
    __ocaml_lex_state14 lexbuf
  | _ ->
(* L=1 [4] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state73 lexbuf

and __ocaml_lex_state10 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |'m' *)
  |109 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state11 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state11 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |'l' *)
  |108 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state12 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state12 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state13 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state13 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'='|'>'|eof *)
  |60|61|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'v' *)
  |118 ->
    __ocaml_lex_state15 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state14 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'='|eof *)
  |60|61|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state15 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'e' *)
  |101 ->
    __ocaml_lex_state17 lexbuf
(* |'<'|'='|eof *)
  |60|61|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state16 lexbuf = (* t0 <- [2] ; t1 <- [3] ; *)
  lexbuf.Lexing.lex_mem.(0) <- lexbuf.Lexing.lex_mem.(2) ;
  lexbuf.Lexing.lex_mem.(1) <- lexbuf.Lexing.lex_mem.(3) ;
  2

and __ocaml_lex_state17 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'='|eof *)
  |60|61|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'r' *)
  |114 ->
    __ocaml_lex_state18 lexbuf
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state18 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'='|eof *)
  |60|61|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'s' *)
  |115 ->
    __ocaml_lex_state19 lexbuf
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state19 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'='|eof *)
  |60|61|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'i' *)
  |105 ->
    __ocaml_lex_state20 lexbuf
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state20 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'='|eof *)
  |60|61|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'o' *)
  |111 ->
    __ocaml_lex_state21 lexbuf
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state21 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'='|eof *)
  |60|61|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'n' *)
  |110 ->
    __ocaml_lex_state22 lexbuf
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state22 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|eof *)
  |60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'=' *)
  |61 ->
    __ocaml_lex_state23 lexbuf
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
    __ocaml_lex_state14 lexbuf

and __ocaml_lex_state23 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state24 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state24 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'1' *)
  |49 ->
    __ocaml_lex_state25 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state25 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'.' *)
  |46 ->
    __ocaml_lex_state26 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state26 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'0' *)
  |48 ->
    __ocaml_lex_state27 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state27 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state28 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state28 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |' ' *)
  |32 ->
    __ocaml_lex_state29 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state29 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'e' *)
  |101 ->
    __ocaml_lex_state30 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state30 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'n' *)
  |110 ->
    __ocaml_lex_state31 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state31 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'c' *)
  |99 ->
    __ocaml_lex_state32 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state32 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'o' *)
  |111 ->
    __ocaml_lex_state33 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state33 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'d' *)
  |100 ->
    __ocaml_lex_state34 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state34 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'i' *)
  |105 ->
    __ocaml_lex_state35 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state35 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'n' *)
  |110 ->
    __ocaml_lex_state36 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state36 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'g' *)
  |103 ->
    __ocaml_lex_state37 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state37 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'=' *)
  |61 ->
    __ocaml_lex_state38 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state38 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
(* L=1 [5] <- p ;  *)
    lexbuf.Lexing.lex_mem.(5) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state39 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state39 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state40 lexbuf
(* |'i' *)
  |105 ->
    __ocaml_lex_state41 lexbuf
(* |'I' *)
  |73 ->
    __ocaml_lex_state42 lexbuf
(* |'u' *)
  |117 ->
    __ocaml_lex_state43 lexbuf
(* |'U' *)
  |85 ->
    __ocaml_lex_state44 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state40 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'?' *)
  |63 ->
    __ocaml_lex_state71 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state41 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'s' *)
  |115 ->
    __ocaml_lex_state63 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state42 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'S' *)
  |83 ->
    __ocaml_lex_state55 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state43 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'t' *)
  |116 ->
    __ocaml_lex_state52 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state44 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'T' *)
  |84 ->
    __ocaml_lex_state45 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state45 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'F' *)
  |70 ->
    __ocaml_lex_state46 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state46 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state47 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state47 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'8' *)
  |56 ->
(* L=1 [6] <- p ;  *)
    lexbuf.Lexing.lex_mem.(6) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state48 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state48 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'"' *)
  |34 ->
    __ocaml_lex_state49 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state49 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'?' *)
  |63 ->
    __ocaml_lex_state50 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state50 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    __ocaml_lex_state51 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state51 lexbuf = (* t1 <- [5] ; t0 <- [6] ; *)
  lexbuf.Lexing.lex_mem.(1) <- lexbuf.Lexing.lex_mem.(5) ;
  lexbuf.Lexing.lex_mem.(0) <- lexbuf.Lexing.lex_mem.(6) ;
  0

and __ocaml_lex_state52 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'f' *)
  |102 ->
    __ocaml_lex_state53 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state53 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state54 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state54 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'8' *)
  |56 ->
(* L=1 [6] <- p ;  *)
    lexbuf.Lexing.lex_mem.(6) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state48 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state55 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'O' *)
  |79 ->
    __ocaml_lex_state56 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state56 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state57 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state57 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'8' *)
  |56 ->
    __ocaml_lex_state58 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state58 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'8' *)
  |56 ->
    __ocaml_lex_state59 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state59 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'5' *)
  |53 ->
    __ocaml_lex_state60 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state60 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'9' *)
  |57 ->
    __ocaml_lex_state61 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state61 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state62 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state62 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'2' *)
  |50 ->
(* L=1 [6] <- p ;  *)
    lexbuf.Lexing.lex_mem.(6) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state48 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state63 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'o' *)
  |111 ->
    __ocaml_lex_state64 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state64 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state65 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state65 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'8' *)
  |56 ->
    __ocaml_lex_state66 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state66 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'8' *)
  |56 ->
    __ocaml_lex_state67 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state67 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'5' *)
  |53 ->
    __ocaml_lex_state68 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state68 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'9' *)
  |57 ->
    __ocaml_lex_state69 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state69 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'-' *)
  |45 ->
    __ocaml_lex_state70 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state70 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'2' *)
  |50 ->
(* L=1 [6] <- p ;  *)
    lexbuf.Lexing.lex_mem.(6) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state48 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state71 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    __ocaml_lex_state72 lexbuf
  | _ ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action

and __ocaml_lex_state72 lexbuf = (* t1 <- -1 ; *)
  lexbuf.Lexing.lex_mem.(1) <- -1 ;
  0

and __ocaml_lex_state73 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|eof *)
  |60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'=' *)
  |61 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf
(* |' ' *)
  |32 ->
(* L=2 [2] <- [4] ;[3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_mem.(4) ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=3 [2] <- [4] ;[4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_mem.(4) ;
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |'/' *)
  |47 ->
    __ocaml_lex_state14 lexbuf
(* |'>' *)
  |62 ->
    __ocaml_lex_state16 lexbuf
  | _ ->
(* L=1 [4] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state73 lexbuf

and __ocaml_lex_state74 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'O' *)
  |79 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state75 lexbuf
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state75 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'C' *)
  |67 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state76 lexbuf
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state76 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'T' *)
  |84 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state77 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state77 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |'Y' *)
  |89 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state78 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state78 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |'P' *)
  |80 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state79 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state79 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'/'|'<'|'>'|eof *)
  |47|60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state8 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state9 lexbuf
(* |'E' *)
  |69 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state80 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state5 lexbuf

and __ocaml_lex_state80 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'<'|'>'|eof *)
  |60|62|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'/' *)
  |47 ->
    __ocaml_lex_state81 lexbuf
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state82 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state84 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state83 lexbuf

and __ocaml_lex_state81 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    __ocaml_lex_state85 lexbuf
(* |'<'|eof *)
  |60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
  | _ ->
    __ocaml_lex_state81 lexbuf

and __ocaml_lex_state82 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    __ocaml_lex_state85 lexbuf
(* |'<'|eof *)
  |60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'=' *)
  |61 ->
    __ocaml_lex_state81 lexbuf
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state82 lexbuf
  | _ ->
    __ocaml_lex_state86 lexbuf

and __ocaml_lex_state83 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    __ocaml_lex_state85 lexbuf
(* |'<'|eof *)
  |60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'/' *)
  |47 ->
    __ocaml_lex_state81 lexbuf
(* |' ' *)
  |32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state82 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=2 [4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state84 lexbuf
  | _ ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state83 lexbuf

and __ocaml_lex_state84 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    __ocaml_lex_state85 lexbuf
(* |'<'|eof *)
  |60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'/' *)
  |47 ->
    __ocaml_lex_state86 lexbuf
(* |' ' *)
  |32 ->
(* L=2 [2] <- [4] ;[3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_mem.(4) ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state82 lexbuf
(* |'=' *)
  |61 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state83 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=3 [2] <- [4] ;[4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_mem.(4) ;
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state84 lexbuf
  | _ ->
(* L=1 [4] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state87 lexbuf

and __ocaml_lex_state85 lexbuf = (* *)
  1

and __ocaml_lex_state86 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    __ocaml_lex_state85 lexbuf
(* |'<'|eof *)
  |60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'=' *)
  |61 ->
    __ocaml_lex_state81 lexbuf
(* |'\t'|'\n'|' ' *)
  |9|10|32 ->
(* L=1 [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state82 lexbuf
  | _ ->
    __ocaml_lex_state86 lexbuf

and __ocaml_lex_state87 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |'>' *)
  |62 ->
    __ocaml_lex_state85 lexbuf
(* |'<'|eof *)
  |60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'/' *)
  |47 ->
    __ocaml_lex_state86 lexbuf
(* |' ' *)
  |32 ->
(* L=2 [2] <- [4] ;[3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_mem.(4) ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state82 lexbuf
(* |'=' *)
  |61 ->
(* L=1 [2] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state83 lexbuf
(* |'\t'|'\n' *)
  |9|10 ->
(* L=3 [2] <- [4] ;[4] <- p ; [3] <- p ;  *)
    lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_mem.(4) ;
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state84 lexbuf
  | _ ->
(* L=1 [4] <- p ;  *)
    lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
    __ocaml_lex_state87 lexbuf

and __ocaml_lex_state88 lexbuf =   match __ocaml_lex_next_char lexbuf with
(* |' '|'/'|'<'|eof *)
  |32|47|60|256 ->
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos ;
    lexbuf.Lexing.lex_last_action
(* |'>' *)
  |62 ->
    __ocaml_lex_state89 lexbuf
  | _ ->
    __ocaml_lex_state88 lexbuf

and __ocaml_lex_state89 lexbuf = (* *)
  3

let rec tokenize lexbuf =
  __ocaml_lex_init_lexbuf lexbuf 7; 
  let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
  lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
  lexbuf.Lexing.lex_curr_p <- {lexbuf.Lexing.lex_curr_p with
    Lexing.pos_cnum = lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos};
  match __ocaml_lex_result with
  | 0 ->
let
# 18 "xml_tokenizer.mll"
                                   version
# 1249 "xml_tokenizer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 15) (lexbuf.Lexing.lex_start_pos + 18)
and
# 18 "xml_tokenizer.mll"
                                                                          encoding
# 1254 "xml_tokenizer.ml"
= Lexing.sub_lexeme_opt lexbuf lexbuf.Lexing.lex_mem.(1) lexbuf.Lexing.lex_mem.(0) in
# 19 "xml_tokenizer.mll"
    ( StartDoc(version, encoding) )
# 1258 "xml_tokenizer.ml"

  | 1 ->
# 20 "xml_tokenizer.mll"
                             (tokenize lexbuf;)
# 1263 "xml_tokenizer.ml"

  | 2 ->
let
# 21 "xml_tokenizer.mll"
                    tag
# 1269 "xml_tokenizer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) lexbuf.Lexing.lex_mem.(0)
and
# 21 "xml_tokenizer.mll"
                                               attrs
# 1274 "xml_tokenizer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 21 "xml_tokenizer.mll"
                                                             (
        let _ = match attrs with
          Some(s) -> Printf.printf "\nattrs\n%s\n" s;
          |None -> ()
        in
        StartE(tag, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf);)
# 1283 "xml_tokenizer.ml"

  | 3 ->
let
# 27 "xml_tokenizer.mll"
                       tag
# 1289 "xml_tokenizer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2) (lexbuf.Lexing.lex_curr_pos + -1) in
# 27 "xml_tokenizer.mll"
                               ( EndE(tag, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf); )
# 1293 "xml_tokenizer.ml"

  | 4 ->
let
# 28 "xml_tokenizer.mll"
                s
# 1299 "xml_tokenizer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 28 "xml_tokenizer.mll"
                      ( Data(s, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf);)
# 1303 "xml_tokenizer.ml"

  | 5 ->
# 29 "xml_tokenizer.mll"
         (raise End_of_file)
# 1308 "xml_tokenizer.ml"

  | _ -> raise (Failure "lexing: empty token")


;;

