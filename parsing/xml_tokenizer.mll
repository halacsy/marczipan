
{
  type token = StartE of string * int * int | EndE of string * int * int | Data of string * int * int | StartDoc of string * string option 
}


let lt = '<' 
let gt = '>'
let per = '/'
let tagname = [^  '/' '<' '>' ' ']+
let no_duck = [^ '<' '>']+  
let attrname = [^ '<' '>' '=' ' ' '\t' '\n']+
let space = [' ' '\t' '\n']+
let version = "1.0"
let encoding = "UTF-8" | "utf-8"| "ISO-8859-2" | "iso-8859-2"

rule tokenize = parse
  | "<?xml version=\"" (version as version) "\" encoding=\"" (encoding as encoding)? "\"?>"
    { StartDoc(version, encoding) }
  | "<!DOCTYPE" no_duck+ ">" {tokenize lexbuf;}
  | '<' (tagname as tag)   '>' {
        StartE(tag, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf);}
  | lt per (tagname as tag) gt { EndE(tag, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf); } 
  | no_duck+ as s     { Data(s, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf);}
  | eof		{raise End_of_file}
  
  