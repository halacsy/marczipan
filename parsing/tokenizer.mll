(* scanner for a toy language *)

{
  type token = Token of string * string * int * int 
  
  let tok surface typ lexbuf = Token(surface, typ, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
    
}

(* HTML 4.0 ISO 8859-1 karakterentitások, amelyek előfordulhatnak a szavakban *)
let latin1 = "&Agrave;"|"&Atilde;"|"&Aring;"|"&AElig;"|"&Egrave;"|"&Ecirc;"|"&Igrave;"|"&Iuml;"|"&ETH;"|"&Ntilde;"|"&Ograve;"|"&Oslash;"|"&Ugrave;"|"&THORN;"|"&agrave;"|"&atilde;"|"&aring;"|"&aelig;"|"&egrave;"|"&ecirc;"|"&igrave;"|"&iuml;"|"&eth;"|"&ntilde;"|"&ograve;"|"&oslash;"|"&ugrave;"|"&thorn;"|"&yuml;"

(* HTML 4.0 ISO szimbólum karakterentitások */
  XXX tízszeresére növeli a flex forrást és a fordítási időt a szimbólumok kezelése, *)

let symbols = "&pi;" |"&fnof;"|"&Alpha;"|"&Beta;"|"&Gamma;"|"&Delta;"|"&Epsilon;"|"&Zeta;"|"&Eta;"|"&Theta;"|"&Iota;"|"&Kappa;"|"&Lambda;"|"&Mu;"|"&Nu;"|"&Xi;"|"&Omicron;"|"&Pi;"|"&Rho;"|"&Sigma;"|"&Tau;"|"&Upsilon;"|"&Phi;"|"&Chi;"|"&Psi;"|"&Omega;"|"&alpha;"|"&beta;"|"&gamma;"|"&delta;"|"&epsilon;"|"&zeta;"|"&eta;"|"&theta;"|"&iota;"|"&kappa;"|"&lambda;"|"&mu;"|"&nu;"|"&xi;"|"&omicron;"|"&pi;"|"&rho;"|"&sigmaf;"|"&sigma;"|"&tau;"|"&upsilon;"|"&phi;"|"&chi;"|"&psi;"|"&omega;"|"&thetasym;"|"&upsih;"|"&piv;"|"&bull;"|"&hellip;"|"&prime;"|"&Prime;"|"&oline;"|"&frasl;"|"&weierp;"|"&image;"|"&real;"|"&trade;"|"&alefsym;"|"&larr;"|"&uarr;"|"&rarr;"|"&darr;"|"&harr;"|"&crarr;"|"&lArr;"|"&uArr;"|"&rArr;"|"&dArr;"|"&hArr;"|"&forall;"|"&part;"|"&exist;"|"&empty;"|"&nabla;"|"&isin;"|"&notin;"|"&ni;"|"&prod;"|"&sum;"|"&minus;"|"&lowast;"|"&radic;"|"&prop;"|"&infin;"|"&ang;"|"&and;"|"&or;"|"&cap;"|"&cup;"|"&int;"|"&there4;"|"&sim;"|"&cong;"|"&asymp;"|"&ne;"|"&equiv;"|"&le;"|"&ge;"|"&sub;"|"&sup;"|"&nsub;"|"&sube;"|"&supe;"|"&oplus;"|"&otimes;"|"&perp;"|"&sdot;"|"&lceil;"|"&rceil;"|"&lfloor;"|"&rfloor;"|"&lang;"|"&rang;"|"&loz;"|"&spades;"|"&clubs;"|"&hearts;"|"&diams;" 

(*
	/*  WORDCHARS: A szavak az ISO-8859-2 betûi mellett számokat, pontot,			*/
	/*  paragrafus-, fok-, százalék- és kötõjelet, valamint HTML entitásként megadva	*/
	/*  nagykötõjelet (&ndash;), valamint kvirtmínuszt (&mdash;) tartalmazhatnak,		*/
	/*  továbbá betûszerepre nem vizsgált decimális UNICODE entitást (például &345;).	*/
	/*  Az ISO-8859-2 betûi a locale csomag alapján automatikusan lettek kiválogatva.	*/
	/*  További információk: man iso-8859-2.									*/

	/* fok, és százalékjel kivéve */
		
*)
let nonascii_letters = [
	(* capital A, grave accent              À   *) '\192'
	(* capital A, acute accent              Á   *) '\193'
	(* capital A, circumflex accent         Â   *) '\194'
	(* capital A, tilde                     Ã   *) '\195'
	(* capital A, dieresis or umlaut mark   Ä   *) '\196'
	(* capital A, ring                      Å   *) '\197'
	(* capital AE diphthong (ligature)      Æ   *) '\198'
	(* capital C, cedilla                   Ç   *) '\199'
	(* capital E, grave accent              È   *) '\200'
	(* capital E, acute accent              É   *) '\201'
	(* capital E, circumflex accent         Ê   *) '\202'
	(* capital E, dieresis or umlaut mark   Ë   *) '\203'
	(* capital I, grave accent              Ì   *) '\204'
	(* capital I, acute accent              Í   *) '\205'
	(* capital I, circumflex accent         Î   *) '\206'
	(* capital I, dieresis or umlaut mark   Ï   *) '\207'
	(* capital Eth, Icelandic               Ð   *) '\208'
	(* capital N, tilde                     Ñ   *) '\209'
	(* capital O, grave accent              Ò   *) '\210'
	(* capital O, acute accent              Ó   *) '\211'
	(* capital O, circumflex accent         Ô   *) '\212'
	(* capital O, tilde                     Õ   *) '\213'
	(* capital O, dieresis or umlaut mark   Ö   *) '\214'
	(* multiply sign                        ×   *) '\215'
	(* capital O, slash                     Ø   *) '\216'
	(* capital U, grave accent              Ù   *) '\217'
	(* capital U, acute accent              Ú   *) '\218'
	(* capital U, circumflex accent         Û   *) '\219'
	(* capital U, dieresis or umlaut mark   Ü   *) '\220'
	(* capital Y, acute accent              Ý   *) '\221'
	(* capital THORN, Icelandic             Þ   *) '\222'
	(* small sharp s, German (sz ligature)  ß   *) '\223'
	(* small a, grave accent                à   *) '\224'
	(* small a, acute accent                á   *) '\225'
	(* small a, circumflex accent           â   *) '\226'
	(* small a, tilde                       ã   *) '\227'
	(* small a, dieresis or umlaut mark     ä   *) '\228'
	(* small a, ring                        å   *) '\229'
	(* small ae diphthong (ligature)        æ   *) '\230'
	(* small c, cedilla                     ç   *) '\231'
	(* small e, grave accent                è   *) '\232'
	(* small e, acute accent                é   *) '\233'
	(* small e, circumflex accent           ê   *) '\234'
	(* small e, dieresis or umlaut mark     ë   *) '\235'
	(* small i, grave accent                ì   *) '\236'
	(* small i, acute accent                í   *) '\237'
	(* small i, circumflex accent           î   *) '\238'
	(* small i, dieresis or umlaut mark     ï   *) '\239'
	(* small eth, Icelandic                 ð   *) '\240'
	(* small n, tilde                       ñ   *) '\241'
	(* small o, grave accent                ò   *) '\242'
	(* small o, acute accent                ó   *) '\243'
	(* small o, circumflex accent           ô   *) '\244'
	(* small o, tilde                       õ   *) '\245'
	(* small o, dieresis or umlaut mark     ö   *) '\246'
	(* division sign                        ÷   *) '\247'
	(* small o, slash                       ø   *) '\248'
	(* small u, grave accent                ù   *) '\249'
	(* small u, acute accent                ú   *) '\250'
	(* small u, circumflex accent           û   *) '\251'
	(* small u, dieresis or umlaut mark     ü   *) '\252'
	(* small y, acute accent                ý   *) '\253'
	(* small thorn, Icelandic               þ   *) '\254'
	(* small y, dieresis or umlaut mark     ÿ   *) '\255'
	]
let ascii_letters = ['a'-'z' 'A'-'Z']	
let letters = ascii_letters | nonascii_letters
let digit = ['0'-'9']
let dash = "&ndash;"|"&mdash;"
let spunct = ['.' '!' '?' ]
let wpunct = ['-' '_' '/' ';']
let whitespace = [' ' '\t']
  
let newline = ['\n']
let html_entity = "&" digit digit digit ";"
let wordchars = (latin1 | letters | digit | dash | html_entity)*

let token_chars = [^ '.' ',' ':' '/' ';' '(' ')' '?' '!' '\"' ' ' '\t' '\r' '\n']
  
let top_level_domains = ".hu"|".HU"|".org"|".ORG"|".com"|".COM"|".net"|".NET"|".info"|".INFO"|".edu"|".EDU"|".gov"|".GOV"|".mil"|".MIL"


let host_name = (letters | digit)+ ('.' (letters | digit)+ ('.' (letters | digit)+)? )?  top_level_domains
let email = letters+ '@' host_name

rule tokenize = parse
   
  | email as s {tok s "email" lexbuf;} 
  | host_name as s {tok s "host" lexbuf;}
  | digit+ as s {tok s "num" lexbuf;} 
  | digit+ '.' digit+ as s {tok s "fnum" lexbuf;} 
   
  | wordchars+  as s {tok s "word" lexbuf;} 
  | whitespace+  { (* skip *) tokenize lexbuf} 
  | newline+ { (* skip *) tokenize lexbuf}
  | _ 
  	{ (* Printf.printf "Unrecognized character: %c\n" c; *)
	  tokenize lexbuf
	}
  | eof		{raise End_of_file }


{

let  iterate f lexbuf =
  try
  while (true) do
    f (tokenize lexbuf)
  done;
	with End_of_file -> ()	
(*
let _ =
			iterate (fun (Token(s, typ, _, _)) -> print_string typ;print_endline s;) (Lexing.from_channel stdin)
*)
}