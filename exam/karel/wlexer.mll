{
open Wparser
}

let space = [' ' '\t' '\n']
let int = ['0' - '9']+

rule scan =
parse	int as i					{ INT (int_of_string i) }
|		"World"						{ WORLD }
|		"Beepers"					{ BEEPERS }
|		"Robot"						{ ROBOT }
|		"Wall"						{ WALL }
|		space+						{ scan lexbuf }
|		eof							{ EOF }
|		_ as c						{ raise (Common.LexerError (Printf.sprintf "unknown character '%c'" c)) }
