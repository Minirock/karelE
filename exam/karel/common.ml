exception LexerError of string
exception SyntaxError of string

let number path offset =
	let file = open_in path in
	let rec next n sum =
		let size = String.length (input_line file) + 1 in
		if sum + size > offset then (n, offset - sum + 1)
		else next (n + 1) (sum + size) in
	next 1 0

let print_error lexbuf prog msg =
	let (line, col) = number prog (Lexing.lexeme_start lexbuf) in
	Printf.printf "ERROR:%d:%d: %s\n" line col msg

let print_fatal lexbuf prog msg =
	print_error lexbuf prog msg;
	raise Exit
