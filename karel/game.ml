(** Entry point of the game application. *)
exception Exit

open Common
open Vector
open Printf

(* configuration *)
let x_space = 32
let y_space = 32
(*let width = 20
let height = 20*)
let karel_width = 20
let karel_height = 20
let grid_color = Graphics.cyan
let back_color = Graphics.white
let text_color = Graphics.black
let wall_color = Graphics.black
let karel_color = Graphics.black
let karel_back = Graphics.white
(*let karel_speed = 1.*)

(* beeper definition *)
let beeper_width = 16
let beeper_height = 16
let beeper_drawing = [
		COLOR Graphics.red;
		CIRCLE (beeper_width / 2);
		FILL_CIRCLE (beeper_width * 2 / 6)
	]


(* option processing *)
let just_compile = ref false
let time = ref 500
let itime = ref 0
let debug = ref false

let opts = [
	("-c", Arg.Set just_compile, "Stop after compilation and print quadruplets.");
	("-t", Arg.Set_int time, "time in ms between moves of the robot (default 500)");
	("-i", Arg.Set_int itime, "time in ms between two instructions (not used by default)");
	("-d", Arg.Set debug, "dump to stderr the executed quads")
]
let  doc = "Karel Game Emulator"


(**** application ***)
let run prog world =
	let karel_speed =
		if !itime = 0
		then (float_of_int !time) /. 1000.
		else (float_of_int !itime) /. 1000. in

	let centerx x ox = ox + x * x_space + x_space / 2 in
	let centery y oy = oy - y * y_space - y_space / 2 in

	let draw_karel window state ox oy =
		let ((x, y, dir, _), _) = state in
		let xc, yc = centerx x ox, centery y oy in
		let xl = xc - karel_width / 2 in
		let xr = xc + karel_width / 2 in
		let yt = yc + karel_height / 2 in
		let yb = yc - karel_height / 2 in
		Graphics.set_color karel_back;
		Graphics.fill_rect xl yb karel_width karel_height;
		Graphics.set_color karel_color;
		Graphics.draw_poly [| (xl, yt); (xr, yt); (xr, yb); (xl, yb) |];
		let pts = (List.nth
					[
						(fun _ -> [| (xl, yb); (xc, yt); (xr, yb) |]);
						(fun _ -> [| (xl, yb); (xr, yc); (xl, yt) |]);
						(fun _ -> [| (xl, yt); (xc, yb); (xr, yt) |]);
						(fun _ -> [| (xr, yb); (xl, yc); (xr, yt) |])
					]
					(dir - 1)) () in
		Graphics.fill_poly pts in

	let clear_karel window (state: Karel.karel) ox oy =
		let ((x, y, _, _), (_, map)) = state in
		let xc, yc = centerx x ox, centery y oy in
		let xl = xc - karel_width / 2 in
		let xr = xc + karel_width / 2 in
		let yt = yc + karel_height / 2 in
		let yb = yc - karel_height / 2 in
		Graphics.set_color back_color;
		Graphics.fill_rect xl yb karel_width karel_height;
		Graphics.set_color grid_color;
		Graphics.moveto xl yc;
		Graphics.lineto xr yc;
		Graphics.moveto xc yt;
		Graphics.lineto xc yb;
		if (Karel.map_beeper map x y) > 0
		then draw xc yc beeper_drawing in

	let draw_grid window kstate widget =
		let (x, y, _, h) = Ui.widget_get_box window (Ui.widget_name widget) in
		let (_, (_, (ww, wh, _))) = kstate in
		let draw = Graphics.draw_poly_line in
		let y = y + h - 1 in
		
		(* display grid *)
		Graphics.set_color grid_color;
		for i = 0 to ww - 1 do
			draw [|
				(x + i * x_space + x_space / 2, y);
				(x + i * x_space + x_space / 2, y - wh * y_space)
			|]
		done;
		for i = 0 to wh - 1 do
			draw [|
				(x, y - i * y_space - y_space / 2);
				(x + ww * x_space, y - i * y_space - y_space / 2)
			|]
		done;
		
		(* draw walls *)		
		let draw_cell xc yc (w, b) =
			let xl = x + xc * x_space in
			let xr = xl + x_space - 1 in
			let yt = y - yc * y_space + 1 in
			let yb = yt - y_space + 1 in
			let draw = Graphics.draw_poly_line in
			Graphics.set_color wall_color;
			if Karel.has_wall Karel.north w then
				draw [| (xl, yt); (xr, yt) |];
			if Karel.has_wall Karel.east w then
				draw [| (xr, yt); (xr, yb) |];
			if Karel.has_wall Karel.south w then
				draw [| (xl, yb); (xr, yb) |];
			if Karel.has_wall Karel.west w then
				draw [| (xl, yt); (xl, yb) |];
			if b > 0 then
				Vector.draw (centerx xc x) (centery yc y) beeper_drawing in
				
		let (_, world) = kstate in
		Karel.iter_map world draw_cell;
		
		(* draw karel *)
		draw_karel window kstate x y in

	let handle_x window event =
		Ui.window_quit window in

	let rec exec window =
		try
			if Vm.ended (Ui.get_app window) then window else
			let pc = Vm.get_pc (Ui.get_app window) in
			let disasm = Printf.sprintf "%04d %s" pc (Quad.to_string prog.(pc)) in
			let window = Ui.console_add window "console" disasm in
			if !debug then Printf.fprintf stderr "%s\n" disasm;
			let window = Ui.set_app window (Vm.step (Ui.get_app window) prog) in
			if Vm.ended (Ui.get_app window)
			then
				Ui.statusbar_display_untimed (Ui.widget_config window "status" [Ui.TEXT_COLOR Ui.blue]) "status" "Stopped!"
			else if !itime <> 0 then window
			else
				match prog.(Vm.get_pc (Ui.get_app window)) with
				| Quad.INVOKE (d, _, _) when d = Karel.move || d = Karel.turn_left
					-> window
				| _	
					-> exec window
		with
		| Karel.Error m ->
			let window = Ui.statusbar_display_untimed (Ui.widget_config window "status" [Ui.TEXT_COLOR Ui.red]) "status" m in
			Ui.set_app window (Vm.stop (Ui.get_app window))
		| Vm.Error (_, m) ->
			let window = Ui.statusbar_display_untimed (Ui.widget_config window "status" [Ui.TEXT_COLOR Ui.red]) "status" ("VM Error: " ^ m) in
			Ui.set_app window (Vm.stop (Ui.get_app window)) in

	let rec handle_time window =
		let (x, y, w, h) = Ui.widget_get_box window "world" in
		clear_karel window (Vm.get_istate (Ui.get_app window)) x (y + h - 1);
		let window = exec window in
		draw_karel window (Vm.get_istate (Ui.get_app window)) x (y + h -1);
		Ui.window_handle_timeout window karel_speed handle_time in

	let grid (widget: 'a Ui.widget) window msg =
		let kstate = Vm.get_istate (Ui.get_app window) in
		match msg with
		| Ui.DRAW
			-> draw_grid window kstate widget; (Ui.NOTHING, window)
		| Ui.GET_SIZE
			->	let (_, (_, (w, h, _))) = kstate in
				(Ui.SIZE (x_space * w, y_space * h), window)
		| _
			-> Ui.default_inst widget window msg in
	
	let kstate = Karel.init_state world in
	let vstate = Vm.new_state Karel.invoke kstate in
	let window 	= Ui.window_make "Karel 1.0" vstate in
	let window 	= Ui.widget_make window "world" grid in
	(*let window  = Ui.label_make window "status" "Welcome to karel!" in*)
	let window  = Ui.statusbar_make window "status" "Welcome to karel!" in
	let window	= Ui.vbox_make window "vbox" ["world"; "status"] in
	let window  = Ui.console_make window "console" "XXXX XXXXXXXXXXXXXXXXXXXXX" in
	let window  = Ui.hbox_make window "hbox" ["console"; "vbox"] in
	let window	= Ui.window_set_top window "hbox" in
	let window 	= Ui.window_handle_event window "" (Ui.KEY 'x') handle_x in
	let window 	= Ui.window_handle_timeout window karel_speed handle_time in
	Ui.window_run window


let process prog (world: Karel.world) =
	if !just_compile then
		Quad.print_prog prog
	else
		ignore (run prog world)


let scan args =

	let make prog =
		let file = open_in prog in
		let lexbuf = Lexing.from_channel file in
		try
			Parser.prog Lexer.scan lexbuf;
			Comp.get_program ()
		with
		|	Parsing.Parse_error		-> print_fatal lexbuf prog "syntax error"
		|	Common.LexerError msg 	-> print_fatal lexbuf prog msg
		|	Common.SyntaxError msg 	-> print_fatal lexbuf prog msg in

	let load_world path =
		let file = open_in path in
		let lexbuf = Lexing.from_channel file in
		try
			Wparser.top Wlexer.scan lexbuf;
			!Karel.world
		with
		|	Parsing.Parse_error		-> print_fatal lexbuf path "syntax error in world file"
		|	Common.LexerError msg 	-> print_fatal lexbuf path msg in
	
	match args with
	| [ prog ]			-> process (make prog) Karel.empty_world
	| [ world; prog]	-> process (make prog) (load_world world)
	| _					-> Arg.usage opts "ERROR: syntax: game program [map]"

let _ = 
	let free_args = ref [] in
	Arg.parse opts (fun arg -> free_args := arg :: !free_args) doc;
	scan !free_args

