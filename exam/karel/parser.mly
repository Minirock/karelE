%{

open Quad
open Common
open Comp
open Karel

let gen_iter (i, a) =
	let t = new_temp () in
	gen (SETI (t, 1));
	gen (ADD (i, i, t));
	gen (GOTO (a));
	backpatch a (nextquad())

let gen_while (b, a) =
	gen (GOTO b);
	backpatch a (nextquad ())

%}

%token BEGIN_PROG
%token BEGIN_EXEC
%token END_EXEC
%token END_PROG
%token DEF_NEW
%token AS

%token MOVE
%token TURN_LEFT
%token TURN_OFF
%token PICK_BEEPER
%token PUT_BEEPER

%token NEXT_TO_A_BEEPER
%token NOT_NEXT_TO_A_BEEPER
%token FRONT_IS_CLEAR
%token FRONT_IS_BLOCKED
%token LEFT_IS_CLEAR
%token LEFT_IS_BLOCKED
%token RIGHT_IS_CLEAR
%token RIGHT_IS_BLOCKED
%token FACING_NORTH
%token NOT_FACING_NORTH
%token FACING_EAST
%token NOT_FACING_EAST
%token FACING_SOUTH
%token NOT_FACING_SOUTH
%token FACING_WEST
%token NOT_FACING_WEST
%token ANY_BEEPERS_IN_BEEPER_BAG
%token NO_BEEPERS_IN_BEEPER_BAG

%token SEMI
%token BEGIN
%token END
%token ITERATE
%token TIMES
%token WHILE
%token DO
%token IF
%token THEN
%token ELSE
%token DEFINE_NEW_INSTRUCTION
%token AS

%token SET
%token INC
%token DEC
%token EQ
%token NE
%token MAX
%token MIN
%token <int>REG

%token <int> INT
%token <string> ID

%type <unit> prog
%start prog

%%

	/*
prog:	BEGIN_PROG BEGIN_EXEC stmts_opt END_EXEC END_PROG
			{ () }
;
	*/

prog:	BEGIN_PROG initial_goto defines_opt BEGIN_EXEC initial_patch stmts_opt END_EXEC END_PROG
			{ () }
;

initial_goto:				{ gen (GOTO 0) }
;

initial_patch:				{ backpatch 0 (nextquad ()) }
;

defines_opt:
			/* empty */		{ () }
|			defines			{ () }
;

defines:	define			{ () }
|			defines define	{ () }
;

define:		def_header stmts
				{ gen RETURN }

def_header:	DEFINE_NEW_INSTRUCTION ID AS
					{
						if is_defined $2
						then raise (SyntaxError (Printf.sprintf "%s already defined!" $2))
						else define $2 (nextquad ())
					}
;


stmts_opt:	/* empty */		{ () }
|			stmts			{ () }
;

stmts:		stmt			{ () }
|			stmts SEMI stmt	{ () }
|			stmts SEMI		{ () }
;

stmt:		simple_stmt
				{ () }
|			if_head THEN stmt
				{ backpatch $1 (nextquad ()) }
|			if_then ELSE stmt
				{ backpatch $1 (nextquad ()) }
|			iter_head stmt
				{ gen_iter $1 }
|			while_head stmt
				{ gen_while $1 }
;

reg_stmt:	simple_stmt
				{ () }
|			if_then ELSE reg_stmt
				{ backpatch $1 (nextquad ()) }
|			iter_head reg_stmt
				{ gen_iter $1 }
|			while_head reg_stmt
				{ gen_while $1 }
;


if_head:	IF test
				{
					let r = new_temp ()+ 20 in
					gen (SETI (r, 0));
					let a = nextquad () in
					gen (GOTO_EQ (0, $2, r));
					a
				}
|		IF test_diff
				{
					let r = new_temp ()+20 in
					gen (SETI (r, 0));
					let a = nextquad () in
					gen (GOTO_NE (0, $2, r));
					a
				}
;

then_part:	THEN reg_stmt
				 { nextquad () }
;

if_then:	if_head then_part
				{
					let a = nextquad () in
					gen (GOTO 0);
					backpatch $1 (nextquad ());
					a
				}
;


simple_stmt: TURN_LEFT
				{ gen (INVOKE (turn_left, 0, 0)) }
|			TURN_OFF
				{ gen STOP  }
|			MOVE
				{ gen (INVOKE (move, 0, 0)) }
|			PICK_BEEPER
				{ gen (INVOKE (pick_beeper, 0, 0)) }
|			PUT_BEEPER
				{ gen (INVOKE (put_beeper, 0, 0)) }
|			BEGIN stmts END
				{ () }
|			ID
				{
					if not (is_defined $1)
					then raise (SyntaxError (Printf.sprintf "%s is not defined!" $1))
					else gen (CALL (get_define $1))
				}
|			REG SET INT
				{
					gen (SETI($1,$3));
				}
|			REG INC
				{
					let v = new_temp ()+20 in
					gen (SETI (v,1));
					gen (ADD($1,$1,v));
				}
|			REG DEC
				{
					let v = new_temp ()+20 in
					gen (SETI (v,1));
					gen (SUB($1,$1,v));
				}
;


while_head:	WHILE mark test DO
				{
					let t = new_temp () in
					gen (SETI (t, 0));
					let a = nextquad () in
					gen (GOTO_EQ (0, $3, t));
					($2, a)
				}

mark:
				{ nextquad () }
;

iter_head:	ITERATE INT TIMES
				{
					let i = new_temp () in
					let n = new_temp () in
					gen (SETI (n, $2));
					gen (SETI (i, 0));
					let a = nextquad () in
					gen (GOTO_GE (0, i, n));
					(i, a)
				}

test:
			NEXT_TO_A_BEEPER
				{
					let r = new_temp () in
					gen (INVOKE (next_beeper, r, 0));
					r
				}
|			NOT_NEXT_TO_A_BEEPER
				{
					let r = new_temp () in
					gen (INVOKE (no_next_beeper, r, 0));
					r
				}
|			FRONT_IS_CLEAR
				{
					let r = new_temp () in
					gen (INVOKE (is_clear, front, r));
					r
				}
|			FRONT_IS_BLOCKED
				{
					let r = new_temp () in
					gen (INVOKE (is_blocked, front, r));
					r
				}
|			LEFT_IS_CLEAR
				{
					let r = new_temp () in
					gen (INVOKE (is_clear, left, r));
					r
				}
|			LEFT_IS_BLOCKED
				{
					let r = new_temp () in
					gen (INVOKE (is_blocked, left, r));
					r
				}
|			RIGHT_IS_CLEAR
				{
					let r = new_temp () in
					gen (INVOKE (is_clear, right, r));
					r
				}
|			RIGHT_IS_BLOCKED
				{
					let r = new_temp () in
					gen (INVOKE (is_blocked, right, r));
					r
				}
|			FACING_NORTH
				{
					let r = new_temp () in
					gen (INVOKE (facing, north, r));
					r
				}
|			NOT_FACING_NORTH
				{
					let r = new_temp () in
					gen (INVOKE (not_facing, north, r));
					r
				}
|			FACING_EAST
				{
					let r = new_temp () in
					gen (INVOKE (facing, east, r));
					r
				}
|			NOT_FACING_EAST
				{
					let r = new_temp () in
					gen (INVOKE (not_facing, east, r));
					r
				}
|			FACING_SOUTH
				{
					let r = new_temp () in
					gen (INVOKE (facing, south, r));
					r
				}
|			NOT_FACING_SOUTH
				{
					let r = new_temp () in
					gen (INVOKE (not_facing, south, r));
					r
				}
|			FACING_WEST
				{
					let r = new_temp () in
					gen (INVOKE (facing, west, r));
					r
				}
|			NOT_FACING_WEST
				{
					let r = new_temp () in
					gen (INVOKE (not_facing, west, r));
					r
				}
|			ANY_BEEPERS_IN_BEEPER_BAG
				{
					let r = new_temp () in
					gen (INVOKE (any_beeper, r, 0));
					r
				}
|			NO_BEEPERS_IN_BEEPER_BAG
				{
					let r = new_temp () in
					gen (INVOKE (no_beeper, r, 0));
					r
				}
|			REG NE INT
				{
					let v = new_temp()+20 in
					gen (SETI(v,$3));
					let v' = new_temp()+20 in
				  	gen (SUB(v',$1,v));
				  	v'
				}
;

test_diff:			REG EQ INT
				{
					let v = new_temp()+20 in
					gen (SETI(v,$3));
					let v' = new_temp()+20 in
					gen (SUB(v',$1,v));
					v'
				}
;

