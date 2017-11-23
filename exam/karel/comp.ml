(** Karel compilation *)

let quads = Array.make 1000 Quad.STOP
let cnt = ref 0
let temp = ref 0
let symtab: (string * int) list ref = ref []

(** Ajoute un nouveau quad au programme.
	@param quad		Quad ajouté. *)
let gen quad =
	quads.(!cnt) <- quad;
	incr cnt


(** Obtient le numéro de quad suivant.
	@return	Numéro de quad suivant. *)
let nextquad _ =
	!cnt


(** Obtiens un nouveau numéro de variable temporaire.
	@return		Numéro de variable. *)
let new_temp _ =
	let r = !temp in
	incr temp;
	r


(** Applique un backpatch sur le goto à l'adresse g pour qu'il branche
	à l'adresse a.
	@param g	Adresse du goto.
	@param a	Adresse de branchement. *)
let backpatch g a =
	quads.(g) <-
		match quads.(g) with
		| Quad.GOTO _				-> Quad.GOTO a
		| Quad.GOTO_EQ (_, r, s)	-> Quad.GOTO_EQ (a, r, s)
		| Quad.GOTO_NE (_, r, s)	-> Quad.GOTO_NE (a, r, s)
		| Quad.GOTO_LT (_, r, s)	-> Quad.GOTO_LT (a, r, s)
		| Quad.GOTO_LE (_, r, s)	-> Quad.GOTO_LE (a, r, s)
		| Quad.GOTO_GT (_, r, s)	-> Quad.GOTO_GT (a, r, s)
		| Quad.GOTO_GE (_, r, s)	-> Quad.GOTO_GE (a, r, s)
		| _							-> failwith "Backpatched a non-goto instruction!"



(** Renvoie le programme courant.
	@return	Programme courant. *)
let get_program _ =
	gen Quad.STOP;
	Array.sub quads 0 !cnt


(** Test si le paramètre id est défini ou non.
	@parm id	Identificateur à tester.
	@return		Vrai si le symbole existe, faux sinon. *)
let is_defined id =
	List.mem_assoc id !symtab


(** Permet de définir une nouvelle définition.
	@param id	Identificateur de la définition.
	@param addr	Adresse du sous-programme. *)
let define id addr =
	symtab := (id, addr) :: !symtab


(** Renvoie l'adresse du sous-programme associé avec l'identificateur.
	@param id	Identifiant du sous-programme.
	@return		Adresse du sous-programme. *)
let get_define id =
	List.assoc id !symtab
