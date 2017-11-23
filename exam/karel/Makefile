export SHELL:=/bin/bash
OCAMLC = ocamlc -thread -g
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

SOURCES = \
	common.ml \
	quad.ml \
	vm.ml \
	karel.ml \
	wparser.mly \
	wlexer.mll \
	comp.ml \
	parser.mly \
	lexer.mll \
	vector.ml \
	ui.ml \
	game.ml
OBJECTS = $(patsubst %.mll,%.cmo,$(patsubst %.mly,%.cmo,$(patsubst %.ml,%.cmo,$(SOURCES))))
CLEAN = $(OBJECTS) parser.ml lexer.ml parser.mli

all: game console

clean:
	rm -rf $(CLEAN)

game: $(OBJECTS)
	$(OCAMLC) -o $@ unix.cma threads.cma graphics.cma $(OBJECTS) $(OBJECT:.cmo=.cmi)

CONSOLE_SOURCES = \
	common.ml \
	quad.ml \
	vm.ml \
	karel.ml \
	wparser.mly \
	wlexer.mll \
	comp.ml \
	parser.mly \
	lexer.mll \
	console.ml
CONSOLE_OBJECTS = $(patsubst %.mll,%.cmo,$(patsubst %.mly,%.cmo,$(patsubst %.ml,%.cmo,$(CONSOLE_SOURCES))))
CLEAN += $(CONSOLE_OBJECTS) parser.ml lexer.ml parser.mli
console: $(CONSOLE_OBJECTS)
	$(OCAMLC) -o $@ unix.cma threads.cma graphics.cma $(CONSOLE_OBJECTS) $(CONSOLE_OBJECT:.cmo=.cmi)

parser.cmo: parser.cmi common.cmo vm.cmo karel.cmo
parser.cmi: comp.cmo common.cmo vm.cmo karel.cmo
lexer.cmo: 	parser.cmi common.cmo
wparser.cmo: wparser.cmi karel.cmo
wlexer.cmo:	 wparser.cmi common.cmo
game.cmo: 	lexer.cmo parser.cmo common.cmo comp.cmo quad.cmo ui.cmo vm.cmo wparser.cmo wlexer.cmo
comp.cmo:	common.cmo quad.cmo
ui.cmo:		quad.cmo vm.cmo
karel.cmo:	vm.cmo quad.cmo
console.cmo:	vm.cmo

%.cmo: %.ml
	$(OCAMLC) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

%.ml %.mli: %.mly
	$(OCAMLYACC) -v $<

%.ml: %.mll
	$(OCAMLLEX) $< -o $@

DIST = \
	$(SOURCES) \
	$(CONSOLE_SOURCES) \
	*.mly \
	*.mll \
	samples \
	Makefile \
	karel.txt
ARC=karel

dist:
	if [ test -d $(ARC) ]; then rm -rf $(ARC); fi
	mkdir $(ARC)
	cp -R $(DIST) $(ARC)
	./clean.py < parser.mly > $(ARC)/parser.mly
	./clean.py < lexer.mll > $(ARC)/lexer.mll
	tar cvfz $(ARC).tgz $(ARC)
	cd karel; make

complete:
	#echo $(SHELL)
	#if [ test -d $(ARC) ]; then rm -rf $(ARC); fi
	-rm -rf $(ARC)
	mkdir $(ARC)
	cp -R $(DIST) $(ARC)
	./complete.py < parser.mly > $(ARC)/parser.mly
	./complete.py < lexer.mll > $(ARC)/lexer.mll
	tar cvfz $(ARC)-complete.tgz $(ARC)
	cd karel; make

SAVE=$(HOME)/.karel
TO_SAVE=lexer.mll parser.mly

save:
	@test -d $(SAVE) || mkdir $(SAVE)
	@cp $(TO_SAVE) $(SAVE)
	@touch "$(SAVE)/Do not modify this!"
	@echo "Save done!"

restore:
	@test -d $(SAVE) || (echo "No save found!"; exit 1)
	@cp $(patsubst %,$(SAVE)/%,$(TO_SAVE)) .
	@echo "Restore done!"
 

