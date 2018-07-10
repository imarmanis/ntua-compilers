EXEFILE=Alan
MLFILES=Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml \
  Ast.ml Lexer.ml Parser.ml Main.ml
MLIFILES=Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
  Parser.mli

CMOFILES=$(patsubst %.ml,%.cmo,$(MLFILES))
CMIFILES=$(patsubst %.ml,%.cmi,$(MLFILES))
CMXFILES=$(patsubst %.ml,%.cmx,$(MLFILES))
OBJFILES=$(patsubst %.ml,%.o,$(MLFILES))
PARSERFILES=Parser.ml Parser.mli Parser.output Lexer.ml

CAMLP5_FLAGS=-pp "camlp5o ./extend.cmo"
OCAMLC_FLAGS=-g
OCAMLOPT_FLAGS=
OCAMLC=ocamlc $(OCAMLC_FLAGS)
OCAMLOPT=ocamlopt $(OCAMLOPT_FLAGS)
OCAMLDEP=ocamldep
INCLUDES=

all: $(EXEFILE)

extend.cmo: extend.ml
	$(OCAMLC) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I `camlp5 -where` -c $<

%.cmo: %.ml %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmx: %.ml extend.cmo
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

%.cmi: %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmo %.cmi: %.ml extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

#%.mli: %.ml \
	$(OCAMLC) $(OCAMLC_FLAGS) -i $< > $@

$(EXEFILE): Parser.mli Lexer.ml $(CMOFILES)
	$(OCAMLC) -o $@ $(CMOFILES)

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

Parser.ml Parser.mli: Parser.mly
	ocamlyacc -v Parser.mly

.PHONY: clean distclean

-include .depend

depend: $(MLFILES) $(MLIFILES) extend.cmo
	$(OCAMLDEP) $(CAMLP5_FLAGS) $(INCLUDES) \
          $(filter-out extend.cmo,$^) > .depend

clean:
	$(RM) $(CMXFILES) $(CMOFILES) $(CMIFILES) $(OBJFILES) $(EXEFILES) \
           extend.cmi extend.cmo \
           $(patsubst %,%.cm?,$(EXEFILES)) $(PARSERFILES) pplib.cma *~

distclean: clean
	$(RM) $(EXEFILE) .depend