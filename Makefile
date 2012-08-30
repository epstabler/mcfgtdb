OCAMLBYTE = ocamlc
OCAMLOPT = ocamlopt
OCAMLFLAGS = 

LIBS = str.cma
OPTLIBS = str.cmxa

GRAMMAR = g0

ML_SOURCES  = makeq.ml  mcfgtdb.ml $(GRAMMAR).ml
MLI_SOURCES = makeq.mli mcfgtdb.mli $(GRAMMAR).mli

CMIFILES = makeq.cmi mcfgtdb.cmi $(GRAMMAR).cmi
CMOFILES = makeq.cmo mcfgtdb.cmo $(GRAMMAR).cmo
CMXFILES = makeq.cmx mcfgtdb.cmx $(GRAMMAR).cmx

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mly .mll

# I set the default compilation to be opt (native). Use "make byte" to get byte code
all: opt

byte:  $(CMIFILES) $(CMOFILES) 
	$(OCAMLBYTE) $(OCAMLFLAGS) $(LIBS) $(CMOFILES) top.ml -o mcfgtdb

opt: $(CMIFILES) $(CMXFILES) 
	$(OCAMLOPT) $(OCAMLFLAGS) $(OPTLIBS) $(CMXFILES) top.ml -o mcfgtdb

%.cmo: %.ml %.cmi
	$(OCAMLBYTE) $(OCAMLFLAGS) $(LIBS) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLFLAGS) $(OPTLIBS) -c $<

%.cmi: %.mli 
	$(OCAMLBYTE) $(OCAMLFLAGS) -c $<

clean:
	rm -f *.cm*
	rm -f *.o
	rm -f *.obj
	rm -f *.d
	rm -f *~

realclean: clean
	rm -f mcfgtdb
	rm -f doc/*

doc:    all
	ocamldoc -html -d doc $(ML_SOURCES) $(MLI_SOURCES)
	ocamldoc -dot -o doc/tools.dot $(ML_SOURCES) $(MLI_SOURCES)
	ocamldoc -texi -o doc/tools.texinfo $(ML_SOURCES) $(MLI_SOURCES)
