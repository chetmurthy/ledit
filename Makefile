# $Id$

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib
MANDIR=/usr/local/man/man1
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
CAMLP5=camlp5 -I ext pa_s.cmo pr_dump.cmo
ZOFILES=cursor.cmo ledit.cmo go.cmo
TARGET=ledit.out
MKDIR=mkdir -p
EXT=ext/pa_s.cmo ext/pa_def.cmo ext/pa_local.cmo

all: $(EXT) $(TARGET) ledit.1

$(TARGET): $(ZOFILES)
	$(OCAMLC) -custom unix.cma $(ZOFILES) -o $(TARGET)

$(TARGET:.out=.opt): $(ZOFILES:.cmo=.cmx)
	$(OCAMLOPT) unix.cmxa $(ZOFILES:.cmo=.cmx) -o $(TARGET:.out=.opt)

ledit.1: ledit.1.tpl go.ml
	VERSION=`sed -n -e 's/^.* version = "\(.*\)".*$$/\1/p' go.ml`; \
	sed s/LEDIT_VERSION/$$VERSION/ ledit.1.tpl > ledit.1

clean:
	/bin/rm -f *.cm[iox] *.pp[oi] *.o ext/*.cm[io] *.bak $(TARGET) ledit.1

install:
	-$(MKDIR) $(BINDIR) $(MANDIR)
	-cp ledit.out $(BINDIR)/ledit
	-cp ledit.1 $(MANDIR)/ledit.1

depend:
	> .depend.new
	for i in $(ZOFILES:.cmo=.ml); do \
	  $(CAMLP5) pr_depend.cmo $$i >> .depend.new; \
	done
	mv .depend .depend.old
	mv .depend.new .depend

include .depend

ext/%.cmo: ext/%.ml
	camlp5r -I ext -loc loc $< -o ext/$*.ppo
	$(OCAMLC) -I +camlp5 -c -impl ext/$*.ppo
	rm -f ext/$*.ppo

%.cmo: %.ml
	$(CAMLP5) $< -o $*.ppo
	$(OCAMLC) -I +camlp5 -c -impl $*.ppo
	/bin/rm -f $*.ppo
%.cmx: %.ml
	$(CAMLP5) $< -o $*.ppo
	$(OCAMLOPT) -I +camlp5 -c -impl $*.ppo
	/bin/rm -f $*.ppo
%.cmi: %.mli
	$(CAMLP5) $< -o $*.ppi
	$(OCAMLC) -c -intf $*.ppi
	/bin/rm -f $*.ppi
