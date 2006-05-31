# $Id$

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib
MANDIR=/usr/local/man/man1
COMP=ocamlc
COMPOPT=ocamlopt
PP=camlp4r -I ext
ZOFILES=cursor.cmo ledit.cmo go.cmo
TARGET=ledit.out
MKDIR=mkdir -p
EXT=ext/pa_def.cmo ext/pa_local.cmo

all: $(EXT) $(TARGET) ledit.1

$(TARGET): $(ZOFILES)
	$(COMP) -custom unix.cma $(ZOFILES) -o $(TARGET)

$(TARGET:.out=.opt): $(ZOFILES:.cmo=.cmx)
	$(COMPOPT) unix.cmxa $(ZOFILES:.cmo=.cmx) -o $(TARGET:.out=.opt)

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
	  $(PP) pr_depend.cmo $$i >> .depend.new; \
	done
	mv .depend .depend.old
	mv .depend.new .depend

include .depend

%.cmo: %.ml
	$(PP) $< -o $*.ppo
	$(COMP) -I +camlp4 -c -impl $*.ppo
	/bin/rm -f $*.ppo
%.cmx: %.ml
	$(PP) $< -o $*.ppo
	$(COMPOPT) -I +camlp4 -c -impl $*.ppo
	/bin/rm -f $*.ppo
%.cmi: %.mli
	$(PP) $< -o $*.ppi
	$(COMP) -c -intf $*.ppi
	/bin/rm -f $*.ppi
