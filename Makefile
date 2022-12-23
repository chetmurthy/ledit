# $Id$

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib
MANDIR=/usr/local/man/man1
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
CAMLP5=camlp5r -I ext -I +../camlp-streams `ocamlfind query camlp-streams`/camlp_streams.cma
OBJS=cursor.cmo ledit.cmo go.cmo
INCLUDES= -I `camlp5 -where` -I +unix -I +../camlp-streams
OTHER_OBJS=unix.cma gramlib.cma camlp_streams.cma
OTHER_XOBJS=unix.cmxa gramlib.cmxa camlp_streams.cmxa
TARGET=ledit.out
MKDIR=mkdir -p
EXT=ext/pa_def.cmo ext/pa_local.cmo
CUSTOM=-custom
LEDIT_LIBDIR=$(shell $(OCAMLC) -where)/ledit

all: $(EXT) $(TARGET) ledit.1
all: ledit.cma
opt: ledit.cmxa

$(TARGET): $(OBJS)
	$(OCAMLC) $(CUSTOM) $(INCLUDES) $(OTHER_OBJS) $(OBJS) -o $(TARGET)

$(TARGET:.out=.opt): $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) $(INCLUDES) $(OTHER_XOBJS) $(OBJS:.cmo=.cmx) -o $(TARGET:.out=.opt)

ledit.1: ledit.1.tpl go.ml
	VERSION=`sed -n -e 's/^.* version = "\(.*\)".*$$/\1/p' go.ml`; \
	sed s/LEDIT_VERSION/$$VERSION/ ledit.1.tpl > ledit.1

ledit.cma: cursor.cmo ledit.cmo
	$(OCAMLC) -a -o $@ $^
ledit.cmxa: cursor.cmx ledit.cmx
	$(OCAMLOPT) -a -o $@ $^

clean:
	/bin/rm -f *.cm[iox] *.pp[oi] *.o ext/*.cm[io] *.bak $(TARGET) ledit.1
	rm -f ledit.cma ledit.cmxa *.a
	rm -f META

install:
	-$(MKDIR) $(BINDIR) $(MANDIR)
	-cp ledit.out $(BINDIR)/ledit
	-cp ledit.1 $(MANDIR)/ledit.1

install-lib: META
	-$(MKDIR) $(LEDIT_LIBDIR)
	cp META $(LEDIT_LIBDIR)/
	cp ledit.cma ledit.cmi cursor.cmi $(LEDIT_LIBDIR)/
	if [ -f ledit.cmxa ] ; then cp ledit.cmxa ledit.a $(LEDIT_LIBDIR)/ ; fi

META: META.tpl
	VERSION=`sed -n -e 's/^.* version = "\(.*\)".*$$/\1/p' go.ml`; \
	sed s/LEDIT_VERSION/$$VERSION/ META.tpl > META

depend:
	> .depend.new
	for i in $(OBJS:.cmo=.ml); do \
	  $(CAMLP5) pr_depend.cmo $$i >> .depend.new; \
	done
	mv .depend .depend.old
	mv .depend.new .depend

include .depend

ext/%.cmo: ext/%.ml
	$(CAMLP5) -loc loc $< -o ext/$*.ppo
	$(OCAMLC) $(INCLUDES) -c -impl ext/$*.ppo
	rm -f ext/$*.ppo

%.cmo: %.ml
	$(CAMLP5) $< -o $*.ppo
	$(OCAMLC) $(INCLUDES) -c -impl $*.ppo
	/bin/rm -f $*.ppo
%.cmx: %.ml
	$(CAMLP5) $< -o $*.ppo
	$(OCAMLOPT) $(INCLUDES) -c -impl $*.ppo
	/bin/rm -f $*.ppo
%.cmi: %.mli
	$(CAMLP5) $< -o $*.ppi
	$(OCAMLC) $(INCLUDES) -c -intf $*.ppi
	/bin/rm -f $*.ppi
