# $Id$

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib
MANDIR=/usr/man/manl
COMP=ocamlc
PP=camlp4r
ZOFILES=cursor.cmo ledit.cmo go.cmo
TARGET=ledit.out
MKDIR=mkdir -p

all: pa_local.cmo $(TARGET) ledit.l

$(TARGET): $(ZOFILES)
	$(COMP) -custom unix.cma -cclib -lunix $(ZOFILES) -o $(TARGET)

ledit.l: ledit.l.tpl go.ml
	VERSION=`sed -n -e 's/^.* version = "\(.*\)".*$$/\1/p' go.ml`; \
	sed s/LEDIT_VERSION/$$VERSION/ ledit.l.tpl > ledit.l

pa_local.cmo: pa_local.ml
	$(PP) pa_extend.cmo q_MLast.cmo -warn_seq pa_local.ml -o pa_local.ppo
	$(COMP) -I `camlp4 -where` -c -impl pa_local.ppo
	/bin/rm -f pa_local.ppo

clean:
	/bin/rm -f *.cm[oix] *.pp[oi] *.o *.bak $(TARGET) ledit.l

install:
	-$(MKDIR) $(BINDIR) $(MANDIR)
	-cp ledit.out $(BINDIR)/ledit
	-cp ledit.l $(MANDIR)/ledit.l

depend:
	> .depend.new
	for i in $(ZOFILES:.cmo=.ml); do \
	  $(PP) ./pa_local.cmo pr_depend.cmo $$i >> .depend.new; \
	done
	mv .depend .depend.old
	mv .depend.new .depend

include .depend

.ml.cmo:
	$(PP) ./pa_local.cmo -warn_seq $< -o $*.ppo
	$(COMP) -I `camlp4 -where` -c -impl $*.ppo
	/bin/rm -f $*.ppo
.mli.cmi:
	$(PP) $< -o $*.ppi
	$(COMP) -c -intf $*.ppi
	/bin/rm -f $*.ppi

.SUFFIXES: .ml .cmo .mli .cmi
