# $Id$

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib
MANDIR=/usr/man/manl
COMP=ocamlc
PP=-pp camlp4r
ZOFILES=cursor.cmo ledit.cmo go.cmo
TARGET=ledit.out

all: pa_local.cmo $(TARGET) ledit.l

$(TARGET): $(ZOFILES)
	$(COMP) -custom unix.cma -cclib -lunix $(ZOFILES) -o $(TARGET)

ledit.l: ledit.l.tpl go.ml
	VERSION=`sed -n -e 's/^.* version = "\(.*\)".*$$/\1/p' go.ml`; \
	sed s/LEDIT_VERSION/$$VERSION/ ledit.l.tpl > ledit.l

ledit.cmo: ledit.ml pa_local.cmo
	$(COMP) -pp "camlp4r ./pa_local.cmo" -c $<

pa_local.cmo: pa_local.ml
	$(COMP) -pp "camlp4r pa_extend.cmo q_MLast.cmo" -I `camlp4 -where` -c pa_local.ml

clean:
	/bin/rm -f *.cm[oix] *.pp[oi] *.bak $(TARGET) ledit.l

install:
	mkdirhier $(BINDIR) $(MANDIR)
	-cp ledit.out $(BINDIR)/ledit
	-cp ledit.l $(MANDIR)/ledit.l

depend:
	ocamldep *.mli *.ml > .depend

include .depend

.ml.cmo:
	$(COMP) $(PP) -c $<
.mli.cmi:
	$(COMP) $(PP) -c $<

.SUFFIXES: .ml .cmo .mli .cmi
