# $Id$

BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib
MANDIR=/usr/man/manl
COMP=ocamlc
PP=camlp4r
ZOFILES=cursor.cmo ledit.cmo go.cmo
TARGET=ledit.out

all: $(TARGET)

$(TARGET): $(ZOFILES)
	$(COMP) -custom unix.cma -cclib -lunix $(ZOFILES) -o $(TARGET)

clean:
	/bin/rm -f *.cmo *.cmi *.cmx *.bak $(TARGET)

install:
	-cp ledit.out $(BINDIR)/ledit
	-cp ledit.l $(MANDIR)/ledit.l

depend:
	/bin/rm -f Makefile.bak
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ocamldep *.mli *.ml) > Makefile

.ml.cmo:
	$(COMP) -pp "$(PP)" -c $<
.mli.cmi:
	$(COMP) -pp "$(PP)" -c $<

.SUFFIXES: .ml .cmo .mli .cmi

### DO NOT DELETE THIS LINE
cursor.cmo: cursor.cmi 
cursor.cmx: cursor.cmi 
go.cmo: ledit.cmi 
go.cmx: ledit.cmx 
ledit.cmo: cursor.cmi ledit.cmi 
ledit.cmx: cursor.cmx ledit.cmi 
