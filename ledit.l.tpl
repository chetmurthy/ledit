.TH LEDIT 1 "Thu Apr 17, 97" "INRIA"
.SH NAME
ledit \- line editor, version LEDIT_VERSION

.SH SYNOPSIS
.B ledit
[-h \fIfile\fP]
[-x]
[-l \fIlength\fP]
[-c \fIcommand options\fP]

.SH DESCRIPTION
The command \fIledit\fP allows to edit lines one by one when running an
interactive command. When typing a line, some keys with control or meta
are interpreted: it is possible to insert characters in the middle of
the line, go to the beginning or the end of the line, get a previous line,
search for a line with a pattern, etc.

.SH OPTIONS
The options are:
.TP
.B -h \fIfile\fP
Save the lines typed (history) in \fIfile\fP. The default is to have them
only in memory (so, they are lost at the end of the program).
.TP
.B -x
Extend the history file (given in option "-h") if it already exists. The
default is to truncate the history file.
.TP
.B -v
Print ledit version and exit.
.TP
.B -l \fIlength\fP
Tells that \fIlength\fP is the maximum line length displayed. If the
line edited is longer than this length, the line scrolls horizontally,
while editing. The default value is 70.
.TP
.B -c \fIcommand options\fP
Runs the command \fIcommand\fP and its possible options. This
must be the last option of ledit. The default value is "cat".

.SH KEYS BINDINGS
In the following lines, the caret sign "^" means "control" and the
sequence "M-" means "meta" (either with the "meta" prefix, or by
pressing the "escape" key before). Examples:
.TP 1.0i
^a
press the "control" key, then press "a", then release "a", then
release "control".
.TP
M-a
press the "meta" key, then press "a", then release "a", then release
"meta", or: press and release the "escape" key, then press and release
"a" (the manipulation with "meta" may not work in some systems: in
this case, use the manipulation with "escape").
.PP

The editing commands are:
.nf

      ^a  : beginning of line
      ^e  : end of line
      ^f  : forward char
      ^b  : backward char
      M-f : forward word
      M-b : backard word
      ^p  : previous line in history
      ^n  : next line in history
      M-< : first line in history
      M-> : last line in history
      ^r  : reverse search in history (see below)
      ^d  : delete char (or EOF if the line is empty)
      ^h  : (or backspace) backward delete char
      del : backward delete char
      M-d : delete word
      ^q  : insert next char
      ^k  : cut until end of line
      ^y  : paste
      ^u  : line discard
      ^l  : refresh line
      ^g  : abort prefix
      ^c  : interrupt
      ^\\  : quit
      return : send line
      ^x     : send line and show next history line
      other  : insert char
.fi

The arrow keys can be used, providing your keyword returns standard key
sequences:
.nf

      up arrow    : previous line in history
      down arrow  : next line in history
      right arrow : forward char
      left arrow  : backward char

.fi
The reverse search in incremental, i.e. \fIledit\fP backward searchs in the
history a line holding the characters typed. If you type "a", its search the
first line before the current line holding an "a" and displays it. If you then
type a "b", its search a line holding "ab", and so on. If you type ^h (or
backspace), it returns to the previous line found. To cancel the search,
type ^g. To find another line before holding the same string, type ^r.
To stop the editing and display the current line found, type "escape"
(other commands of the normal editing, different from ^h, ^g, and ^r stop
the editing too).

Summary of reverse search commands:
.nf

      ^g  : abort search
      ^r  : search previous same pattern
      ^h  : (or backspace) search without the last char
      del : search without the last char
      any other command : stop search and show the line found

.fi

.SH KNOWN BUGS
It is not possible to suspend \fIledit\fP (using the suspend command,
usually ctrl-z).

.SH AUTHOR
Daniel de Rauglaudre, at INRIA, france.
.br
daniel.de_rauglaudre@inria.fr

.SH FILES
/usr/local/bin/ledit
