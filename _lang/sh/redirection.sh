#!/bin/sh

printf "\n*   Using : #!/bin/sh\n"

printf "\n"
a='if (echo echoing); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "1A.\nAttempt the following script :\n*   \"$a\"\n*\n"
sh -c "$a"

printf "\n"
b='if (echo echoing > /dev/null); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "1B.\nAttempt the following script :\n*   \"$b\"\n*\n"
sh -c "$b"
printf "*\n*   Because, \">FILE\" means, FILE (in the SYSTEM-WIDE file \
table) is mapped to STDOUT (the standard output STREAM, commonly digit \
1, in the PER-PROCESS file descriptor table of the (echo) process). \
Prior to this remapping, the shell process's STDOUT would have been \
mapped to the TERMINAL process's STDIN. Furthermore, \"/dev/null\" \
refers to what is called the NULL DEVICE, which discards all data \
written to it, while reporting success ( you may read further on that ).\n"

printf "\n"
e='if (ls -invalid-option > /dev/null); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "2A.\nAttempt the following script :\n*   \"$e\"\n*\n"
sh -c "$e"
printf "*\n*   The same redirection order was provided, but this time \
the command's output was not hidden. This is because, the output was a \
different kind than before. Due to the error in the command, the (ls) \
process's output was NOT sent to its STDOUT ( digit 1, in its \
PER-PROCESS file descriptor table), but to its STDERR ( digit 2, the \
standard error STREAM, which was still mapped to the terminal process's\
STDIN. )\n"

printf "\n"
c='if (ls -invalid-option 1> /dev/null); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "2B.\nAttempt the following script :\n*   \"$c\"\n*\n"
sh -c "$c"
printf "*\n*   This executes in the same way as the previous example, \
beacuse \"1>FILE\" means, the same as \">FILE\", except that it is more \
explicit in mentioning (ls)'s PER-PROCESS file descriptor table's handler \
index 1.\n"

printf "\n"
d='if (ls -invalid-option 2> /dev/null); then echo evaluated to TRUE; else echo evaluated to FALSE; fi'
printf "2C.\nAttempt the following script :\n*   \"$d\"\n*\n"
sh -c "$d"
printf "*\n*   However, \"2>FILE\" means something different, and \
explicitly redirects the PER-PROCESS file descriptor table's handler \
index 2, referring to STDERR, to FILE in the SYSTEM-WIDE file table.\n"


printf "\n"
printf "# ... further examples may be helpful : REDIRECTIONS are
processed FROM LEFT TO RIGHT, so something like \"1>FILE 2>&1\" ...
which would mean the same as \">FILE 2>&1\", would FIRST map STDOUT, 1,
to FILE, and AFTER THA map 2 to FILE, because \"&1\" would be
interpreted as \"FILE, in the SYSTEM-WIDE file table, the referent of /
what is pointed to by, 1, in the PER-PROCESS file descriptor table.\"\n"


https://stackoverflow.com/questions/5129276/how-to-redirect-stdout-of-2nd-process-back-to-stdin-of-1st-process

<<HERE
The kernel manages a bunch of look-up tables/maps.

A1.
Block storage : raw file data

A2.
Block storage : inode table 
- this copied to RAM, at runtime
- metadata : size, device ID, user ID, group ID
- metadata : timestamps
- outgoing pointers from A2 to A1
- directory relationships : N-to-N pointers from A2 to A2 ( parents and
  children only; sometimes only children ) ... path resolutions
  traverses this graph ... garbage collection also ( hard links )
- symbolic links uncounted : pointers from A2. to <<nominal path>> (
  soft links )

B.
Random access memory : system-wide OPEN FILE TABLE table :
- NAMES of all open files/d-entries, in memory
- directory entry types include : regular files, directories, soft
  links, Unix sockets character and block devices, named pipes
- current cursor position / file offset
- access mode : read, write, readwrite
- outgoing N-to-1 pointers to B to A2
- incoming N-to-1 pointers from C to B
- etc.

C.
Per-process file descriptor table :
- keys  : integers : initially 0:STDIN, 1:STDOUT, 2:STDERR
- values: outgoing pointers from C to B

Shell command language elements pertaining to the above :


HERE
