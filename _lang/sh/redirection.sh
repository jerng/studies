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
***
* The kernel manages a bunch of look-up tables/maps.
***

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

B1.
Random access memory : system-wide OPEN FILE TABLE table :
- NAMES of all open files/d-entries, in memory
- directory entry types include : regular files, directories, soft
  links, Unix sockets character and block devices, named pipes
- current cursor position / file offset
- access mode : read, write, readwrite
- outgoing N-to-1 pointers to B1 to A2
- incoming N-to-1 pointers from B2 to B1
- etc.

B2.
Random access memory : per-process file descriptor table :
- keys  : integers : initially 0:STDIN, 1:STDOUT, 2:STDERR
- values: outgoing pointers from B2 to B1

***
* Shell command language elements pertaining to the above :
***

CONTEXT : when you use a CLI shell, there are at LEAST two types of
processes going on which you need to know about : shell, and terminal.
Terminal processes refer to the UI for all practical purposes; shell
processes refer to environments which can attach and detach from the
terminal process.

A gross illustration ...
-----------------------+-------------------+--------------------
AT THE BEGINNING :     |Shell Process      |Terminal Process
-----------------------+-------------------+--------------------
Std Input              |File Descriptor 0  |File Descriptor 0
                       |-> from Keyboard   |-> from Shell
                       |                   |
Std (Non-Error) Output |File Descriptor 1  |File Descriptor 1
                       |-> to Terminal     |-> to Screen
                       |                   |
Std Error (Output)     |File Descriptor 2  |File Descriptor 2
                       |-> to Terminal     |-> to Screen
-----------------------+-------------------+--------------------

Subsequently, COMMANDS run from the shell, runs in a new CHILD
PROCESSES.
    
    SHELL/processA -> COMMAND/processB

It is also common for shells to launch SUBSHELLS. E.g.,

    SHELL/processA -> B ... C -> SHELL/D -> COMMAND/processE

In each case ... still a gross illustration ...
-----------------------+-------------------+--------------------
                       |Parent Process     |Child Process
-----------------------+-------------------+--------------------
Std Input              |File Descriptor 0  |File Descriptor 0
                       |-> from Grandparent|-> from Parent 
                       |                   |
Std (Non-Error) Output |File Descriptor 1  |File Descriptor 1
                       |-> to Child        |-> to Parent 
                       |                   |
Std Error (Output)     |File Descriptor 2  |File Descriptor 2
                       |-> to Child        |-> to Parent 
-----------------------+-------------------+--------------------

[INTEGER][OPERATOR][WORD]   is the general form of redirection
                            expressions. [INTEGER] refers to an entity
                            from B2, and if OMITTED from the expression,
                            may be ASSUMED to be a conventional value.

** Where [WORD] is a token, without reference to entities : 

<<WORD          Make WORD the value of FD0/STDIN ( unless specified by
here-document   INTEGER )of the SHELL's process, for the CURRENT COMMAND
WORD            LINE.

0<<WORD         (here-document) begins AFTER the NEWLINE, and ends
here-document   BEFORE the sequence, NEWLINE-WORD.
WORD         

COMMAND1 <<WORD1; COMMAND2 <<WORD2      This example demonstrates
here-document1                          how multiple 
WORD1                                   (here-document) expressions
here-document2                          can be interleaved.
WORD2

COMMAND1 0<<WORD1; COMMAND2 0<<WORD2  
here-document1                        
WORD1                                 
here-document2                        
WORD2

    <<-WORD                             A variety of where leading 
    here-document                       TAB characters are stripped 
    WORD                                from the input lines and the
                                        linecontaining the trailing 
    0<<-WORD                            delimiter.
    here-document
    WORD         

** Where [WORD] is an entity from B1 ( SYSTEM-WIDE open files ) : 

<WORD           Make WORD the value of FD0/STDIN ( unless specified by
0<WORD          INTEGER ) of the SHELL's process, for the CURRENT COMMAND
                LINE.

>WORD           Make WORD the value of FD1/STDOUT ( unless specified by 
1>WORD          INTEGER ) of the SHELL's process, for the CURRENT COMMAND
                LINE.
            
                Beware race conditions, from multiple writers.
                This completely overwrites WORD.
                The "noclobber" option prevents overwriting by accident.

    >|WORD      MORE dangerous version of >WORD. Always overwrites 
    1>|WORD     WORD. Ignores any "noclobber" configurations.

    >>WORD      LESS dangerous version of >WORD. Appends data, 
    1>>WORD     instead of overwriting any existing WORD.

<>WORD          Make WORD open for both reading and writing on
0<>WORD         FD0/STDIN ( unless specified by INTEGER ) of the
                SHELL's process, for the CURRENT COMMAND LINE.

                Very limited documentation on this. Apparently the
                access mode for readwrite is set in the SYSTEM-WIDE open
                file table, so it is not the case that two entries are
                created in the PER-PROCESS file descriptor. Not clear.

                See mode "a+", "r+", "w+" in C's fopen(). "a+ in which
                case is "read and append", where writes always occur at
                the end of the file.

** Where [WORD] is an entity from B2 ( PER-PROCESS file descriptors ) or
                the character '-' signifying ( reversible ) closure : 

<&WORD          Make FD0/STDIN ( unless specified by INTEGER ), a copy
0<&WORD         of WORD. Anything READ FROM RIGHT will be subsequently
                READ FROM LEFT.

    <&-         Close FD0/STDIN ( unless specified by INTEGER ).
    0<&-

>&WORD          Make FD1/STDOUT ( unless specified by INTEGER ), a copy
1>&WORD         of WORD. Anything WRITTEN TO RIGHT will be subsequently
                WRITTEN TO LEFT.

    >&-         Close FD1/STDOUT ( unless specified by INTEGER ).
    0>&-

HERE


























