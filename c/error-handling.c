#include <errno.h>
#include <stdio.h>

void main () {

    printf("errno : %d : \n", 
            errno,              // error number
            strerr(errno)       // string message of error number
          );

    perror("prints abitrary prefix, followed by `strerr` message : ");

    // ferror(file) : is used to check for and print errors while accessing  files
    // feof(file)   : is used to check for EOF errors 
    // clearerr()   : clears errors

}
