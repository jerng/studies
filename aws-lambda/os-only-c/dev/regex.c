#include <stdio.h>
#include <string.h>
#include <regex.h>
#define MAX_MATCHES 2

int main() {
    regex_t regex;
    regmatch_t matches[MAX_MATCHES];
    char result[36];
    const char *needle = " *prefix *: *([^[:space:]]*)";  
    const char *haystack = " prefix : TARGET noise ";
    // Compile the regular expression
    if (regcomp(&regex, needle, REG_EXTENDED)) {
        printf("Could not compile regex\n");
        return 1;
    }
    // Execute the regular
    // expression
    if (regexec(&regex, haystack, MAX_MATCHES, matches, 0) == 0) 
    {
        strncpy( result, 
                haystack + matches[1].rm_so,
                ( matches[1].rm_eo - matches[1].rm_so )
               );
        printf( "Match found : start : %i, end : %i, '%s'\n", 
                matches[1].rm_so,
                matches[1].rm_eo,
                result
              );
    } else {
        printf("No match found\n");
    }
    // Free
    // the
    // memory
    // used
    // by
    // the
    // regex
    regfree(&regex);
    return 0;
}
