#include <stdio.h>

void main () {

    char * address = NULL;
    char person = 'L';
    address = &person;

    printf( "\e[1;1H\e[2J\n"
            "GIVEN ...\n"
            "\n"
            "char * address = NULL;\n"
            "char person = 'L';\n"
            "address = &person;\n\n");

    printf("PRINT ...\n");
    printf("\n");
    printf("`person` \t\t: as char \t\t\t\t: '%c'\n",person);
    printf("&`person` \t\t: as pointer to char\t\t\t: '%p'\n",&person);
    printf("`address` \t\t: as pointer \t\t\t\t: '%p'\n",address);
    printf("`address` \t\t: as string (rude)\t\t\t: '%s'\n",address);
    printf("*`address` \t\t: as dereferenced pointer to a char \t: '%c'\n",*address);
    printf("\n\n");

    char letters[] = { 'M','N','O'};
    char * addresses = letters;

    printf( "GIVEN...\n"
            "char letters[] = { 'M','N','O'};\n"
            "char * addresses = letters;\n\n");

    printf("PRINT ...\n");
    printf("\n");
    printf("`letters` \t\t: as pointer to array\t\t\t\t: '%p'\n",letters);
    printf("`addresses` \t\t: as pointer to array\t\t\t\t: '%p'\n",addresses);
    printf("&`letters` \t\t: as pointer to array\t\t\t\t: '%p'\n",&letters);
    printf("&`letters`[0] \t\t: as pointer to array's _0 element\t\t: '%p'\n",&letters[0]);
    printf("&0[`letters`] \t\t: as pointer to _0 element of array\t\t: '%p'\n",&0[letters]);
    printf("\n");
    printf("(`letters` + 1)\t\t: as pointer to array's _1 element\t\t: '%p'\n",letters+1);
    printf("(1 + `letters`)\t\t: as pointer to array's _1 element\t\t: '%p'\n",1+letters);
    printf("(1 + `addresses`)\t: as pointer to array's _1 element\t\t: '%p'\n",1+addresses);
    printf("&`letters`[1]\t\t: as pointer to array's _1 element\t\t: '%p'\n",&letters[1]);
    printf("&1[`letters`]\t\t: as pointer to _1 element of array\t\t: '%p'\n",&1[letters]);
    printf("\n");
    printf("`letters`[0] \t\t: as char array's _0 element \t\t\t: '%c'\n",letters[0]);
    printf("[0]`letters` \t\t: as _0 element of char array \t\t\t: '%c'\n",letters[0]);
    printf("`addresses`[0] \t\t: as char array's _0 element \t\t\t: '%c'\n",addresses[0]);
    printf("[0]`addresses` \t\t: as _0 element of char array \t\t\t: '%c'\n",addresses[0]);
    printf("*`letters` \t\t: as dereferenced pointer to a char array \t: '%c'\n",*letters);
    printf("*`addresses` \t\t: as dereferenced pointer to a char array \t: '%c'\n",*addresses);
    printf("*(int *)'letters'\t: as dereferenced pointer to a char array \t: '%c'\n",*(int *)letters);
    printf("\n");
    printf("*(`letters` + 1)\t: as dereferenced pointer to a char array's _1 element\t: '%c'\n",*(letters+1));
    printf("*(1 + `letters`)\t: as dereferenced pointer to a char array's _1 element\t: '%c'\n",*(1+letters));
    printf("*(&(*(1 + `letters`)))\t: ... arbitrarily deep nesting ... \t\t\t: '%c'\n",*(&(*(1+letters))));

}
