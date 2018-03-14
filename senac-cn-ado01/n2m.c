#include <stdio.h>
#include <stdlib.h>
#include "lib.h"

int main(int argc, char *argv[]) 
{
    char* enterValue = argv[1];
    int baseN = atoi(argv[2]);
    int test = ConvertToBase10(enterValue, baseN);
    printf("%d\n", test);
}