#include <stdio.h>
#include <stdlib.h>
#include "lib.h"

int main(int argc, char *argv[]) 
{
    if(argc != 4)
    {
        printf("Numero de argumentos invalidos\n");
        exit(1);
    }

    char alphabet[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    char* enterValue = argv[1];
    long int initialValue = atoi(enterValue);

    if(atoi(enterValue) == 0)
    {
        printf("0\n");
        exit(0);
    }
    int baseN = atoi(argv[2]);
    int baseM = atoi(argv[3]);

    long int limit = MakePow(2, 32);
    if(initialValue < 0 || initialValue > limit)
    {
        exit(1);
    }
    if(baseN < 2 || baseN > 36)
    {
        exit(1);
    }
    if(baseM < 2 || baseM > 36)
    {
        exit(1);
    }

    int valueInDecimal = ConvertToBase10(enterValue, baseN);
    ConvertToBaseM(valueInDecimal, baseM, alphabet);

    exit(0);
}