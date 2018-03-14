#include <stdio.h>
#include <string.h>
#include <math.h>
#include "lib.h"

int ConvertToDecimal(char c) 
{
    if (c >= '0' && c <= '9')
        return (int) c - '0';
    else 
        return (int) c - 'A' + 10;
}

int ConvertToBase10(char *NumberToConvert, int currentBase) 
{
    int i = 0, decimalValue = 0;
    for (i = 0; i < strlen(NumberToConvert); i++)
        decimalValue += ConvertToDecimal(NumberToConvert[strlen(NumberToConvert) - 1 - i]) * pow(currentBase, i);

    return decimalValue;
}