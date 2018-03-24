#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lib.h"

double seno(double x)
{
    double result = 0;
    int n = 0;
    for(n = 0; n < 16; n++)
    {
        result += (MakePow(-1, n) * MakePow(x, 2*n + 1)) / fat(2*n + 1);
    }

    return result;
}

double cosseno(double x)
{
    double result = 0;
    int n = 0;
    for(n = 0; n < 16; n++)
    {
        result += (MakePow(-1, n) * MakePow(x, 2*n)) / fat(2*n);
    }

    return result;
}

double MakePow(double a, double b)
{
    int i = 0;
    double newValue = 1;
    while(i < b)
    {
        newValue *= a;
        i++;
    }

    return newValue;
}

double fat(int n)
{
    int currentN = n;
    double result = 1;

    while(currentN > 0)
    {
        result *= currentN;
        currentN--;
    }

    return result;
}