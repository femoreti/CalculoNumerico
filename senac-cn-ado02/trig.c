#define _XOPEN_SOURCE 500

#include <stdio.h>
#include <stdlib.h>
#include "lib.h"
#include <math.h>

int main() 
{
    int i = 0;

    FILE *fp;
    fp = fopen("seno.dat", "w");
    fp = fopen("seno.dat", "a");
    for(i = 0; i <= 720; i ++)
    {
        double rad = i*(PI/180);
        double absolutError = fabs(sin(rad) - seno(rad));
        double relativeError = fabs(absolutError / seno(rad));
        fprintf(fp, "%d\t%f\t%f\t%f\n", i, seno(rad), absolutError, relativeError);
    }
    fclose(fp);

    fp = fopen("cosseno.dat", "w");
    fp = fopen("cosseno.dat", "a");
    for(i = 0; i <= 720; i ++)
    {
        double rad = i*(PI/180);
        double absolutError = fabs(cos(rad) - cosseno(rad));
        double relativeError = fabs(absolutError / cosseno(rad));
        fprintf(fp, "%d\t%f\t%f\t%f\n", i, cosseno(rad), absolutError, relativeError);
    }

    fclose(fp);

    FILE *gnuplot = popen("gnuplot -persistent", "w");
    fprintf(gnuplot, "%s\n%s", "set terminal png size 400,300\nset output 'seno.png'", "plot 'seno.dat'\n");
    fprintf(gnuplot, "%s\n%s", "set terminal png size 400,300\nset output 'cosseno.png'", "plot 'cosseno.dat'");

    exit(0);
}
