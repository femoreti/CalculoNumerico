#define _XOPEN_SOURCE 500
#include <stdio.h>
#include <stdlib.h>
#include "lib.h"
#include <math.h>

void eulerFloat()
{
    FILE *gnu = popen("gnuplot -persistent", "w");
    fprintf(gnu, "set terminal png\n");
    fprintf(gnu, "set output 'euler_flt.png'\n");
    fprintf(gnu, "set logscale x\n");

    fprintf(gnu, "plot '-' w l\n");
    
    float euler = 0.0;

    int i;
    for(i = 1; i <= 20; i++) //float
    {
        float n = powf(10, i);
        euler = powf((1 + 1 / n), n);

        fprintf(gnu, "%f %f\n", n, euler);
    }
    fprintf(gnu, "e");

    pclose(gnu);
}

void eulerDouble()
{
    FILE *gnu = popen("gnuplot -persistent", "w");
    fprintf(gnu, "set terminal png\n");
    fprintf(gnu, "set output 'euler_dbl.png'\n");
    fprintf(gnu, "set logscale x\n");

    fprintf(gnu, "plot '-' w l\n");
    
    double euler = 0.0;

    int i;
    for(i = 1; i <= 20; i++) //double
    {
        double n = pow(10, i);
        euler = pow((1 + 1 / n), n);

        fprintf(gnu, "%lf %lf\n", n, euler);
    }
    fprintf(gnu, "e");

    pclose(gnu);
}

void eulerLongDouble()
{
    FILE *gnu = popen("gnuplot -persistent", "w");
    fprintf(gnu, "set terminal png\n");
    fprintf(gnu, "set output 'euler_ldbl.png'\n");
    fprintf(gnu, "set logscale x\n");

    fprintf(gnu, "plot '-' w l\n");
    
    long double euler = 0.0;

    int i;
    for(i = 1; i <= 20; i++) //float
    {
        long double n = powl(10, i);
        euler = powl((1 + 1 / n), n);

        fprintf(gnu, "%LF %LF\n", n, euler);
    }
    fprintf(gnu, "e");

    pclose(gnu);
}