#include <stdarg.h>
#include <stdio.h>

int main(argc, argv)
int argc;
char *argv[];
{
   double re, ne, alpha, rm, beta;
 
   /* Program to read Effective Radius (in microns) and Variance 
      in Hansen's form and to convert them to the form adopted 
      for the modified gamma distribution. */

   sscanf(argv[1], "%lf", &re);
   sscanf(argv[2], "%lf", &ne);
   alpha=1.0/ne-2.0;
   beta=1.0;
   rm=ne*re*1.e-6;

   printf("   %.7e   %.7e   %.7e\n", alpha, rm, beta);
}
