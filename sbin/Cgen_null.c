#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

#define I_LINEAR 0
#define I_LOG 1
#define LEN_STR 80
#define N_POINTS 1000

FILE *fp;

int main(argc, argv)
int argc;
char *argv[];
{
   extern char *optarg;
   extern int optind;
  
   char tempfile[LEN_STR], s_pid[LEN_STR], command[LEN_STR];
   char *outfile, *unit;
   char *null;
  
   int c, i;
   int division;
   int np;
   double p1, p2, p[N_POINTS];
   int nlon;
   double lon1, lon2, lon[N_POINTS];
   int nlat;
   double lat1, lat2, lat[N_POINTS];
     
   void fill();

   np=0;
   nlon=0;
   nlat=0;

   while ((c=getopt(argc, argv, "u:n:g:o:N:T")) != EOF)
      switch(c){
         case 'o':
            outfile=optarg;
            break;
         case 'u':
            unit=optarg;
            break;
         case 'n':
            sscanf(strtok(optarg,","), "%lf", &p1);
            sscanf(strtok(null,":"), "%lf", &p2);
            sscanf(strtok(null,","), "%d", &division);
            fill(&np, p, p1, p2, division, I_LINEAR);
            break;
         case 'g':
            sscanf(strtok(optarg,","), "%lf", &p1);
            sscanf(strtok(null,":"), "%lf", &p2);
            sscanf(strtok(null,","), "%d", &division);
            fill(&np, p, p1, p2, division, I_LOG);
            break;
         case 'N':
            sscanf(strtok(optarg,","), "%lf", &lon1);
            sscanf(strtok(null,":"), "%lf", &lon2);
            sscanf(strtok(null,","), "%d", &division);
            fill(&nlon, lon, lon1, lon2, division, I_LINEAR);
            break;
         case 'T':
            sscanf(strtok(optarg,","), "%lf", &lat1);
            sscanf(strtok(null,":"), "%lf", &lat2);
            sscanf(strtok(null,","), "%d", &division);
            fill(&nlat, lat, lat1, lat2, division, I_LINEAR);
            break;
         default:
            printf("Unknown option: EXIT.\n");
            exit(-1);
         }

   sprintf(tempfile, "/tmp/gnl.");
   sprintf(s_pid, "%d", getpid());
   strcat(tempfile, s_pid);
   fp=fopen(tempfile, "w");
   fprintf(fp, "%s\n", unit);
   fprintf(fp, "%d\n", np);
   for (i=0; i<np; i++) {
      fprintf(fp, "%g ", p[i]);
   }
   fprintf(fp, "\n");
   fprintf(fp, "%d\n", nlon);
   fprintf(fp, "%d\n", nlat);
   for (i=0; i<nlon; i++) {
      fprintf(fp, "%g ", lon[i]);
   }
   fprintf(fp, "\n");
   for (i=0; i<nlat; i++) {
      fprintf(fp, "%g ", lat[i]);
   }
   fprintf(fp, "\n");
   fprintf(fp, "%s\n", outfile);
   fclose(fp);

   sprintf(command, "( gen_null < ");
   strcat(command, tempfile);
   strcat(command, " ) > /dev/null ");
   system(command);
   sprintf(command, "rm -f ");
   strcat(command, tempfile);
   system(command);

}

void fill(n, p, p1, p2, division, mode)
int *n, mode, division;
double p[], p1, p2;
{
   double incr; 
   int i;

   if (mode==I_LINEAR) {
      if (division >= 0 ) incr=(p2-p1)/(double)division;
      if (*n>0) {
         if (p[(*n)-1]<(p1+1.e-4)) p[*n]=p1;
         }
      else {
         p[0]=p1;
         (*n)++;
      }
      for (i=0;i<division;i++) {
         p[*n]=p[(*n)-1]+incr;
         (*n)++;
      }
   }
   else {
      if (mode==I_LOG) {
         incr=exp((1./(double)division)*log(p2/p1));
         if (*n>0) {
            if (p[(*n)-1]<(p1+1.e-4)) p[*n]=p1;
            }
         else {
            p[0]=p1;
            (*n)++;
         }
         for (i=0;i<division;i++) {
            p[*n]=p[(*n)-1]*incr;
            (*n)++;
         }
      }
      else {
         printf("Illegal mode.\n");
         exit(-1);
      }
   }
}

