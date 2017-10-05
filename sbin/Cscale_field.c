#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <getopt.h>

#define LEN_STR 80
#define N_RANGE 10

FILE *fp;

int main(argc, argv)
int argc;
char *argv[];
{
   int c, n_read;
   extern char *optarg;
   extern int optind;

   char tempfile[LEN_STR], s_pid[LEN_STR], command[LEN_STR];
   char *outfile, *outname, *outunit, *outlong, *infile;
   char *null;
   char *test;
  
   int n, i;
   float p1[N_RANGE], p2[N_RANGE], factor[N_RANGE];

   n=0;
   while ((c=getopt(argc, argv, "R:o:n:u:L:")) != EOF)
      switch(c){
         case 'R':
            n_read=sscanf(strtok(optarg,","), "%f", &p1[n]); 
            if (n_read!=1) {
               printf("Error in specifying range: EXIT.\n");
               exit(1);
            }
            n_read=sscanf(strtok(NULL,":"), "%f", &p2[n]);
            if (n_read!=1) {
               printf("Error in specifying range: EXIT.\n");
               exit(1);
            }
            n_read=sscanf(strtok(NULL,","), "%f", &factor[n++]);
            if (n_read!=1) {
               printf("Error in specifying range: EXIT.\n");
               exit(1);
            }
            break;
         case 'o':
            outfile=optarg;
            break;
         case 'n':
            outname=optarg;
            break;
         case 'u':
            outunit=optarg;
            break;
         case 'L':
            outlong=optarg;
            break;
         default:
            printf("Unknown option: EXIT.\n");
            exit(-1);
         }

   for( ; optind<argc; optind++) infile=argv[optind];

   sprintf(tempfile, "/tmp/scf.");
   sprintf(s_pid, "%d", getpid());
   strcat(tempfile, s_pid);
   fp=fopen(tempfile, "w");
   fprintf(fp, "%s\n", infile);
   fprintf(fp, "%g %g\n", p1[0], p2[0]);
   fprintf(fp, "%g\n", factor[0]);
   for (i=1; i<n; i++) {
      fprintf(fp, "y\n");
      fprintf(fp, "%g %g\n", p1[i], p2[i]);
      fprintf(fp, "%g\n", factor[i]);
   }
   fprintf(fp, "n\n");
   fprintf(fp, "%s\n", outfile);
   fprintf(fp, "%s\n", outname);
   fprintf(fp, "%s\n", outunit);
   fprintf(fp, "%s\n", outlong);
   fclose(fp);

   sprintf(command, "( scale_field < ");
   strcat(command, tempfile);
   strcat(command, " ) > /dev/null");
   system(command);
   sprintf(command, "rm -f ");
   strcat(command, tempfile);
   system(command);

}
