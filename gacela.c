#include <stdio.h>
#include <readline/readline.h>
#include <sys/types.h>
#include <unistd.h>

/* Read-Send-Print-Loop */
void rspl(int pin, int pout)
{
  static char *line = (char *)NULL;
  int exit = 0;

  while (!exit) {
    if (line) {
      free(line);
      line = (char *)NULL;
    }
    
    line = readline("gacela>");
    
    if (line && *line) {
      add_history(line);
      if (strcmp(line, "(quit)") == 0)
	exit = 1;
      else {
	write(pout, line, strlen(line));
	write(pout, "\n", 1);
      }
    }
  }
}

int main (int argc, char *argv[])
{
  pid_t cpid;
  int pfd[2];

  pipe(pfd);
  cpid = fork();
  if (cpid != 0) {
    rspl(pfd[0], pfd[1]);
    return 0;
  }
  else {
    char buf;

    while (1) {
      while (read(pfd[0], &buf, 1) > 0) {
	if (buf == '\n')
	  write(STDOUT_FILENO, "-\n", 2);
	else
	  write(STDOUT_FILENO, &buf, 1);
      }
    }
  }
}
