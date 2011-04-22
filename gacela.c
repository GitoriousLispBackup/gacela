#include <stdio.h>
#include <readline/readline.h>
#include <sys/types.h>
#include <unistd.h>
#include <libguile.h>

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
    static char *line = (char *)NULL;

    dup2(pfd[0], 0);
    close(pfd[0]);

    while (1) {
      scm_init_guile();
      scm_c_eval_string("(if (char-ready? (current-input-port)) (format #t \"~a guile~%\" (read)))");
      /*
      if (line) {
	free(line);
	line = (char *)NULL;
      }

      line = readline("");
      if (line && *line) {
	printf("%s-\n", line);
      }
      */
    }
  }
}
