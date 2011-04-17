#include <stdio.h>
#include <readline/readline.h>

/* Read-Send-Print-Loop */
void rspl ()
{
  static char *line = (char *)NULL;
  int exit = 0;

  while (!exit)
    {
      if (line)
	{
	  free (line);
	  line = (char *)NULL;
	}
      
      line = readline ("gacela>");

      if (line && *line)
	{
	  add_history (line);
	  if (strcmp (line, "(quit)") == 0)
	    exit = 1;
	}
    }
}

int main (int argc, char *argv[])
{
  rspl ();
  return 0;
}
