/* Gacela, a GNU Guile extension for fast games development
   Copyright (C) 2009 by Javier Sancho Fernandez <jsf at jsancho dot org>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <libguile.h>
#include <libgen.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <signal.h>
#include "gacela_SDL.h"
#include "gacela_GL.h"
#include "gacela_FTGL.h"


static int
find_matching_paren (int k)
{
  register int i;
  register char c = 0;
  int end_parens_found = 0;

  /* Choose the corresponding opening bracket.  */
  if (k == ')') c = '(';
  else if (k == ']') c = '[';
  else if (k == '}') c = '{';

  for (i = rl_point-2; i >= 0; i--)
    {
      /* Is the current character part of a character literal?  */
      if (i - 2 >= 0
 	  && rl_line_buffer[i - 1] == '\\'
 	  && rl_line_buffer[i - 2] == '#')
 	;
      else if (rl_line_buffer[i] == k)
 	end_parens_found++;
      else if (rl_line_buffer[i] == '"')
 	{
 	  /* Skip over a string literal.  */
 	  for (i--; i >= 0; i--)
 	    if (rl_line_buffer[i] == '"'
 		&& ! (i - 1 >= 0
 		      && rl_line_buffer[i - 1] == '\\'))
 	      break;
 	}
      else if (rl_line_buffer[i] == c)
 	{
 	  if (end_parens_found==0) return i;
 	  else --end_parens_found;
 	}
    }
  return -1;
}

static int
match_paren (int x, int k)
{
  int tmp;
  int fno;
  SELECT_TYPE readset;
  struct timeval timeout;

  rl_insert (x, k);

  /* Did we just insert a quoted paren?  If so, then don't bounce.  */
  if (rl_point - 1 >= 1
      && rl_line_buffer[rl_point - 2] == '\\')
    return 0;

  tmp = 500000;
  timeout.tv_sec = tmp / 1000000;
  timeout.tv_usec = tmp % 1000000;
  FD_ZERO (&readset);
  fno = fileno (rl_instream);
  FD_SET (fno, &readset);

  if (rl_point > 1)
    {
      tmp = rl_point;
      rl_point = find_matching_paren (k);
      if (rl_point > -1)
        {
          rl_redisplay ();
          scm_std_select (fno + 1, &readset, NULL, NULL, &timeout);
        }
      rl_point = tmp;
    }
  return 0;
}

void
ctrl_c_handler (int signum)
{
  printf ("ERROR: User interrupt\nABORT: (signal)\n");
}
     
static void
init_gacela_client ()
{
  struct sigaction new_action;

  /* init bouncing parens */
  rl_bind_key (')', match_paren);
  rl_bind_key (']', match_paren);
  rl_bind_key ('}', match_paren);

  /* SIGINT */
  new_action.sa_handler = ctrl_c_handler;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = 0;

  sigaction (SIGINT, &new_action, NULL);
}

void
gacela_client (void)
{
  char *line;
  char *history_path;

  asprintf (&history_path, "%s/.gacela_history", getenv("HOME"));

  init_gacela_client ();
  read_history (history_path);

  while (1) {
    line = readline ("gacela> ");
    if (!line) break;
    if (line && *line)
      {
	printf ("%s\n", line);
	add_history (line);
      }
    free (line);
  }

  write_history (history_path);
  free (history_path);
}

static void*
init_gacela (void *data, int argc, char **argv)
{
  // Guile configuration
  scm_c_eval_string ("(set-repl-prompt! \"gacela>\")");
  scm_c_eval_string ("(use-modules (ice-9 readline))");
  scm_c_eval_string ("(activate-readline)");
  scm_c_eval_string ("(use-modules (ice-9 optargs))");
  scm_c_eval_string ("(use-modules (ice-9 receive))");
  //  scm_c_eval_string ("(read-enable 'case-insensitive)");

  // Bindings for C functions and structs
  SDL_register_functions (NULL);
  GL_register_functions (NULL);
  FTGL_register_functions (NULL);

  return NULL;
}

void
load_scheme_files (char *path)
{
  char load_path[strlen (path) + 1024];

  sprintf (load_path, "(set! %%load-path (cons \"%s\" %%load-path))", path);
  scm_c_eval_string (load_path);
  scm_primitive_load_path (scm_from_locale_string ("gacela_loader.scm"));
}

int
main (int argc, char *argv[])
{
  if (fork () == 0) {
    scm_with_guile (&init_gacela, NULL);
    load_scheme_files (dirname (argv[0]));
    //scm_shell (argc, argv);
    scm_c_eval_string ("(start-server 1234)");
    scm_c_eval_string ("(run-game)");
  }
  else {
    scm_init_guile ();
    gacela_client ();
  }
}
