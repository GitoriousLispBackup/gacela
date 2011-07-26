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

#include <stdio.h>
#include <string.h>
#include <libguile.h>
#include <libgen.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "gacela_SDL.h"
#include "gacela_GL.h"
#include "gacela_FTGL.h"

// Generic variables
int ctrl_c = 0;

static int
find_matching_paren (int k)
{
  register int i;
  register char c = 0;
  int end_parens_found = 0;

  // Choose the corresponding opening bracket
  if (k == ')') c = '(';
  else if (k == ']') c = '[';
  else if (k == '}') c = '{';

  for (i = rl_point-2; i >= 0; i--)
    {
      // Is the current character part of a character literal?
      if (i - 2 >= 0
 	  && rl_line_buffer[i - 1] == '\\'
 	  && rl_line_buffer[i - 2] == '#')
 	;
      else if (rl_line_buffer[i] == k)
 	end_parens_found++;
      else if (rl_line_buffer[i] == '"')
 	{
 	  // Skip over a string literal
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

  // Did we just insert a quoted paren?  If so, then don't bounce
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
  ctrl_c = 1;
}
     
static void
init_gacela_client ()
{
  struct sigaction new_action;

  // init bouncing parens
  rl_bind_key (')', match_paren);
  rl_bind_key (']', match_paren);
  rl_bind_key ('}', match_paren);

  // SIGINT
  new_action.sa_handler = ctrl_c_handler;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = 0;

  sigaction (SIGINT, &new_action, NULL);
}

void
gacela_client (SCM rec_channel, SCM send_channel)
{
  int n;
  SCM buffer;
  char *line;
  char *history_path;

  // Command line
  asprintf (&history_path, "%s/.gacela_history", getenv("HOME"));

  init_gacela_client ();
  read_history (history_path);

  while (1) {
    line = readline ("gacela> ");
    ctrl_c = 0;
    if (!line) break;
    if (line && *line)
      {
	add_history (line);
	scm_write (scm_from_locale_string ("("), send_channel);
	scm_write (scm_from_locale_string (line), send_channel);
	scm_write (scm_from_locale_string (")"), send_channel);

	while (scm_char_ready_p (rec_channel) == SCM_BOOL_F) {
	  if (ctrl_c) break;
	  sleep (1);
	}
	if (ctrl_c)
	  ctrl_c = 0;
	else {
	  buffer = scm_read_line (rec_channel);
	  printf ("%s\n", scm_to_locale_string (buffer));
	}
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

void
start_single (char *working_path)
{
  char *argv = "guile";

  scm_with_guile (&init_gacela, NULL);
  load_scheme_files (working_path);
  scm_shell (1, &argv);
}

void
start_server (char *working_path, int port)
{
  char start_server[100];

  scm_with_guile (&init_gacela, NULL);
  load_scheme_files (working_path);
  sprintf (start_server, "(start-server %d)", port);
  scm_c_eval_string (start_server);
}

void
start_local_server (char *working_path, SCM pipes)
{
  char start_server[100];

  scm_with_guile (&init_gacela, NULL);
  load_scheme_files (working_path);
  scm_c_define ("pipes", pipes);
  scm_c_eval_string ("(start-server pipes)");
}
/*
void
start_remote_client (char *hostname, int port)
{
  int sockfd;
  struct hostent *server;
  struct sockaddr_in serv_addr;

  // Connect to the server
  sockfd = socket (AF_INET, SOCK_STREAM, 0);
  server = gethostbyname (hostname);
  bzero ((char *) &serv_addr, sizeof (serv_addr));
  serv_addr.sin_family = AF_INET;
  bcopy ((char *)server->h_addr, (char *)&serv_addr.sin_addr.s_addr, server->h_length);
  serv_addr.sin_port = htons (port);
  if (connect (sockfd, (struct sockaddr *) &serv_addr, sizeof (serv_addr)) == -1) {
    printf ("%s [%d.%d.%d.%d] %d: Connection refused\n", hostname, server->h_addr[0], server->h_addr[1], server->h_addr[2], server->h_addr[3], port);
  }
  else {
    gacela_client (sockfd, sockfd);
    close (sockfd);
  }
}
*/
int
main (int argc, char *argv[])
{
  char *arg;
  int mode = 0;   // shell: 1, server: 2, client: 3
  char *host;
  int port = 0;
  int i;
  SCM pipes;
  int pid;

  char *string = "prueba\n";
  int p[2];
  char buffer[150];

  // Checking arguments
  for (i = 1; i < argc; i++) {
    if (strcmp (argv[i], "--shell-mode") == 0)
      mode = 1;
    else if (strncmp (argv[i], "--server", 8) == 0) {
      mode = 2;
      arg = strtok (argv[i], "=");
      arg = strtok (NULL, "=");
      if (arg != NULL)
	port = atoi (arg);
    }
    else if (strncmp (argv[i], "--client", 8) == 0) {
      mode = 3;
      arg = strtok (argv[i], "=");
      arg = strtok (NULL, "=");
      if (arg != NULL) {
	host = strtok (arg, ":");
	arg = strtok (NULL, ":");
	if (arg != NULL)
	  port = atoi (arg);
      }
    }
  }

  scm_init_guile ();

  if (mode == 1)
    start_single (dirname (argv[0]));
  else if (mode == 2 && port != 0)
    start_server (dirname (argv[0]), port);
  else if (mode == 3 && port != 0)
    //start_remote_client (host, port);
    return;
  else {
    /*
    pipes = scm_pipe ();
    pid = fork ();

    if (pid == 0)
      start_local_server (dirname (argv[0]), pipes);
    else
      gacela_client (SCM_CAR (pipes), SCM_CDR (pipes));
    */
    /*
    if (pid == 0) {
      scm_write (scm_from_locale_string ("prueba"), SCM_CDR (pipes));
      sleep (10);
    }
    else {
      while (scm_char_ready_p (SCM_CAR (pipes)) == SCM_BOOL_F) {
	sleep (1);
	printf ("1\n");
      }
      SCM buffer;
      buffer = scm_read_line (SCM_CAR (pipes));
      printf ("%s\n", scm_to_locale_string (buffer));
    }
    */
    FILE *stream;
    pipe (p);
    if (pid == 0) {
      close (p[1]);
      stream = fdopen (p[0], "w");
      fprintf (stream, "prueba\n");
      fclose (stream);
      printf ("1\n");
      sleep (10);
    }
    else {
      int c;
      close (p[0]);
      stream = fdopen (p[1], "r");
      while ((c = fgetc (stream)) != EOF)
	putchar (c);
    }
  }
}
