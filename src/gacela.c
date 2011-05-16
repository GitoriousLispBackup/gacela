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
#include "gacela_SDL.h"
#include "gacela_GL.h"

static void*
register_functions (void* data)
{
  SDL_register_functions (NULL);
  GL_register_functions (NULL);
  return NULL;
}

void
load_scheme_files (char *path)
{
  load_scheme_file (path, "gacela.scm");
}

void
load_scheme_file (char *path, char *filename)
{
  char fn[strlen (path) + 1024];

  strcpy (fn, path);
  strcat (fn, "/");
  strcat (fn, filename);

  scm_c_primitive_load (fn);
}

int
main (int argc, char *argv[])
{
  scm_with_guile (&register_functions, NULL);
  scm_init_guile ();
  scm_c_eval_string ("(set-repl-prompt! \"gacela>\")");
  scm_c_eval_string ("(use-modules (ice-9 readline))");
  scm_c_eval_string ("(activate-readline)");
  scm_c_eval_string ("(use-modules (ice-9 optargs))");
  load_scheme_files (dirname (argv[0]));
  scm_shell (argc, argv);
}
