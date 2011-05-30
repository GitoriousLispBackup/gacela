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
#include "gacela_FTGL.h"

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

int
main (int argc, char *argv[])
{
  scm_with_guile (&init_gacela, NULL);
  load_scheme_files (dirname (argv[0]));
  scm_shell (argc, argv);
}
