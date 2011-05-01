#include <libguile.h>
#include "gacela_SDL.h"

static void*
register_functions (void* data)
{
  SDL_register_functions (NULL);
  return NULL;
}

int
main (int argc, char *argv[])
{
  scm_with_guile (&register_functions, NULL);
  scm_init_guile ();
  scm_c_eval_string ("(set-repl-prompt! \"gacela>\")");
  scm_c_eval_string ("(use-modules (ice-9 readline))");
  scm_c_eval_string ("(activate-readline)");
  scm_shell (argc, argv);
}
