#include <libguile.h>
#include <SDL/SDL.h>
#include "gacela_SDL.h"

SCM
gacela_SDL_Init (SCM flags)
{
  return scm_int2num (SDL_Init (scm_num2int (flags, 0, "SDL_Init")));
}

SCM
gacela_SDL_Quit ()
{
  SDL_Quit ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_SDL_SetVideoMode (SCM width, SCM height, SCM bpp, SCM flags)
{
}

void*
SDL_register_functions (void* data)
{
  scm_c_define ("SDL_INIT_EVERYTHING", scm_int2num(SDL_INIT_EVERYTHING));
  scm_c_define_gsubr ("SDL_Init", 1, 0, 0, gacela_SDL_Init);
  scm_c_define_gsubr ("SDL_Quit", 0, 0, 0, gacela_SDL_Quit);
  return NULL;
}
