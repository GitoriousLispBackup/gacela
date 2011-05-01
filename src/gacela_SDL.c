#include <libguile.h>
#include <SDL/SDL.h>
#include <GL/gl.h>
#include "gacela_SDL.h"

SCM
gacela_SDL_Init (SCM flags)
{
  return scm_make_integer (SDL_Init (scm_num2int (flags, "SDL_Init")));
}

void*
SDL_register_functions (void* data)
{
  scm_c_define_gsubr ("SDL_Init", 1, 0, 0, &gacela_SDL_Init);
  return NULL;
}
