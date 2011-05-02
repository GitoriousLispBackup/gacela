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
  scm_c_define ("SDL_INIT_TIMER", scm_int2num(SDL_INIT_TIMER));
  scm_c_define ("SDL_INIT_AUDIO", scm_int2num(SDL_INIT_AUDIO));
  scm_c_define ("SDL_INIT_VIDEO", scm_int2num(SDL_INIT_VIDEO));
  scm_c_define ("SDL_INIT_CDROM", scm_int2num(SDL_INIT_CDROM));
  scm_c_define ("SDL_INIT_JOYSTICK", scm_int2num(SDL_INIT_JOYSTICK));
  scm_c_define ("SDL_INIT_NOPARACHUTE", scm_int2num(SDL_INIT_NOPARACHUTE));
  scm_c_define ("SDL_INIT_EVENTTHREAD", scm_int2num(SDL_INIT_EVENTTHREAD));
  scm_c_define ("SDL_INIT_EVERYTHING", scm_int2num(SDL_INIT_EVERYTHING));

  scm_c_define ("SDL_SWSURFACE", scm_int2num(SDL_SWSURFACE));
  scm_c_define ("SDL_HWSURFACE", scm_int2num(SDL_HWSURFACE));
  scm_c_define ("SDL_ASYNCBLIT", scm_int2num(SDL_ASYNCBLIT));

  scm_c_define ("SDL_ANYFORMAT", scm_int2num(SDL_ANYFORMAT));
  scm_c_define ("SDL_HWPALETTE", scm_int2num(SDL_HWPALETTE));
  scm_c_define ("SDL_DOUBLEBUF", scm_int2num(SDL_DOUBLEBUF));
  scm_c_define ("SDL_FULLSCREEN", scm_int2num(SDL_FULLSCREEN));
  scm_c_define ("SDL_OPENGL", scm_int2num(SDL_OPENGL));
  scm_c_define ("SDL_OPENGLBLIT", scm_int2num(SDL_OPENGLBLIT));
  scm_c_define ("SDL_RESIZABLE", scm_int2num(SDL_RESIZABLE));

  scm_c_define_gsubr ("SDL_Init", 1, 0, 0, gacela_SDL_Init);
  scm_c_define_gsubr ("SDL_Quit", 0, 0, 0, gacela_SDL_Quit);
  return NULL;
}
