#include <libguile.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_mixer.h>
#include "gacela_SDL.h"

SCM
gacela_SDL_Init (SCM flags)
{
  return scm_from_int (SDL_Init (scm_to_int (flags)));
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
  return scm_from_int ((int)SDL_SetVideoMode (scm_to_int (width), scm_to_int (height), \
					      scm_to_int (bpp), scm_to_int (flags)));
}

SCM
gacela_SDL_WM_SetCaption (SCM title, SCM icon)
{
  SDL_WM_SetCaption (scm_to_locale_string(title), scm_to_locale_string(icon));
  return SCM_UNSPECIFIED;
}

SCM
gacela_SDL_Flip (SCM screen)
{
  return scm_from_int (SDL_Flip ((SDL_Surface *)scm_to_int (screen)));
}

SCM
gacela_SDL_FreeSurface (SCM surface)
{
  SDL_FreeSurface ((SDL_Surface *)scm_to_int (surface));
  return SCM_UNSPECIFIED;
}

SCM
gacela_SDL_Delay (SCM ms)
{
  SDL_Delay (scm_to_int (ms));
  return SCM_UNSPECIFIED;
}

SCM
gacela_SDL_GetTicks ()
{
  return scm_from_int (SDL_GetTicks ());
}

SCM
gacela_SDL_DisplayFormat (SCM surface)
{
  return scm_from_int ((int)SDL_DisplayFormat ((SDL_Surface *)scm_to_int (surface)));
}

SCM
gacela_SDL_MapRGB (SCM format, SCM r, SCM g, SCM b)
{
  return scm_from_int (SDL_MapRGB ((SDL_PixelFormat *)scm_to_int (format), scm_to_int (r), scm_to_int (g), scm_to_int (b)));
}

SCM
gacela_SDL_SetColorKey (SCM surface, SCM flag, SCM key)
{
  return scm_from_int (SDL_SetColorKey ((SDL_Surface *)scm_to_int (surface), scm_to_int (flag), scm_to_int (key)));
}

SCM
gacela_SDL_LoadBMP (SCM file)
{
  return scm_from_int ((int)SDL_LoadBMP (scm_to_locale_string (file)));
}

SCM
gacela_IMG_Load (SCM filename)
{
  return scm_from_int ((int)IMG_Load (scm_to_locale_string (filename)));
}

SCM
gacela_SDL_GetVideoInfo ()
{
  const SDL_VideoInfo *info;
  SCM vi;

  info = SDL_GetVideoInfo ();
  ('nil vi)
  ((cons (int info->blit_hw) vi) vi) (':blit_hw label) ((cons label vi) vi)
  ((cons (int info->hw_available) vi) vi) (':hw_available label) ((cons label vi) vi)
  "return vi;")

void*
SDL_register_functions (void* data)
{
  scm_c_define ("SDL_INIT_TIMER", scm_from_int(SDL_INIT_TIMER));
  scm_c_define ("SDL_INIT_AUDIO", scm_from_int(SDL_INIT_AUDIO));
  scm_c_define ("SDL_INIT_VIDEO", scm_from_int(SDL_INIT_VIDEO));
  scm_c_define ("SDL_INIT_CDROM", scm_from_int(SDL_INIT_CDROM));
  scm_c_define ("SDL_INIT_JOYSTICK", scm_from_int(SDL_INIT_JOYSTICK));
  scm_c_define ("SDL_INIT_NOPARACHUTE", scm_from_int(SDL_INIT_NOPARACHUTE));
  scm_c_define ("SDL_INIT_EVENTTHREAD", scm_from_int(SDL_INIT_EVENTTHREAD));
  scm_c_define ("SDL_INIT_EVERYTHING", scm_from_int(SDL_INIT_EVERYTHING));

  scm_c_define ("SDL_SWSURFACE", scm_from_int(SDL_SWSURFACE));
  scm_c_define ("SDL_HWSURFACE", scm_from_int(SDL_HWSURFACE));
  scm_c_define ("SDL_ASYNCBLIT", scm_from_int(SDL_ASYNCBLIT));

  scm_c_define ("SDL_ANYFORMAT", scm_from_int(SDL_ANYFORMAT));
  scm_c_define ("SDL_HWPALETTE", scm_from_int(SDL_HWPALETTE));
  scm_c_define ("SDL_DOUBLEBUF", scm_from_int(SDL_DOUBLEBUF));
  scm_c_define ("SDL_FULLSCREEN", scm_from_int(SDL_FULLSCREEN));
  scm_c_define ("SDL_OPENGL", scm_from_int(SDL_OPENGL));
  scm_c_define ("SDL_OPENGLBLIT", scm_from_int(SDL_OPENGLBLIT));
  scm_c_define ("SDL_RESIZABLE", scm_from_int(SDL_RESIZABLE));
  scm_c_define ("SDL_NOFRAME", scm_from_int(SDL_NOFRAME));

  scm_c_define ("SDL_HWACCEL", scm_from_int(SDL_HWACCEL));
  scm_c_define ("SDL_SRCCOLORKEY", scm_from_int(SDL_SRCCOLORKEY));

  scm_c_define ("SDL_GL_DOUBLEBUFFER", scm_from_int(SDL_GL_DOUBLEBUFFER));

  scm_c_define ("SDL_DEFAULT_REPEAT_DELAY", scm_from_int(SDL_DEFAULT_REPEAT_DELAY));
  scm_c_define ("SDL_DEFAULT_REPEAT_INTERVAL", scm_from_int(SDL_DEFAULT_REPEAT_INTERVAL));

  scm_c_define ("SDL_LIL_ENDIAN", scm_from_int(SDL_LIL_ENDIAN));
  scm_c_define ("SDL_BIG_ENDIAN", scm_from_int(SDL_BIG_ENDIAN));

  scm_c_define ("MIX_DEFAULT_FORMAT", scm_from_int(MIX_DEFAULT_FORMAT));

  scm_c_define_gsubr ("SDL_Init", 1, 0, 0, gacela_SDL_Init);
  scm_c_define_gsubr ("SDL_Quit", 0, 0, 0, gacela_SDL_Quit);
  scm_c_define_gsubr ("SDL_SetVideoMode", 4, 0, 0, gacela_SDL_SetVideoMode);
  scm_c_define_gsubr ("SDL_WM_SetCaption", 2, 0, 0, gacela_SDL_WM_SetCaption);
  scm_c_define_gsubr ("SDL_Flip", 1, 0, 0, gacela_SDL_Flip);
  scm_c_define_gsubr ("SDL_FreeSurface", 1, 0, 0, gacela_SDL_FreeSurface);
  scm_c_define_gsubr ("SDL_Delay", 1, 0, 0, gacela_SDL_Delay);
  scm_c_define_gsubr ("SDL_GetTicks", 0, 0, 0, gacela_SDL_GetTicks);
  scm_c_define_gsubr ("SDL_DisplayFormat", 1, 0, 0, gacela_SDL_DisplayFormat);
  scm_c_define_gsubr ("SDL_MapRGB", 4, 0, 0, gacela_SDL_MapRGB);
  scm_c_define_gsubr ("SDL_SetColorKey", 3, 0, 0, gacela_SDL_SetColorKey);
  scm_c_define_gsubr ("SDL_LoadBMP", 1, 0, 0, gacela_SDL_LoadBMP);
  scm_c_define_gsubr ("IMG_Load", 1, 0, 0, gacela_IMG_Load);

  return NULL;
}
