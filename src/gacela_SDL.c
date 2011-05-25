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
#include <SDL/SDL.h>
#include <SDL/SDL_events.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_mixer.h>
#include "gacela_SDL.h"

struct surface
{
  SCM filename;
  SDL_Surface *surface_address;
};

static scm_t_bits surface_tag;

SCM
make_surface (SCM file, SDL_Surface *surface_address)
{
  SCM smob;
  struct surface *surface;

  surface = (struct surface *) scm_gc_malloc (sizeof (struct surface), "surface");

  surface->filename = SCM_BOOL_F;
  surface->surface_address = NULL;

  SCM_NEWSMOB (smob, surface_tag, surface);

  surface->filename = file;
  surface->surface_address = surface_address;

  return smob;
}

SDL_Surface *
get_surface_address (SCM surface_smob)
{
  struct surface *surface;

  scm_assert_smob_type (surface_tag, surface_smob);
  surface = (struct surface *) SCM_SMOB_DATA (surface_smob);
  return surface->surface_address;
}

SCM
get_surface_width (SCM surface_smob)
{
  SDL_Surface *surface = get_surface_address (surface_smob);

  return scm_from_int (surface->w);
}

SCM
get_surface_height (SCM surface_smob)
{
  SDL_Surface *surface = get_surface_address (surface_smob);

  return scm_from_int (surface->h);
}

SCM
mark_surface (SCM surface_smob)
{
  struct surface *surface = (struct surface *) SCM_SMOB_DATA (surface_smob);

  scm_gc_mark (surface->filename);
     
  return SCM_BOOL_F;
}

size_t
free_surface (SCM surface_smob)
{
  struct surface *surface = (struct surface *) SCM_SMOB_DATA (surface_smob);

  SDL_FreeSurface (surface->surface_address);
  scm_gc_free (surface, sizeof (struct surface), "surface");

  return 0;
}

static int
print_surface (SCM surface_smob, SCM port, scm_print_state *pstate)
{
  struct surface *surface = (struct surface *) SCM_SMOB_DATA (surface_smob);

  scm_puts ("#<surface \"", port);
  scm_display (surface->filename, port);
  scm_puts ("\">", port);

  /* non-zero means success */
  return 1;
}


SCM
gacela_SDL_Init (SCM flags)
{
  return scm_from_int (SDL_Init (scm_to_int (flags)));
}

SCM
gacela_SDL_Quit (void)
{
  SDL_Quit ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_SDL_SetVideoMode (SCM width, SCM height, SCM bpp, SCM flags)
{
  SDL_Surface *screen = SDL_SetVideoMode (scm_to_int (width), scm_to_int (height), \
					  scm_to_int (bpp), scm_to_int (flags));

  if (screen) {
    return make_surface (scm_from_locale_string ("screen"), screen);
  }
  else {
    return SCM_BOOL_F;
  }
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
  return scm_from_int (SDL_Flip (get_surface_address (screen)));
}

SCM
gacela_SDL_Delay (SCM ms)
{
  SDL_Delay ((int)scm_to_double (ms));
  return SCM_UNSPECIFIED;
}

SCM
gacela_SDL_GetTicks (void)
{
  return scm_from_int (SDL_GetTicks ());
}

SCM
gacela_SDL_DisplayFormat (SCM surface)
{
  return scm_from_int ((int)SDL_DisplayFormat (get_surface_address (surface)));
}

SCM
gacela_SDL_MapRGB (SCM format, SCM r, SCM g, SCM b)
{
  return scm_from_int (SDL_MapRGB ((SDL_PixelFormat *)scm_to_int (format), scm_to_int (r), scm_to_int (g), scm_to_int (b)));
}

SCM
gacela_SDL_SetColorKey (SCM surface, SCM flag, SCM key)
{
  return scm_from_int (SDL_SetColorKey (get_surface_address (surface), scm_to_int (flag), scm_to_int (key)));
}

SCM
gacela_SDL_LoadBMP (SCM file)
{
  SDL_Surface *image = SDL_LoadBMP (scm_to_locale_string (file));

  if (image) {
    return make_surface (file, image);
  }
  else {
    return SCM_BOOL_F;
  }
}

SCM
gacela_IMG_Load (SCM filename)
{
  SDL_Surface *image = IMG_Load (scm_to_locale_string (filename));

  if (image) {
    return make_surface (filename, image);
  }
  else {
    return SCM_BOOL_F;
  }
}

SCM
gacela_SDL_GetVideoInfo (void)
{
  const SDL_VideoInfo *info;
  SCM vi;

  info = SDL_GetVideoInfo ();
  vi = scm_list_n (SCM_UNDEFINED);

  vi = scm_cons (scm_cons (scm_from_locale_symbol ("blit_hw"), scm_from_int (info->blit_hw)), vi);
  vi = scm_cons (scm_cons (scm_from_locale_symbol ("hw_available"), scm_from_int (info->hw_available)), vi);

  return vi;
}

SCM
gacela_SDL_GL_SetAttribute (SCM attr, SCM value)
{
  return scm_from_int (SDL_GL_SetAttribute (scm_to_int (attr), scm_to_int (value)));
}

SCM
gacela_SDL_PollEvent (void)
{
  SDL_Event sdl_event;
  SCM event;

  event = scm_list_n (SCM_UNDEFINED);

  if (SDL_PollEvent (&sdl_event)) {
    switch (sdl_event.type) {
    case SDL_KEYDOWN:
    case SDL_KEYUP:
      event = scm_cons (scm_cons (scm_from_locale_symbol ("key.keysym.sym"), scm_from_int (sdl_event.key.keysym.sym)), event);
      break;
    }
    event = scm_cons (scm_cons (scm_from_locale_symbol ("type"), scm_from_int (sdl_event.type)), event);
  }

  return event;
}

SCM
gacela_SDL_GL_SwapBuffers (void)
{
  SDL_GL_SwapBuffers ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_SDL_EnableKeyRepeat (SCM delay, SCM interval)
{
  return scm_from_int (SDL_EnableKeyRepeat (scm_to_int (delay), scm_to_int (interval)));
}

SCM
gacela_Mix_OpenAudio (SCM frequency, SCM format, SCM channels, SCM chunksize)
{
  return scm_from_int (Mix_OpenAudio (scm_to_int (frequency), scm_to_int (format), scm_to_int (channels), scm_to_int (chunksize)));
}

SCM
gacela_Mix_LoadMUS (SCM file)
{
  return scm_from_int ((int)Mix_LoadMUS (scm_to_locale_string (file)));
}

SCM
gacela_Mix_LoadWAV (SCM file)
{
  return scm_from_int ((int)Mix_LoadWAV (scm_to_locale_string (file)));
}

SCM
gacela_Mix_PlayChannel (SCM channel, SCM chunk, SCM loops)
{
  return scm_from_int (Mix_PlayChannel (scm_to_int (channel), (Mix_Chunk *)scm_to_int (chunk), scm_to_int (loops)));
}

SCM
gacela_Mix_PlayMusic (SCM music, SCM loops)
{
  return scm_from_int (Mix_PlayMusic ((Mix_Music *)scm_to_int (music), scm_to_int (loops)));
}

SCM
gacela_Mix_PlayingMusic (void)
{
  return scm_from_int (Mix_PlayingMusic ());
}

SCM
gacela_Mix_PausedMusic (void)
{
  return scm_from_int (Mix_PausedMusic ());
}

SCM
gacela_Mix_PauseMusic (void)
{
  Mix_PauseMusic ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_Mix_ResumeMusic (void)
{
  Mix_ResumeMusic ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_Mix_HaltMusic (void)
{
  return scm_from_int (Mix_HaltMusic ());
}

SCM
gacela_Mix_FreeMusic (SCM music)
{
  Mix_FreeMusic ((Mix_Music *)scm_to_int (music));
  return SCM_UNSPECIFIED;
}

SCM
gacela_Mix_FreeChunk (SCM chunk)
{
  Mix_FreeChunk ((Mix_Chunk *)scm_to_int (chunk));
  return SCM_UNSPECIFIED;
}

SCM
gacela_Mix_CloseAudio (void)
{
  Mix_CloseAudio ();
  return SCM_UNSPECIFIED;
}


void*
SDL_register_functions (void* data)
{
  surface_tag = scm_make_smob_type ("surface", sizeof (struct surface));
  scm_set_smob_mark (surface_tag, mark_surface);
  scm_set_smob_free (surface_tag, free_surface);
  scm_set_smob_print (surface_tag, print_surface);
  scm_c_define_gsubr ("surface-w", 1, 0, 0, get_surface_width);
  scm_c_define_gsubr ("surface-h", 1, 0, 0, get_surface_height);

  scm_c_define ("SDL_INIT_TIMER", scm_from_int (SDL_INIT_TIMER));
  scm_c_define ("SDL_INIT_AUDIO", scm_from_int (SDL_INIT_AUDIO));
  scm_c_define ("SDL_INIT_VIDEO", scm_from_int (SDL_INIT_VIDEO));
  scm_c_define ("SDL_INIT_CDROM", scm_from_int (SDL_INIT_CDROM));
  scm_c_define ("SDL_INIT_JOYSTICK", scm_from_int (SDL_INIT_JOYSTICK));
  scm_c_define ("SDL_INIT_NOPARACHUTE", scm_from_int (SDL_INIT_NOPARACHUTE));
  scm_c_define ("SDL_INIT_EVENTTHREAD", scm_from_int (SDL_INIT_EVENTTHREAD));
  scm_c_define ("SDL_INIT_EVERYTHING", scm_from_int (SDL_INIT_EVERYTHING));

  scm_c_define ("SDL_SWSURFACE", scm_from_int (SDL_SWSURFACE));
  scm_c_define ("SDL_HWSURFACE", scm_from_int (SDL_HWSURFACE));
  scm_c_define ("SDL_ASYNCBLIT", scm_from_int (SDL_ASYNCBLIT));

  scm_c_define ("SDL_ANYFORMAT", scm_from_int (SDL_ANYFORMAT));
  scm_c_define ("SDL_HWPALETTE", scm_from_int (SDL_HWPALETTE));
  scm_c_define ("SDL_DOUBLEBUF", scm_from_int (SDL_DOUBLEBUF));
  scm_c_define ("SDL_FULLSCREEN", scm_from_int (SDL_FULLSCREEN));
  scm_c_define ("SDL_OPENGL", scm_from_int (SDL_OPENGL));
  scm_c_define ("SDL_OPENGLBLIT", scm_from_int (SDL_OPENGLBLIT));
  scm_c_define ("SDL_RESIZABLE", scm_from_int (SDL_RESIZABLE));
  scm_c_define ("SDL_NOFRAME", scm_from_int (SDL_NOFRAME));

  scm_c_define ("SDL_HWACCEL", scm_from_int (SDL_HWACCEL));
  scm_c_define ("SDL_SRCCOLORKEY", scm_from_int (SDL_SRCCOLORKEY));

  scm_c_define ("SDL_GL_DOUBLEBUFFER", scm_from_int (SDL_GL_DOUBLEBUFFER));

  scm_c_define ("SDL_DEFAULT_REPEAT_DELAY", scm_from_int (SDL_DEFAULT_REPEAT_DELAY));
  scm_c_define ("SDL_DEFAULT_REPEAT_INTERVAL", scm_from_int (SDL_DEFAULT_REPEAT_INTERVAL));

  scm_c_define ("SDL_LIL_ENDIAN", scm_from_int (SDL_LIL_ENDIAN));
  scm_c_define ("SDL_BIG_ENDIAN", scm_from_int (SDL_BIG_ENDIAN));
  scm_c_define ("SDL_BYTEORDER", scm_from_int (SDL_BYTEORDER));

  scm_c_define ("MIX_DEFAULT_FORMAT", scm_from_int (MIX_DEFAULT_FORMAT));

  scm_c_define ("SDL_NOEVENT", scm_from_int (SDL_NOEVENT));
  scm_c_define ("SDL_ACTIVEEVENT", scm_from_int (SDL_ACTIVEEVENT));
  scm_c_define ("SDL_KEYDOWN", scm_from_int (SDL_KEYDOWN));
  scm_c_define ("SDL_KEYUP", scm_from_int (SDL_KEYUP));
  scm_c_define ("SDL_MOUSEMOTION", scm_from_int (SDL_MOUSEMOTION));
  scm_c_define ("SDL_MOUSEBUTTONDOWN", scm_from_int (SDL_MOUSEBUTTONDOWN));
  scm_c_define ("SDL_MOUSEBUTTONUP", scm_from_int (SDL_MOUSEBUTTONUP));
  scm_c_define ("SDL_JOYAXISMOTION", scm_from_int (SDL_JOYAXISMOTION));
  scm_c_define ("SDL_JOYBALLMOTION", scm_from_int (SDL_JOYBALLMOTION));
  scm_c_define ("SDL_JOYHATMOTION", scm_from_int (SDL_JOYHATMOTION));
  scm_c_define ("SDL_JOYBUTTONDOWN", scm_from_int (SDL_JOYBUTTONDOWN));
  scm_c_define ("SDL_JOYBUTTONUP", scm_from_int (SDL_JOYBUTTONUP));
  scm_c_define ("SDL_QUIT", scm_from_int (SDL_QUIT));
  scm_c_define ("SDL_SYSWMEVENT", scm_from_int (SDL_SYSWMEVENT));
  scm_c_define ("SDL_EVENT_RESERVEDA", scm_from_int (SDL_EVENT_RESERVEDA));
  scm_c_define ("SDL_EVENT_RESERVEDB", scm_from_int (SDL_EVENT_RESERVEDB));
  scm_c_define ("SDL_VIDEORESIZE", scm_from_int (SDL_VIDEORESIZE));
  scm_c_define ("SDL_VIDEOEXPOSE", scm_from_int (SDL_VIDEOEXPOSE));
  scm_c_define ("SDL_EVENT_RESERVED2", scm_from_int (SDL_EVENT_RESERVED2));
  scm_c_define ("SDL_EVENT_RESERVED3", scm_from_int (SDL_EVENT_RESERVED3));
  scm_c_define ("SDL_EVENT_RESERVED4", scm_from_int (SDL_EVENT_RESERVED4));
  scm_c_define ("SDL_EVENT_RESERVED5", scm_from_int (SDL_EVENT_RESERVED5));
  scm_c_define ("SDL_EVENT_RESERVED6", scm_from_int (SDL_EVENT_RESERVED6));
  scm_c_define ("SDL_EVENT_RESERVED7", scm_from_int (SDL_EVENT_RESERVED7));
  scm_c_define ("SDL_USEREVENT", scm_from_int (SDL_USEREVENT));
  scm_c_define ("SDL_NUMEVENTS", scm_from_int (SDL_NUMEVENTS));

  scm_c_define_gsubr ("SDL_Init", 1, 0, 0, gacela_SDL_Init);
  scm_c_define_gsubr ("SDL_Quit", 0, 0, 0, gacela_SDL_Quit);
  scm_c_define_gsubr ("SDL_SetVideoMode", 4, 0, 0, gacela_SDL_SetVideoMode);
  scm_c_define_gsubr ("SDL_WM_SetCaption", 2, 0, 0, gacela_SDL_WM_SetCaption);
  scm_c_define_gsubr ("SDL_Flip", 1, 0, 0, gacela_SDL_Flip);
  scm_c_define_gsubr ("SDL_Delay", 1, 0, 0, gacela_SDL_Delay);
  scm_c_define_gsubr ("SDL_GetTicks", 0, 0, 0, gacela_SDL_GetTicks);
  scm_c_define_gsubr ("SDL_DisplayFormat", 1, 0, 0, gacela_SDL_DisplayFormat);
  scm_c_define_gsubr ("SDL_MapRGB", 4, 0, 0, gacela_SDL_MapRGB);
  scm_c_define_gsubr ("SDL_SetColorKey", 3, 0, 0, gacela_SDL_SetColorKey);
  scm_c_define_gsubr ("SDL_LoadBMP", 1, 0, 0, gacela_SDL_LoadBMP);
  scm_c_define_gsubr ("IMG_Load", 1, 0, 0, gacela_IMG_Load);
  scm_c_define_gsubr ("SDL_GetVideoInfo", 0, 0, 0, gacela_SDL_GetVideoInfo);
  scm_c_define_gsubr ("SDL_GL_SetAttribute", 2, 0, 0, gacela_SDL_GL_SetAttribute);
  scm_c_define_gsubr ("SDL_PollEvent", 0, 0, 0, gacela_SDL_PollEvent);
  scm_c_define_gsubr ("SDL_GL_SwapBuffers", 0, 0, 0, gacela_SDL_GL_SwapBuffers);
  scm_c_define_gsubr ("SDL_EnableKeyRepeat", 2, 0, 0, gacela_SDL_EnableKeyRepeat);
  scm_c_define_gsubr ("Mix_OpenAudio", 4, 0, 0, gacela_Mix_OpenAudio);
  scm_c_define_gsubr ("Mix_LoadMUS", 1, 0, 0, gacela_Mix_LoadMUS);
  scm_c_define_gsubr ("Mix_LoadWAV", 1, 0, 0, gacela_Mix_LoadWAV);
  scm_c_define_gsubr ("Mix_PlayChannel", 3, 0, 0, gacela_Mix_PlayChannel);
  scm_c_define_gsubr ("Mix_PlayMusic", 2, 0, 0, gacela_Mix_PlayMusic);
  scm_c_define_gsubr ("Mix_PlayingMusic", 0, 0, 0, gacela_Mix_PlayingMusic);
  scm_c_define_gsubr ("Mix_PausedMusic", 0, 0, 0, gacela_Mix_PausedMusic);
  scm_c_define_gsubr ("Mix_PauseMusic", 0, 0, 0, gacela_Mix_PauseMusic);
  scm_c_define_gsubr ("Mix_ResumeMusic", 0, 0, 0, gacela_Mix_ResumeMusic);
  scm_c_define_gsubr ("Mix_HaltMusic", 0, 0, 0, gacela_Mix_HaltMusic);
  scm_c_define_gsubr ("Mix_FreeMusic", 1, 0, 0, gacela_Mix_FreeMusic);
  scm_c_define_gsubr ("Mix_FreeChunk", 1, 0, 0, gacela_Mix_FreeChunk);
  scm_c_define_gsubr ("Mix_CloseAudio", 0, 0, 0, gacela_Mix_CloseAudio);

  return NULL;
}
