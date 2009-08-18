#define max(a, b) ((a > b) ? a : b)
#define min(a, b) ((a < b) ? a : b)

int
gacela_SDL_SurfaceFormat (int surface)
{
  SDL_Surface *s = surface;

  return s->format;
}

void
gacela_SDL_BlitSurface (int src, int srcrect, int dst, int dstrect)
{
  SDL_BlitSurface (src, srcrect, dst, dstrect);
}

int
gacela_SDL_Rect (int x, int y, int w, int h)
{
  SDL_Rect *rect;

  rect = (SDL_Rect *)malloc (sizeof (SDL_Rect));
  rect->x = x;
  rect->y = y;
  rect->w = w;
  rect->h = h;

  return rect;
}

int
gacela_TTF_Init (void)
{
  return TTF_Init ();
}

int
gacela_TTF_OpenFont (char *file, int ptsize)
{
  return TTF_OpenFont (file, ptsize);
}

void
gacela_TTF_CloseFont (int font)
{
  TTF_CloseFont (font);
}

void
gacela_TTF_Quit (void)
{
  TTF_Quit ();
}

int
gacela_Mix_OpenAudio (int frequency, int channels, int chunksize)
{
  return Mix_OpenAudio (frequency, MIX_DEFAULT_FORMAT, channels, chunksize);
}

int
gacela_Mix_LoadMUS (char *filename)
{
  return Mix_LoadMUS (filename);
}

int
gacela_Mix_LoadWAV (char *filename)
{
  return Mix_LoadWAV (filename);
}

int
gacela_Mix_PlayChannel (int channel, int chunk, int loops)
{
  return Mix_PlayChannel (channel, chunk, loops);
}

int
gacela_Mix_PlayMusic (int music, int loops)
{
  return Mix_PlayMusic (music, loops);
}

int
gacela_Mix_PlayingMusic (void)
{
  return Mix_PlayingMusic ();
}

int
gacela_Mix_PausedMusic (void)
{
  return Mix_PausedMusic ();
}

void
gacela_Mix_PauseMusic (void)
{
  Mix_PauseMusic ();
}

void
gacela_Mix_ResumeMusic (void)
{
  Mix_ResumeMusic ();
}

int
gacela_Mix_HaltMusic (void)
{
  return Mix_HaltMusic ();
}

void
gacela_Mix_FreeChunk (int chunk)
{
  Mix_FreeChunk (chunk);
}

void
gacela_Mix_FreeMusic (int music)
{
  Mix_FreeMusic (music);
}

void
gacela_Mix_CloseAudio (void)
{
  Mix_CloseAudio ();
}

void
gacela_sge_FilledCircle (int surface, int x, int y, int r, int red, int green, int blue)
{
  SDL_Surface *s = surface;

  sge_FilledCircle (s, x, y, r, SDL_MapRGB (s->format, red, green, blue));
}

void
gacela_sge_FilledRect (int surface, int x1, int y1, int x2, int y2, int red, int green, int blue)
{
  SDL_Surface *s = surface;

  sge_FilledRect (s, x1, y1, x2, y2, SDL_MapRGB (s->format, red, green, blue));
}

void
gacela_free (int pointer)
{
  free (pointer);
}

void
apply_surface (int x, int y, int source, int destination, \
	       int cx, int cy, int cw, int ch, int cid)
{
  SDL_Rect offset;
  SDL_Rect *clip = NULL;
  SDL_Surface *tmps = source;
  int tmpw, tmpx, tmpy;

  if (cw != 0 || ch != 0)
    {
      clip = (SDL_Rect *)malloc(sizeof(SDL_Rect));
      if (cid == 0)
	{
	  clip->x = cx;
	  clip->y = cy;
	}
      else
	{
	  tmpw = tmps->w / cw;
	  if (tmps->w % cw > 0) tmpw++;
	  tmpy = cid / tmpw;
	  tmpx = cid - tmpw*tmpy;

	  if (tmpx * cw > tmps->w || tmpy * ch > tmps->h)
	    {
	      clip->x = 0;
	      clip->y = 0;
	    }
	  else
	    {
	      clip->x = tmpx * cw;
	      clip->y = tmpy * ch;
	    }
	  printf ("Id: %d cx: %d cy: %d\n", cid, clip->x, clip->y);
	}
      clip->w = cw;
      clip->h = ch;
    }

  offset.x = x;
  offset.y = y;
  SDL_BlitSurface (source, clip, destination, &offset);
  free(clip);
}

int
render_text (int font, char *text, int red, int green, int blue)
{
  SDL_Color textColor = {red, green, blue};
  return TTF_RenderText_Solid (font, text, textColor); 
}

int
load_image (char *filename, int red, int green, int blue)
{
  SDL_Surface *loadedImage = NULL;
  SDL_Surface *optimizedImage = NULL;

  loadedImage = IMG_Load (filename);
  if (loadedImage != NULL)
    {
      optimizedImage = SDL_DisplayFormat (loadedImage);
      SDL_FreeSurface (loadedImage);
      if (optimizedImage != NULL)
	{
	  SDL_SetColorKey (optimizedImage, SDL_SRCCOLORKEY, SDL_MapRGB (optimizedImage->format, red, green, blue));
	}
    }
  return optimizedImage;
}

void
fill_surface (int surface, int red, int green, int blue)
{
  SDL_Surface *s = surface;

  SDL_FillRect (s, &s->clip_rect, SDL_MapRGB (s->format, red, green, blue));
}

int
box_collision (int surface1, int x1, int y1, int surface2, int x2, int y2)
{
  SDL_Surface *s1 = surface1;
  SDL_Surface *s2 = surface2;
  int left1, left2, bottom1, bottom2;
  int xstart, xend, ystart, yend;
  int x, y;
  
  left1 = x1 + s1->w - 1;
  bottom1 = y1 + s1->h - 1;
  left2 = x2 + s2->w - 1;
  bottom2 = y2 + s2->h - 1;
  
  if ((x1 > left2) || (x2 > left1)) return 0;
  if ((y1 > bottom2) || (y2 > bottom1)) return 0;
  return 1;
}

int
transparent_pixel (SDL_Surface *surface, int x, int y)
{
  int bpp = surface->format->BytesPerPixel;
  Uint8 *p;
  Uint32 pixelcolor;

  if (SDL_MUSTLOCK (surface)) SDL_LockSurface (surface);
  assert ((x < surface->w) && (y < surface->h));

  p = (Uint8 *)surface->pixels + y*surface->pitch + x*bpp;

  switch (bpp)
    {
      case (1):
	pixelcolor = *p;
	break;

      case (2):
	pixelcolor = *(Uint16 *)p;
	break;

      case (3):
	if (SDL_BYTEORDER == SDL_BIG_ENDIAN)
	  pixelcolor = p[0] << 16 | p[1] << 8 | p[2];
	else
	  pixelcolor = p[0] | p[1] << 8 | p[2] << 16;
	break;

      case (4):
	pixelcolor = *(Uint32 *)p;
	break;
    }

  if (SDL_MUSTLOCK (surface)) SDL_UnlockSurface (surface);

  return (pixelcolor == surface->format->colorkey);
}

int
create_SDL_Surface (int screen, int w, int h, int red, int green, int blue)
{
  SDL_Surface *s = screen;
  SDL_Surface *new = NULL;

  new = SDL_CreateRGBSurface (s->flags, w, h, \
			      s->format->BitsPerPixel, \
			      s->format->Rmask, s->format->Gmask, \
			      s->format->Bmask, s->format->Amask);
  if (new != NULL)
    {
      SDL_SetColorKey (new, SDL_SRCCOLORKEY, SDL_MapRGB (new->format, red, green, blue));
    }

  return new;
}

int
copy_SDL_Surface (int surface)
{
  SDL_Surface *s = surface;
  
  return SDL_ConvertSurface (s, s->format, s->flags);
}
