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
#include <FTGL/ftgl.h>

struct font
{
  SCM filename;
  FTGLfont *font_address;
  int size;
};

static scm_t_bits font_tag;

SCM
make_font (SCM file, SCM size, FTGLfont *font_address)
{
  SCM smob;
  struct font *font;

  font = (struct font *) scm_gc_malloc (sizeof (struct font), "font");

  font->filename = SCM_BOOL_F;
  font->size = scm_to_int (size);
  font->font_address = NULL;

  SCM_NEWSMOB (smob, font_tag, font);

  font->filename = file;
  font->font_address = font_address;

  return smob;
}

FTGLfont *
get_font_address (SCM font_smob)
{
  struct font *font;

  scm_assert_smob_type (font_tag, font_smob);
  font = (struct font *) SCM_SMOB_DATA (font_smob);
  return font->font_address;
}

SCM
mark_font (SCM font_smob)
{
  struct font *font = (struct font *) SCM_SMOB_DATA (font_smob);

  scm_gc_mark (font->filename);
     
  return SCM_BOOL_F;
}

size_t
free_font (SCM font_smob)
{
  struct font *font = (struct font *) SCM_SMOB_DATA (font_smob);

  ftglDestroyFont (font->font_address);
  scm_gc_free (font, sizeof (struct font), "font");

  return 0;
}

static int
print_font (SCM font_smob, SCM port, scm_print_state *pstate)
{
  struct font *font = (struct font *) SCM_SMOB_DATA (font_smob);

  scm_puts ("#<font \"", port);
  scm_display (font->filename, port);
  scm_puts ("\", size ", port);
  scm_display (scm_from_int (font->size), port);
  scm_puts (">", port);

  /* non-zero means success */
  return 1;
}


SCM
gacela_ftglCreateTextureFont (SCM file, SCM size)
{
  FTGLfont *font_address = ftglCreateTextureFont (scm_to_locale_string (file));

  if (font_address) {
    return make_font (file, size, font_address);
  }
  else {
    return SCM_BOOL_F;
  }
}

SCM
gacela_ftglSetFontFaceSize (SCM font, SCM size, SCM res)
{
  return scm_from_int (ftglSetFontFaceSize (get_font_address (font), scm_to_int (size), scm_to_int (res)));
}

SCM
gacela_ftglSetFontCharMap (SCM font, SCM encoding)
{
  return scm_from_int (ftglSetFontCharMap (get_font_address (font), scm_to_int (encoding)));
}

SCM
gacela_ftglRenderFont (SCM font, SCM string, SCM mode)
{
  ftglRenderFont (get_font_address (font), scm_to_locale_string(string), scm_to_int (mode));
  return SCM_UNSPECIFIED;
}


void
init_gacela_ftgl (void *data)
{
  font_tag = scm_make_smob_type ("font", sizeof (struct font));
  scm_set_smob_mark (font_tag, mark_font);
  scm_set_smob_free (font_tag, free_font);
  scm_set_smob_print (font_tag, print_font);
  //  scm_set_smob_equalp (surface_tag, equalp_surface);

  scm_c_define ("ft_encoding_unicode", scm_from_int (ft_encoding_unicode));
  scm_c_define ("FTGL_RENDER_ALL", scm_from_int (FTGL_RENDER_ALL));

  scm_c_define_gsubr ("ftglCreateTextureFont", 2, 0, 0, gacela_ftglCreateTextureFont);
  scm_c_define_gsubr ("ftglSetFontFaceSize", 3, 0, 0, gacela_ftglSetFontFaceSize);
  scm_c_define_gsubr ("ftglSetFontCharMap", 2, 0, 0, gacela_ftglSetFontCharMap);
  scm_c_define_gsubr ("ftglRenderFont", 3, 0, 0, gacela_ftglRenderFont);
}


void
scm_init_gacela_ftgl ()
{
  init_gacela_ftgl (NULL);
}
