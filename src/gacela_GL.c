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
#include <GL/gl.h>
#include <GL/glu.h>
#include "gacela_GL.h"

SCM
gacela_glBegin (SCM mode)
{
  glBegin (scm_to_int (mode));
  return SCM_UNSPECIFIED;
}


void*
GL_register_functions (void* data)
{
  // Data types
  scm_c_define ("GL_UNSIGNED_BYTE", scm_from_int (GL_UNSIGNED_BYTE));

  // Primitives
  scm_c_define ("GL_POINTS", scm_from_int (GL_POINTS));
  scm_c_define ("GL_LINES", scm_from_int (GL_LINES));
  scm_c_define ("GL_LINE_LOOP", scm_from_int (GL_LINE_LOOP));
  scm_c_define ("GL_LINE_STRIP", scm_from_int (GL_LINE_STRIP));
  scm_c_define ("GL_TRIANGLES", scm_from_int (GL_TRIANGLES));
  scm_c_define ("GL_TRIANGLE_STRIP", scm_from_int (GL_TRIANGLE_STRIP));
  scm_c_define ("GL_TRIANGLE_FAN", scm_from_int (GL_TRIANGLE_FAN));
  scm_c_define ("GL_QUADS", scm_from_int (GL_QUADS));
  scm_c_define ("GL_QUAD_STRIP", scm_from_int (GL_QUAD_STRIP));
  scm_c_define ("GL_POLYGON", scm_from_int (GL_POLYGON));

  // Matrix Mode
  scm_c_define ("GL_MODELVIEW", scm_from_int (GL_MODELVIEW));
  scm_c_define ("GL_PROJECTION", scm_from_int (GL_PROJECTION));

  // Depth buffer
  scm_c_define ("GL_LEQUAL", scm_from_int (GL_LEQUAL));
  scm_c_define ("GL_DEPTH_TEST", scm_from_int (GL_DEPTH_TEST));

  // Lighting
  scm_c_define ("GL_LIGHTING", scm_from_int (GL_LIGHTING));
  scm_c_define ("GL_LIGHT1", scm_from_int (GL_LIGHT1));
  scm_c_define ("GL_AMBIENT", scm_from_int (GL_AMBIENT));
  scm_c_define ("GL_DIFFUSE", scm_from_int (GL_DIFFUSE));
  scm_c_define ("GL_POSITION", scm_from_int (GL_POSITION));
  scm_c_define ("GL_SMOOTH", scm_from_int (GL_SMOOTH));

  // Blending
  scm_c_define ("GL_BLEND", scm_from_int (GL_BLEND));
  scm_c_define ("GL_ONE", scm_from_int (GL_ONE));
  scm_c_define ("GL_SRC_ALPHA", scm_from_int (GL_SRC_ALPHA));

  // Fog
  scm_c_define ("GL_LINEAR", scm_from_int (GL_LINEAR));

  // Buffers, Pixel Drawing/Reading
  scm_c_define ("GL_RGB", scm_from_int (GL_RGB));
  scm_c_define ("GL_RGBA", scm_from_int (GL_RGBA));

  // Hints
  scm_c_define ("GL_PERSPECTIVE_CORRECTION_HINT", scm_from_int (GL_PERSPECTIVE_CORRECTION_HINT));
  scm_c_define ("GL_NICEST", scm_from_int (GL_NICEST));

  // Texture mapping
  scm_c_define ("GL_TEXTURE_2D", scm_from_int (GL_TEXTURE_2D));
  scm_c_define ("GL_TEXTURE_MAG_FILTER", scm_from_int (GL_TEXTURE_MAG_FILTER));
  scm_c_define ("GL_TEXTURE_MIN_FILTER", scm_from_int (GL_TEXTURE_MIN_FILTER));
  scm_c_define ("GL_LINEAR_MIPMAP_NEAREST", scm_from_int (GL_LINEAR_MIPMAP_NEAREST));
  scm_c_define ("GL_NEAREST", scm_from_int (GL_NEAREST));

  // glPush/PopAttrib bits
  scm_c_define ("GL_DEPTH_BUFFER_BIT", scm_from_int (GL_DEPTH_BUFFER_BIT));
  scm_c_define ("GL_COLOR_BUFFER_BIT", scm_from_int (GL_COLOR_BUFFER_BIT));

  // OpenGL 1.2
  scm_c_define ("GL_BGR", scm_from_int (GL_BGR));
  scm_c_define ("GL_BGRA", scm_from_int (GL_BGRA));


  scm_c_define_gsubr ("glBegin", 1, 0, 0, gacela_glBegin);

  return NULL;
}
