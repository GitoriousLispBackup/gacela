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


struct glTexture
{
  GLuint texture_id;
  int width, height;
};

static scm_t_bits glTexture_tag;

SCM
make_glTexture (GLuint texture_id)
{
  SCM smob;
  struct glTexture *glTexture;

  glTexture = (struct glTexture *) scm_gc_malloc (sizeof (struct glTexture), "glTexture");

  glTexture->texture_id = 0;

  SCM_NEWSMOB (smob, glTexture_tag, glTexture);

  glTexture->texture_id = texture_id;

  return smob;
}

GLuint
get_glTexture_id (SCM glTexture_smob)
{
  struct glTexture *glTexture;

  scm_assert_smob_type (glTexture_tag, glTexture_smob);
  glTexture = (struct glTexture *) SCM_SMOB_DATA (glTexture_smob);
  return glTexture->texture_id;
}

SCM
get_glTexture_width (SCM glTexture_smob)
{
  struct glTexture *glTexture;

  scm_assert_smob_type (glTexture_tag, glTexture_smob);
  glTexture = (struct glTexture *) SCM_SMOB_DATA (glTexture_smob);
  return scm_from_int (glTexture->width);
}

SCM
get_glTexture_height (SCM glTexture_smob)
{
  struct glTexture *glTexture;

  scm_assert_smob_type (glTexture_tag, glTexture_smob);
  glTexture = (struct glTexture *) SCM_SMOB_DATA (glTexture_smob);
  return scm_from_int (glTexture->height);
}

size_t
free_glTexture (SCM glTexture_smob)
{
  struct glTexture *glTexture = (struct glTexture *) SCM_SMOB_DATA (glTexture_smob);
  GLuint text[1];

  text[0] = glTexture->texture_id;
  glDeleteTextures (1, &text[0]);
  scm_gc_free (glTexture, sizeof (struct glTexture), "glTexture");

  return 0;
}


SCM
gacela_glBegin (SCM mode)
{
  glBegin (scm_to_int (mode));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glClear (SCM mask)
{
  glClear (scm_to_int (mask));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glClearColor (SCM red, SCM green, SCM blue, SCM alpha)
{
  glClearColor (scm_to_double (red), scm_to_double (green), scm_to_double (blue), scm_to_double (alpha));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glClearDepth (SCM depth)
{
  glClearDepth (scm_to_double (depth));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glColor3f (SCM red, SCM green, SCM blue)
{
  glColor3f (scm_to_double (red), scm_to_double (green), scm_to_double (blue));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glColor4f (SCM red, SCM green, SCM blue, SCM alpha)
{
  glColor4f (scm_to_double (red), scm_to_double (green), scm_to_double (blue), scm_to_double (alpha));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glDepthFunc (SCM func)
{
  glDepthFunc (scm_to_int (func));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glEnable (SCM cap)
{
  glEnable (scm_to_int (cap));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glDisable (SCM cap)
{
  glDisable (scm_to_int (cap));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glEnd (void)
{
  glEnd ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_glHint (SCM target, SCM mode)
{
  glHint (scm_to_int (target), scm_to_int (mode));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glLoadIdentity (void)
{
  glLoadIdentity ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_glMatrixMode (SCM mode)
{
  glMatrixMode (scm_to_int (mode));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glRotatef (SCM angle, SCM x, SCM y, SCM z)
{
  glRotatef (scm_to_double (angle), scm_to_double (x), scm_to_double (y), scm_to_double (z));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glShadeModel (SCM mode)
{
  glShadeModel (scm_to_int (mode));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glTranslatef (SCM x, SCM y, SCM z)
{
  glTranslatef (scm_to_double (x), scm_to_double (y), scm_to_double (z));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glVertex2f (SCM x, SCM y)
{
  glVertex2f (scm_to_double (x), scm_to_double (y));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glVertex3f (SCM x, SCM y, SCM z)
{
  glVertex3f (scm_to_double (x), scm_to_double (y), scm_to_double (z));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glViewport (SCM x, SCM y, SCM width, SCM height)
{
  glViewport (scm_to_int (x), scm_to_int (y), scm_to_int (width), scm_to_int (height));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glGenTextures (SCM n)
{
  SCM textures;
  int nint = scm_to_int (n);
  GLuint text[nint];
  int i;

  textures = scm_list_n (SCM_UNDEFINED);
  glGenTextures (nint, &text[0]);

  for (i = nint - 1; i >= 0; i--) {
    textures = scm_cons (scm_from_int (text[i]), textures);
  }

  return textures;
}

SCM
gacela_glDeleteTextures (SCM n, SCM textures)
{
  int nint = scm_to_int (n);
  GLuint text[nint];
  int i;

  for (i = 0; i < nint; i++) {
    text[i] = scm_to_int (scm_list_ref (textures, scm_from_int (i)));
  }

  glDeleteTextures (nint, &text[0]);
  return SCM_UNSPECIFIED;
}

SCM
gacela_glBindTexture (SCM target, SCM texture)
{
  glBindTexture (scm_to_int (target), get_glTexture_id (texture));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glTexImage2D (SCM target, SCM level, SCM internalFormat, SCM width, SCM height, SCM border, SCM format, SCM type, SCM pixels)
{
  glTexImage2D (scm_to_int (target), scm_to_int (level), scm_to_int (internalFormat), scm_to_int (width), \
		scm_to_int (height), scm_to_int (border), scm_to_int (format), scm_to_int (type), \
		(GLvoid *)scm_to_int (pixels));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glTexParameteri (SCM target, SCM pname, SCM param)
{
  glTexParameteri (scm_to_int (target), scm_to_int (pname), scm_to_int (param));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glTexCoord2f (SCM s, SCM t)
{
  glTexCoord2f (scm_to_double (s), scm_to_double (t));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glLightfv (SCM light, SCM pname, SCM params)
{
  int n = scm_to_int (scm_length (params));
  GLfloat gl_params[n];
  int i;

  for (i = 0; i < n; i++) {
    gl_params[i] = scm_to_double (scm_list_ref (params, scm_from_int (i)));
  }

  glLightfv (scm_to_int (light), scm_to_int (pname), gl_params);
  return SCM_UNSPECIFIED;
}

SCM
gacela_glNormal3f (SCM nx, SCM ny, SCM nz)
{
  glNormal3f (scm_to_double (nx), scm_to_double (ny), scm_to_double (nz));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glBlendFunc (SCM sfactor, SCM dfactor)
{
  glBlendFunc (scm_to_int (sfactor), scm_to_int (dfactor));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glOrtho (SCM left, SCM right, SCM bottom, SCM top, SCM near_val, SCM far_val)
{
  glOrtho (scm_to_double (left), scm_to_double (right), scm_to_double (bottom), scm_to_double (top), \
	   scm_to_double (near_val), scm_to_double (far_val));
  return SCM_UNSPECIFIED;
}

SCM
gacela_glPushMatrix (void)
{
  glPushMatrix ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_glPopMatrix (void)
{
  glPopMatrix ();
  return SCM_UNSPECIFIED;
}

SCM
gacela_gluPerspective (SCM fovy, SCM aspect, SCM zNear, SCM zFar)
{
  gluPerspective (scm_to_double (fovy), scm_to_double (aspect), scm_to_double (zNear), scm_to_double (zFar));
  return SCM_UNSPECIFIED;
}

SCM
gacela_gluBuild2DMipmaps (SCM target, SCM internalFormat, SCM width, SCM height, SCM format, SCM type, SCM data)
{
  return scm_from_int (gluBuild2DMipmaps (scm_to_int (target), scm_to_int (internalFormat), scm_to_int (width), \
					  scm_to_int (height), scm_to_int (format), scm_to_int (type), \
					  (void *)scm_to_int (data)));
}

SCM
gacela_gluLookAt (SCM eyeX, SCM eyeY, SCM eyeZ, SCM centerX, SCM centerY, SCM centerZ, SCM upX, SCM upY, SCM upZ)
{
  gluLookAt (scm_to_double (eyeX), scm_to_double (eyeY), scm_to_double (eyeZ), \
	     scm_to_double (centerX), scm_to_double (centerY), scm_to_double (centerZ), \
	     scm_to_double (upX), scm_to_double (upY), scm_to_double (upZ));
  return SCM_UNSPECIFIED;
}


void*
GL_register_functions (void* data)
{
  glTexture_tag = scm_make_smob_type ("glTexture", sizeof (struct glTexture));
  scm_set_smob_free (glTexture_tag, free_glTexture);
  scm_c_define_gsubr ("texture-w", 1, 0, 0, get_glTexture_width);
  scm_c_define_gsubr ("texture-h", 1, 0, 0, get_glTexture_height);

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
  scm_c_define_gsubr ("glClear", 1, 0, 0, gacela_glClear);
  scm_c_define_gsubr ("glClearColor", 4, 0, 0, gacela_glClearColor);
  scm_c_define_gsubr ("glClearDepth", 1, 0, 0, gacela_glClearDepth);
  scm_c_define_gsubr ("glColor3f", 3, 0, 0, gacela_glColor3f);
  scm_c_define_gsubr ("glColor4f", 4, 0, 0, gacela_glColor4f);
  scm_c_define_gsubr ("glDepthFunc", 1, 0, 0, gacela_glDepthFunc);
  scm_c_define_gsubr ("glEnable", 1, 0, 0, gacela_glEnable);
  scm_c_define_gsubr ("glDisable", 1, 0, 0, gacela_glDisable);
  scm_c_define_gsubr ("glEnd", 0, 0, 0, gacela_glEnd);
  scm_c_define_gsubr ("glHint", 2, 0, 0, gacela_glHint);
  scm_c_define_gsubr ("glLoadIdentity", 0, 0, 0, gacela_glLoadIdentity);
  scm_c_define_gsubr ("glMatrixMode", 1, 0, 0, gacela_glMatrixMode);
  scm_c_define_gsubr ("glRotatef", 4, 0, 0, gacela_glRotatef);
  scm_c_define_gsubr ("glShadeModel", 1, 0, 0, gacela_glShadeModel);
  scm_c_define_gsubr ("glTranslatef", 3, 0, 0, gacela_glTranslatef);
  scm_c_define_gsubr ("glVertex2f", 2, 0, 0, gacela_glVertex2f);
  scm_c_define_gsubr ("glVertex3f", 3, 0, 0, gacela_glVertex3f);
  scm_c_define_gsubr ("glViewport", 4, 0, 0, gacela_glViewport);
  scm_c_define_gsubr ("glGenTextures", 1, 0, 0, gacela_glGenTextures);
  scm_c_define_gsubr ("glDeleteTextures", 2, 0, 0, gacela_glDeleteTextures);
  scm_c_define_gsubr ("glBindTexture", 2, 0, 0, gacela_glBindTexture);
  scm_c_define_gsubr ("glTexImage2D", 9, 0, 0, gacela_glTexImage2D);
  scm_c_define_gsubr ("glTexParameteri", 3, 0, 0, gacela_glTexParameteri);
  scm_c_define_gsubr ("glTexCoord2f", 2, 0, 0, gacela_glTexCoord2f);
  scm_c_define_gsubr ("glLightfv", 3, 0, 0, gacela_glLightfv);
  scm_c_define_gsubr ("glNormal3f", 3, 0, 0, gacela_glNormal3f);
  scm_c_define_gsubr ("glBlendFunc", 2, 0, 0, gacela_glBlendFunc);
  scm_c_define_gsubr ("glOrtho", 6, 0, 0, gacela_glOrtho);
  scm_c_define_gsubr ("glPushMatrix", 0, 0, 0, gacela_glPushMatrix);
  scm_c_define_gsubr ("glPopMatrix", 0, 0, 0, gacela_glPopMatrix);

  scm_c_define_gsubr ("gluPerspective", 4, 0, 0, gacela_gluPerspective);
  scm_c_define_gsubr ("gluBuild2DMipmaps", 7, 0, 0, gacela_gluBuild2DMipmaps);
  scm_c_define_gsubr ("gluLookAt", 9, 0, 0, gacela_gluLookAt);

  return NULL;
}
