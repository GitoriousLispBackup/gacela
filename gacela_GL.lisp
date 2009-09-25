;;; Gacela, a GNU Common Lisp extension for fast games development
;;; Copyright (C) 2009 by Javier Sancho Fernandez <jsf at jsancho dot org>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package :gacela)

(clines "#include <GL/gl.h>")
(clines "#include <GL/glu.h>")

;;; Data types
(mapcconst int "int" "GL_UNSIGNED_BYTE")

;;; Primitives
(mapcconst int "int" "GL_POINTS")
(mapcconst int "int" "GL_LINES")
(mapcconst int "int" "GL_LINE_LOOP")
(mapcconst int "int" "GL_LINE_STRIP")
(mapcconst int "int" "GL_TRIANGLES")
(mapcconst int "int" "GL_TRIANGLE_STRIP")
(mapcconst int "int" "GL_TRIANGLE_FAN")
(mapcconst int "int" "GL_QUADS")
(mapcconst int "int" "GL_QUAD_STRIP")
(mapcconst int "int" "GL_POLYGON")

;;; Matrix Mode
(mapcconst int "int" "GL_MODELVIEW")
(mapcconst int "int" "GL_PROJECTION")

;;; Depth buffer
(mapcconst int "int" "GL_LEQUAL")
(mapcconst int "int" "GL_DEPTH_TEST")

;;; Lighting
(mapcconst int "int" "GL_LIGHTING")
(mapcconst int "int" "GL_LIGHT1")
(mapcconst int "int" "GL_AMBIENT")
(mapcconst int "int" "GL_DIFFUSE")
(mapcconst int "int" "GL_POSITION")
(mapcconst int "int" "GL_SMOOTH")

;;; Blending
(mapcconst int "int" "GL_BLEND")
(mapcconst int "int" "GL_ONE")
(mapcconst int "int" "GL_SRC_ALPHA")

;;; Fog
(mapcconst int "int" "GL_LINEAR")

;;; Buffers, Pixel Drawing/Reading
(mapcconst int "int" "GL_RGB")
(mapcconst int "int" "GL_RGBA")

;;; Hints
(mapcconst int "int" "GL_PERSPECTIVE_CORRECTION_HINT")
(mapcconst int "int" "GL_NICEST")

;;; Texture mapping
(mapcconst int "int" "GL_TEXTURE_2D")
(mapcconst int "int" "GL_TEXTURE_MAG_FILTER")
(mapcconst int "int" "GL_TEXTURE_MIN_FILTER")
(mapcconst int "int" "GL_LINEAR_MIPMAP_NEAREST")
(mapcconst int "int" "GL_NEAREST")

;;; glPush/PopAttrib bits
(mapcconst int "int" "GL_DEPTH_BUFFER_BIT")
(mapcconst int "int" "GL_COLOR_BUFFER_BIT")

;;; OpenGL 1.2
(mapcconst int "int" "GL_BGR")
(mapcconst int "int" "GL_BGRA")

;;; OpenGL Functions
(defcfun "void gacela_glBegin (int mode)" 0
  "glBegin (mode);")

(defcfun "void gacela_glClear (int mask)" 0
  "glClear (mask);")

(defcfun "void gacela_glClearColor (float red, float green, float blue, float alpha)" 0
  "glClearColor (red, green, blue, alpha);")

(defcfun "void gacela_glClearDepth (double depth)" 0
  "glClearDepth (depth);")

(defcfun "void gacela_glColor3f (float red, float green, float blue)" 0
  "glColor3f (red, green, blue);")

(defcfun "void gacela_glColor4f (float red, float green, float blue, float alpha)" 0
  "glColor4f (red, green, blue, alpha);")

(defcfun "void gacela_glDepthFunc (int func)" 0
  "glDepthFunc (func);")

(defcfun "void gacela_glEnable (int cap)" 0
  "glEnable (cap);")

(defcfun "void gacela_glDisable (int cap)" 0
  "glDisable (cap);")

(defcfun "void gacela_glEnd (void)" 0
  "glEnd ();")

(defcfun "void gacela_glHint (int target, int mode)" 0
  "glHint (target, mode);")

(defcfun "void gacela_glLoadIdentity (void)" 0
  "glLoadIdentity ();")

(defcfun "void gacela_glMatrixMode (int mode)" 0
  "glMatrixMode (mode);")

(defcfun "void gacela_glRotatef (float angle, float x, float y, float z)" 0
  "glRotatef (angle, x, y, z);")

(defcfun "void gacela_glShadeModel (int mode)" 0
  "glShadeModel (mode);")

(defcfun "void gacela_glTranslatef (float x, float y, float z)" 0
  "glTranslatef (x, y, z);")

(defcfun "void gacela_glVertex2f (float x, float y)" 0
  "glVertex2f (x, y);")

(defcfun "void gacela_glVertex3f (float x, float y, float z)" 0
  "glVertex3f (x, y, z);")

(defcfun "void gacela_glViewport (int x, int y, int width, int height)" 0
  "glViewport (x, y, width, height);")

(defcfun "static object gacela_glGenTextures (int n)" 0
  "object textures;"
  "GLuint text[n];"
  "int i, t;"
  ('nil textures)
  "glGenTextures (n, &text[0]);"
  "for (i = n - 1; i >= 0; i--) {"
  "t = text[i];"
  ((cons (int t) textures) textures)
  "}"
  "return textures;")

(defcfun "void gacela_glBindTexture (int target, int texture)" 0
  "glBindTexture (target, texture);")

(defcfun "void gacela_glTexImage2D (int target, int level, int internalFormat, int width, int height, int border, int format, int type, int pixels)" 0
  "glTexImage2D (target, level, internalFormat, width, height, border, format, type, pixels);")

(defcfun "void gacela_glTexParameteri (int target, int pname, int param)" 0
  "glTexParameteri (target, pname, param);")

(defcfun "void gacela_glTexCoord2f (float s, float t)" 0
  "glTexCoord2f (s, t);")

(defcfun "void gacela_glLightfv (int light, int pname, float param1, float param2, float param3, float param4)" 0
  "GLfloat params[4];"
  "params[0] = param1;"
  "params[1] = param2;"
  "params[2] = param3;"
  "params[3] = param4;"
  "glLightfv (light, pname, params);")

(defcfun "void gacela_glNormal3f (float nx, float ny, float nz)" 0
  "glNormal3f (nx, ny, nz);")

(defcfun "void gacela_glBlendFunc (int sfactor, int dfactor)" 0
  "glBlendFunc (sfactor, dfactor);")

(defcfun "void gacela_glOrtho (float left, float right, float bottom, float top, float near_val, float far_val)" 0
  "glOrtho (left, right, bottom, top, near_val, far_val);")

(defcfun "void gacela_gluPerspective (double fovy, double aspect, double zNear, double zFar)" 0
  "gluPerspective (fovy, aspect, zNear, zFar);")

(defcfun "int gacela_gluBuild2DMipmaps (int target, int internalFormat, int width, int height, int format, int type, int data)" 0
  "return gluBuild2DMipmaps (target, internalFormat, width, height, format, type, data);")

(defentry glBegin (int) (void "gacela_glBegin"))
(defentry glClear (int) (void "gacela_glClear"))
(defentry glClearColor (float float float float) (void "gacela_glClearColor"))
(defentry glClearDepth (double) (void "gacela_glClearDepth"))
(defentry glColor3f (float float float) (void "gacela_glColor3f"))
(defentry glColor4f (float float float float) (void "gacela_glColor4f"))
(defentry glDepthFunc (int) (void "gacela_glDepthFunc"))
(defentry glEnable (int) (void "gacela_glEnable"))
(defentry glDisable (int) (void "gacela_glDisable"))
(defentry glEnd () (void "gacela_glEnd"))
(defentry glHint (int int) (void "gacela_glHint"))
(defentry glLoadIdentity () (void "gacela_glLoadIdentity"))
(defentry glMatrixMode (int) (void "gacela_glMatrixMode"))
(defentry glRotatef (float float float float) (void "gacela_glRotatef"))
(defentry glShadeModel (int) (void "gacela_glShadeModel"))
(defentry glTranslatef (float float float) (void "gacela_glTranslatef"))
(defentry glVertex2f (float float) (void "gacela_glVertex2f"))
(defentry glVertex3f (float float float) (void "gacela_glVertex3f"))
(defentry glViewport (int int int int) (void "gacela_glViewport"))
(defentry glGenTextures (int) (object "gacela_glGenTextures"))
(defentry glBindTexture (int int) (void "gacela_glBindTexture"))
(defentry glTexImage2D (int int int int int int int int int) (void "gacela_glTexImage2D"))
(defentry glTexParameteri (int int int) (void "gacela_glTexParameteri"))
(defentry glTexCoord2f (float float) (void "gacela_glTexCoord2f"))
(defentry glLightfv (int int float float float float) (void "gacela_glLightfv"))
(defentry glNormal3f (float float float) (void "gacela_glNormal3f"))
(defentry glBlendFunc (int int) (void "gacela_glBlendFunc"))
(defentry glOrtho (float float float float float float) (void "gacela_glOrtho"))

(defentry gluPerspective (double double double double) (void "gacela_gluPerspective"))
(defentry gluBuild2DMipmaps (int int int int int int int) (int "gacela_gluBuild2DMipmaps"))
