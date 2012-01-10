AC_INIT([gacela], [0.5], [jsf@jsancho.org])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
LT_INIT
AC_PROG_CC
AC_PROG_LIBTOOL
GUILE_FLAGS

for lib in SDL SDL_image SDL_gfx SDL_mixer ; do
    AC_CHECK_LIB(${lib}, main, [SDL_LIBS="-l${lib} $SDL_LIBS"], AC_MSG_ERROR([*** ${lib} library not found!]))
done
AC_SUBST(SDL_LIBS)
for lib in GL GLU ; do
    AC_CHECK_LIB(${lib}, main, [GL_LIBS="-l${lib} $GL_LIBS"], AC_MSG_ERROR([*** ${lib} library not found!]))
done
AC_CHECK_LIB(ftgl, main, [FTGL_LIBS="-lftgl $FTGL_LIBS"], AC_MSG_ERROR([*** FTGL library not found!]))

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/sdl.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
    Makefile
    src/Makefile
])
AC_OUTPUT
