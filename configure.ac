AC_INIT([gacela], [0.5], [jsf@jsancho.org])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_PROG_CC
AC_PROG_LIBTOOL
GUILE_FLAGS
CFLAGS="$CFLAGS -I/usr/include/freetype2"
AC_CHECK_LIB(readline, main, AC_MSG_ERROR([*** readline library not found!]))
AC_CHECK_LIB(SDL, main, AC_MSG_ERROR([*** SDL library not found!]))
AC_CHECK_LIB(SDL_image, main, AC_MSG_ERROR([*** SDL_image library not found!]))
AC_CHECK_LIB(SDL_gfx, main, AC_MSG_ERROR([*** SDL_gfx library not found!]))
AC_CHECK_LIB(SDL_mixer, main, AC_MSG_ERROR([*** SDL_mixer library not found!]))
AC_CHECK_LIB(GL, main, AC_MSG_ERROR([*** GL library not found!]))
AC_CHECK_LIB(GLU, main, AC_MSG_ERROR([*** GLU library not found!]))
AC_CHECK_LIB(ftgl, main, AC_MSG_ERROR([*** FTGL library not found!]))
AC_CONFIG_SRCDIR([src/sdl.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
    Makefile
    src/Makefile
])
AC_OUTPUT
