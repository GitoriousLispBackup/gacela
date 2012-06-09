AC_INIT([gacela],[0.5],[jsf@jsancho.org])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
LT_INIT
AC_PROG_CC
LT_INIT
PKG_CHECK_MODULES([GUILE], [guile-2.0])

AC_DEFUN([GACELA_DEPS],
    for lib in $1 ; do
	AC_CHECK_LIB(${lib}, main, [$2="${$2} -l${lib}"], AC_MSG_ERROR([*** ${lib} library not found!]))
    done
    [AC_SUBST([$2])]
)
GACELA_DEPS(SDL SDL_image SDL_gfx SDL_mixer, SDL_LIBS)
GACELA_DEPS(GL GLU, GL_LIBS)
GACELA_DEPS(ftgl, FTGL_LIBS)

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/sdl.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
    Makefile
    src/Makefile
])
AC_OUTPUT
