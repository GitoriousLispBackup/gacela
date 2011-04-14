#include <SDL/SDL.h>
#include <GL/gl.h>
#include <libguile.h>
#include <pthread.h>

SCM prueba () {
	int flags;

	SDL_Init (SDL_INIT_EVERYTHING);

	SDL_GL_SetAttribute (SDL_GL_DOUBLEBUFFER, 1);

	flags = SDL_OPENGL | SDL_GL_DOUBLEBUFFER | SDL_HWPALETTE | SDL_RESIZABLE | SDL_SWSURFACE;
	SDL_SetVideoMode (200, 200, 32, flags);

	glShadeModel (GL_SMOOTH);
	glClearColor (0, 0, 0, 0);
	glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glViewport (0, 0, 200, 200);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	glOrtho (-200, 200, -200, 200, 0, 1);
	glMatrixMode (GL_MODELVIEW);
	glLoadIdentity ();

	return SCM_UNSPECIFIED;
}

void *bucle () {
	while (1) {
		scm_c_eval_string("(define contador (+ contador incremento))");
		scm_c_eval_string("(if (> contador 1000) (define incremento -1))");
		scm_c_eval_string("(if (< contador 0) (define incremento 1))");
	}
	pthread_exit(NULL);
}

SCM lanzar_bucle () {
	pthread_t t;

	pthread_create(&t, NULL, bucle, NULL);
	return SCM_UNSPECIFIED;
}

/*SCM ver_contador () {
	return scm_from_int(contador);
}*/

static void*
register_functions (void* data)
{
	scm_c_define_gsubr ("prueba", 0, 0, 0, &prueba);
//	scm_c_define_gsubr ("ver-contador", 0, 0, 0, &ver_contador);
	scm_c_define_gsubr ("lanzar-bucle", 0, 0, 0, &lanzar_bucle);
	return NULL;
}


int main (int argc, char *argv[]) {
	scm_with_guile (&register_functions, NULL);
	scm_c_eval_string("(set-repl-prompt! \"gacela>\")");
	scm_c_eval_string("(use-modules (ice-9 readline))");
	scm_c_eval_string("(activate-readline)");
	scm_c_eval_string("(define contador 0)");
	scm_c_eval_string("(define incremento 1)");
	scm_shell (argc, argv);
}

