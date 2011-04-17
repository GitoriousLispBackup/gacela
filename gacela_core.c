#include <SDL/SDL.h>
#include <GL/gl.h>
#include <libguile.h>

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
//	scm_shell (argc, argv);
	while (1) {}
}

