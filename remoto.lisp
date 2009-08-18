(clines "#include <pthread.h>")
(clines "#include <time.h>")

(clines "#define inheap(pp) ((char *)(pp) < heap_end)")
(clines "static object pepe;")

(defcfun "static object staticp (object array)" 0
  "if (inheap (array->st.st_self)) return Ct;"
  "else return Cnil;")

(defcfun "static void *eval_code (void *parameter)" 0
  "int t = time (NULL);"
  "while (time (NULL) - t < 10);"
  (eval pepe))

(defcfun "int run_thread (object code)" 0
  "pthread_t tid;"
  "int ret;"
  "pepe = code;"
  "ret = pthread_create (&tid, NULL, eval_code, NULL);"
  "return ret;")

;(defentry eval-code (object) (void "eval_code"))
(defentry run-thread (object) (int "run_thread"))
(defentry staticp (object) (object "staticp"))

(defun runt (code)
  (and (staticp code) (run-thread code)))
