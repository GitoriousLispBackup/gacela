(clines "#include <pthread.h>")

(clines "#define inheap(pp) ((char *)(pp) < heap_end)")
(clines "static object code_for_eval_code;")

(defcfun "static object staticp (object array)" 0
  "if (inheap (array->st.st_self)) return Ct;"
  "else return Cnil;")

(defcfun "static void *eval_code (void *parameter)" 0
  (eval code_for_eval_code))

(defcfun "int run_thread (object code)" 0
  "pthread_t tid;"
  "int ret;"
  "code_for_eval_code = code;"
  "ret = pthread_create (&tid, NULL, eval_code, NULL);"
  "return ret;")

(defcfun "int runprocess (object code)" 0
  "int pid;"
  "pid = fork ();"
  "if (pid == 0) {"
  "close (0);"
  (eval code)
  "exit (0);"
  "} else {"
  "return pid;"
  "}")

(defentry run-thread2 (object) (int "run_thread"))
(defentry staticp (object) (object "staticp"))
(defentry run-process (object) (int "runprocess"))

(defun run-thread (code)
  (and (staticp code) (run-thread2 code)))
