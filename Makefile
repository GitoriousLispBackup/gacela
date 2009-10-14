LISP = gcl

SRC = gacela.lisp gacela_misc.lisp gacela_SDL.lisp gacela_GL.lisp \
      gacela_draw.lisp gacela_events.lisp gacela_mobs.lisp \
      gacela_widgets.lisp

OBJ = gacela.o gacela_misc.o gacela_SDL.o gacela_GL.o gacela_draw.o \
      gacela_events.o gacela_mobs.o gacela_widgets.o

.lisp.o:
	$(LISP) -compile -system-p $<

all: $(OBJ)
	touch pepe

clean:
	rm $(OBJ) gacela

gacela.o: gacela.lisp
gacela_misc.o: gacela_misc.lisp
gacela_SDL.o: gacela_SDL.lisp
gacela_GL.o: gacela_GL.lisp
gacela_draw.o: gacela_draw.lisp
gacela_events.o: gacela_events.lisp
gacela_mobs.o: gacela_mobs.lisp
gacela_widgets.o: gacela_widgets.lisp

