LISP = gcl
INC = -I/usr/include/FTGL -I/usr/include/freetype2
LIBS = -lSDL -lSDL_image -lSDL_ttf -lSDL_mixer -lSDL_gfx -lGL -lGLU -lftgl

OBJ = gacela.o gacela_misc.o gacela_SDL.o gacela_GL.o gacela_FTGL.o \
      gacela_draw.o gacela_ttf.o gacela_events.o gacela_mobs.o \
      gacela_widgets.o

GCL_OBJ = $(OBJ:%.o=\"%.o\")

%.o: %.lisp
	$(LISP) -eval "(progn (setq compiler::*cc* (concatenate 'string compiler::*cc* \"$(INC)\")) (compile-file \"$<\" :system-p t))" -batch

all: $(OBJ)
	$(LISP) -eval "(compiler::link '($(GCL_OBJ)) \"gacela\" \"\" \"$(LIBS)\")" -batch

clean:
	rm $(OBJ) gacela

