LISP = gcl
INC = -I/usr/include/FTGL -I/usr/include/freetype2
# LIBS = -lSDL -lSDL_image -lSDL_ttf -lSDL_mixer -lSDL_gfx -lGL -lGLU -lftgl
LIBS = -lSDL -lSDL_image -lSDL_mixer -lGL -lGLU -lftgl

OBJ = gacela.o gacela_misc.o gacela_SDL.o gacela_GL.o gacela_FTGL.o \
      gacela_draw.o gacela_ttf.o gacela_events.o gacela_mobs.o \
      gacela_widgets.o

GCL_OBJ = $(OBJ:%.o=\"%.o\")

%.o: %.lisp
	$(LISP) -eval "(progn (load \"gacela_make.lisp\") (compile-gfile \"$<\" :include \"$(INC)\"))" -batch

all: $(OBJ)
	$(LISP) -eval "(compiler::link '($(GCL_OBJ)) \"gacela\" \"\" \"$(LIBS)\")" -batch

clean:
	rm -f $(OBJ) gacela

