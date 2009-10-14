LISP = gcl
LIBS = "-lSDL -lSDL_image -lSDL_ttf -lSDL_mixer -lSDL_gfx -lGL -lGLU -lftgl"

OBJ = "gacela.o" "gacela_misc.o" "gacela_SDL.o" "gacela_GL.o" "gacela_FTGL.o" \
      "gacela_draw.o" "gacela_ttf.o" "gacela_events.o" "gacela_mobs.o" \
      "gacela_widgets.o"

OBJ = "gacela.o" "gacela_misc.o" "gacela_SDL.o" "gacela_GL.o" "gacela_FTGL.o" \
      "gacela_draw.o" "gacela_ttf.o" "gacela_events.o" "gacela_mobs.o" \
      "gacela_widgets.o"

%.o: %.lisp
	$(LISP) -compile $< -system-p

all: $(OBJ)
	$(LISP) -eval "(compiler::link '($(OBJ)) \"gacela\" \"\" $(LIBS))"

clean:
	rm $(OBJ) gacela

