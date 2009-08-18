struct SDL_Rect {
 int x, y;
 int w, h;
};
struct SDL_Rect SSS1;

main() {

printf("(");
printf("(|SDL_Rect| ");
printf(" %d ",((char *)&SSS1.x - (char *)&SSS1));
printf(" %d ",((char *)&SSS1.y - (char *)&SSS1));
printf(" %d ",((char *)&SSS1.w - (char *)&SSS1));
printf(" %d ",((char *)&SSS1.h - (char *)&SSS1));
printf(")");
printf(")"); ;}