#include <stdbool.h>
#include <stdint.h>

bool init_window(char* filename);
void close_window();
void clear_screen();
void hires();
void lores();
void draw_sprite(unsigned char* I, int xpos, int ypos, int lines);
