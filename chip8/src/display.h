#include "SDL.h"
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

bool init_window(char* filename);
void close_window();
void draw_sprite_fragment(unsigned char* I, SDL_Rect box, int xoffset, int yoffset);
void draw_sprite(unsigned char* I, int xpos, int ypos, int lines);
