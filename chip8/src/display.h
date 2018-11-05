#include "SDL.h"
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

bool init_window(char* filename);
void close_window();
void clear_window();
void draw_window(bool* buffer, bool hires, int screen_width,
	int screen_height);
