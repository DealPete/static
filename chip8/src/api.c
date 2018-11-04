#include "SDL.h"
#include "SDL_thread.h"
#include "display.h"
#include "input.h"
#include "sodium.h"

SDL_mutex* window_lock = NULL;
SDL_cond* window_free = NULL;

int screen_width;
int screen_height;
bool use_hires;

int init(char* filename) {
	if (sodium_init() < 0) {
		printf("Couldn't initialize random number generator.");
		return -1;
	}

	if (!init_window(filename)) {
		return -1;
	}

	window_lock = SDL_CreateMutex();
	window_free = SDL_CreateCond();
	SDL_CondSignal(window_free);

	lores();

	return 0;
}

void cleanup() {
	SDL_LockMutex(window_lock);
	SDL_CondWait(window_free, window_lock);

	close_window();

	SDL_DestroyMutex(window_lock);
	SDL_DestroyCond(window_free);
}

void clear_screen() {
	clear_window();
}

void hires() {
	hires_screen_mode();
	screen_width = 128;
	screen_height = 64;
	use_hires = true;
	clear_screen();
}

void lores() {
	lores_screen_mode();
	screen_width = 64;
	screen_height = 32;
	use_hires = false;
	clear_screen();
}

int wait_for_keypress() {
	int key;

	while ((key = process_input()) == -1);
	return key;
}

void draw_sprite(unsigned char *I, int xpos, int ypos, int lines) {
	SDL_LockMutex(window_lock);
	SDL_CondWait(window_free, window_lock);

	SDL_Rect box;
	box.x = xpos;
	box.y = ypos;

	if (xpos + 8 < screen_width && ypos + lines < screen_height) {
		box.w = 8;
		box.h = lines;
		draw_sprite_fragment(I, box, 0, 0);

	} else if (xpos + 8 < screen_width) {
		box.w = 8;
		box.h = screen_height - ypos;
		draw_sprite_fragment(I, box, 0, 0);

		box.y = 0;
		box.h = lines + ypos - screen_height;
		draw_sprite_fragment(I, box, 0, screen_height - ypos);

	} else if (ypos + lines < screen_height) {
		box.w = screen_width - xpos;
		box.h = lines;
		draw_sprite_fragment(I, box, 0, 0);

		box.x = 0;
		box.w = 8 + xpos - screen_width;
		draw_sprite_fragment(I, box, screen_width - xpos, 0);
	
	} else {
		box.w = screen_width - xpos;
		box.h = screen_height - ypos;
		draw_sprite_fragment(I, box, 0, 0);

		box.y = 0;
		box.h = lines + ypos - screen_height;
		draw_sprite_fragment(I, box, 0, screen_height - ypos);

		box.x = 0;
		box.w = 8 + xpos - screen_width;
		draw_sprite_fragment(I, box, screen_width - xpos, screen_height - ypos);

		box.y = ypos;
		box.h = screen_height - ypos;
		draw_sprite_fragment(I, box, screen_width - xpos, 0);
	}

	SDL_CondSignal(window_free);
}
