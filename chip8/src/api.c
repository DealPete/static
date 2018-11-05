#include <api.h>
#include "SDL.h"
#include "SDL_thread.h"
#include "display.h"
#include "input.h"
#include "sodium.h"

SDL_Event e;
SDL_mutex* buffer_lock = NULL;
SDL_mutex* keyboard_lock = NULL;
SDL_cond* key_ready = NULL;

bool buffer[8192];
int screen_width;
int screen_height;
int last_key;
bool use_hires;
bool quitting;

bool init(char* filename) {
	if (sodium_init() < 0) {
		printf("Couldn't initialize random number generator.");
		return false;
	}

	if (!init_window(filename)) {
		return false;
	}

	buffer_lock = SDL_CreateMutex();
	keyboard_lock = SDL_CreateMutex();
	key_ready = SDL_CreateCond();

	last_key = -1;
	quitting = false;

	lores();

	return true;
}

void cleanup() {
	SDL_LockMutex(buffer_lock);
	SDL_LockMutex(keyboard_lock);

	close_window();
}

void clear_screen() {
	SDL_LockMutex(buffer_lock);

	for(int i = 0; i < 8192; i++) {
		buffer[i] = 0;
	}

	SDL_UnlockMutex(buffer_lock);

}

void hires() {
	screen_width = 128;
	screen_height = 64;
	use_hires = true;
	clear_screen();
}

void lores() {
	screen_width = 64;
	screen_height = 32;
	use_hires = false;
	clear_screen();
}

void check_for_input() {
	SDL_LockMutex(keyboard_lock);
	last_key = process_input(&quitting);	

	if (last_key != -1)
		SDL_CondSignal(key_ready);

	SDL_UnlockMutex(keyboard_lock);
}

bool check_for_quit() {
	return quitting;
}

int8_t wait_for_keypress() {
	int key;

	SDL_LockMutex(keyboard_lock);

	if (last_key == -1)
		SDL_CondWait(key_ready, keyboard_lock);
	
	key = last_key;
	last_key = -1;

	SDL_UnlockMutex(keyboard_lock);

	return key;
}

int random_int8() {
	return randombytes_random() % 256;
}

void draw_screen() {
	SDL_LockMutex(buffer_lock);

	draw_window(buffer, use_hires, screen_width, screen_height);

	SDL_UnlockMutex(buffer_lock);
}

bool draw_sprite_fragment(unsigned char *I, SDL_Rect box, int xoffset, int yoffset) {
	bool pixel_erased = false;

	for(int y = 0; y < box.h; y++) {
		int x;
		unsigned char bit;
		for(x = 0, bit = 0b10000000 >> xoffset; x < box.w; x++, bit >>= 1) {
			int screen_position = x + box.x + screen_width*(y + box.y);
			int buffer_value = (*(I + yoffset) & bit) > 0;

			if (buffer[screen_position] && buffer_value)
				pixel_erased = true;

			buffer[screen_position] ^= buffer_value;
		}

		I++;
	}

	return pixel_erased;
}

int8_t draw_sprite(unsigned char *I, int xpos, int ypos, int lines) {
	SDL_LockMutex(buffer_lock);

	bool pixel_erased = false;
	SDL_Rect box;
	box.x = xpos;
	box.y = ypos;

	if (xpos + 8 < screen_width && ypos + lines < screen_height) {
		box.w = 8;
		box.h = lines;
		pixel_erased = draw_sprite_fragment(I, box, 0, 0);

	} else if (xpos + 8 < screen_width) {
		box.w = 8;
		box.h = screen_height - ypos;
		pixel_erased = draw_sprite_fragment(I, box, 0, 0);

		box.y = 0;
		box.h = lines + ypos - screen_height;
		pixel_erased |= draw_sprite_fragment(I, box, 0, screen_height - ypos);

	} else if (ypos + lines < screen_height) {
		box.w = screen_width - xpos;
		box.h = lines;
		pixel_erased = draw_sprite_fragment(I, box, 0, 0);

		box.x = 0;
		box.w = 8 + xpos - screen_width;
		pixel_erased |= draw_sprite_fragment(I, box, screen_width - xpos, 0);
	
	} else {
		box.w = screen_width - xpos;
		box.h = screen_height - ypos;
		pixel_erased = draw_sprite_fragment(I, box, 0, 0);

		box.y = 0;
		box.h = lines + ypos - screen_height;
		pixel_erased |= draw_sprite_fragment(I, box, 0, screen_height - ypos);

		box.x = 0;
		box.w = 8 + xpos - screen_width;
		pixel_erased |= draw_sprite_fragment(I, box, screen_width - xpos, screen_height - ypos);

		box.y = ypos;
		box.h = screen_height - ypos;
		pixel_erased |= draw_sprite_fragment(I, box, screen_width - xpos, 0);
	}

	SDL_UnlockMutex(buffer_lock);

	return pixel_erased ? 1 : 0;
}
