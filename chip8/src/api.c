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
int8_t last_key;
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

bool key_pressed(int8_t key) {
	return key == last_key;
}

int random_int8() {
	return randombytes_random() % 256;
}

void draw_screen() {
	SDL_LockMutex(buffer_lock);

	draw_window(buffer, use_hires, screen_width, screen_height);

	SDL_UnlockMutex(buffer_lock);
}

int8_t draw_sprite(unsigned char *I, int8_t xpos, int8_t ypos, int8_t lines) {
	SDL_LockMutex(buffer_lock);

	bool pixel_erased = false;

	for(int y = 0; y < lines; y++) {
		int8_t x;
		unsigned char bit;
		for(x = 0, bit = 0b10000000; x < 8; x++, bit >>= 1) {
			int buffer_index = ((uint8_t)(x + xpos) % screen_width) + screen_width*((uint8_t)(y + ypos) % screen_height);
			int buffer_value = (*I & bit) > 0;

			if (buffer[buffer_index] && buffer_value)
				pixel_erased = true;

			buffer[buffer_index] ^= buffer_value;
		}

		I++;
	}

	SDL_UnlockMutex(buffer_lock);

	return pixel_erased ? 1 : 0;
}
