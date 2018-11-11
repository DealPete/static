#include <api.h>
#include "SDL.h"
#include "SDL_thread.h"
#include "display.h"
#include "input.h"
#include "sodium.h"

SDL_Event e;
SDL_mutex* buffer_lock = NULL;
SDL_mutex* keyboard_lock = NULL;
SDL_mutex* delay_lock = NULL;
SDL_cond* key_ready = NULL;
SDL_cond* new_timer = NULL;
bool buffer[8192];
int screen_width;
int screen_height;
uint8_t last_key;
bool use_hires;
bool quitting;
uint8_t timer = 0;

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
	delay_lock = SDL_CreateMutex();
	key_ready = SDL_CreateCond();
	new_timer = SDL_CreateCond();

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

	if (last_key != 0xff)
		SDL_CondSignal(key_ready);

	SDL_UnlockMutex(keyboard_lock);
}

bool check_for_quit() {
	return quitting;
}

uint8_t wait_for_keypress() {
	uint8_t key;

	SDL_LockMutex(keyboard_lock);

	if (last_key == 0xff)
		SDL_CondWait(key_ready, keyboard_lock);
	
	key = last_key;
	last_key = 0xff;

	SDL_UnlockMutex(keyboard_lock);

	return key;
}

bool key_pressed(uint8_t key) {
	return key == last_key;
}

uint8_t random_byte() {
	return randombytes_random() % 256;
}

void load_bcd(unsigned char* I, uint8_t vx) {
	I[0] = vx / 100;
	I[1] = (vx % 100) / 10;
	I[2] = vx % 10; 
}

void draw_screen() {
	SDL_LockMutex(buffer_lock);

	draw_window(buffer, use_hires, screen_width, screen_height);

	SDL_UnlockMutex(buffer_lock);
}

bool draw_byte(char byte, uint8_t xpos, uint8_t ypos) {
	bool pixel_erased = false;

	uint8_t x, bit;
	for(x = 0, bit = 0b10000000; x < 8; x++, bit >>= 1) {
		int buffer_index = ((x + xpos) % screen_width) + (screen_width * (ypos % screen_height));
		int buffer_value = (byte & bit) > 0;

		if (buffer[buffer_index] && buffer_value)
			pixel_erased = true;

		buffer[buffer_index] ^= buffer_value;
	}

	return pixel_erased ? 1 : 0;
}

uint8_t draw_sprite(unsigned char *I, uint8_t xpos, uint8_t ypos, uint8_t lines) {
	bool pixel_erased = false;

	SDL_LockMutex(buffer_lock);

	if (lines > 0) {
		for(uint8_t y = 0; y < lines; y++) {
			pixel_erased = pixel_erased
				|| draw_byte(*I, xpos, y + ypos);
			I++;
		}
	} else {
		for(uint8_t y = 0; y < 16; y++) {
			pixel_erased = pixel_erased
				|| draw_byte(*I, xpos, y + ypos);
			I++;
			pixel_erased = pixel_erased
				|| draw_byte(*I, xpos + 8, y + ypos);
			I++;
		}
	}

	SDL_UnlockMutex(buffer_lock);

	return pixel_erased ? 1 : 0;
}

void scroll_left() {
	SDL_LockMutex(buffer_lock);

	bool new_buffer[8192];

	for(int x = 0; x < screen_width; x++) {
		for(int y = 0; y < screen_height; y++) {
			int source_pixel = x + screen_width*y;
			int target_pixel =
				(x % screen_width) >= 4 ?
				source_pixel - 4 :
				source_pixel + screen_width - 4;
			new_buffer[target_pixel] = buffer[source_pixel];
		}
	}

	memcpy(&buffer, new_buffer, 8192);

	SDL_UnlockMutex(buffer_lock);
}

void scroll_right() {
	SDL_LockMutex(buffer_lock);

	bool new_buffer[8192];

	for(int x = 0; x < screen_width; x++) {
		for(int y = 0; y < screen_height; y++) {
			int source_pixel = x + screen_width*y;
			int target_pixel =
				x >= screen_width - 4 ?
				source_pixel - screen_width + 4 :
				source_pixel + 4;
			new_buffer[target_pixel] = buffer[source_pixel];
		}
	}

	memcpy(&buffer, new_buffer, 8192);

	SDL_UnlockMutex(buffer_lock);
}

void scroll_down(uint8_t pixels) {
	SDL_LockMutex(buffer_lock);

	for(int x = 0; x < screen_width; x++) {
		for(int y = screen_height - pixels - 1; y >= 0; y--) {
			buffer[x + screen_width * (y + pixels)] =
				buffer[x + screen_width * y]; 
		}
	}

	for(int x = 0; x < screen_width; x++) {
		for(int y = 0; y < pixels; y++) {
			buffer[x + screen_width*y] = false;
		}
	}

	SDL_UnlockMutex(buffer_lock);
	SDL_Delay(100);
}

uint8_t get_timer() {
	return timer;
}

void set_timer(uint8_t new_time) {
	SDL_LockMutex(delay_lock);

	timer = new_time;
	
	SDL_CondSignal(new_timer);
	SDL_UnlockMutex(delay_lock);
}

int delay_timer(void* data) {
	while (true) {
		SDL_LockMutex(delay_lock);
		SDL_CondWait(new_timer, delay_lock);
		SDL_UnlockMutex(delay_lock);

		while(get_timer()) {
			SDL_LockMutex(delay_lock);
			SDL_CondWaitTimeout(new_timer, delay_lock, 17);
			timer -= 1;
			SDL_UnlockMutex(delay_lock);
		}
	}
}
