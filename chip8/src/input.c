#include "input.h"
#include "SDL.h"
#include "display.h"
#include <stdbool.h>

SDL_Event e;

int8_t process_input(bool* quitting) {
	SDL_PollEvent(&e);

	if (e.type == SDL_QUIT) {
		*quitting = true;
	}

	if (e.type != SDL_KEYDOWN) {
		return -1;
	}

	switch (e.key.keysym.sym) {
		case SDLK_1:
		return 0x1;
		case SDLK_2:
		return 0x2;
		case SDLK_3:
		return 0x3;
		case SDLK_4:
		return 0xc;
		case SDLK_q:
		return 0x4;
		case SDLK_w:
		return 0x5;
		case SDLK_e:
		return 0x6;
		case SDLK_r:
		return 0xd;
		case SDLK_a:
		return 0x7;
		case SDLK_s:
		return 0x8;
		case SDLK_d:
		return 0x9;
		case SDLK_f:
		return 0xe;
		case SDLK_z:
		return 0xa;
		case SDLK_x:
		return 0x0;
		case SDLK_c:
		return 0xb;
		case SDLK_v:
		return 0xf;
	}

	return -1;
}
