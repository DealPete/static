#include "display.h"

const int WINDOW_WIDTH = 1024;
const int WINDOW_HEIGHT = 512;

const int r = 0xb0;
const int g = 0x60;
const int b = 0x0;

SDL_Window* window = NULL;
SDL_Renderer* renderer = NULL;
SDL_Texture* lores_texture = NULL;
SDL_Texture* hires_texture = NULL;
SDL_Texture* texture = NULL;

bool init_window(char* filename) {
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		printf("Error initializing SDL video: %s\n", SDL_GetError());
		return false;
	}

	window = SDL_CreateWindow(filename, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN);
	if (window == NULL) {
		printf("Error creating SDL window: %s\n", SDL_GetError());
		return false;
	}

	renderer = SDL_CreateRenderer(window, -1, 0);
	if (renderer == NULL) {
		printf("Error creating SDL renderer: %s\n", SDL_GetError());
		return false;
	}

	hires_texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_STREAMING, 128, 64);

	if (hires_texture == NULL) {
		printf("Error creating hires texture: %s\n", SDL_GetError());
	}

	lores_texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_STREAMING, 64, 32);

	if (lores_texture == NULL) {
		printf("Error creating lores texture: %s\n", SDL_GetError());
	}

	return true;
}

void close_window() {
	SDL_DestroyTexture(hires_texture);
	SDL_DestroyTexture(lores_texture);
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();
}

void clear_window() {
	SDL_
void draw_window(bool hires) {

	if (hires)
		texture = hires_texture;
	else
		texture = lores_texture;

	SDL_RenderCopy(renderer, texture, NULL, NULL);
	SDL_RenderPresent(renderer);
}

void draw_sprite_fragment(unsigned char *I, SDL_Rect box, int xoffset, int yoffset) {

	void* pixels;
	int pitch;

	SDL_LockTexture(texture, &box, &pixels, &pitch);

	for(int y = 0; y < box.h; y++) {
		int x;
		unsigned char bit;
		for(x = 0, bit = 0b10000000 >> xoffset; x < box.w; x++, bit >>= 1) {
			if (*(I + xoffset + 8 * yoffset) & bit) {
				((uint32_t*)pixels)[x + (pitch/4)*y] =
					0xd2691eff;
			} else {
				((uint32_t*)pixels)[x + (pitch/4)*y] = 0;
			}
		}

		I++;
	}

	SDL_UnlockTexture(texture); 
}
