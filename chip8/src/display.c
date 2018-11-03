#include "SDL.h"
#include "display.h"
#include <stdio.h>

const int WINDOW_WIDTH = 1024;
const int WINDOW_HEIGHT = 512;

const int r = 0xb0;
const int g = 0x60;
const int b = 0x0;

SDL_Window* window = NULL;
SDL_Surface* surface = NULL;
SDL_Renderer* renderer = NULL;
bool use_hires = false;

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

	surface = SDL_GetWindowSurface(window);

	clear_screen();

	return true;
}

void close_window() {
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();
}

void clear_screen() {
	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
	SDL_RenderClear(renderer);
}

void hires() {
	use_hires = true;
	clear_screen();
}

void lores() {
	use_hires = false;
	clear_screen();
}

void draw_sprite(unsigned char *I, int xpos, int ypos, int lines) {
	SDL_Rect box;

	for(int y = 0; y < lines; y++) {
		int x;
		unsigned char bit;
		for(x = 0, bit = 0b10000000; x < 8; x++, bit >>= 1) {
			if (*I & bit) {
				SDL_SetRenderDrawColor(renderer, r, g, b, 0xff);
			} else {
				SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
			}
				
			if (use_hires) {
				box.h = 8;
				box.w = 8;
				box.x = (xpos + x) * 8;
				box.y = (ypos + y) * 8;
				SDL_RenderFillRect(renderer, &box);
			} else {
				box.h = 16;
				box.w = 16;
				box.x = (xpos + x) * 16; 
				box.y = (ypos + y) * 16;
				SDL_RenderFillRect(renderer, &box);
			}
		}

		I++;
	}

	SDL_RenderPresent(renderer);
	SDL_UpdateWindowSurface(window);
}
