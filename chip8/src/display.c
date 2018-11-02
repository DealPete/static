#include "SDL.h"
#include "display.h"
#include <stdio.h>

const int WINDOW_WIDTH = 1024;
const int WINDOW_HEIGHT = 512;

const int r = 0xa0;
const int g = 0x50;
const int b = 0x0;

SDL_Window* window = NULL;
SDL_Surface* surface = NULL;
SDL_Renderer* renderer = NULL;
SDL_Rect box;

bool init_window() {
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		printf("Error initializing SDL video: %s\n", SDL_GetError());
		return false;
	}

	window = SDL_CreateWindow("Fun Game", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN);
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

	box.w = 16;
	box.h = 16;

	SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
	SDL_RenderClear(renderer);
	SDL_RenderPresent(renderer);

	return true;
}

void close_window() {
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();
}

void draw(unsigned char* I, int xpos, int ypos, int lines) {
	for(int y = 0; y < lines; y++) {
		int x;
		unsigned char bit;
		for(x = 0, bit = 0b10000000; x < 8; x++, bit >>= 1) {
			if (*I & bit) {
				SDL_SetRenderDrawColor(renderer, r, g, b, 0xff);
			} else {
				SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
			}
				
			box.x = (xpos + x) * 16; 
			box.y = (ypos + y) * 16;
			SDL_RenderFillRect(renderer, &box);
		}

		I++;
	}

	SDL_RenderPresent(renderer);
	SDL_UpdateWindowSurface(window);
}
