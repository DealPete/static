#include "api.h"

int run_game(void *data);

int main(int argc, char* args[]) {
	if (!init(filename)) {
		return -1;
	}

	SDL_Thread* thread = SDL_CreateThread(run_game, "GameCode", NULL);

	while(true) {
		if (process_input() == -2) {
			cleanup();
			exit(0);
		}
	}
}
