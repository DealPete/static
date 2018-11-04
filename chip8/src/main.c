#include "SDL.h"
#include "SDL_thread.h"
#include "api.h"

char* get_filename();
int run_game(void *data);

int main(int argc, char* args[]) {
	if (!init(get_filename())) {
		return -1;
	}

	SDL_Thread* thread = SDL_CreateThread(run_game, "GameCode", NULL);

	int last_update = SDL_GetTicks();
	int current_time = last_update;

	while(!check_for_quit()) {
		current_time = SDL_GetTicks();

		if (current_time - last_update < 100) {
			draw_screen();
			last_update = SDL_GetTicks();
		}
	}

	cleanup();
	exit(0);
}
