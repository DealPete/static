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

	while(!check_for_quit()) {
		check_for_input();
		draw_screen();
	}

	cleanup();
	return 0;
}
