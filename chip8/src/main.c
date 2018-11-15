#include "SDL.h"
#include "SDL_thread.h"
#include "api.h"

char* get_filename();
int run_game(void *data);

int main(int argc, char* args[]) {
	uint8_t input;

	if (!init(get_filename())) {
		return -1;
	}

	SDL_CreateThread(run_game, "GameCode", NULL);
	SDL_CreateThread(run_delay_timer, "DelayTimer", NULL);
	SDL_CreateThread(run_sound_timer, "SoundTimer", NULL);

	while(true) {
		input = check_for_input();

		if (input == INPUT_QUIT) {
			cleanup();
			return 0;
		}

		//if (input == INPUT_RESTART) {

		draw_screen();
	}

}
