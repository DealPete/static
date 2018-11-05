#include <stdbool.h>
#include <stdint.h>

bool init(char* filename);
void cleanup();
void clear_screen();
void hires();
void lores();
void check_for_input();
bool check_for_quit();
int8_t wait_for_keypress();
int random_int8();
void draw_screen();
int8_t draw_sprite(unsigned char* I, int xpos, int ypos, int lines);
