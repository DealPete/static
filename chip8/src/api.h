#include <stdbool.h>
#include <stdint.h>

int init(char* filename);
void cleanup();
void clear_screen();
void hires();
void lores();
int8_t wait_for_keypress();
bool check_for_quit();
int random_int32();
void draw_screen();
void draw_sprite(unsigned char* I, int xpos, int ypos, int lines);
