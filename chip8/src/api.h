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
bool key_pressed(int8_t);
int random_int8();
void draw_screen();
int8_t draw_sprite(unsigned char* I, int8_t xpos, int8_t ypos, int8_t lines);
void scroll_left();
void scroll_right();
