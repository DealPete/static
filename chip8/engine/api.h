#include "input.h"
#include <stdbool.h>
#include <stdint.h>

bool init(char* filename);
void cleanup();
void clear_screen();
void hires();
void lores();
uint8_t check_for_input();
bool check_for_quit();
uint8_t wait_for_keypress();
bool key_pressed(uint8_t);
uint8_t random_byte();
void load_bcd(unsigned char* I, uint8_t);
void draw_screen();
uint8_t draw_sprite(unsigned char* I, uint8_t xpos, uint8_t ypos, uint8_t lines);
void scroll_left();
void scroll_right();
void scroll_down(uint8_t pixels);

uint8_t get_delay_timer();
void set_delay_timer(uint8_t);
int run_delay_timer(void*);

uint8_t get_sound_timer();
void set_sound_timer(uint8_t);
int run_sound_timer(void*);
