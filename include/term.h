// file: "term.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)
#ifndef TERM_H
#define TERM_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "video.h"

#define cout &term_console
//-----------------------------------------------------------------------------

// "term" class declaration.

const int term_max_nb_params = 10;
const int term_normal_foreground = 0;  // black
const int term_normal_background = 7;  // white
const int term_outer_border = 1;
const int term_frame_border = 2;
const int term_inner_border = 2;

typedef struct term {
  int _x;
  int _y;
  int _nb_columns;
  int _nb_rows;
  font_c* _fn;
  unicode_string _title;
  int _cursor_column;
  int _cursor_row;
  bool _cursor_visible;
  bool _visible;
  // for vt100 emulation:
  int _param[term_max_nb_params];
  int _param_num;
  bool _bold;
  bool _underline;
  bool _reverse;
  int _fg;
  int _bg;
} term;

term new_term(int x, int y, int nb_columns, int nb_rows, font_c* font,
              unicode_string title, bool initialy_visible);

void term_show(term* self);  //#!

int term_write(term* self, unicode_char* buf, int count);  //#!

void term_char_coord_to_screen_coord(term* self, int column, int row, int* sx,
                                     int* sy, int* ex, int* ey);  //#!

void term_color_to_pattern(term* self, int color, pattern** pat);  //#!

void term_show_cursor(term* self);    //#!
void term_hide_cursor(term* self);    //#!
void term_toggle_cursor(term* self);  //#!
void term_scroll_up(term* self);      //#!

term* term_write(term* self, native_char x);\
term* term_write(term* self, bool x);
term* term_write(term* self, int8 x);
term* term_write(term* self, int16 x);
term* term_write(term* self, int32 x);
term* term_write(term* self, int64 x);
term* term_write(term* self, uint8 x);
term* term_write(term* self, uint16 x);
term* term_write(term* self, uint32 x);
term* term_write(term* self, uint64 x);
term* term_write(term* self, void* x);
term* term_write(term* self, native_string x);
term* term_write(term* self, unicode_string x);

void debug_write(uint32 x);
void debug_write(native_string x);

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Static objects
//-----------------------------------------------------------------------------
extern term term_console;

#endif

// Local Variables: //
// mode: C++ //
// End: //
