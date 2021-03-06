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

#define cout &term_log

//-----------------------------------------------------------------------------

// "term" class declaration.

const int term_max_nb_params = 10;
const int term_normal_foreground = 0; // black
const int term_normal_background = 7; // white
const int term_outer_border = 1;
const int term_frame_border = 2;
const int term_inner_border = 2;

typedef struct term {
  int _x;
  int _y;
  int _nb_columns;
  int _nb_rows;
  font_c *_fn_normal;
  font_c *_fn_bold;

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

error_code init_terms();

term *term_init(term *self, int x, int y, int nb_columns, int nb_rows,
                font_c *font_normal, font_c *font_bold, unicode_string title,
                bool initialy_visible);

void term_show(term *self); //#!

int term_write(term *self, unicode_char *buf, int count); //#!

void term_char_coord_to_screen_coord(term *self, int column, int row, int *sx,
                                     int *sy, int *ex, int *ey); //#!

void term_color_to_pattern(term *self, int color, pattern **pat); //#!

void term_show_cursor(term *self);   //#!
void term_hide_cursor(term *self);   //#!
void term_toggle_cursor(term *self); //#!
void term_scroll_up(term *self);     //#!

term *term_writeline(term *self);
term *term_write(term *self, native_char x);
term *term_write(term *self, bool x);
term *term_write(term *self, int8 x);
term *term_write(term *self, int16 x);
term *term_write(term *self, int32 x);
term *term_write(term *self, int64 x);
term *term_write(term *self, uint8 x);
term *term_write(term *self, uint16 x);
term *term_write(term *self, uint32 x);
term *term_write(term *self, uint64 x);
term *term_write(term *self, void *x);
term *term_write(term *self, native_string x);
term *term_write(term *self, unicode_string x);

void debug_write(void *ptr);
void debug_write(uint32 x);
void __debug_write(uint32 x);
void debug_write(native_string x);
void _debug_write(native_char x);
void __debug_write(native_string x);

size_t strlen(char *str);

unsigned char strcmpl(char *a, char *b, size_t sz);

unsigned char strcmp(char *a, char *b);

//-----------------------------------------------------------------------------

#ifdef ENABLE_DEBUG_MARKER
#define __debug_marker()                                                       \
  do {                                                                         \
    term_write(cout, __FILE__);                                                \
    term_write(cout, ":");                                                     \
    term_write(cout, __LINE__);                                                \
    term_writeline(cout);                                                      \
  } while (0);
#else
#define __debug_marker() NOP()
#endif

#define __surround_with_debug(code)                                            \
  do {                                                                         \
    debug_write("SURROUND IN ");                                               \
    debug_write(__FILE__);                                                     \
    debug_write(__LINE__ - 2);                                                 \
    code;                                                                      \
    debug_write("END SURROUND");                                               \
  } while (0)

#define __surround_with_debug_t(tag, code)                                     \
  do {                                                                         \
    debug_write("[IN ] " tag);                                                 \
    code;                                                                      \
    debug_write("[OUT] " tag);                                                 \
  } while (0)

//-----------------------------------------------------------------------------
// Static objects
//-----------------------------------------------------------------------------

extern term term_console;
extern term term_log;

#endif

// Local Variables: //
// mode: C++ //
// End: //
