// file: "term.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "term.h"

//-----------------------------------------------------------------------------

term_c* new_term(int x, int y, int nb_columns, int nb_rows, font* font, unicode_string title, bool initialy_visible) {
  static term_c term;

  term._x = x;
  term._y = y;
  term._nb_columns = nb_columns;
  term._nb_rows = nb_rows;
  term._fn = font;
  term._title = title;
  term._visible = FALSE;
  term._cursor_column = term._cursor_row = 0;
  term._cursor_visible = FALSE;
  // VT 100
  term._param_num = -2;
  term._bold = FALSE;
  term._underline = FALSE;
  term._reverse = FALSE;
  term._fg = term_normal_foreground;
  term._bg = term_normal_background;    

  if (initialy_visible) {
    term_show(&term);
  }

  return &term;
}

void term_show(term_c* self) {
  if (self->_visible) {
    return;
  }

  int sx, sy, ex, ey;
  int char_height = self->_fn->get_height();
  pattern* background;

  term_char_coord_to_screen_coord(self, self->_nb_columns - 1,
                                  self->_nb_rows - 1, &sx, &sy, &ex, &ey);

  term_color_to_pattern(self, term_normal_background, &background);

  // TODO: Call video properly
  // Video "static methods"
  video::screen.frame_rect(
      self->_x, self->_y,
      ex + term_outer_border + term_frame_border + term_inner_border,
      ey + term_outer_border + term_frame_border + term_inner_border,
      term_outer_border, &pattern::white);

  video::screen.frame_rect(self->_x + term_outer_border,
                           self->_y + term_outer_border,
                           ex + term_frame_border + term_inner_border,
                           ey + term_frame_border + term_inner_border,
                           term_frame_border, &pattern::gray50);

  video::screen.fill_rect(self->_x + term_outer_border + term_frame_border,
                          self->_y + char_height + term_outer_border +
                              term_frame_border + 2 * term_inner_border,
                          ex + term_inner_border,
                          self->_y + char_height + term_outer_border +
                              2 * term_frame_border + 2 * term_inner_border,
                          &pattern::gray50);

  video::screen.fill_rect(self->_x + term_outer_border + term_frame_border,
                          self->_y + term_outer_border + term_frame_border,
                          ex + term_inner_border,
                          self->_y + char_height + term_outer_border +
                              term_frame_border + 2 * term_inner_border,
                          &pattern::black);
  // EO TODO

  self->_fn->draw_string(
      &video::screen,
      self->_fn->draw_string(
          &video::screen,
          self->_x + term_outer_border + term_frame_border + term_inner_border,
          self->_y + term_outer_border + term_frame_border + term_inner_border,
          L"\x25b6 ",  // rightward triangle and space
          &pattern::blue, &pattern::black),
      self->_y + term_outer_border + term_frame_border + term_inner_border,
      self->_title, &pattern::blue, &pattern::black);

  video::screen.fill_rect(self->_x + term_outer_border + term_frame_border,
                          self->_y + char_height + term_outer_border +
                              2 * term_frame_border + 2 * term_inner_border,
                          ex + term_inner_border, ey + term_inner_border,
                          background);

  term_show_cursor(self);
  self->_visible = TRUE;
}

void term_char_coord_to_screen_coord(term_c* self, int column, int row, int* sx,
                                     int* sy, int* ex, int* ey) {
  int char_max_width = self->_fn->get_max_width();
  int char_height = self->_fn->get_height();

  *sx = self->_x + column * char_max_width + term_outer_border +
        term_frame_border + term_inner_border;
  *sy = self->_y + row * char_height + term_outer_border +
        2 * term_frame_border + 3 * term_inner_border + char_height;
  *ex = *sx + char_max_width;
  *ey = *sy + char_height;
}

void term_color_to_pattern(term_c* self, int color, pattern** pat) {
  switch (color) {
    case 0:
      *pat = &pattern::black;
      break;
    case 1:
      *pat = &pattern::red;
      break;
    case 2:
      *pat = &pattern::green;
      break;
    case 3:
      *pat = &pattern::yellow;
      break;
    case 4:
      *pat = &pattern::blue;
      break;
    case 5:
      *pat = &pattern::magenta;
      break;
    case 6:
      *pat = &pattern::cyan;
      break;
    case 7:
      *pat = &pattern::white;
      break;
  }
}

void term_show_cursor(term_c* self) {
  if (!self->_cursor_visible) {
    term_toggle_cursor(self);
  }
}

void term_hide_cursor(term_c* self) {
  if (self->_cursor_visible) {
    term_toggle_cursor(self);
  }
}

void term_toggle_cursor(term_c* self) {
  int sx, sy, ex, ey;

  term_char_coord_to_screen_coord(self, self->_cursor_column, self->_cursor_row,
                                  &sx, &sy, &ex, &ey);

  video::screen.invert_rect (sx, sy, ex, ey);

  self->_cursor_visible = !self->_cursor_visible;
}

int term_write(term_c* self, unicode_char* buf, int count) {
  int start, end, i;
  unicode_char c;

  video::screen.hide_mouse();
  term_show(self);

  start = end = 0;

  while (end < count) {
    i = start;
    end = count;

    if (self->_param_num == -2) {  // not inside an escape sequence

      while (i < end) {
        c = buf[i++];
        if (c == 0x08 || c == 0x0a || c == 0x0d || c == 0x1b)  // special char?
        {
          i--;  // only process characters before the special character
          break;
        }
      }
    }

    if (i > start) {
      end = i;  // stop drawing characters at the first special one
    } else {
      while (i < end) {
        int op = -999;  // noop
        int arg = 0;
        int pn = self->_param_num;

        c = buf[i++];

        switch (pn) {
          case -2:
            if (c == 0x08) {         // backspace character?
              op = -1;               // move cursor horizontally
              arg = -1;              // one column left
            } else if (c == 0x0a) {  // linefeed character?

              term_hide_cursor(self);
              self->_cursor_column = 0;

              if (self->_cursor_row == self->_nb_rows - 1) {  // on last row?
                term_scroll_up(self);
              } else {
                op = 0;   // move cursor vertically
                arg = 1;  // one row down
              }

            } else if (c == 0x0d)  // carriage return character?
            {
              op = self->_cursor_row + 1;  // move cursor on same row
              arg = 1;                     // to leftmost column
            } else if (c == 0x1b)          // ESC character?
              self->_param_num = -1;
            else {
              end = i - 1;  // special character processing is done
            }
            break;

          case -1:
            if (c == '[') {
              self->_param_num = 0;
              self->_param[0] = 0;
            } else {
              self->_param_num = -2;
            }
            break;

          default:
            if (c >= '0' && c <= '9') {
              int x = c - '0';
              int p = self->_param[pn];
              if (p < 1000) {
                self->_param[pn] = p * 10 + x;
              }

            } else if (c == ';') {
              pn++;
              if (pn < term_max_nb_params) {
                self->_param_num = pn;
                self->_param[pn] = 0;
              }
            } else {
              self->_param_num = -2;

              if (c == 'A') {
                op = 0;  // move cursor vertically
                arg = -self->_param[0];
                if (arg >= 0) arg = -1;
              } else if (c == 'B') {
                op = 0;  // move cursor vertically
                arg = -self->_param[0];
                if (arg <= 0) arg = 1;
              } else if (c == 'C') {
                op = -1;  // move cursor horizontally
                arg = -self->_param[0];
                if (arg <= 0) arg = 1;
              } else if (c == 'D') {
                op = -1;  // move cursor horizontally
                arg = -self->_param[0];
                if (arg >= 0) arg = -1;
              } else if (c == 'H' || c == 'f') {
                op = self->_param[0];   // move cursor, op = row
                arg = self->_param[1];  // arg = column
                if (op <= 0) op = 1;
                if (pn < 1 || arg <= 0) arg = 1;
              } else if (c == 'J') {
                op = -2;  // clear characters
                arg = self->_param[0];
                if (arg <= 0) arg = 0;
              } else if (c == 'K') {
                op = -3;  // clear characters
                arg = self->_param[0];
                if (arg <= 0) arg = 0;
              } else if (c == 'm') {
                int j;
                op = -4;  // set attributes
                for (j = 0; j <= pn; j++) {
                  int x = self->_param[j];
                  if (x <= 0) {
                    self->_bold = FALSE;
                    self->_underline = FALSE;
                    self->_reverse = FALSE;
                    self->_fg = term_normal_foreground;
                    self->_bg = term_normal_background;
                  } else if (x == 1) {
                    self->_bold = TRUE;
                  } else if (x == 4) {
                    self->_underline = TRUE;
                  } else if (x == 7) {
                    self->_reverse = TRUE;
                  } else if (x >= 30 && x <= 37) {
                    self->_fg = x - 30;
                  } else if (x >= 40 && x <= 47) {
                    self->_bg = x - 40;
                  }
                }
              }
            }
        }

        // execute appropriate operation as indicated by "op" and "arg"

        if (op != -999)  // not noop?
        {
          if (op >= -1)  // move cursor?
          {
            term_hide_cursor(self);

            if (op <= 0) {
              if (op == 0) {
                self->_cursor_row += arg;
              } else {
                self->_cursor_column += arg;
              }
            } else {
              self->_cursor_row = op - 1;
              self->_cursor_column = arg - 1;
            }

            if (self->_cursor_row < 0) {
              self->_cursor_row = 0;
            } else if (self->_cursor_row >= self->_nb_rows) {
              self->_cursor_row = self->_nb_rows - 1;
            }

            if (self->_cursor_column < 0) {
              self->_cursor_column = 0;
            } else if (self->_cursor_column >= self->_nb_columns) {
              self->_cursor_column = self->_nb_columns - 1;
            }
          } else if (op >= -3) {  // clear characters
            if (arg <= 2) {
              int sx;
              int sy;
              int ex;
              int ey;
              int csx;
              int csy;
              int cex;
              int cey;
              pattern* background;

              term_color_to_pattern(self, term_normal_background, &background);

              term_char_coord_to_screen_coord(self, 0, 0, &sx, &sy, &cex, &cey);

              term_char_coord_to_screen_coord(self, self->_nb_columns - 1,
                                              self->_nb_rows - 1, &csx, &csy,
                                              &ex, &ey);

              term_char_coord_to_screen_coord(self, self->_cursor_column,
                                              self->_cursor_row, &csx, &csy,
                                              &cex, &cey);

              if (arg != 1) term_hide_cursor(self);

              if (op == -2 && arg != 0) {
                video::screen.fill_rect(sx, sy, ex, csy, background);
              }

              video::screen.fill_rect((arg == 0) ? csx : sx, csy,
                                      (arg == 1) ? csx : ex, cey, background);

              if (op == -2 && arg != 1)
                video::screen.fill_rect(sx, cey, ex, ey, background);
            }
          } else if (op == -4) {  // set attributes
            // note: attributes are handled when characters
            // are displayed
          }
        }
      }

      start = end;
    }
    // All escape sequence and special character processing is
    // done.

    while (start < end) {
      int sx;
      int sy;
      int ex;
      int ey;
      int fg;
      int bg;
      pattern* foreground;
      pattern* background;
      int n = end - start;

      if (n > self->_nb_columns - self->_cursor_column)  // one line at a time
        n = self->_nb_columns - self->_cursor_column;

      if (self->_reverse) {
        fg = self->_bg;
        bg = self->_fg;
      } else {
        fg = self->_fg;
        bg = self->_bg;
      }

      term_char_coord_to_screen_coord(self, self->_cursor_column,
                                      self->_cursor_row, &sx, &sy, &ex, &ey);

      term_color_to_pattern(self, fg, &foreground);
      term_color_to_pattern(self, bg, &background);

      term_hide_cursor(self);

      self->_fn->draw_text(&video::screen, sx, sy, buf + start, n, foreground,
                           background);

      start += n;

      self->_cursor_column += n;

      if (self->_cursor_column >= self->_nb_columns) {
        self->_cursor_column = 0;
        self->_cursor_row++;
        if (self->_cursor_row >= self->_nb_rows) {
          term_scroll_up(self);
          self->_cursor_row = self->_nb_rows - 1;
        }
      }
    }

    end = start;
  }

  if (!self->_cursor_visible) {
    term_show_cursor(self);
  }

  video::screen.show_mouse();
  return end;
}

void term_scroll_up(term_c* self) {
  int x0, y0, x1, y1, x2, y2, x3, y3;

  pattern* background;

  term_char_coord_to_screen_coord(self, 0, 0, &x0, &y0, &x1, &y1);
  term_char_coord_to_screen_coord(self, self->_nb_columns - 1,
                                  self->_nb_rows - 1, &x2, &y2, &x3, &y3);

  video::screen.bitblt(x0, y0, x3, y2, &video::screen, x0, y1, &pattern::white,
                       &pattern::black);

  term_color_to_pattern(self, term_normal_background, &background);

  video::screen.fill_rect(x0, y2, x3, y3, background);
}

term_c* term_write(term_c* self, bool x) {
  return term_write(self, x ? L"TRUE" : L"FALSE");
}

term_c* term_write(term_c* self, int8 x) {
  return term_write(self, CAST(int32, x));
}

term_c* term_write(term_c* self, int16 x) {
  return term_write(self, CAST(int32, x));
}

term_c* term_write(term_c* self, int32 x) {
  if (x < 0) {
    return term_write(term_write(self, L"-"), CAST(uint32, -x));
  } else {
    return term_write(self, CAST(uint32, x));
  }
}

term_c* term_write(term_c* self, int64 x) {
  return term_write(self, CAST(uint64, x));
}

term_c* term_write(term_c* self, uint8 x) {
  return term_write(self, CAST(uint32, x));
}

term_c* term_write(term_c* self, uint16 x) {
  return term_write(self, CAST(uint32, x));
}

term_c* term_write(term_c* self, uint32 x) {
  const int max_digits = 10;  // 2^32 contains 10 decimal digits
  unicode_char buf[max_digits + 1];
  unicode_char* str = buf + max_digits;

  *str = '\0';

  if (x == 0)
    *--str = '0';
  else {
    while (x != 0) {
      uint32 x10 = x / 10;
      *--str = '0' + (x - x10 * 10);
      x = x10;
    }
  }

  return term_write(self, str);
}

term_c* term_write(term_c* self, uint64 x) {
  const int max_digits = 20;  // 2^64 contains 20 decimal digits
  unicode_char buf[max_digits + 1];
  unicode_char* str = buf + max_digits;

  *str = '\0';

  if (x == 0)
    *--str = '0';
  else {
    while (x != 0) {
      uint64 x10 = x / 10;
      *--str = '0' + (x - x10 * 10);
      x = x10;
    }
  }

  return term_write(self, str);
}

term_c* term_write(term_c* self, void* x) {
  const int nb_digits = 8;  // 32 bit pointer contains 8 hexadecimal digits
  unicode_char buf[2 + nb_digits + 1];
  unicode_string str = buf + 2 + nb_digits;
  uint32 n = CAST(uint32, x);
  int i;

  *str = '\0';

  for (i = 0; i < nb_digits; i++) {
    *--str = "0123456789abcdef"[n & 0xf];
    n = n >> 4;
  }

  *--str = 'x';
  *--str = '0';

  return term_write(self, str);
}

term_c* term_write(term_c* self, native_string x) {
  unicode_char buf[2];

  buf[1] = '\0';

  while (*x != '\0') {
    buf[0] = CAST(uint8, *x++);
    term_write(self, buf);
  }

  return self;
}

term_c* term_write(term_c* self, unicode_string x) {
  int n = 0;

  while (x[n] != '\0') n++;

  term_write(self, x, n);

  return self;
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
