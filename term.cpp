// file: "term.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "term.h"

//-----------------------------------------------------------------------------

// "term" class implementation.

term term::console (0, 0, 80, 30, &font::mono_6x9, L"console");

term::term (int x,
            int y,
            int nb_columns,
            int nb_rows,
            font* fn,
            unicode_string title,
            bool initially_visible)
{
  _x = x;
  _y = y;
  _nb_columns = nb_columns;
  _nb_rows = nb_rows;
  _fn = fn;
  _title = title;
  _visible = FALSE;
  _cursor_column = 0;
  _cursor_row = 0;
  _cursor_visible = FALSE;

  _param_num = -2;
  _bold = FALSE;
  _underline = FALSE;
  _reverse = FALSE;
  _fg = term_normal_foreground;
  _bg = term_normal_background;

  if (initially_visible)
    show ();
}

void term::show ()
{
  if (!_visible)
    {
      int sx;
      int sy;
      int ex;
      int ey;
      int char_height = _fn->get_height ();
      pattern* background;

      char_coord_to_screen_coord (_nb_columns-1, _nb_rows-1, sx, sy, ex, ey);

      color_to_pattern (term_normal_background, background);

      video::screen.frame_rect
        (_x,
         _y,
         ex + term_outer_border + term_frame_border + term_inner_border,
         ey + term_outer_border + term_frame_border + term_inner_border,
         term_outer_border,
         &pattern::white);

      video::screen.frame_rect
        (_x + term_outer_border,
         _y + term_outer_border,
         ex + term_frame_border + term_inner_border,
         ey + term_frame_border + term_inner_border,
         term_frame_border,
         &pattern::gray50);

      video::screen.fill_rect
        (_x + term_outer_border + term_frame_border,
         _y + char_height + term_outer_border + term_frame_border + 2*term_inner_border,
         ex + term_inner_border,
         _y + char_height + term_outer_border + 2*term_frame_border + 2*term_inner_border,
         &pattern::gray50);

      video::screen.fill_rect
        (_x + term_outer_border + term_frame_border,
         _y + term_outer_border + term_frame_border,
         ex + term_inner_border,
         _y + char_height + term_outer_border + term_frame_border + 2*term_inner_border,
         &pattern::black);

      _fn->draw_string
        (&video::screen,
         _fn->draw_string
           (&video::screen,
            _x + term_outer_border + term_frame_border + term_inner_border,
            _y + term_outer_border + term_frame_border + term_inner_border,
            L"\x25b6 ", // rightward triangle and space
            &pattern::blue,
            &pattern::black),
         _y + term_outer_border + term_frame_border + term_inner_border,
         _title,
         &pattern::blue,
         &pattern::black);

      video::screen.fill_rect
        (_x + term_outer_border + term_frame_border,
         _y + char_height + term_outer_border + 2*term_frame_border + 2*term_inner_border,
         ex + term_inner_border,
         ey + term_inner_border,
         background);

      show_cursor ();

      _visible = TRUE;
    }
}

void term::show_cursor ()
{
  if (!_cursor_visible)
    toggle_cursor ();
}

void term::hide_cursor ()
{
  if (_cursor_visible)
    toggle_cursor ();
}

void term::toggle_cursor ()
{
  int sx;
  int sy;
  int ex;
  int ey;

  char_coord_to_screen_coord (_cursor_column, _cursor_row, sx, sy, ex, ey);

  video::screen.invert_rect (sx, sy, ex, ey);

  _cursor_visible = !_cursor_visible;
}

void term::scroll_up ()
{
  int x0;
  int y0;
  int x1;
  int y1;
  int x2;
  int y2;
  int x3;
  int y3;
  pattern* background;

  char_coord_to_screen_coord (0, 0, x0, y0, x1, y1);
  char_coord_to_screen_coord (_nb_columns-1, _nb_rows-1, x2, y2, x3, y3);

  video::screen.bitblt (x0,
                        y0,
                        x3,
                        y2,
                        &video::screen,
                        x0,
                        y1,
                        &pattern::white,
                        &pattern::black);

  color_to_pattern (term_normal_background, background);

  video::screen.fill_rect (x0, y2, x3, y3, background);
}

int term::write (unicode_char* buf, int count)
{
  unicode_char c;
  int start;
  int end;
  int i;

  video::screen.hide_mouse ();

  show ();

  start = 0;
  end = 0;

  while (end < count)
    {
      i = start;
      end = count;

      if (_param_num == -2) // not inside an escape sequence
        {
          while (i < end)
            {
              c = buf[i++];
              if (c==0x08 || c==0x0a || c==0x0d || c==0x1b) // special char?
                {
                  i--; // only process characters before the special character
                  break;
                }
            }
        }

      if (i > start)
        end = i; // stop drawing characters at the first special one
      else
        {
          while (i < end)
            {
              int op = -999; // noop
              int arg = 0;
              int pn = _param_num;

              c = buf[i++];

              switch (pn)
                {
                case -2:
                  if (c == 0x08) // backspace character?
                    {
                      op = -1; // move cursor horizontally
                      arg = -1; // one column left
                    }
                  else if (c == 0x0a) // linefeed character?
                    {
                      hide_cursor ();
                      _cursor_column = 0;
                      if (_cursor_row == _nb_rows-1) // on last row?
                        scroll_up ();
                      else
                        {
                          op = 0; // move cursor vertically
                          arg = 1; // one row down
                        }
                    }
                  else if (c == 0x0d) // carriage return character?
                    {
                      op = _cursor_row+1; // move cursor on same row
                      arg = 1; // to leftmost column
                    }
                  else if (c == 0x1b) // ESC character?
                    _param_num = -1;
                  else
                    end = i-1; // special character processing is done
                  break;

                case -1:
                  if (c == '[')
                    {
                      _param_num = 0;
                      _param[0] = 0;
                    }
                  else
                    _param_num = -2;
                  break;

                default:
                  if (c >= '0' && c <= '9')
                    {
                      int x = c - '0';
                      int p = _param[pn];
                      if (p < 1000)
                        _param[pn] = p*10 + x;
                    }
                  else if (c == ';')
                    {
                      pn++;
                      if (pn < term_max_nb_params)
                        {
                          _param_num = pn;
                          _param[pn] = 0;
                        }
                    }
                  else
                    {
                      _param_num = -2;

                      if (c == 'A')
                        {
                          op = 0; // move cursor vertically
                          arg = -_param[0];
                          if (arg >= 0) arg = -1;
                        }
                      else if (c == 'B')
                        {
                          op = 0; // move cursor vertically
                          arg = _param[0];
                          if (arg <= 0) arg = 1;
                        }
                      else if (c == 'C')
                        {
                          op = -1; // move cursor horizontally
                          arg = _param[0];
                          if (arg <= 0) arg = 1;
                        }
                      else if (c == 'D')
                        {
                          op = -1; // move cursor horizontally
                          arg = -_param[0];
                          if (arg >= 0) arg = -1;
                        }
                      else if (c == 'H' || c == 'f')
                        {
                          op = _param[0]; // move cursor, op = row
                          arg = _param[1]; // arg = column
                          if (op <= 0) op = 1;
                          if (pn < 1 || arg <= 0) arg = 1;
                        }
                      else if (c == 'J')
                        {
                          op = -2; // clear characters
                          arg = _param[0];
                          if (arg <= 0) arg = 0;
                        }
                      else if (c == 'K')
                        {
                          op = -3; // clear characters
                          arg = _param[0];
                          if (arg <= 0) arg = 0;
                        }
                      else if (c == 'm')
                        {
                          int j;
                          op = -4; // set attributes
                          for (j=0; j<=pn; j++)
                            {
                              int x = _param[j];
                              if (x <= 0)
                                {
                                  _bold = FALSE;
                                  _underline = FALSE;
                                  _reverse = FALSE;
                                  _fg = term_normal_foreground;
                                  _bg = term_normal_background;
                                }
                              else if (x == 1)
                                _bold = TRUE;
                              else if (x == 4)
                                _underline = TRUE;
                              else if (x == 7)
                                _reverse = TRUE;
                              else if (x >= 30 && x <= 37)
                                _fg = x-30;
                              else if (x >= 40 && x <= 47)
                                _bg = x-40;
                            }
                        }
                    }
                }

              // execute appropriate operation as indicated by "op" and "arg"

              if (op != -999) // not noop?
                {
                  if (op >= -1) // move cursor?
                    {
                      hide_cursor ();

                      if (op <= 0)
                        {
                          if (op == 0)
                            _cursor_row += arg;
                          else
                            _cursor_column += arg;
                        }
                      else
                        {
                          _cursor_row = op-1;
                          _cursor_column = arg-1;
                        }

                      if (_cursor_row < 0)
                        _cursor_row = 0;
                      else if (_cursor_row >= _nb_rows)
                        _cursor_row = _nb_rows-1;

                      if (_cursor_column < 0)
                        _cursor_column = 0;
                      else if (_cursor_column >= _nb_columns)
                        _cursor_column = _nb_columns-1;
                    }
                  else if (op >= -3) // clear characters
                    {
                      if (arg <= 2)
                        {
                          int sx;
                          int sy;
                          int ex;
                          int ey;
                          int csx;
                          int csy;
                          int cex;
                          int cey;
                          pattern* background;

                          color_to_pattern (term_normal_background, background);

                          char_coord_to_screen_coord
                            (0, 0, sx, sy, cex, cey);

                          char_coord_to_screen_coord
                            (_nb_columns-1, _nb_rows-1, csx, csy, ex, ey);

                          char_coord_to_screen_coord
                            (_cursor_column, _cursor_row, csx, csy, cex, cey);

                          if (arg != 1)
                            hide_cursor ();

                          if (op == -2 && arg != 0)
                            video::screen.fill_rect
                              (sx, sy, ex, csy, background);

                          video::screen.fill_rect
                            ((arg == 0) ? csx : sx,
                             csy,
                             (arg == 1) ? csx : ex,
                             cey,
                             background);

                          if (op == -2 && arg != 1)
                            video::screen.fill_rect
                              (sx, cey, ex, ey, background);
                        }
                    }
                  else if (op == -4) // set attributes
                    {
                      // note: attributes are handled when characters
                      // are displayed
                    }
                }
            }

          start = end;

          // All escape sequence and special character processing is
          // done.
        }

      while (start < end)
        {
          int sx;
          int sy;
          int ex;
          int ey;
          int fg;
          int bg;
          pattern* foreground;
          pattern* background;
          int n = end - start;

          if (n > _nb_columns - _cursor_column) // one line at a time
            n = _nb_columns - _cursor_column;

          if (_reverse)
            {
              fg = _bg;
              bg = _fg;
            }
          else
            {
              fg = _fg;
              bg = _bg;
            }

          char_coord_to_screen_coord
            (_cursor_column, _cursor_row, sx, sy, ex, ey);

          color_to_pattern (fg, foreground);
          color_to_pattern (bg, background);

          hide_cursor ();

          _fn->draw_text
            (&video::screen, sx, sy, buf+start, n, foreground, background);

          start += n;

          _cursor_column += n;

          if (_cursor_column >= _nb_columns)
            {
              _cursor_column = 0;
              _cursor_row++;
              if (_cursor_row >= _nb_rows)
                {
                  scroll_up ();
                  _cursor_row = _nb_rows-1;
                }
            }
        }

      end = start;
    }

  if (!_cursor_visible)
    show_cursor ();

  video::screen.show_mouse ();

  return end;
}

void term::char_coord_to_screen_coord (int column,
                                       int row,
                                       int& sx,
                                       int& sy,
                                       int& ex,
                                       int& ey)
{
  int char_max_width = _fn->get_max_width ();
  int char_height = _fn->get_height ();
  sx = _x + column*char_max_width + term_outer_border + term_frame_border + term_inner_border;
  sy = _y + row*char_height + term_outer_border + 2*term_frame_border + 3*term_inner_border
       + char_height;
  ex = sx + char_max_width;
  ey = sy + char_height;
}

void term::color_to_pattern (int color, pattern*& pat)
{
  switch (color)
    {
    case 0: pat = &pattern::black;   break;
    case 1: pat = &pattern::red;     break;
    case 2: pat = &pattern::green;   break;
    case 3: pat = &pattern::yellow;  break;
    case 4: pat = &pattern::blue;    break;
    case 5: pat = &pattern::magenta; break;
    case 6: pat = &pattern::cyan;    break;
    case 7: pat = &pattern::white;   break;
    }
}

term& operator<< (term& t, bool x)
{
  return t << (x ? L"TRUE" : L"FALSE");
}

term& operator<< (term& t, int8 x)
{
  return t << CAST(int32,x);
}

term& operator<< (term& t, int16 x)
{
  return t << CAST(int32,x);
}

term& operator<< (term& t, int32 x)
{
  if (x < 0)
    return t << L"-" << CAST(uint32,-x);
  else
    return t << CAST(uint32,x);
}

term& operator<< (term& t, int64 x)
{
  return t << CAST(uint64,x);
}

term& operator<< (term& t, uint8 x)
{
  return t << CAST(uint32,x);
}

term& operator<< (term& t, uint16 x)
{
  return t << CAST(uint32,x);
}

term& operator<< (term& t, uint32 x)
{
  const int max_digits = 10; // 2^32 contains 10 decimal digits
  unicode_char buf[max_digits+1];
  unicode_char* str = buf + max_digits;

  *str = '\0';

  if (x == 0)
    *--str = '0';
  else
    {
      while (x != 0)
        {
          uint32 x10 = x / 10;
          *--str = '0' + (x - x10*10);
          x = x10;
        }
    }

  return t << str;
}

term& operator<< (term& t, uint64 x)
{
  const int max_digits = 20; // 2^64 contains 20 decimal digits
  unicode_char buf[max_digits+1];
  unicode_char* str = buf + max_digits;

  *str = '\0';

  if (x == 0)
    *--str = '0';
  else
    {
      while (x != 0)
        {
          uint64 x10 = x / 10;
          *--str = '0' + (x - x10*10);
          x = x10;
        }
    }

  return t << str;
}

term& operator<< (term& t, void* x)
{
  const int nb_digits = 8; // 32 bit pointer contains 8 hexadecimal digits
  unicode_char buf[2+nb_digits+1];
  unicode_string str = buf + 2 + nb_digits;
  uint32 n = CAST(uint32,x);
  int i;

  *str = '\0';

  for (i=0; i<nb_digits; i++)
    {
      *--str = "0123456789abcdef"[n & 0xf];
      n = n >> 4;
    }

  *--str = 'x';
  *--str = '0';

  return t << str;
}

term& operator<< (term& t, native_string x)
{
  unicode_char buf[2];

  buf[1] = '\0';

  while (*x != '\0')
    {
      buf[0] = CAST(uint8,*x++);
      t << buf;
    }

  return t;
}

term& operator<< (term& t, unicode_string x)
{
  int n = 0;

  while (x[n] != '\0')
    n++;

  t.write (x, n);

  return t;
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
