// file: "term.h"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef TERM_H
#define TERM_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "video.h"

//-----------------------------------------------------------------------------

// "term" class declaration.

class term
  {
  public:

    term (int x,
          int y,
          int nb_columns,
          int nb_rows,
          font* fn,
          unicode_string title,
          bool initially_visible = TRUE);

    void show ();

    int write (unicode_char* buf, int count);

    static term console;

  protected:

    void char_coord_to_screen_coord (int column,
                                     int row,
                                     int& sx,
                                     int& sy,
                                     int& ex,
                                     int& ey);

    void color_to_pattern (int color, pattern*& pat);

    void show_cursor ();
    void hide_cursor ();
    void toggle_cursor ();

    void scroll_up ();

    static const int outer_border = 1;
    static const int frame_border = 2;
    static const int inner_border = 2;

    int _x;
    int _y;
    int _nb_columns;
    int _nb_rows;
    font* _fn;
    unicode_string _title;
    int _cursor_column;
    int _cursor_row;
    bool _cursor_visible;
    bool _visible;

    // for vt100 emulation:

    static const int max_nb_params = 10;
    static const int normal_foreground = 0; // black
    static const int normal_background = 7; // white

    int _param[max_nb_params];
    int _param_num;
    bool _bold;
    bool _underline;
    bool _reverse;
    int _fg;
    int _bg;
  };

term& operator<< (term& t, bool x);
term& operator<< (term& t, int8 x);
term& operator<< (term& t, int16 x);
term& operator<< (term& t, int32 x);
term& operator<< (term& t, int64 x);
term& operator<< (term& t, uint8 x);
term& operator<< (term& t, uint16 x);
term& operator<< (term& t, uint32 x);
term& operator<< (term& t, uint64 x);
term& operator<< (term& t, void* x);
term& operator<< (term& t, native_string x);
term& operator<< (term& t, unicode_string x);

#define cout term::console

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
