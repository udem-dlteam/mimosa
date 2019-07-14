// file: "video.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "video.h"
#include "asm.h"
#include "vga.h"
#include "term.h"



// "pattern" class implementation.

static bitmap_word black_bitmap_words[] = {0x00, 0x00, 0x00, 0x00,
                                           0x00, 0x00, 0x00, 0x00};

pattern pattern_black = new_pattern(black_bitmap_words, 8, 1);

static bitmap_word gray25_bitmap_words[] = {0x00, 0x55, 0x00, 0x55,
                                            0x00, 0x55, 0x00, 0x55};

pattern pattern_gray25 = new_pattern(gray25_bitmap_words, 8, 1);

static bitmap_word gray50_bitmap_words[] = {0xaa, 0x55, 0xaa, 0x55,
                                            0xaa, 0x55, 0xaa, 0x55};

pattern pattern_gray50 = new_pattern(gray50_bitmap_words, 8, 1);

static bitmap_word gray75_bitmap_words[] = {0xff, 0x55, 0xff, 0x55,
                                            0xff, 0x55, 0xff, 0x55};

pattern pattern_gray75 = new_pattern(gray75_bitmap_words, 8, 1);

static bitmap_word white_bitmap_words[] = {0xff, 0xff, 0xff, 0xff,
                                           0xff, 0xff, 0xff, 0xff};

pattern pattern_white = new_pattern(white_bitmap_words, 8, 1);

static bitmap_word red_bitmap_words[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

pattern pattern_red = new_pattern(red_bitmap_words, 8, 4);

static bitmap_word green_bitmap_words[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

pattern pattern_green = new_pattern(green_bitmap_words, 8, 4);

static bitmap_word yellow_bitmap_words[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

pattern pattern_yellow = new_pattern(yellow_bitmap_words, 8, 4);

static bitmap_word blue_bitmap_words[] = {
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

pattern pattern_blue = new_pattern(blue_bitmap_words, 8, 4);

static bitmap_word magenta_bitmap_words[] = {
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

pattern pattern_magenta = new_pattern(magenta_bitmap_words, 8, 4);

static bitmap_word cyan_bitmap_words[] = {
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

pattern pattern_cyan = new_pattern(cyan_bitmap_words, 8, 4);

//-----------------------------------------------------------------------------

// "raw_bitmap" class implementation.

#define MOUSE_WIDTH  8
#define MOUSE_WIDTH_IN_BITMAP_WORDS ((MOUSE_WIDTH+BITMAP_WORD_WIDTH-1) >> LOG2_BITMAP_WORD_WIDTH)
#define MOUSE_HEIGHT 12

//-----------------------------------------------------------------------------
video_c v_screen = new_video(18);

static bitmap_word mouse_bitmap[MOUSE_WIDTH_IN_BITMAP_WORDS * MOUSE_HEIGHT * 4];
raw_bitmap_in_memory_c v_mouse_save = new_raw_bitmap_in_memory(
    mouse_bitmap, MOUSE_WIDTH_IN_BITMAP_WORDS << LOG2_BITMAP_WORD_WIDTH,
    MOUSE_HEIGHT, 4);
//-----------------------------------------------------------------------------

video::video (int mode)
{
  _mode = mode;

  switch (mode)
    {
    case 17:
      _start = CAST(bitmap_word*,0xa0000);
      _width = 640;
      _height = 480;
      _depth = 1;
      break;

    default:
    case 18:
      _start = CAST(bitmap_word*,0xa0000);
      _width = 640;
      _height = 480;
      _depth = 4;
      break;
    }

  _mouse_x = _width/2;
  _mouse_y = _height/2;
  _mouse_hides = 1;

  screen.fill_rect (0,
                    0,
                    _width,
                    _height,
                    &pattern_gray50);

  //show_mouse ();
}

void video::hide_mouse ()
{
  _mouse_hides++;

  if (_mouse_hides == 1)
    {
      int width;
      int height;

      get_mouse_rect (&width, &height);

      screen.bitblt (_mouse_x,
                     _mouse_y,
                     _mouse_x + width,
                     _mouse_y + height,
                     &mouse_save,
                     0,
                     0,
                     &pattern_white,
                     &pattern_black);
    }
}

void video::show_mouse ()
{
  if (_mouse_hides == 1)
    {
      int width;
      int height;

      get_mouse_rect (&width, &height);

      mouse_save.bitblt (0,
                         0,
                         width,
                         height,
                         &screen,
                         _mouse_x,
                         _mouse_y,
                         &pattern_white,
                         &pattern_black);

      draw_mouse ();
    }

  _mouse_hides--;
}

bitmap_word* video::select_layer (int layer)
{
  layer = layer % _depth;

  if (_mode == 18)
    {
      outb (VGA_MAP_MASK_REG, VGA_PORT_SEQ_INDEX);
      outb (1 << layer, VGA_PORT_SEQ_DATA);
      outb (VGA_READ_MAP_SELECT_REG, VGA_PORT_GRCTRL_INDEX);
      outb (layer, VGA_PORT_GRCTRL_DATA);
    }

  return _start;
}

static void clip (int* x, int min_val, int max_val)
{
  if (*x < min_val)
    *x = min_val;
  else if (*x > max_val)
    *x = max_val;
}

void video::move_mouse (int dx, int dy)
{
  hide_mouse ();

  _mouse_x += dx;
  _mouse_y += dy;

  clip (&_mouse_x, 0, _width);
  clip (&_mouse_y, 0, _height);

  show_mouse ();
}

void video::get_mouse_rect (int* width, int* height)
{
  *width = _width - _mouse_x;
  *height = _height - _mouse_y;

  clip (width, 0, MOUSE_WIDTH);
  clip (height, 0, MOUSE_HEIGHT);
}

void video::draw_mouse ()
{
#define minimum(a,b) (((a)<(b))?(a):(b))

  int x = _mouse_x;
  int y = _mouse_y;
  int width;
  int height;

  get_mouse_rect (&width, &height);

  if (width < 1) return;
  fill_rect (x+0, y+0, x+1, y+minimum(11,height), &pattern_red);
  if (width < 2) return;
  fill_rect (x+1, y+1, x+2, y+minimum(10,height), &pattern_red);
  if (width < 3) return;
  fill_rect (x+2, y+2, x+3, y+minimum(9,height), &pattern_red);
  if (width < 4) return;
  fill_rect (x+3, y+3, x+4, y+minimum(10,height), &pattern_red);
  if (width < 5) return;
  fill_rect (x+4, y+4, x+5, y+minimum(12,height), &pattern_red);
  if (width < 6) return;
  fill_rect (x+5, y+5, x+6, y+minimum(8,height), &pattern_red);
  fill_rect (x+5, y+10, x+6, y+minimum(14,height), &pattern_red);
  if (width < 7) return;
  fill_rect (x+6, y+6, x+7, y+minimum(8,height), &pattern_red);
  fill_rect (x+6, y+12, x+7, y+minimum(14,height), &pattern_red);
  if (width < 8) return;
  fill_rect (x+7, y+7, x+8, y+minimum(8,height), &pattern_red);
}


raw_bitmap_in_memory video::mouse_save
  (mouse_bitmap,
   MOUSE_WIDTH_IN_BITMAP_WORDS << LOG2_BITMAP_WORD_WIDTH,
   MOUSE_HEIGHT,
   4);

video video::screen (18);

raw_bitmap_in_memory::raw_bitmap_in_memory
  (bitmap_word* start,
   int width,
   int height,
   int depth)
{
  _start = start;
  _width = width;
  _height = height;
  _depth = depth;
}

void raw_bitmap_in_memory::hide_mouse ()
{
}

void raw_bitmap_in_memory::show_mouse ()
{
}

bitmap_word* raw_bitmap_in_memory::select_layer (int layer)
{
  layer = layer % _depth;
  return _start + (_width >> LOG2_BITMAP_WORD_WIDTH) * _height * layer;
}

// "bitblt" is the "BIT BLock Transfer" function which transfers a
// rectangular area of a source bitmap to a destination bitmap.

#define COMBINE_BITS(src1,src2,mask) \
(((src1) & ~(mask)) | ((src2) & (mask)))
#define COMBINE_WORDS(src1,src2) \
(src2)

#define COMBINE_BITS_FG_BG(src1,src2,mask,fg,bg) \
(((src1) & ~(mask)) | ((((fg) & (src2)) | ((bg) & ~(src2))) & (mask)))
#define COMBINE_WORDS_FG_BG(src1,src2,fg,bg) \
(((fg) & (src2)) | ((bg) & ~(src2)))

void raw_bitmap::bitblt (int x,
                         int y,
                         int x_end,
                         int y_end,
                         raw_bitmap* src,
                         int src_x,
                         int src_y,
                         pattern* foreground,
                         pattern* background)
{
  if (x < x_end && y < y_end)
    {
      int realignment = (((x & (BITMAP_WORD_WIDTH-1))
                          - (src_x & (BITMAP_WORD_WIDTH-1)))
                         & (BITMAP_WORD_WIDTH*2-1)) ^ BITMAP_WORD_WIDTH;
      int nb_words_per_row = ((x_end-1) >> LOG2_BITMAP_WORD_WIDTH)
                             - (x >> LOG2_BITMAP_WORD_WIDTH);
      int nb_rows = y_end - y;
      int row;
      int layer;

      hide_mouse ();
      src->hide_mouse ();

      if (nb_words_per_row > 0)
        {
          for (row=nb_rows; row>0; row--)
            {
              for (layer=_depth-1; layer>=0; layer--)
                {
                  bitmap_word fg = pattern_get_word(foreground, y, layer);
                  bitmap_word bg = pattern_get_word(background, y, layer);

                  bitmap_word* s = src->select_layer (layer)
                                   + ((src_y*src->_width + src_x)
                                      >> LOG2_BITMAP_WORD_WIDTH);
                  bitmap_word* d = select_layer (layer)
                                   + ((y*_width + x)
                                      >> LOG2_BITMAP_WORD_WIDTH);
                  bitmap_quad_word b;
                  bitmap_word m;
                  int col;

                  b = (CAST(bitmap_quad_word,s[0]) << BITMAP_WORD_WIDTH)
                      | s[1];
                  s += 2;
                  m = CAST(bitmap_word,-1) >> (x & (BITMAP_WORD_WIDTH-1));
                  *d = COMBINE_BITS_FG_BG(*d, b>>realignment, m, fg, bg);

                  for (col=nb_words_per_row-1; col>0; col--)
                    {
                      b = (b << BITMAP_WORD_WIDTH) | *s++;
                      d++;
                      *d = COMBINE_WORDS_FG_BG(*d, b>>realignment, fg, bg);
                    }

                  m = CAST(bitmap_word,-1)
                      << ((-x_end) & (BITMAP_WORD_WIDTH-1));
                  b = (b << BITMAP_WORD_WIDTH) | *s;
                  d++;
                  *d = COMBINE_BITS_FG_BG(*d, b>>realignment, m, fg, bg);
                }

              src_y++;
              y++;
            }
      } else {
        for (row = nb_rows; row > 0; row--) {
          for (layer = _depth - 1; layer >= 0; layer--) {

            bitmap_word fg = pattern_get_word(foreground, y, layer);
            bitmap_word bg = pattern_get_word(background, y, layer);

            bitmap_word* s =
                src->select_layer(layer) +
                ((src_y * src->_width + src_x) >> LOG2_BITMAP_WORD_WIDTH);
            bitmap_word* d = select_layer(layer) +
                             ((y * _width + x) >> LOG2_BITMAP_WORD_WIDTH);
            bitmap_quad_word b;
            bitmap_word m;

            b = (CAST(bitmap_quad_word, s[0]) << BITMAP_WORD_WIDTH) | s[1];
            m = (CAST(bitmap_word, -1) >> (x & (BITMAP_WORD_WIDTH - 1))) &
                (CAST(bitmap_word, -1) << ((-x_end) & (BITMAP_WORD_WIDTH - 1)));
            *d = COMBINE_BITS_FG_BG(*d, b >> realignment, m, fg, bg);
          }

          src_y++;
          y++;
        }
      }

      show_mouse ();
      src->show_mouse ();
    }
}

void raw_bitmap::fill_rect (int x,
                            int y,
                            int x_end,
                            int y_end,
                            pattern* foreground)
{
  if (x < x_end && y < y_end)
    {
      int nb_words_per_row = ((x_end-1) >> LOG2_BITMAP_WORD_WIDTH) -
                             (x >> LOG2_BITMAP_WORD_WIDTH);
      int nb_rows = y_end - y;
      int row;
      int layer;

      hide_mouse ();

      if (nb_words_per_row > 0)
        {
          for (row=nb_rows; row>0; row--)
            {
              for (layer=_depth-1; layer>=0; layer--)
                {
                  bitmap_word fg = pattern_get_word(foreground, y, layer);

                  bitmap_word* d = select_layer (layer)
                                   + ((y*_width + x)
                                      >> LOG2_BITMAP_WORD_WIDTH);
                  bitmap_word m;
                  int col;

                  m = CAST(bitmap_word,-1) >> (x & (BITMAP_WORD_WIDTH-1));
                  *d = COMBINE_BITS(*d, fg, m);

                  for (col=nb_words_per_row-1; col>0; col--)
                    {
                      d++;
                      *d = COMBINE_WORDS(*d, fg);
                    }

                  m = CAST(bitmap_word,-1)
                      << ((-x_end) & (BITMAP_WORD_WIDTH-1));
                  d++;
                  *d = COMBINE_BITS(*d, fg, m);
                }

              y++;
            }
        }
      else
        {
          for (row=nb_rows; row>0; row--)
            {
              for (layer=_depth-1; layer>=0; layer--)
                {
                  bitmap_word fg = pattern_get_word(foreground, y, layer);
                  bitmap_word* d = select_layer (layer)
                                   + ((y*_width + x)
                                      >> LOG2_BITMAP_WORD_WIDTH);
                  bitmap_word m;

                  m = (CAST(bitmap_word,-1)
                       >> (x & (BITMAP_WORD_WIDTH-1)))
                      & (CAST(bitmap_word,-1)
                         << ((-x_end) & (BITMAP_WORD_WIDTH-1)));
                  *d = COMBINE_BITS(*d, fg, m);
                }

              y++;
            }
        }

      show_mouse ();
    }
}

void raw_bitmap::frame_rect (int x,
                             int y,
                             int x_end,
                             int y_end,
                             int border,
                             pattern* foreground)
{
  hide_mouse ();
  fill_rect (x,            y,            x_end,    y+border,     foreground);
  fill_rect (x,            y+border,     x+border, y_end-border, foreground);
  fill_rect (x_end-border, y+border,     x_end,    y_end-border, foreground);
  fill_rect (x,            y_end-border, x_end,    y_end,        foreground);
  show_mouse ();
}

void raw_bitmap::invert_rect (int x, int y, int x_end, int y_end)
{
  hide_mouse ();
  bitblt(x, y, x_end, y_end, this, x, y, &pattern_black, &pattern_white);
  show_mouse ();
}

//-----------------------------------------------------------------------------

// "font" class implementation.

//-----------------------------------------------------------------------------
// C rewrite
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// video
//-----------------------------------------------------------------------------

video_c new_video(int mode) {
  video_c video;
  video._mode = mode;

  switch (mode) {
    case 17:
      video._start = CAST(bitmap_word*, 0xa0000);
      video.super._width = 640;
      video.super._height = 480;
      video.super._depth = 1;
      break;

    default:
    case 18:
      video._start = CAST(bitmap_word*, 0xa0000);
      video.super._width = 640;
      video.super._height = 480;
      video.super._depth = 4;
      break;
  }

  video._mouse_x = video.super._width / 2;
  video._mouse_y = video.super._height / 2;
  video._mouse_hides = 1;

  raw_bitmap_fill_rect(&v_screen.super, 0, 0, video.super._width,
                       video.super._height, &pattern_gray50);

  return video;
}

void video_hide_mouse(void* self) {
  video_c* sself = (video_c*) self;
  sself->_mouse_hides++;

  if (sself->_mouse_hides == 1) {
    int width;
    int height;

    video_get_mouse_rect(sself, &width, &height);

    raw_bitmap_bitblt(&v_screen.super, sself->_mouse_x, sself->_mouse_y,
                      sself->_mouse_x + width, sself->_mouse_y + height,
                      &v_mouse_save.super, 0, 0, &pattern_white,
                      &pattern_black);
  }
}

void video_show_mouse(video_c* self) {
  if (self->_mouse_hides == 1) {
    int width;
    int height;

    video_get_mouse_rect(self, &width, &height);

    raw_bitmap_bitblt(&v_mouse_save.super, 0, 0, width, height, &v_screen.super,
                      self->_mouse_x, self->_mouse_y, &pattern_white,
                      &pattern_black);

    video_draw_mouse(self);
  }

  self->_mouse_hides--;
}

void video_move_mouse(video_c* self,int dx, int dy)
{
  self->super.vtable->hide_mouse((void*)self);

  self->_mouse_x += dx;
  self->_mouse_y += dy;

  clip (&self->_mouse_x, 0, self->super._width);
  clip (&self->_mouse_y, 0, self->super._height);

  self->super.vtable->show_mouse((void*)self);
}

void video_get_mouse_rect (video_c* self, int* width, int* height)
{
  *width = self->super._width - self->_mouse_x;
  *height = self->super._height - self->_mouse_y;

  clip (width, 0, MOUSE_WIDTH);
  clip (height, 0, MOUSE_HEIGHT);
}

void video_draw_mouse (video_c* self)
{
#define minimum(a,b) (((a)<(b))?(a):(b))

  int x = self->_mouse_x;
  int y = self->_mouse_y;
  int width;
  int height;

  video_get_mouse_rect(self, &width, &height);

  if (width < 1) return;
  raw_bitmap_fill_rect(&self->super, x + 0, y + 0, x + 1,
                       y + minimum(11, height), &pattern_red);

  if (width < 2) return;
  raw_bitmap_fill_rect(&self->super, x + 1, y + 1, x + 2,
                       y + minimum(10, height), &pattern_red);

  if (width < 3) return;
  raw_bitmap_fill_rect(&self->super, x + 2, y + 2, x + 3,
                       y + minimum(9, height), &pattern_red);

  if (width < 4) return;
  raw_bitmap_fill_rect(&self->super, x + 3, y + 3, x + 4,
                       y + minimum(10, height), &pattern_red);

  if (width < 5) return;
  raw_bitmap_fill_rect(&self->super, x + 4, y + 4, x + 5,
                       y + minimum(12, height), &pattern_red);

  if (width < 6) return;
  raw_bitmap_fill_rect(&self->super, x + 5, y + 5, x + 6,
                       y + minimum(8, height), &pattern_red);

  raw_bitmap_fill_rect(&self->super, x + 5, y + 10, x + 6,
                       y + minimum(14, height), &pattern_red);

  if (width < 7) return;
  raw_bitmap_fill_rect(&self->super, x + 6, y + 6, x + 7,
                       y + minimum(8, height), &pattern_red);

  raw_bitmap_fill_rect(&self->super, x + 6, y + 12, x + 7,
                       y + minimum(14, height), &pattern_red);

  if (width < 8) return;
  raw_bitmap_fill_rect(&self->super, x + 7, y + 7, x + 8,
                       y + minimum(8, height), &pattern_red);
}

//-----------------------------------------------------------------------------
// raw_bitmap_in_memory
//-----------------------------------------------------------------------------

raw_bitmap_in_memory_c new_raw_bitmap_in_memory(bitmap_word* start, int width,
                                                int height, int depth) {
  raw_bitmap_in_memory_c bmp;

  bmp._start = start;
  bmp.super._width = width;
  bmp.super._height = height;
  bmp.super._height = height;

  return bmp;
}

void raw_bitmap_in_memory_hide_mouse(void* self)
{
  ;
}

void raw_bitmap_in_memory_show_mouse(void* self)
{
  ;
}

bitmap_word* _raw_bitmap_in_memory_select_layer(void* self, int layer) {
  raw_bitmap_in_memory_c* sself = (raw_bitmap_in_memory_c*) self;
  layer = layer % sself->super._depth;
  return sself->_start + (sself->super._width >> LOG2_BITMAP_WORD_WIDTH) * sself->super._height * layer;
}

//-----------------------------------------------------------------------------
// raw_bitmap
//-----------------------------------------------------------------------------

void raw_bitmap_bitblt(raw_bitmap_c* self, int x, int y, int x_end,
                               int y_end, raw_bitmap_c* src, int src_x,
                               int src_y, pattern* foreground,
                               pattern* background) {
  if (x < x_end && y < y_end) {
    int realignment =
        (((x & (BITMAP_WORD_WIDTH - 1)) - (src_x & (BITMAP_WORD_WIDTH - 1))) &
         (BITMAP_WORD_WIDTH * 2 - 1)) ^
        BITMAP_WORD_WIDTH;
    int nb_words_per_row =
        ((x_end - 1) >> LOG2_BITMAP_WORD_WIDTH) - (x >> LOG2_BITMAP_WORD_WIDTH);
    int nb_rows = y_end - y;
    int row;
    int layer;

    self->vtable->hide_mouse(self);
    src->vtable->hide_mouse(src);

    if (nb_words_per_row > 0) {
      for (row = nb_rows; row > 0; row--) {
        for (layer = self->_depth - 1; layer >= 0; layer--) {
          bitmap_word fg = pattern_get_word(foreground, y, layer);
          bitmap_word bg = pattern_get_word(background, y, layer);

          bitmap_word* s =
              src->vtable->_select_layer(src, layer) +
              ((src_y * src->_width + src_x) >> LOG2_BITMAP_WORD_WIDTH);
          bitmap_word* d = self->vtable->_select_layer(self, layer) +
                           ((y * self->_width + x) >> LOG2_BITMAP_WORD_WIDTH);
          bitmap_quad_word b;
          bitmap_word m;
          int col;

          b = (CAST(bitmap_quad_word, s[0]) << BITMAP_WORD_WIDTH) | s[1];
          s += 2;
          m = CAST(bitmap_word, -1) >> (x & (BITMAP_WORD_WIDTH - 1));
          *d = COMBINE_BITS_FG_BG(*d, b >> realignment, m, fg, bg);

          for (col = nb_words_per_row - 1; col > 0; col--) {
            b = (b << BITMAP_WORD_WIDTH) | *s++;
            d++;
            *d = COMBINE_WORDS_FG_BG(*d, b >> realignment, fg, bg);
          }

          m = CAST(bitmap_word, -1) << ((-x_end) & (BITMAP_WORD_WIDTH - 1));
          b = (b << BITMAP_WORD_WIDTH) | *s;
          d++;
          *d = COMBINE_BITS_FG_BG(*d, b >> realignment, m, fg, bg);
        }

        src_y++;
        y++;
      }
    } else {
      for (row = nb_rows; row > 0; row--) {
        for (layer = self->_depth - 1; layer >= 0; layer--) {
          bitmap_word fg = pattern_get_word(foreground, y, layer);
          bitmap_word bg = pattern_get_word(background, y, layer);

          bitmap_word* s =
              src->vtable->_select_layer(src,layer) +
              ((src_y * src->_width + src_x) >> LOG2_BITMAP_WORD_WIDTH);
          bitmap_word* d = self->vtable->_select_layer(self, layer) +
                           ((y * self->_width + x) >> LOG2_BITMAP_WORD_WIDTH);
          bitmap_quad_word b;
          bitmap_word m;

          b = (CAST(bitmap_quad_word, s[0]) << BITMAP_WORD_WIDTH) | s[1];
          m = (CAST(bitmap_word, -1) >> (x & (BITMAP_WORD_WIDTH - 1))) &
              (CAST(bitmap_word, -1) << ((-x_end) & (BITMAP_WORD_WIDTH - 1)));
          *d = COMBINE_BITS_FG_BG(*d, b >> realignment, m, fg, bg);
        }

        src_y++;
        y++;
      }
    }


    self->vtable->show_mouse(self);
    src->vtable->show_mouse(src);
  }
}

void raw_bitmap_fill_rect(raw_bitmap_c* self, int x, int y, int x_end,
                          int y_end, pattern* foreground) {
  if (x < x_end && y < y_end) {
    int nb_words_per_row =
        ((x_end - 1) >> LOG2_BITMAP_WORD_WIDTH) - (x >> LOG2_BITMAP_WORD_WIDTH);
    int nb_rows = y_end - y;
    int row;
    int layer;

    self->vtable->hide_mouse(self);

    if (nb_words_per_row > 0) {
      for (row = nb_rows; row > 0; row--) {
        for (layer = self->_depth - 1; layer >= 0; layer--) {
          bitmap_word fg = pattern_get_word(foreground, y, layer);

          bitmap_word* d = self->vtable->_select_layer(self,layer) +
                           ((y * self->_width + x) >> LOG2_BITMAP_WORD_WIDTH);
          bitmap_word m;
          int col;

          m = CAST(bitmap_word, -1) >> (x & (BITMAP_WORD_WIDTH - 1));
          *d = COMBINE_BITS(*d, fg, m);

          for (col = nb_words_per_row - 1; col > 0; col--) {
            d++;
            *d = COMBINE_WORDS(*d, fg);
          }

          m = CAST(bitmap_word, -1) << ((-x_end) & (BITMAP_WORD_WIDTH - 1));
          d++;
          *d = COMBINE_BITS(*d, fg, m);
        }

        y++;
      }
    } else {
      for (row = nb_rows; row > 0; row--) {
        for (layer = self->_depth - 1; layer >= 0; layer--) {
          bitmap_word fg = pattern_get_word(foreground, y, layer);
          bitmap_word* d = self->vtable->_select_layer(self, layer) +
                           ((y * self->_width + x) >> LOG2_BITMAP_WORD_WIDTH);
          bitmap_word m;

          m = (CAST(bitmap_word, -1) >> (x & (BITMAP_WORD_WIDTH - 1))) &
              (CAST(bitmap_word, -1) << ((-x_end) & (BITMAP_WORD_WIDTH - 1)));
          *d = COMBINE_BITS(*d, fg, m);
        }

        y++;
      }
    }

    self->vtable->show_mouse(self);
  }
}

void raw_bitmap_frame_rect(raw_bitmap_c* self, int x, int y, int x_end,
                           int y_end, int border, pattern* foreground) {
  self->vtable->hide_mouse(self);
  raw_bitmap_fill_rect(self, x, y, x_end, y + border, foreground);
  raw_bitmap_fill_rect(self, x, y + border, x + border, y_end - border,
                       foreground);
  raw_bitmap_fill_rect(self, x_end - border, y + border, x_end, y_end - border,
                       foreground);
  raw_bitmap_fill_rect(self, x, y_end - border, x_end, y_end, foreground);
  self->vtable->show_mouse(self);
}

void raw_bitmap_invert_rect(raw_bitmap_c* self, int x, int y, int x_end, int y_end) {
  self->vtable->hide_mouse(self);
  raw_bitmap_bitblt(self, x, y, x_end, y_end, self, x, y, &pattern_black,
                    &pattern_white);
  self->vtable->show_mouse(self);
}

bitmap_word* _raw_bitmap_select_layer(void* self, int layer) {
  ;
}

void raw_bitmap_show_mouse(void* self) {
  ;
}

void raw_bitmap_hide_mouse(void* self) {
  ;
}

//-----------------------------------------------------------------------------
// PATTERN
//-----------------------------------------------------------------------------

pattern new_pattern(bitmap_word* words, int height, int depth) {
  pattern pattern;

  pattern._words = words;
  pattern._height = height;
  pattern._depth = depth;

  return pattern;
}

bitmap_word pattern_get_word(pattern* self, int y, int layer) {
  layer = layer % self->_depth;
  return self->_words[(y % self->_height) + self->_height * layer];
}

//-----------------------------------------------------------------------------
// FONT
//-----------------------------------------------------------------------------
#include "mono_5x7.cpp"
#include "mono_6x9.cpp"

font_c new_font(int max_width, int height, int nb_chars, uint16* char_map,
                uint32* char_end, raw_bitmap* raw) {
  font_c font;
  font._max_width = max_width;
  font._height = height;
  font._nb_chars = nb_chars;
  font._char_map = char_map;
  font._char_end = char_end;
  font._raw = raw;

  return font;
}

int font_get_max_width(font_c* self) {
  return self->_max_width;
}

int font_get_height(font_c* self) {
  return self->_height;
}

void _font_get_char_data(font_c* self, unicode_char c, int& start, int& width) {
  int i;

  if (c >= self->_nb_chars) {
    c = 0;
  }

  i = self->_char_map[c];

  if (i == 0) {
    start = 0;
  } else {
    start = self->_char_end[i - 1];
  }

  width = self->_char_end[i] - start;
}

int font_draw_text(font_c* self, raw_bitmap* dst, int x, int y,
                   unicode_char* text, int count, pattern* foreground,
                   pattern* background) {
  while (count-- > 0) {
    unicode_char c = *text++;
    int start;
    int width;

    _font_get_char_data(self, c, start, width);

    dst->bitblt(x, y, x + width, y + self->_height, self->_raw, start, 0,
                foreground, background);

    x += width;
  }

  return x;
}

int font_draw_string(font_c* self, raw_bitmap* dst, int x, int y,
                     unicode_string str, pattern* foreground,
                     pattern* background) {
  int n = 0;

  while (str[n] != '\0') n++;

  return font_draw_text(self, dst, x, y, str, n, foreground, background);
}

// Local Variables: //
// mode: C++ //
// End: //
