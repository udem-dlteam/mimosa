// file: "video.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef VIDEO_H
#define VIDEO_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

// A "bitmap" is a 2D grid of pixels, having a certain width (W) and
// height (H) and depth (D).  Each pixel has a color that is encoded
// with an integer of D bits.  For space efficiency the bitmap is
// organized in D "layers"; layer number "i" contains all the "i"th
// bit of the pixels (i.e. layer 0 contains all the least significant
// bits of the pixels and layer D-1 contains all the most significant
// bits of the pixels).  Each layer is represented in memory by a
// sequence of "words" (typically 8 bit bytes) where each bit
// corresponds to a pixel.  The first word corresponds to the leftmost
// group of pixels and the most significant bit of each word
// corresponds to the leftmost pixel in that group.  For example a
// bitmap of width 32 and height 2 and depth 1 can be represented with
// 8 bytes as follows:
//
//  X =                      1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3
//       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
//
//  Y         byte 0          byte 1          byte 2          byte 3
//  =    7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
//      v-+-+-+-+-+-+-+-v-+-+-+-+-+-+-+-v-+-+-+-+-+-+-+-v-+-+-+-+-+-+-+-v
//  0   I | | | | | | | I | | | | | | | I | | | | | | | I | | | | | | | I
//      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
//  1   I | | | | | | | I | | | | | | | I | | | | | | | I | | | | | | | I
//      ^-+-+-+-+-+-+-+-^-+-+-+-+-+-+-+-^-+-+-+-+-+-+-+-^-+-+-+-+-+-+-+-^
//       7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
//            byte 4          byte 5          byte 6          byte 7
//
// Note that the X coordinate of the pixels increases from left to
// right (which is opposite to the bit numbers), and the Y coordinate
// increases from top to bottom.  This may seem odd, but it is a
// common low-level representation used by the display hardware and in
// particular the PC's VGA card "mode 17" which has a resolution of
// 640 by 480 and where byte 0 is at physical address 0xa0000.

#define BITMAP_WORD_SELECT(b8, b16) b8 // how many bits per word
#define MOUSE_WIDTH 8
#define MOUSE_WIDTH_IN_BITMAP_WORDS                                            \
  ((MOUSE_WIDTH + BITMAP_WORD_WIDTH - 1) >> LOG2_BITMAP_WORD_WIDTH)
#define MOUSE_HEIGHT 12
#define LOG2_BITMAP_WORD_WIDTH BITMAP_WORD_SELECT(3, 4) // log2(word width)
#define BITMAP_WORD_WIDTH BITMAP_WORD_SELECT(8, 16)
typedef BITMAP_WORD_SELECT(uint8, uint16) bitmap_word;
typedef BITMAP_WORD_SELECT(uint32, uint64) bitmap_quad_word; // to be able to
                                                             // shift 4 words
                                                             // at a time

//-----------------------------------------------------------------------------
// Pattern

typedef struct pattern {
  bitmap_word *_words;
  int _height;
  int _depth;
  //  int _bpp;
} pattern;

pattern new_pattern(bitmap_word *words, int height, int depth);

bitmap_word pattern_get_word(pattern *self, int y, int layer);

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// raw_bitmap

typedef struct raw_bitmap_vtable {
  void (*hide_mouse)(void *self);
  void (*show_mouse)(void *self);
  bitmap_word *(*_select_layer)(void *self, int layer);
} raw_bitmap_vtable;

typedef struct raw_bitmap {
  raw_bitmap_vtable *vtable;
  int _width;
  int _height;
  int _depth;
} raw_bitmap_c;

void raw_bitmap_bitblt(raw_bitmap *self, int x, int y, int x_end, int y_end,
                       raw_bitmap *src, int src_x, int src_y,
                       pattern *foreground, pattern *background);

void raw_bitmap_fill_rect(raw_bitmap *self, int x, int y, int x_end, int y_end,
                          pattern *foreground);

void raw_bitmap_frame_rect(raw_bitmap *self, int x, int y, int x_end, int y_end,
                           int border, pattern *foreground);

void raw_bitmap_show_mouse(void *self);

void raw_bitmap_hide_mouse(void *self);

bitmap_word *_raw_bitmap_select_layer(void *self, int layer);

void raw_bitmap_invert_rect(raw_bitmap *self, int x, int y, int x_end,
                            int y_end);

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// raw_bitmap_in_memory

typedef struct raw_bitmap_in_memory {
  raw_bitmap super;
  bitmap_word *_start;
} raw_bitmap_in_memory;

#define literal_raw_bitmap_in_memory(start, width, height, depth)              \
  { {&_raw_bitmap_in_memory_vtable, width, height, depth}, start }

raw_bitmap_in_memory *raw_bitmap_in_memory_init(raw_bitmap_in_memory *self,
                                                bitmap_word *start, int width,
                                                int height, int depth);

void raw_bitmap_in_memory_hide_mouse(void *self);

void raw_bitmap_in_memory_show_mouse(void *self);

bitmap_word *_raw_bitmap_in_memory_select_layer(void *self, int layer);

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// video

typedef struct video {
  raw_bitmap super;
  int _mode;
  bitmap_word *_start;
  int _mouse_x;
  int _mouse_y;
  int _mouse_hides;
} video;

video *video_init(video *self);

void video_move_mouse(video *self, int dx, int dy);

void video_hide_mouse(void *self);

void video_show_mouse(void *self);

bitmap_word *video_select_layer(void *self, int layer);

void video_get_mouse_rect(video *self, int *width, int *height);

void video_draw_mouse(video *self);
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Font
typedef struct font_c {
  int _max_width;
  int _height;
  int _nb_chars;
  uint16 *_char_map;
  uint32 *_char_end;
  raw_bitmap *_raw;
} font_c;

#define literal_font(max_width, height, nb_chars, char_map, char_end, raw)     \
  { max_width, height, nb_chars, char_map, char_end, CAST(raw_bitmap *, raw) }

font_c *font_init(font_c *self, int max_width, int height, int nb_chars,
                  uint16 *char_map, uint32 *char_end, raw_bitmap *raw);

int font_get_max_width(font_c *self);

int font_get_height(font_c *self);

int font_draw_text(font_c *self, raw_bitmap *dst, int x, int y,
                   unicode_char *text, int count, pattern *foreground,
                   pattern *background);

int font_draw_string(font_c *self, raw_bitmap *dst, int x, int y,
                     unicode_string str, pattern *foreground,
                     pattern *background);

void _font_get_char_data(font_c *self, unicode_char c, int &start, int &width);
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Extern declarations for statics
extern pattern pattern_black;
extern pattern pattern_gray25;
extern pattern pattern_gray50;
extern pattern pattern_gray75;
extern pattern pattern_white;
extern pattern pattern_red;
extern pattern pattern_green;
extern pattern pattern_yellow;
extern pattern pattern_blue;
extern pattern pattern_magenta;
extern pattern pattern_cyan;

extern font_c font_mono_4x6;
extern font_c font_mono_5x7;
extern font_c font_mono_5x8;
extern font_c font_mono_6x9;
extern font_c font_mono_6x10;
extern font_c font_mono_6x12;
extern font_c font_mono_6x13;
extern font_c font_mono_6x13B;
extern font_c font_mono_6x13O;
extern font_c font_mono_7x13;
extern font_c font_mono_7x13B;
extern font_c font_mono_7x13O;
extern font_c font_mono_7x14;
extern font_c font_mono_7x14B;
extern font_c font_mono_8x13;
extern font_c font_mono_8x13B;
extern font_c font_mono_8x13O;
extern font_c font_mono_9x15;
extern font_c font_mono_9x15B;
extern font_c font_mono_9x18;
extern font_c font_mono_9x18B;
extern font_c font_mono_10x20;

extern video screen;
extern raw_bitmap_in_memory mouse_save;

/*
raw_bitmap_in_memory video::mouse_save
  (mouse_bitmap,
   MOUSE_WIDTH_IN_BITMAP_WORDS << LOG2_BITMAP_WORD_WIDTH,
   MOUSE_HEIGHT,
   4);

video video::screen (18);
*/

extern raw_bitmap_vtable _raw_bitmap_vtable;
extern raw_bitmap_vtable _raw_bitmap_in_memory_vtable;
extern raw_bitmap_vtable _video_vtable;

/* VBE structures. */

struct VBE_info {
  char Signature[4];       /* 'VESA' 4 byte signature */
  uint16 Version;          /* vbe version number */
  uint32 OEMStringPointer; /* Pointer to OEM string */
  uint32 Capabilities;     /* Capabilities of video card */
  uint32 VideoModePointer; /* Pointer to supported modes */
  uint16 TotalMemory;      /* Number of 64kb memory blocks */
  uint16 OEMSoftwareRevision;
  uint32 OEMVendorNamePointer;
  uint32 OEMProductNamePointer;
  uint32 OEMProductRevisionPointer;
  uint8 Reserved[222];
  uint8 OEMData[256];
};

struct VBE_mode_info {
  uint16 ModeAttributes;
  uint8 WindowAAttributes;
  uint8 WindowBAttributes;
  uint16 WindowGranularity; /* Granularity in kb */
  uint16 WindowSize;        /* Size in kb */
  uint16 WindowASegment;
  uint16 WindowBSegment;
  uint32 WindowBankPointer; /* Pointer to bank switching function */
  uint16 BytesPerScanLine;
  /* VBE 1.2 */
  uint16 XResolution;
  uint16 YResolution;
  uint8 XCharSize;
  uint8 YCharSize;
  uint8 NumberOfPlanes;
  uint8 BitsPerPixel;
  uint8 NumberOfBanks;
  uint8 MemoryModel;
  uint8 BankSize;
  uint8 NumberOfImagePages;
  uint8 Reserved0;
  uint8 RedMaskSize;
  uint8 RedFieldPosition;
  uint8 GreenMaskSize;
  uint8 GreenFieldPosition;
  uint8 BlueMaskSize;
  uint8 BlueFieldPosition;
  uint8 ReservedMaskSize;
  uint8 ReservedFieldPosition;
  uint8 DirectColorModeAttributes;
  /* VBE 2.0 */
  uint32 PhysicalBasePtr;    /* Physical address for flat frame buffer  */
  uint32 ScreenMemoryOffset; /* Pointer to start of off screen memory   */
  uint16 ScreenMemorySize;   /* Amount of off screen memory in 1k units */
  /* VBE 3.0 */
  uint16 LinearBytesPerScanLine;
  uint8 BankNumberOfPages;
  uint8 LinearNumberOfPages;
  uint8 LinearRedMaskSize;
  uint8 LinearRedFieldPosition;
  uint8 LinearGreenMaskSize;
  uint8 LinearGreenFieldPosition;
  uint8 LinearBlueMaskSize;
  uint8 LinearBlueFieldPosition;
  uint8 LinearReservedMaskSize;
  uint8 LinearReservedFieldPosition;
  uint32 MaxPixelClock;
  uint8 Reserved1[190];
};

#endif

// Local Variables: //
// mode: C++ //
// End: //
