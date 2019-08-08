// file: "dump_x_font.c"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

// A quick hack to dump X-windows fonts.

#include <stdio.h>
#include <stdlib.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef unsigned char uint8;       // 8 bit unsigned integers
typedef unsigned short uint16;     // 16 bit unsigned integers
typedef unsigned int uint32;       // 32 bit unsigned integers
typedef unsigned long long uint64; // 64 bit unsigned integers

#define BITMAP_WORD_SELECT(b8,b16) b8 // how many bits per word

#define LOG2_BITMAP_WORD_WIDTH BITMAP_WORD_SELECT(3,4) // log2(word width)
#define BITMAP_WORD_WIDTH BITMAP_WORD_SELECT(8,16)
typedef BITMAP_WORD_SELECT(uint8,uint16) bitmap_word;

typedef uint16 unicode;

#define MAX_WIDTH 100
#define MAX_HEIGHT 100

/*--------------------------------------------------------------------------*/

Display *x_display;
int x_screen;
int x_depth;
int x_black, x_white;
Visual *x_visual;
XImage *x_image;

struct
  {
    int width, height;
    Window wind;
    GC color[2];
    XFontStruct *font;
  } x_wind;


void x_clear_window ()
{
  XFillRectangle (x_display,
                  x_wind.wind,
                  x_wind.color[0],
                  0,
                  0,
                  x_wind.width,
                  x_wind.height);
  XFlush (x_display);
}


void x_open_window (char *title, int width, int height, char *font_name)
{
  int i;
  XEvent report;
  XGCValues values;

  /* connect to X server */

  if ((x_display = XOpenDisplay (NULL)) == NULL)
    {
      fprintf (stderr,
               "Can't connect to X server %s\n",
               XDisplayName (NULL));
      exit (1);
    }

  x_screen = DefaultScreen (x_display);

  x_black = BlackPixel (x_display, x_screen);
  x_white = WhitePixel (x_display, x_screen);

  x_visual = DefaultVisual (x_display, x_screen);

  x_depth = DefaultDepth (x_display, x_screen);

  /* get font */

  if ((x_wind.font = XLoadQueryFont (x_display, font_name)) == NULL)
    {
      fprintf (stderr, "Can't find font \"%s\"\n", font_name);
      exit (1);
    }

  /* create the window */

  x_wind.width  = width;
  x_wind.height = height;

  x_wind.wind =
    XCreateSimpleWindow (x_display,
                         RootWindow (x_display, x_screen),
                         0,
                         0,
                         width,
                         height,
                         0,
                         x_white,
                         x_black);

  if (x_wind.wind == 0)
    {
      fprintf (stderr, "Can't create window\n");
      exit (1);
    }

  XSetStandardProperties (x_display,
                          x_wind.wind,
                          title,
                          title,
                          0,
                          NULL,
                          0,
                          NULL);

  /* set graphic context */

  for (i=0; i<2; i++)
    {
      values.foreground = (i==0) ? x_black : x_white;
      values.background = x_black;

      x_wind.color[i] =
        XCreateGC (x_display,
                   RootWindow (x_display, x_screen),
                   (GCForeground | GCBackground),
                   &values);

      XSetFont (x_display, x_wind.color[i], x_wind.font->fid);
    }

  /* display window */

  XMapWindow (x_display, x_wind.wind);
  XFlush (x_display);

  /* wait until window appears */

  XSelectInput (x_display, x_wind.wind, ExposureMask);
  XWindowEvent (x_display, x_wind.wind, ExposureMask, &report);

  x_clear_window ();
}


void x_close_window ()
{
  XDestroyWindow (x_display, x_wind.wind);
  XCloseDisplay (x_display);
}


void x_draw_unicode (int x, int y, unicode c)
{
  XChar2b xc;

  xc.byte1 = c >> 8;
  xc.byte2 = c & 0xff;

  XDrawString16 (x_display,
                 x_wind.wind,
                 x_wind.color[1],
                 x,
                 y,
                 &xc,
                 1);

  XFlush (x_display);
}


void x_get_image (int x, int y, int width, int height)
{
  x_image = XGetImage (x_display,
                       x_wind.wind,
                       x,
                       y,
                       width,
                       height,
                       0xffffffff,
                       ZPixmap);
}


void x_destroy_image ()
{
  XDestroyImage (x_image);
}


uint64 x_image_pixel_on (int x, int y)
{
  return (XGetPixel (x_image, x, y) == x_white) ? 1 : 0;
}


/*---------------------------------------------------------------------------*/

uint64 max_c;
int width;
int height;
char* bitmap;
uint16* char_map;
int last_char_map;
uint32* char_end;
uint32 last_char_end;

void print_hex (uint64 n, int nb_bits)
{
  if (nb_bits <= 0)
    printf ("0x");
  else
    {
      print_hex (n >> 4, nb_bits - 4);
      printf ("%c", "0123456789abcdef"[n&15]);
    }
}

void print_bytes (uint64 n, int nb_bits)
{
  if (nb_bits > 8)
    {
      print_bytes (n >> 8, nb_bits - 8);
      printf (",");
    }
  print_hex (n, 8);
}

void extract_char (uint64 c)
{
  int x;
  int y;
  XChar2b xc;
  int char_width;
  int i;

  xc.byte1 = c >> 8;
  xc.byte2 = c & 0xff;

  x_clear_window ();
  x_draw_unicode (0, x_wind.font->max_bounds.ascent, c);
  char_width = XTextWidth16 (x_wind.font, &xc, 1);
  x_get_image (0, 0, char_width, height);

  for (y=0; y<height; y++)
    for (x=0; x<char_width; x++)
      bitmap[y*((max_c+1)*width + BITMAP_WORD_WIDTH)+x+last_char_end] =
        x_image_pixel_on (x, y);

  for (i=0; i<last_char_map; i++)
    {
      int start;

      if (i == 0)
        start = 0;
      else
        start = char_end[i];

      for (y=0; y<height; y++)
        for (x=0; x<char_width; x++)
          if (bitmap[y*((max_c+1)*width + BITMAP_WORD_WIDTH)+x+start] !=
              bitmap[y*((max_c+1)*width + BITMAP_WORD_WIDTH)+x+last_char_end])
            goto try_next;

      break;

    try_next:;
    }

  if (i < last_char_map)
    char_map[c] = i;
  else
    {
      last_char_map++;

      last_char_end += char_width;

      char_end[last_char_map] = last_char_end;

      char_map[c] = last_char_map;
    }

  x_destroy_image ();
}

void dump_font (char *font_name, char *font_id)
{
  uint64 c;
  int i, j, k;
  bitmap_word bits;
  int col;

  x_open_window (font_name, MAX_WIDTH, MAX_HEIGHT, font_name);

  max_c = (uint64)x_wind.font->max_char_or_byte2 +
          (uint64)x_wind.font->max_byte1*256;

#ifdef DEBUG
  fprintf(stderr,"x_wind.font->min_char_or_byte2 = %d\n",x_wind.font->min_char_or_byte2);
  fprintf(stderr,"x_wind.font->max_char_or_byte2 = %d\n",x_wind.font->max_char_or_byte2);
  fprintf(stderr,"x_wind.font->min_byte1         = %d\n",x_wind.font->min_byte1);
  fprintf(stderr,"x_wind.font->max_byte1         = %d\n",x_wind.font->max_byte1);
  fprintf(stderr,"max_c                          = %d\n",max_c);
#endif

  width = x_wind.font->max_bounds.width;
  height = x_wind.font->max_bounds.ascent + x_wind.font->max_bounds.descent;

  bitmap = malloc (((max_c+1)*width + BITMAP_WORD_WIDTH) * height);
  char_map = malloc ((max_c+1) * sizeof (uint16));
  char_end = malloc ((max_c+1) * sizeof (uint32));

  if (bitmap == NULL || char_map == NULL || char_end == NULL)
    exit (1);

  for (i = ((max_c+1)*width + BITMAP_WORD_WIDTH) * height - 1; i>=0; i--)
    bitmap[i] = 0;

  last_char_map = -1;
  last_char_end = 0;

  for (c=0; c<=max_c; c++) {
#ifdef DEBUG
    if (c > 0 && c % 100 == 0)
      fprintf(stderr,"c = %d\n",c);
#endif
    extract_char (c);
  }

#if 0
  for (i=0; i<height; i++)
    {
      j = char_end[char_map['g']];
      while (j < char_end[char_map['m']])
        {
          if (bitmap[i*((max_c+1)*width + BITMAP_WORD_WIDTH)+j])
            printf ("*");
          else
            printf (" ");
          j++;
        }
      printf ("\n");
    }
#endif

  printf ("static bitmap_word %s_pixels[] = {", font_id);

  for (i=0; i<height; i++)
    {
      col = -1;
      j = 0;
      while (j < last_char_end)
        {
          col = (col+1) % 8;
          if (col == 0)
            printf("\n");
          if (i != 0 || j != 0)
            printf (",");
          else
            printf (" ");
          bits = 0;
          for (k=0; k<BITMAP_WORD_WIDTH; k++)
            bits = (bits << 1)
                   | bitmap[i*((max_c+1)*width + BITMAP_WORD_WIDTH)+j++];
          print_hex (bits, BITMAP_WORD_WIDTH);
        }
      printf ("\n");
    }

  printf ("};\n");
  printf ("\n");

  printf ("static uint16 %s_char_map[] = {", font_id);

  col = -1;

  for (i=0; i<=max_c; i++)
    {
      col = (col+1) % 8;
      if (col == 0)
        printf("\n");
      if (i > 0)
        printf (",");
      else
        printf (" ");
      printf ("%4d", char_map[i]);
    }

  printf ("\n};\n");
  printf ("\n");

  printf ("static uint32 %s_char_end[] = {", font_id);

  col = -1;

  for (i=0; i<=last_char_map; i++)
    {
      col = (col+1) % 8;
      if (col == 0)
        printf("\n");
      if (i > 0)
        printf (",");
      else
        printf (" ");
      printf ("%4d", char_end[i]);
    }

  printf ("\n};\n");
  printf ("\n");

  printf ("static raw_bitmap_in_memory %s_raw_bitmap =\n",
          font_id);
  printf ("  literal_raw_bitmap_in_memory(%s_pixels, %d, %d, %d);\n",
          font_id,
          j,
          height,
          1);
  printf ("\n");

  printf ("font %s = \n",
          font_id);
  printf ("  literal_font(%d, %d, %d, %s_char_map, %s_char_end, &%s_raw_bitmap);\n",
          width,
          height,
          (int)max_c + 1,
          font_id,
          font_id,
          font_id);
}


int main (int argc, char *argv[])
{
  if (argc < 2 || argc > 3)
    {
      fprintf (stderr, "usage: %s <X_font_name> [<font_id>]\n", argv[0]);
      exit (1);
    }

  dump_font (argv[1], (argc==3) ? argv[2] : argv[1]);

  return 0;
}
