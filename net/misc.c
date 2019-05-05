/**************************************************************************
MISC Support Routines
**************************************************************************/

#include "etherboot.h"

extern int console_getc ();
extern void console_putc (int c);
extern int console_ischar ();

/**************************************************************************
SLEEP
**************************************************************************/
void sleep(int secs)
{
	unsigned long tmo;

	for (tmo = currticks()+secs*TICKS_PER_SEC; currticks() < tmo; )
		/* Nothing */;
}

/**************************************************************************
INTERRUPTIBLE SLEEP
**************************************************************************/
void interruptible_sleep(int secs)
{
#ifdef original_code
	unsigned long tmo;

	printf("<sleep>\n");
	for (tmo = currticks()+secs*TICKS_PER_SEC; currticks() < tmo; ) {
		if (iskey() && (getchar() == ESC))
			longjmp(restart_etherboot, -1);
	}
#endif
}

/**************************************************************************
TWIDDLE
**************************************************************************/
void twiddle(void)
{
#ifdef	BAR_PROGRESS
	static unsigned long lastticks = 0;
	static int count=0;
	static const char tiddles[]="-\\|/";
	unsigned long ticks;
	if ((ticks = currticks()) == lastticks)
		return;
	lastticks = ticks;
	putchar(tiddles[(count++)&3]);
	putchar('\b');
#else
        //	putchar('.');
#endif	/* DOT_PROGRESS */
}

/**************************************************************************
STRCASECMP (not entirely correct, but this will do for our purposes)
**************************************************************************/
int strcasecmp(char *a, char *b)
{
	while (*a && *b && (*a & ~0x20) == (*b & ~0x20)) {a++; b++; }
	return((*a & ~0x20) - (*b & ~0x20));
}

/**************************************************************************
PRINTF and friends

	Formats:
		%[#]x	- 4 bytes long (8 hex digits, lower case)
		%[#]X	- 4 bytes long (8 hex digits, upper case)
		%[#]hx	- 2 bytes int (4 hex digits, lower case)
		%[#]hX	- 2 bytes int (4 hex digits, upper case)
		%[#]hhx	- 1 byte int (2 hex digits, lower case)
		%[#]hhX	- 1 byte int (2 hex digits, upper case)
			- optional # prefixes 0x or 0X
		%d	- decimal int
		%c	- char
		%s	- string
		%@	- Internet address in ddd.ddd.ddd.ddd notation
		%!	- Ethernet address in xx:xx:xx:xx:xx:xx notation
	Note: width specification not supported
**************************************************************************/
static int do_printf(char *buf, const char *fmt, const int *dp)
{
	char *p, *s;

	s = buf;
	for ( ; *fmt != '\0'; ++fmt) {
		if (*fmt != '%') {
			buf ? *s++ = *fmt : putchar(*fmt);
			continue;
		}
		if (*++fmt == 's') {
			for (p = (char *)*dp++; *p != '\0'; p++)
				buf ? *s++ = *p : putchar(*p);
		}
		else {	/* Length of item is bounded */
			char tmp[20], *q = tmp;
			int alt = 0;
			int shift = 28;
			if (*fmt == '#') {
				alt = 1;
				fmt++;
			}
			if (*fmt == 'h') {
				shift = 12;
				fmt++;
			}
			if (*fmt == 'h') {
				shift = 4;
				fmt++;
			}
			/*
			 * Before each format q points to tmp buffer
			 * After each format q points past end of item
			 */
			if ((*fmt | 0x20) == 'x') {
				/* With x86 gcc, sizeof(long) == sizeof(int) */
				const long *lp = (const long *)dp;
				long h = *lp++;
				int ncase = (*fmt & 0x20);
				dp = (const int *)lp;
				if (alt) {
					*q++ = '0';
					*q++ = 'X' | ncase;
				}
				for ( ; shift >= 0; shift -= 4)
					*q++ = "0123456789ABCDEF"[(h >> shift) & 0xF] | ncase;
			}
			else if (*fmt == 'd') {
				int i = *dp++;
				char *r;
				if (i < 0) {
					*q++ = '-';
					i = -i;
				}
				p = q;		/* save beginning of digits */
				do {
					*q++ = '0' + (i % 10);
					i /= 10;
				} while (i);
				/* reverse digits, stop in middle */
				r = q;		/* don't alter q */
				while (--r > p) {
					i = *r;
					*r = *p;
					*p++ = i;
				}
			}
			else if (*fmt == '@') {
				unsigned char *r;
				union {
					long		l;
					unsigned char	c[4];
				} u;
				const long *lp = (const long *)dp;
				u.l = *lp++;
				dp = (const int *)lp;
				for (r = &u.c[0]; r < &u.c[4]; ++r)
					q += sprintf(q, "%d.", *r);
				--q;
			}
			else if (*fmt == '!') {
				char *r;
				p = (char *)*dp++;
				for (r = p + 6; p < r; ++p)
					q += sprintf(q, "%hhX:", *p);
				--q;
			}
			else if (*fmt == 'c')
				*q++ = *dp++;
			else
				*q++ = *fmt;
			/* now output the saved string */
			for (p = tmp; p < q; ++p)
				buf ? *s++ = *p : putchar(*p);
		}
	}
	if (buf)
		*s = '\0';
	return (s - buf);
}

int sprintf(char *buf, const char *fmt, ...)
{
	return do_printf(buf, fmt, ((const int *)&fmt)+1);
}

void printf(const char *fmt, ...)
{
	(void)do_printf(0, fmt, ((const int *)&fmt)+1);
}

#ifdef	IMAGE_MENU
/**************************************************************************
INET_ATON - Convert an ascii x.x.x.x to binary form
**************************************************************************/
int inet_aton(char *p, in_addr *i)
{
	unsigned long ip = 0;
	int val;
	if (((val = getdec(&p)) < 0) || (val > 255)) return(0);
	if (*p != '.') return(0);
	p++;
	ip = val;
	if (((val = getdec(&p)) < 0) || (val > 255)) return(0);
	if (*p != '.') return(0);
	p++;
	ip = (ip << 8) | val;
	if (((val = getdec(&p)) < 0) || (val > 255)) return(0);
	if (*p != '.') return(0);
	p++;
	ip = (ip << 8) | val;
	if (((val = getdec(&p)) < 0) || (val > 255)) return(0);
	i->s_addr = htonl((ip << 8) | val);
	return(1);
}

#endif	/* IMAGE_MENU */

int getdec(char **ptr)
{
	char *p = *ptr;
	int ret=0;
	if ((*p < '0') || (*p > '9')) return(-1);
	while ((*p >= '0') && (*p <= '9')) {
		ret = ret*10 + (*p - '0');
		p++;
	}
	*ptr = p;
	return(ret);
}

#define K_RDWR		0x60		/* keyboard data & cmds (read/write) */
#define K_STATUS	0x64		/* keyboard status */
#define K_CMD		0x64		/* keybd ctlr command (write-only) */

#define K_OBUF_FUL	0x01		/* output buffer full */
#define K_IBUF_FUL	0x02		/* input buffer full */

#define KC_CMD_WIN	0xd0		/* read  output port */
#define KC_CMD_WOUT	0xd1		/* write output port */
#define KB_SET_A20	0xdf		/* enable A20,
					   enable output buffer full interrupt
					   enable data line
					   disable clock line */
#define KB_UNSET_A20	0xdd		/* enable A20,
					   enable output buffer full interrupt
					   enable data line
					   disable clock line */
#ifndef	IBM_L40
static void empty_8042(void)
{
	unsigned long time;
	char st;

	time = currticks() + TICKS_PER_SEC;	/* max wait of 1 second */
	while ((((st = inb(K_CMD)) & K_OBUF_FUL) ||
	       (st & K_IBUF_FUL)) &&
	       currticks() < time)
		inb(K_RDWR);
}
#endif	/* IBM_L40 */

/*
 * Gate A20 for high memory
 */
void gateA20_set(void)
{
#ifdef	IBM_L40
	outb(0x2, 0x92);
#else	/* IBM_L40 */
	empty_8042();
	outb(KC_CMD_WOUT, K_CMD);
	empty_8042();
	outb(KB_SET_A20, K_RDWR);
	empty_8042();
#endif	/* IBM_L40 */
}

#if	defined(TAGGED_IMAGE) || defined(CAN_BOOT_DISK)
/*
 * Unset Gate A20 for high memory - some operating systems (mainly old 16 bit
 * ones) don't expect it to be set by the boot loader.
 */
void gateA20_unset(void)
{
#ifdef	IBM_L40
	outb(0x0, 0x92);
#else	/* IBM_L40 */
	empty_8042();
	outb(KC_CMD_WOUT, K_CMD);
	empty_8042();
	outb(KB_UNSET_A20, K_RDWR);
	empty_8042();
#endif	/* IBM_L40 */
}
#endif

int
putchar(int c)
{
#ifndef	ANSIESC
	if (c == '\n')
		putchar('\r');
#endif

#ifdef	CONSOLE_CRT
#ifdef	ANSIESC
	ansi_putc(c);
#else
	console_putc(c);
#endif
#endif
#ifdef	CONSOLE_SERIAL
#ifdef	ANSIESC
	if (c == '\n')
		serial_putc('\r');
#endif
	serial_putc(c);
#endif
        return c;
}

/**************************************************************************
GETCHAR - Read the next character from input device WITHOUT ECHO
**************************************************************************/
int getchar(void)
{
	int c = 256;

	do {
#ifdef	POWERSAVE
		/* Doze for a while (until the next interrupt).  This works
		 * fine, because the keyboard is interrupt-driven, and the
		 * timer interrupt (approx. every 50msec) takes care of the
		 * serial port, which is read by polling.  This reduces the
		 * power dissipation of a modern CPU considerably, and also
		 * makes Etherboot waiting for user interaction waste a lot
		 * less CPU time in a VMware session.  */
		cpu_nap();
#endif	/* POWERSAVE */
#ifdef	CONSOLE_CRT
		if (console_ischar())
			c = console_getc();
#endif
#ifdef	CONSOLE_SERIAL
		if (serial_ischar())
			c = serial_getc();
#endif
	} while (c==256);
	if (c == '\r')
		c = '\n';
	return c;
}

int iskey(void)
{
#ifdef	CONSOLE_CRT
	if (console_ischar())
		return 1;
#endif
#ifdef	CONSOLE_SERIAL
	if (serial_ischar())
		return 1;
#endif
	return 0;
}

/*
 * Local variables:
 *  c-basic-offset: 8
 * End:
 */
