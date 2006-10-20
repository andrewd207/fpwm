{
 * lib.c - various routines
 }

{
 * Copyright (c) 2006 Johan Veenhuizen
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 }
unit lib;

{$mode objfpc}{$H+}

interface

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  { XLib units }
  X, XLib, XUtil;

{#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "global.h"
#include "lib.h"}

function MAX(a, b: Byte): Byte;
function MIN(a, b: Byte): Byte;

//#define NELEM(v)	(sizeof v / sizeof v[0])

type
  TWMImage = record
	data: PByte;
	width: Integer;
	height: Integer;
  end;

  PWMImage = ^TWMImage;

  TWMColor = record
	bright2: Cardinal;
	bright1: Cardinal;
	normal: Cardinal;
	shadow1: Cardinal;
	shadow2: Cardinal;
  end;

  PWMColor = ^TWMColor;

{void *karmen_malloc(size_t);
void *karmen_realloc(void *, size_t);
void karmen_free(void *);

char *karmen_strdup(const char *);

void clerr(void);
void sterr(void);

void grabkey(Display *display, int keycode, unsigned modifiers,
    Window grab_window, Bool owner_events, int pointer_mode,
    int keyboard_mode);
void ungrabkey(Display *display, int keycode, unsigned modifiers,
    Window grab_window);
void grabbutton(Display *display, unsigned button, unsigned modifiers,
    Window grab_window, Bool owner_events, unsigned event_mask,
    int pointer_mode, int keyboard_mode, Window confine_to, Cursor cursor);
void ungrabbutton(Display *display, unsigned button, unsigned modifiers,
    Window grab_window);}

procedure putimage(display: PDisplay; d: TDrawable; gc: TGC; image: PWMImage; x, y: Integer);
{int stringwidth(const char *str);
char *stringfit(char *str, int width);

void debug(const char *, ...);
const char *eventname(int);}

implementation

uses
  { Units from fpwm }
  main;

function MAX(a, b: Byte): Byte;
begin
  if a > b then Result := a
  else Result := b;
end;

function MIN(a, b: Byte): Byte;
begin
  if a > b then Result := b
  else Result := a;
end;

procedure debug(const str: string);
begin
{$ifdef DEBUG}
	WriteLn(str);
{$endif}
end;

procedure putimage(display: PDisplay; d: TDrawable; gc: TGC; image: PWMImage; x, y: Integer);
var
	pixmap: TPixmap;
begin
	pixmap := XCreateBitmapFromData(display, d, PChar(image^.data),
	    image^.width, image^.height);
	XCopyPlane(display, pixmap, d, gc,
	    0, 0, image^.width, image^.height, x, y, 1);
	XFreePixmap(display, pixmap);
end;

function stringwidth(const str: string): Integer;
var
        ch: TXCharStruct;
        direction: Integer;
        ascent: Integer;
        descent: Integer;
begin
        XTextExtents(font, PChar(str), Length(str), @direction,
            @ascent, @descent, @ch);
        Result := ch.width;
end;

function stringfit(str: string; width: Integer): string;
var
	len: Integer;
begin
	len := length(str);
	while (stringwidth(str) > width) do
        begin
		if (len < 3) then
                begin
			str[1] := #0;
			break;
                end;
		strcopy(@str + len - 3, '...');
		Dec(len);
        end;
	Result := str;
end;

{function karmen_malloc(size: Cardinal): Pointer;
var
	ptr: Pointer;
begin
	while ((ptr = GetMem(size)) = nil and size <> 0)
		sleep(1);
	Result := ptr;
end;

function karmen_realloc(optr: Pointer; size: size_t): Pointer;
var
	nptr: Pointer;
begin
	while ((nptr = realloc(optr, size)) == NULL && size != 0)
		sleep(1);
	return nptr;
end;

procedure karmen_free(ptr: Pointer);
begin
	free(ptr);
end;

function karmen_strdup(const str: PChar): PChar
var
	new: PChar;
begin
	assert(str != NULL);
	new = karmen_malloc(strlen(str) + 1);
	return strcpy(new, str);
}

function eventname(type_: Integer): PChar;
begin
	case (type_) of
	 KeyPress: Result := 'KeyPress';
	 KeyRelease: Result := 'KeyRelease';
	 ButtonPress: Result := '';
	 ButtonRelease: Result := '';
	 MotionNotify: Result := '';
	 EnterNotify: Result := '';
	 LeaveNotify: Result := '';
	 FocusIn: Result := '';
	 FocusOut: Result := '';
	 KeymapNotify: Result := '';
	 Expose: Result := '';
	 GraphicsExpose: Result := '';
	 NoExpose: Result := '';
	 VisibilityNotify: Result := '';
	 CreateNotify: Result := '';
	 DestroyNotify: Result := '';
	 UnmapNotify: Result := '';
	 MapNotify: Result := '';
	 MapRequest: Result := '';
	 ReparentNotify: Result := '';
	 ConfigureNotify: Result := '';
	 ConfigureRequest: Result := '';
	 GravityNotify: Result := '';
	 ResizeRequest: Result := '';
	 CirculateNotify: Result := '';
	 CirculateRequest: Result := '';
	 PropertyNotify: Result := '';
	 SelectionClear: Result := '';
	 SelectionRequest: Result := '';
	 SelectionNotify: Result := '';
	 ColormapNotify: Result := '';
	 ClientMessage: Result := '';
	 MappingNotify: Result := '';
  else
		Result := 'INVALID EVENT';
  end;
end;

end.
