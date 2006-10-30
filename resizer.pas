{
 * resizer.c - resizable window borders
 }

{
 * Copyright (c) 2006 Felipe Monteiro de Carvalho, Daniel Franzini
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
unit resizer;

{$mode objfpc}{$H+}

interface

//#include "widget.h"

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  widget, window,
  { XLib units }
  X, Xlib, Xutil, cursorfont;

const
	NORTHWEST = 0;
	NORTH = 1;
	NORTHEAST = 2;
	WEST = 3;
	EAST = 4;
	SOUTHWEST = 5;
	SOUTH = 6;
	SOUTHEAST = 7;

type
  PWMResizer = ^TWMResizer;

  TWMResizer = record
        widget: TWMWidget;
	window: PWMWindow;
	resizing, dir: Integer;
  end;

//struct resizer *resizer_create(struct window *, int);
procedure resizer_destroy(resizer: PWMResizer);
procedure resizer_fit(resizer: PWMResizer);

implementation

{#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/cursorfont.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "global.h"
#include "menu.h"
#include "resizer.h"
#include "window.h"}

uses
  { Units from fpwm }
  menu, main;

type
  Psizewin_t = ^sizewin_t;

  sizewin_t = record
	widget: TWMWidget;

	window: PWMWindow;
	gc: TGC;
  end;

var
  c1, c2, c3, c4, c5, c6, c7, c8: TCursor;

  sizewin: Psizewin_t = nil;

{procedure sizewin_update(int, int);
procedure sizewin_create(struct window *, int xdim, int ydim);
procedure sizewin_destroy();
procedure press(struct resizer *, XButtonEvent *);
procedure release(struct resizer *, XButtonEvent *);
procedure motion(struct resizer *, XMotionEvent *);
procedure resizerevent(struct widget *, XEvent *);}

procedure sizewin_update(xdim, ydim: Integer);
var
	buf: array[0..255] of Char;
	widget: PWMWidget;
	win: PWMWindow;
	width, height: Integer;
begin
	if (sizewin = nil) then Exit;

	widget := PWMWidget(sizewin);
	win := sizewin^.window;

	WriteLn(buf, xdim, 'x', ydim);
	width := 2 * (title_pad + font^.descent) + StrLen(@buf);
	height := 2 * title_pad + font^.ascent + font^.descent;
	widget_moveresize(widget, win^.widget.dim.x + win^.widget.dim.width div 2 - (width + 2) div 2,
	    win^.widget.dim.Y + border_width + button_size + innerborder_width
	     + (win^.widget.dim.Height
	        - 2 * border_width - button_size - innerborder_width) div 2
	     - (height + 2) div 2, width, height);
	if (not MAPPED(sizewin)) then
		widget_map(@sizewin^.widget);
	XClearWindow(display, sizewin^.widget.XWINDOW);
	XSetForeground(display, sizewin^.gc, color_title_active_fg.normal);
	XDrawString(display, sizewin^.widget.XWINDOW, sizewin^.gc,
	    title_pad + font^.descent, title_pad + font^.ascent,
	    buf, strlen(buf));
	XSetForeground(display, sizewin^.gc, color_title_active_bg.bright2);
	XDrawLine(display, sizewin^.widget.XWINDOW, sizewin^.gc,
	    0, 0, width - 2, 0);
	XDrawLine(display, sizewin^.widget.XWINDOW, sizewin^.gc,
	    0, 1, 0, height - 2);
	XSetForeground(display, sizewin^.gc, color_title_active_bg.shadow2);
	XDrawLine(display, sizewin^.widget.XWINDOW, sizewin^.gc,
	    width - 1, 1, width - 1, height - 1);
	XDrawLine(display, sizewin^.widget.XWINDOW, sizewin^.gc,
	    1, height - 1, width - 2, height - 1);
end;

procedure sizewin_create(win: PWMWindow; xdim, ydim: Integer);
var
	attr: TXSetWindowAttributes;
	gcval: TXGCValues;
begin
//	sizewin := karmen_malloc(sizeof *sizewin);
	widget_create(@sizewin^.widget, CLASS_SIZEWIN, root, 0, 0, 1, 1);
	XSetWindowBorder(display, sizewin^.widget.XWINDOW, color_border.normal);
	XSetWindowBorderWidth(display, sizewin^.widget.XWINDOW, 1);
	XSetWindowBackground(display, sizewin^.widget.XWINDOW,
	    color_title_active_bg.normal);
	attr.save_under := True;
	XChangeWindowAttributes(display, sizewin^.widget.XWINDOW, CWSaveUnder, @attr);
	gcval.font := font^.fid;
	sizewin^.gc := XCreateGC(display, sizewin^.widget.XWINDOW, GCFont, @gcval);
	sizewin^.window := win;
	sizewin_update(xdim, ydim);
end;

procedure sizewin_destroy();
begin
	if (sizewin <> nil) then
        begin
		XFreeGC(display, sizewin^.gc);
		widget_unmap(@sizewin^.widget);
		widget_destroy(@sizewin^.widget);
		sizewin := nil;
        end;
end;

procedure press(resizer: PWMResizer; ep: PXButtonEvent);
var
	win: PWMWindow;
	xdim, ydim: Integer;
begin
	win := resizer^.window;

	if (ep^.button = Button1) then
        begin
		resizer^.resizing := 1;
		window_setactive(win);
		window_calcsize(win, win^.widget.dim.width, win^.widget.dim.height,
		    nil, nil, @xdim, @ydim);
		sizewin_create(win, xdim, ydim);
	end else if (ep^.button = Button3) then
		menu_popup(winmenu, ep^.x_root, ep^.y_root, ep^.button);
end;

procedure release(resizer: PWMResizer; ep: PXButtonEvent);
begin
	if (ep^.button = Button1) then
        begin
		resizer^.resizing := 0;
		sizewin_destroy();
        end;
end;

procedure motion(resizer: PWMResizer; ep: PXMotionEvent);
var
	win: PWMWindow;
	x, y, width, height, rwidth, rheight, xdim, ydim: Integer;
begin
	win := resizer^.window;

	if (resizer^.resizing = 0) then Exit;

	case (resizer^.dir) of
	 NORTHWEST:
         begin
		width := win^.widget.dim.x + win^.widget.dim.width - ep^.x_root;
		height := win^.widget.dim.y + win^.widget.dim.height - ep^.y_root;
	 end;
	 NORTH:
         begin
		width := win^.widget.dim.width;
		height := win^.widget.dim.y + win^.widget.dim.height - ep^.y_root;
	 end;
	 NORTHEAST:
         begin
		width := 1 + ep^.x_root - win^.widget.dim.x;
		height := win^.widget.dim.y + win^.widget.dim.height - ep^.y_root;
	 end;
	 WEST:
         begin
		width := win^.widget.dim.x + win^.widget.dim.width - ep^.x_root;
		height := win^.widget.dim.height;
	 end;
	 EAST:
         begin
		width := 1 + ep^.x_root - win^.widget.dim.x;
		height := win^.widget.dim.height;
	 end;
	 SOUTHWEST:
         begin
		width := win^.widget.dim.x + win^.widget.dim.width - ep^.x_root;
		height := 1 + ep^.y_root - win^.widget.dim.y;
	 end;
	 SOUTH:
         begin
		width := win^.widget.dim.width;
		height := 1 + ep^.y_root - win^.widget.dim.y;
	 end;
	 SOUTHEAST:
         begin
		width := 1 + ep^.x_root - win^.widget.dim.x;
		height := 1 + ep^.y_root - win^.widget.dim.y;
	 end;
        else
		abort();
        end;

	window_calcsize(win, width, height, @rwidth, @rheight, @xdim, @ydim);

	case (resizer^.dir) of
	 NORTHWEST:
         begin
		x := win^.widget.dim.x + win^.widget.dim.width - rwidth;
		y := win^.widget.dim.y + win^.widget.dim.height - rheight;
	 end;
	 NORTHEAST:
         begin
		x := win^.widget.dim.x;
		y := win^.widget.dim.y + win^.widget.dim.height - rheight;
	 end;
	 NORTH:
         begin
		x := win^.widget.dim.x;
		y := win^.widget.dim.y + win^.widget.dim.height - rheight;
	 end;
	 WEST:
         begin
		x := win^.widget.dim.x + win^.widget.dim.width - rwidth;
		y := win^.widget.dim.y;
	 end;
	 SOUTHWEST:
         begin
		x := win^.widget.dim.x + win^.widget.dim.width - rwidth;
		y := win^.widget.dim.y;
	 end;
	else
		x := win^.widget.dim.x;
		y := win^.widget.dim.y;
        end;

	if (rwidth <> win^.widget.dim.width) or (rheight <> win^.widget.dim.height) then
        begin
		resizer^.window^.maximized := 0;
		window_moveresize(resizer^.window, x, y, rwidth, rheight);
		sizewin_update(xdim, ydim);
        end;
end;

procedure resizerevent(widget: PWMWidget; ep: PXEvent);
begin
	case (ep^._type) of
	 ButtonPress:
		press(PWMResizer(widget), @ep^.xbutton);
	 ButtonRelease:
		release(PWMResizer(widget), @ep^.xbutton);
	 MotionNotify:
		motion(PWMResizer(widget), @ep^.xmotion);
        end;
end;

function resizer_create(win: PWMWindow; dir: Integer): PWMResizer;
var
	resizer: PWMResizer;
	initialized: Integer = 0;
begin
	if (initialized = 0) then
        begin
		initialized := 1;
		c1 := XCreateFontCursor(display, XC_top_left_corner);
		c2 := XCreateFontCursor(display, XC_top_side);
		c3 := XCreateFontCursor(display, XC_top_right_corner);
		c4 := XCreateFontCursor(display, XC_left_side);
		c5 := XCreateFontCursor(display, XC_right_side);
		c6 := XCreateFontCursor(display, XC_bottom_left_corner);
		c7 := XCreateFontCursor(display, XC_bottom_side);
		c8 := XCreateFontCursor(display, XC_bottom_right_corner);
        end;

	resizer := GetMem(sizeof(TWMResizer));
	widget_create(@resizer^.widget, CLASS_RESIZER,
	    win^.widget.XWindow, 0, 0, 1, 1);
	XSetWindowBackground(display, resizer^.widget.xwindow, color_border.normal);
	resizer^.widget.event := @resizerevent;
	XSelectInput(display, resizer^.widget.xwindow,
	    ButtonPressMask or ButtonReleaseMask or ButtonMotionMask);
	resizer^.window := win;
	resizer^.dir := dir;
	resizer_fit(resizer);

	case (resizer^.dir) of
	 NORTHWEST:
		XDefineCursor(display, resizer^.widget.xwindow, c1);
	 NORTH:
		XDefineCursor(display, resizer^.widget.xwindow, c2);
	 NORTHEAST:
		XDefineCursor(display, resizer^.widget.xwindow, c3);
	 WEST:
		XDefineCursor(display, resizer^.widget.xwindow, c4);
	 EAST:
		XDefineCursor(display, resizer^.widget.xwindow, c5);
	 SOUTHWEST:
		XDefineCursor(display, resizer^.widget.xwindow, c6);
	 SOUTH:
		XDefineCursor(display, resizer^.widget.xwindow, c7);
	 SOUTHEAST:
		XDefineCursor(display, resizer^.widget.xwindow, c8);
        else
		abort();
        end;

	XLowerWindow(display, resizer^.widget.xwindow);
	widget_map(@resizer^.widget);
	Result := resizer;
end;

procedure resizer_destroy(resizer: PWMResizer);
begin
	widget_destroy(@resizer^.widget);
	FreeMem(resizer);
end;

procedure resizer_fit(resizer: PWMResizer);
var
	x, y, width, height: Integer;
begin
	case (resizer^.dir) of
	 NORTHWEST:
         begin
		x := 0;
		y := 0;
		width := button_size;
		height := button_size;
	 end;
	 NORTH:
         begin
		x := button_size;
		y := 0;
		width := resizer^.window^.widget.dim.WIDTH - 2 * button_size;
		height := border_width;
	 end;
	 NORTHEAST:
         begin
		x := resizer^.window^.widget.dim.WIDTH - button_size;
		y := 0;
		width := button_size;
		height := button_size;
	 end;
	 WEST:
         begin
		x := 0;
		y := button_size;
		width := border_width;
		height := resizer^.window^.widget.dim.HEIGHT - 2 * button_size;
	 end;
	 EAST:
         begin
		x := resizer^.window^.widget.dim.WIDTH - border_width;
		y := button_size;
		width := border_width;
		height := resizer^.window^.widget.dim.HEIGHT - 2 * button_size;
	 end;
	 SOUTHWEST:
         begin
		x := 0;
		y := resizer^.window^.widget.dim.HEIGHT - button_size;
		width := button_size;
		height := button_size;
	 end;
	 SOUTH:
         begin
		x := button_size;
		y := resizer^.window^.widget.dim.HEIGHT - border_width;
		width := resizer^.window^.widget.dim.WIDTH - 2 * button_size;
		height := border_width;
	 end;
	 SOUTHEAST:
         begin
		x := resizer^.window^.widget.dim.WIDTH - button_size;
		y := resizer^.window^.widget.dim.HEIGHT - button_size;
		width := button_size;
		height := button_size;
	 end;
	else
		abort();
        end;
        
	widget_moveresize(@resizer^.widget, x, y, width, height);
end;

end.

