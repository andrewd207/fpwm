{
 * title.c - window titles
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
unit title;

{$mode objfpc}{$H+}

interface

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  widget, window,
  { XLib units }
  X, Xlib;

{#include <X11/Xlib.h>

#include "widget.h"}

type
  TWMTitle = record
        widget: TWMWidget;
	window: PWMWindow;
	pixmap: TPixmap;
	gc: TGC;
	xoff: Integer;
	yoff: Integer;
	moving: Integer;
	lastclick: TTime;
  end;
  
  PWMTitle = ^TWMTitle;

function title_create(window: PWMWindow; x, y, width, height: Integer): PWMTitle;
procedure title_resize(title: PWMTitle; width, height: Integer);
procedure title_destroy(title: PWMTitle);
procedure title_repaint(title: PWMTitle);

implementation

{#include <assert.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "global.h"
#include "menu.h"
#include "title.h"
#include "window.h"}

uses
  { Units from fpwm }
  lib, menu, main;

{static procedure beginfastmove(struct title *);
static procedure endfastmove(procedure);
static procedure titleevent(struct widget *, XEvent *);}

procedure title_repaint(title: PWMTitle);
var
	win: PWMWindow;
	fg, bg: PWMColor;
	buf: PChar = nil;
	ypad: Integer;
	xpad: Integer;
        maxwidth: Integer;
	x, y: Integer;
	m: Integer = 1;
begin
        win := title^.window;
        ypad := MAX(3, 2 * title_pad);
        xpad := title_pad + 2 * font^.descent; { this looks resonable }
        if window_isactive(win) then
         maxwidth := title^.widget.dim.width - 2 * xpad - ypad - title^.widget.dim.width div 5
        else maxwidth := title^.widget.dim.width - 2 * xpad;

	if (window_isfamilyactive(win)) then
        begin
		fg := @color_title_active_fg;
		bg := @color_title_active_bg;
	end
        else
        begin
		fg := @color_title_inactive_fg;
		bg := @color_title_inactive_bg;
        end;

	{ clear }
	XSetForeground(display, title^.gc, bg^.normal);
	XFillRectangle(display, title^.pixmap, title^.gc,
	    0, 0, title^.widget.dim.width, title^.widget.dim.height);

	{ repaint }
	XSetForeground(display, title^.gc, fg^.normal);
	if (win^.name <> nil) and (strlen(win^.name) > 0) then
        begin
//		buf := karmen_strdup(win^.name);
//		stringfit(buf, maxwidth);
		XDrawString(display, title^.pixmap, title^.gc,
		    xpad,
		    button_size - title_pad - font^.descent,
		    buf, strlen(buf));
        end;

	if (window_isactive(win)) then
        begin
//		if (buf = nil) or (strlen(buf) = 0) then x := ypad
//                else x := stringwidth(buf) + 2 * xpad;
                
		if (x < title^.widget.dim.Width - 1 - ypad) then
                begin
			for y := ypad to title^.widget.dim.height - ypad - 1 do
                        begin
                                if m <> 0 then XSetForeground(display, title^.gc, bg^.bright2)
                                else XSetForeground(display, title^.gc, bg^.shadow2);

				m := not m;
				XDrawLine(display, title^.pixmap, title^.gc,
				    m + x, y, m + title^.widget.dim.Width - 1 - ypad, y);
                        end;
                end;
        end;

	XSetForeground(display, title^.gc, bg^.bright2);
	XDrawLine(display, title^.pixmap, title^.gc,
	    0, 0, title^.widget.dim.Width - 2, 0);
	XDrawLine(display, title^.pixmap, title^.gc,
	    0, 1, 0, title^.widget.dim.Height - 2);
	XSetForeground(display, title^.gc, bg^.shadow2);
	XDrawLine(display, title^.pixmap, title^.gc,
	    1, title^.widget.dim.Height - 1, title^.widget.dim.Width - 1, title^.widget.dim.Height - 1);
	XDrawLine(display, title^.pixmap, title^.gc,
	    title^.widget.dim.Width - 1, 1, title^.widget.dim.Width - 1, title^.widget.dim.Height - 2);

	{ display }
	XCopyArea(display, title^.pixmap, title^.widget.XWINDOW, title^.gc,
	    0, 0, title^.widget.dim.Width, title^.widget.dim.Height, 0, 0);

//	karmen_free(buf);
end;

var fastmovewin: TWindow  = None;

procedure beginfastmove(title: PWMTitle);
var
	attr: TXSetWindowAttributes;
begin
	if (fastmovewin <> None) then Exit;

	attr.override_redirect := True;
	fastmovewin := XCreateWindow(display, root,
	    0, 0, DisplayWidth(display, screen),
	    DisplayHeight(display, screen), 0, CopyFromParent, InputOnly,
	    PVisual(CopyFromParent), CWOverrideRedirect, @attr);
	XMapWindow(display, fastmovewin);
	XGrabPointer(display, title^.widget.XWINDOW, False,
	    ButtonMotionMask or ButtonReleaseMask,
	    GrabModeAsync, GrabModeAsync,
	    fastmovewin, None, CurrentTime);
end;

procedure endfastmove();
begin
	if (fastmovewin = None) then Exit;

	XUngrabPointer(display, CurrentTime);
	XDestroyWindow(display, fastmovewin);
	fastmovewin := None;
end;

procedure titleevent(widget: PWMWidget; ep: PXEvent);
var
	title: PWMTitle;
	dummy: TWindow;
begin
        title := PWMTitle(widget);

	case (ep^._type) of
	 ButtonPress:
		if (ep^.xbutton.button = Button1) then
                begin
			if (title^.lastclick > ep^.xbutton.time - 250) then
				window_maximize(title^.window)
			else begin
				XTranslateCoordinates(display,
				    title^.widget.XWINDOW,
				    title^.window^.widget.XWINDOW,
				    ep^.xbutton.x,
				    ep^.xbutton.y,
				    @title^.xoff,
				    @title^.yoff,
				    @dummy);
				window_setactive(title^.window);
				title^.moving := 1;
                        end;
			title^.lastclick := ep^.xbutton.time;
		end else if (ep^.xbutton.button = Button3) then
			menu_popup(winmenu,
			    ep^.xbutton.x_root, ep^.xbutton.y_root,
			    ep^.xbutton.button);
	 ButtonRelease:
         begin
		endfastmove();
		if (ep^.xbutton.button = Button1) then
			title^.moving := 0;
         end;
	 MotionNotify:
		if (title^.moving <> 0) then
                begin
			window_move(title^.window,
			    ep^.xmotion.x_root - title^.xoff,
			    ep^.xmotion.y_root - title^.yoff);
			beginfastmove(title);
                end;
	 Expose:
		XCopyArea(display, title^.pixmap, title^.widget.XWINDOW,
		    title^.gc, ep^.xexpose.x, ep^.xexpose.y,
		    ep^.xexpose.width, ep^.xexpose.height,
		    ep^.xexpose.x, ep^.xexpose.y);
        end;
end;

function title_create(window: PWMWindow; x, y, width, height: Integer): PWMTitle;
var
	gcval: TXGCValues;
	tp: PWMTitle;
begin
//	tp := karmen_malloc(sizeof (struct title));
	widget_create(@tp^.widget, CLASS_TITLE, window^.widget.XWINDOW,
	    x, y, width, height);
//	tp^.pixmap := XCreatePixmap(display, tp^.widget.XWINDOW, WIDTH(tp), HEIGHT(tp),
//	    DefaultDepth(display, screen));
	gcval.graphics_exposures := False;
	tp^.gc := XCreateGC(display, tp^.widget.XWINDOW, GCGraphicsExposures, @gcval);
	XSetFont(display, tp^.gc, font^.fid);
	tp^.window := window;
	tp^.widget.event := @titleevent;
	XSelectInput(display, tp^.widget.XWINDOW,
	    ButtonPressMask or ButtonMotionMask or ButtonReleaseMask or
	    ExposureMask);
	tp^.moving := 0;
	tp^.lastclick := 0;
	widget_map(@tp^.widget);
	Result := tp;
end;

procedure title_resize(title: PWMTitle; width, height: Integer);
begin
	widget_resize(@title^.widget, width, height);
	XFreePixmap(display, title^.pixmap);
//	title^.pixmap := XCreatePixmap(display, title^.widget.XWINDOW,
//	    WIDTH(title), HEIGHT(title), DefaultDepth(display, screen));
//	if (MAPPED(title))
//		title_repaint(title);
end;

procedure title_destroy(title: PWMTitle);
begin
	widget_destroy(@title^.widget);
	XFreePixmap(display, title^.pixmap);
	XFreeGC(display, title^.gc);
//	karmen_free(title);
end;

end.

