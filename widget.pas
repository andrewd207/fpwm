{
 * widget.c - general window manager widget
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
unit widget;

{$mode objfpc}{$H+}

interface

{#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Xutil.h>

#include "global.h"
#include "widget.h"}

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  main,
  { XLib units }
  X, Xlib, Xutil, Xresource;

type
  TWMCLASS = (
	CLASS_ANY,
	CLASS_WINDOW,
	CLASS_TITLE,
	CLASS_BUTTON,
	CLASS_MENU,
	CLASS_RESIZER,
	CLASS_SIZEWIN
  );

  TWMDim = record
	x: Integer;
	y: Integer;
	width: Integer;
	height: Integer;
  end;
  
  PWMWidget = ^TWMWidget;

  TWMWidget = record
	_CLASS: Pointer;
	xwindow: TWindow;
	mapped: Integer;
	dim: TWMDim;
	event: procedure (p1: PWMWidget; p2: PXEvent);
  end;

{#define XWINDOW(ptr)	(((struct widget *)(ptr))->xwindow)
#define X(ptr)		(((struct widget *)(ptr))->dim.x)
#define Y(ptr)		(((struct widget *)(ptr))->dim.y)
#define WIDTH(ptr)	(((struct widget *)(ptr))->dim.width)
#define HEIGHT(ptr)	(((struct widget *)(ptr))->dim.height)
#define MAPPED(ptr)	(((struct widget *)(ptr))->mapped)}

procedure widget_init();
{procedure widget_create(struct widget *, CLASS, Window, int, int, int, int);
procedure widget_resize(struct widget *, int, int);
procedure widget_move(struct widget *, int, int);
procedure widget_moveresize(struct widget *, int, int, int, int);
procedure widget_map(struct widget *);
procedure widget_unmap(struct widget *);
procedure widget_destroy(struct widget *);
struct widget *widget_find(Window, CLASS);
procedure widget_deletecontext(Window);
procedure widget_savecontext(struct widget *, Window);}

implementation

var
  wmcontext: TXContext;

procedure widget_init();
begin
	wmcontext := TXContext(XrmUniqueQuark);
end;

procedure widget_create(widget: PWMWidget; _class: Pointer; xparent: TWindow;
    x, y, width, height: Integer);
var
	attr: TXSetWindowAttributes;
begin
	widget^._class := _class;
	widget^.dim.x := x;
	widget^.dim.y := y;
	widget^.dim.width := width;
	widget^.dim.height := height;
	widget^.event := nil;
	widget^.mapped := 0;

	attr.override_redirect := True;
	attr.background_pixel := WhitePixel(display, screen);
	widget^.xwindow := XCreateWindow(display, xparent, x, y, width, height,
	    0, CopyFromParent, InputOutput, CopyFromParent,
	    CWOverrideRedirect or CWBackPixel, @attr);
	XSaveContext(display, widget^.xwindow, wmcontext,
	    (XPointer)widget);
end;

procedure widget_savecontext(struct widget *widget, Window xwindow)
begin
	clerr();
	XSaveContext(display, xwindow, wmcontext, (XPointer)widget);
	sterr();
end;

struct widget *widget_find(Window xwindow, CLASS class)
begin
	struct widget *widget;

	/* the double cast is there to silent gcc warnings */
	if (XFindContext(display, xwindow, wmcontext,
	    (XPointer *)(void *)&widget) == 0)
		return (class == CLASS_ANY || widget^.class == class) ?
		    widget : NULL;
	else
		return nil;
end;

procedure widget_resize(struct widget *widget, int width, int height)
begin
	widget^.dim.width = width;
	widget^.dim.height = height;
	XResizeWindow(display, widget^.xwindow, width, height);
end;

procedure widget_move(struct widget *widget, int x, int y)
begin
	widget^.dim.x = x;
	widget^.dim.y = y;
	XMoveWindow(display, widget^.xwindow, x, y);
end;

procedure widget_moveresize(struct widget *widget, int x, int y,
    int width, int height)
begin
	widget^.dim.x = x;
	widget^.dim.y = y;
	widget^.dim.width = width;
	widget^.dim.height = height;
	XMoveResizeWindow(display, widget^.xwindow, x, y, width, height);
end;

procedure widget_map(struct widget *widget)
begin
	widget^.mapped = 1;
	XMapWindow(display, widget^.xwindow);
end;

procedure widget_unmap(struct widget *widget)
begin
	widget^.mapped = 0;
	XUnmapWindow(display, widget^.xwindow);
end;

procedure widget_destroy(struct widget *widget)
begin
	XDeleteContext(display, widget^.xwindow, wmcontext);
	XDestroyWindow(display, widget^.xwindow);
end;

procedure widget_deletecontext(Window xwindow)
begin
	XDeleteContext(display, xwindow, wmcontext);
end;

end.
