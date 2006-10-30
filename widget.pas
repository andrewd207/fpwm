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
	_CLASS: TWMClass;
	xwindow: TWindow;
	mapped: Integer;
	dim: TWMDim;
	event: procedure (p1: PWMWidget; p2: PXEvent);
  end;

{#define XWINDOW(ptr)	(((struct widget *)(ptr))->xwindow)
#define X(ptr)		(((struct widget *)(ptr))->dim.x)
#define Y(ptr)		(((struct widget *)(ptr))->dim.y)
#define WIDTH(ptr)	(((struct widget *)(ptr))->dim.width)
#define HEIGHT(ptr)	(((struct widget *)(ptr))->dim.height)}

function MAPPED(ptr: Pointer): Boolean;

procedure widget_init();
procedure widget_create(widget: PWMWidget; _class: TWMClass; xparent: TWindow;
    x, y, width, height: Integer);
procedure widget_savecontext(widget: PWMWidget; xwindow: TWindow);
function widget_find(xwindow: TWindow; _CLASS: TWMClass): PWMWidget;
procedure widget_resize(widget: PWMWidget; width, height: Integer);
procedure widget_move(widget: PWMWidget; x, y: Integer);
procedure widget_moveresize(widget: PWMWidget; x, y, width, height: Integer);
procedure widget_map(widget: PWMWidget);
procedure widget_unmap(widget: PWMWidget);
procedure widget_destroy(widget: PWMWidget);
procedure widget_deletecontext(xwindow: TWindow);

implementation

uses
  { Units from fpwm }
  main;

var
  wmcontext: TXContext;

function MAPPED(ptr: Pointer): Boolean;
begin
  Result := PWMWidget(ptr)^.mapped;
end;

procedure widget_init();
begin
	wmcontext := TXContext(XrmUniqueQuark);
end;

procedure widget_create(widget: PWMWidget; _class: TWMClass; xparent: TWindow;
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
	    0, CopyFromParent, InputOutput, PVisual(CopyFromParent),
	    CWOverrideRedirect or CWBackPixel, @attr);
	XSaveContext(display, widget^.xwindow, wmcontext,
	    TXPointer(widget));
end;

procedure widget_savecontext(widget: PWMWidget; xwindow: TWindow);
begin
//	clerr();
	XSaveContext(display, xwindow, wmcontext, TXPointer(widget));
//	sterr();
end;

function widget_find(xwindow: TWindow; _CLASS: TWMClass): PWMWidget;
var
	widget: PWMWidget;
begin
	if (XFindContext(display, xwindow, wmcontext,
	    @widget) = 0) then
        begin
                if (_class = CLASS_ANY) or (widget^._class = _class) then
  		 Result :=  widget
                else Result := nil;
        end
	else
		Result := nil;
end;

procedure widget_resize(widget: PWMWidget; width, height: Integer);
begin
	widget^.dim.width := width;
	widget^.dim.height := height;
	XResizeWindow(display, widget^.xwindow, width, height);
end;

procedure widget_move(widget: PWMWidget; x, y: Integer);
begin
	widget^.dim.x := x;
	widget^.dim.y := y;
	XMoveWindow(display, widget^.xwindow, x, y);
end;

procedure widget_moveresize(widget: PWMWidget; x, y, width, height: Integer);
begin
	widget^.dim.x := x;
	widget^.dim.y := y;
	widget^.dim.width := width;
	widget^.dim.height := height;
	XMoveResizeWindow(display, widget^.xwindow, x, y, width, height);
end;

procedure widget_map(widget: PWMWidget);
begin
	widget^.mapped := 1;
	XMapWindow(display, widget^.xwindow);
end;

procedure widget_unmap(widget: PWMWidget);
begin
	widget^.mapped := 0;
	XUnmapWindow(display, widget^.xwindow);
end;

procedure widget_destroy(widget: PWMWidget);
begin
	XDeleteContext(display, widget^.xwindow, wmcontext);
	XDestroyWindow(display, widget^.xwindow);
end;

procedure widget_deletecontext(xwindow: TWindow);
begin
	XDeleteContext(display, xwindow, wmcontext);
end;

end.
