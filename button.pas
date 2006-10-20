{
 * button.c - window buttons
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
unit button;

{$mode objfpc}{$H+}

interface

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  lib,
  { XLib units }
  X, XLib, XUtil;

{#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "button.h"
#include "global.h"
#include "lib.h"
#include "menu.h"
#include "window.h"}

type
  handler_t = procedure (param: PWindow);

  TWMwidget = Pointer;
  
  TWMbutton = record
        widget: TWMwidget;
	window: PWindow;
       	pixmap: TPixmap;
	gc: TGC;
	image: PWMimage;
	acting: Integer;
	depressed: Integer;
	hover: Integer;
	handler: handler_t;
  end;
  
  PWMbutton = ^TWMbutton;
  
  PWMWidget = Pointer;

{struct button *button_create(struct window *, int, int, int, int);
void button_destroy(struct button *);
void button_move(struct button *, int, int);
void button_repaint(struct button *);
void button_sethandler(struct button *, void (*)(struct window *));
void button_setimage(struct button *, struct image *);}
procedure buttonevent(widget: PWMWidget; ep: PXEvent);

implementation

uses
  { Units from fpwm }
  main;

procedure button_repaint(self: PWMButton);
var
	bg, fg: PWMColor;
	bgpixel, fgpixel: Cardinal;
	x, y: Integer;
begin
{        if window_isfamilyactive(self^.window) then
        begin
    	  fg :=  @color_title_active_fg
          bg := @color_title_active_bg
        end
        else
        begin
          fg := @color_title_inactive_fg;
	  bg := @color_title_inactive_bg;
        end;}
        
	if (self^.depressed <> 0) then begin
		fgpixel := fg^.shadow1;
		bgpixel := bg^.shadow1;
	end else if (self^.hover <> 0) then begin
		fgpixel := fg^.bright1;
		bgpixel := bg^.bright1;
	end else begin
		fgpixel := fg^.normal;
		bgpixel := bg^.normal;
        end;

	{ clear }
	XSetForeground(display, self^.gc, bgpixel);
//	XFillRectangle(display, self^.pixmap, self^.gc,
//	    0, 0, WIDTH(self), HEIGHT(self));

	{ draw }
	XSetForeground(display, self^.gc, fgpixel);
	XSetBackground(display, self^.gc, bgpixel);
	if (self^.image <> nil) then
        begin
//		x := WIDTH(self) / 2 - self->image->width / 2;
//		y := HEIGHT(self) / 2 - self->image->height / 2;
		putimage(display, self^.pixmap, self^.gc, self^.image, x, y);
        end;

        if self^.depressed <> 0 then
        begin
	        fgpixel := bg^.shadow2;
	        bgpixel := bg^.bright2;
        end
        else
        begin
	        fgpixel := bg^.bright2;
	        bgpixel := bg^.shadow2;
        end;
        
	XSetForeground(display, self^.gc, fgpixel);
//	XDrawLine(display, self^.pixmap, self->gc, 0, 0, WIDTH(self) - 2, 0);
//	XDrawLine(display, self^.pixmap, self->gc, 0, 1, 0, HEIGHT(self) - 2);
	XSetForeground(display, self^.gc, bgpixel);
//	XDrawLine(display, self^.pixmap, self^.gc,
//	    WIDTH(self) - 1, 1, WIDTH(self) - 1, HEIGHT(self) - 1);
//	XDrawLine(display, self^.pixmap, self^.gc,
//	    1, HEIGHT(self) - 1, WIDTH(self) - 1, HEIGHT(self) - 1);

	{ display }
//	XCopyArea(display, self->pixmap, XWINDOW(self), self->gc,
//	    0, 0, WIDTH(self), HEIGHT(self), 0, 0);
end;

procedure buttonevent(widget: PWMWidget; ep: PXEvent);
var
	self: PWMButton;
	docall: Boolean;
begin
        self := PWMButton(widget);

	case (ep^._type) of
	  ButtonPress:
          begin
		if (ep^.xbutton.button <> Button1) then
                begin
			self^.acting := 0;
			self^.depressed := 0;
{			if (ep^.xbutton.button = Button3) then
				menu_popup(winmenu,
				    ep^.xbutton.x_root, ep^.xbutton.y_root,
				    ep^.xbutton.button);}
		end else begin
			self^.acting := 1;
			self^.depressed := 1;
                end;
		button_repaint(self);
	  end;
	  ButtonRelease:
          begin
{		docall := ((self^.acting and self^.depressed)
		    and (ep^.xbutton.button = Button1)
		    and (self^.handler <> nil));}
		self^.depressed := 0;
		self^.acting := 0;
		button_repaint(self);
		{ must call handler as the last thing, it might destroy us }
		if (docall) then
			self^.handler(self^.window);
	  end;
{	  Expose:
		XCopyArea(display, self^.pixmap, XWINDOW(self),
		    self^.gc, ep^.xexpose.x, ep^.xexpose.y,
		    ep^.xexpose.width, ep^.xexpose.height,
		    ep^.xexpose.x, ep^.xexpose.y);}
	  EnterNotify:
          begin
		if (self^.acting <> 0) then
			self^.depressed := 1;
		self^.hover := 1;
		button_repaint(self);
	  end;
	  LeaveNotify:
          begin
		if (self^.acting <> 0) then
			self^.depressed := 0;
		self^.hover := 0;
		button_repaint(self);
	  end;
	end;
end;

{function button_create(window: PWMWindow; x, y, width, height: Integer): TWMbutton;
var
	gcval: TXGCValues;
	bp: PWMButton;
begin
	bp := karmen_malloc(sizeof (struct button));
	widget_create(&bp^.widget, CLASS_BUTTON, XWINDOW(window),
	    x, y, width, height);

	bp^.pixmap := XCreatePixmap(display, XWINDOW(bp), WIDTH(bp), HEIGHT(bp),
	    DefaultDepth(display, screen));
	gcval.graphics_exposures = False;
	bp^.gc := XCreateGC(display, XWINDOW(bp), GCGraphicsExposures, &gcval);
	bp^.image := nil;

	bp^.window := window;
	bp^.acting := 0;
	bp^.depressed := 0;
	bp^.hover := 0;
	bp^.handler := nil;
	bp^.widget.event := buttonevent;
	XSelectInput(display, XWINDOW(bp), ButtonPressMask | ButtonReleaseMask
	    | ExposureMask | EnterWindowMask | LeaveWindowMask);
	widget_map(&bp^.widget);
	Result := bp;
end;

procedure button_move(button: TWMbutton; x, y: Integer);
begin
	widget_move(@button^.widget, x, y);
end;

procedure button_destroy(struct button *button);
begin
	XFreePixmap(display, button^.pixmap);
	XFreeGC(display, button^.gc);
	widget_destroy(@button^.widget);
	karmen_free(button);
end;

procedure button_sethandler(struct button *button, void (*handler)(struct window *))
begin
	button^.handler := handler;
end;

procedure button_setimage(struct button *button, struct image *image)
begin
	button^.image := image;
end;}

end.
