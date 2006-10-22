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
  lib, widget, window,
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
  handler_t = procedure (param: PWMWindow);

  TWMbutton = record
        widget: TWMwidget;
	window: PWMWindow;
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


procedure button_repaint(self: PWMButton);
procedure buttonevent(widget: PWMWidget; ep: PXEvent);
function button_create(window: PWMWindow; int, int, int, int): PWMButton;
procedure button_destroy(button: PWMButton);
procedure button_move(button: PWMButton; x, y: Integer);
procedure button_sethandler(button: PWMButton; handler: handler_t);
procedure button_setimage(button: PWMButton; image: PWMImage);

implementation

uses
  { Units from fpwm }
  main, menu;

procedure button_repaint(self: PWMButton);
var
	bg, fg: PWMColor;
	bgpixel, fgpixel: Cardinal;
	x, y: Integer;
begin
        if window_isfamilyactive(self^.window) then
        begin
    	  fg :=  @color_title_active_fg
          bg := @color_title_active_bg
        end
        else
        begin
          fg := @color_title_inactive_fg;
	  bg := @color_title_inactive_bg;
        end;
        
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
	XFillRectangle(display, self^.pixmap, self^.gc,
	    0, 0, self^.widget.dim.width, self^.widget.dim.height);

	{ draw }
	XSetForeground(display, self^.gc, fgpixel);
	XSetBackground(display, self^.gc, bgpixel);
	if (self^.image <> nil) then
        begin
		x := self^.widget.dim.width div 2 - self^.image^.width div 2;
		y := self^.widget.dim.height div 2 - self^.image^.height div 2;
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
	XDrawLine(display, self^.pixmap, self^.gc, 0, 0, self^.widget.dim.width - 2, 0);
	XDrawLine(display, self^.pixmap, self^.gc, 0, 1, 0, self^.widget.dim.height - 2);
	XSetForeground(display, self^.gc, bgpixel);
	XDrawLine(display, self^.pixmap, self^.gc,
	    self^.widget.dim.width - 1, 1, self^.widget.dim.width - 1, self^.widget.dim.height - 1);
	XDrawLine(display, self^.pixmap, self^.gc,
	    1, self^.widget.dim.height - 1, self^.widget.dim.width - 1, self^.widget.dim.height - 1);

	{ display }
	XCopyArea(display, self^.pixmap, self^.widget.XWINDOW, self^.gc,
	    0, 0, self^.widget.dim.width, self^.widget.dim.height, 0, 0);
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
			if (ep^.xbutton.button = Button3) then
				menu_popup(winmenu,
				    ep^.xbutton.x_root, ep^.xbutton.y_root,
				    ep^.xbutton.button);
		end else begin
			self^.acting := 1;
			self^.depressed := 1;
                end;
		button_repaint(self);
	  end;
	  ButtonRelease:
          begin
		docall := ((self^.acting and self^.depressed) <> 0)
		    and (ep^.xbutton.button = Button1)
		    and (self^.handler <> nil);
		self^.depressed := 0;
		self^.acting := 0;
		button_repaint(self);
		{ must call handler as the last thing, it might destroy us }
		if (docall) then
			self^.handler(self^.window);
	  end;
	  Expose:
		XCopyArea(display, self^.pixmap, self^.widget.XWINDOW,
		    self^.gc, ep^.xexpose.x, ep^.xexpose.y,
		    ep^.xexpose.width, ep^.xexpose.height,
		    ep^.xexpose.x, ep^.xexpose.y);
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

function button_create(window: PWMWindow; x, y, width, height: Integer): TWMbutton;
var
	gcval: TXGCValues;
	bp: PWMButton;
begin
	bp := karmen_malloc(sizeof (struct button));
	widget_create(@bp^.widget, CLASS_BUTTON, window^.XWINDOW,
	    x, y, width, height);

	bp^.pixmap := XCreatePixmap(display, bp^.widget.XWINDOW,
            bp^.widget.dim.width, bp^.widget.dim.height,
	    DefaultDepth(display, screen));
	gcval.graphics_exposures = False;
	bp^.gc := XCreateGC(display, XWINDOW(bp), GCGraphicsExposures, @gcval);
	bp^.image := nil;

	bp^.window := window;
	bp^.acting := 0;
	bp^.depressed := 0;
	bp^.hover := 0;
	bp^.handler := nil;
	bp^.widget.event := buttonevent;
	XSelectInput(display, bp^.widget.XWINDOW, ButtonPressMask or ButtonReleaseMask
	    or ExposureMask or EnterWindowMask or LeaveWindowMask);
	widget_map(@bp^.widget);
	Result := bp;
end;

procedure button_move(button: PWMbutton; x, y: Integer);
begin
	widget_move(@button^.widget, x, y);
end;

procedure button_destroy(button: PWMButton);
begin
	XFreePixmap(display, button^.pixmap);
	XFreeGC(display, button^.gc);
	widget_destroy(@button^.widget);
	FreeMem(button);
end;

procedure button_sethandler(button: PWMButton; handler: handler_t);
begin
	button^.handler := handler;
end;

procedure button_setimage(button: PWMButton; image: PWMImage);
begin
	button^.image := image;
end;

end.
