{
 * menu.c
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
unit menu;

interface

{#include <X11/Xlib.h>

#include "widget.h"}

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  widget,
  { XLib units }
  X, Xlib;

type
  PWMmenuitem = ^TWMmenuitem;

  TWMmenuitem = record
	name: PChar;
	ptr: Pointer;
	select: procedure (p1: Pointer);
	prev, next: PWMmenuitem;
  end;

  PWMMenu = ^TWMMenu;

  TWMMenu = record
        widget: TWMWidget;
	gc: TGC;
	pixmap: TPixmap;
	current: Integer;
	button: Integer;
	nitems: Integer;
	items: TWMmenuitem;
  end;
  
  select_t = procedure (p1: Pointer);

function menu_create(): PWMMenu;
function menu_additem(menu: PWMMenu; name: string;
    select: select_t; ptr: Pointer): PWMmenuitem;
procedure menu_delitem(menu: PWMMenu; item: PWMmenuitem);
procedure menu_hide(menu: PWMMenu);
procedure menu_movetotop(menu: PWMMenu; ip: PWMmenuitem);
procedure menu_popup(menu: PWMMenu; x, y, button: Integer);
procedure menu_renameitem(menu: PWMMenu; ip: PWMmenuitem; name: string);
procedure menu_repaint(menu: PWMMenu);
procedure menu_select(menu: PWMMenu);

implementation

{#include <stdio.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "global.h"
#include "menu.h"}

uses
  { Units from fpwm }
  lib, main;

{static void menuevent(struct widget *, XEvent *);
static void trim(menu: PWMMenu);}

{
 * Perform the action of the currently selected item.
 }
procedure menu_select(menu: PWMMenu);
var
	ip: PWMmenuitem;
	i: Integer = 0;
begin
        ip := menu^.items.next;
        
	while ip <> @menu^.items do
        begin
		if (i = menu^.current) then
                begin
			menu^.current := -1;
			menu_movetotop(menu, ip);
			ip^.select(ip^.ptr);
			Exit;
                end;

		Inc(i);
                ip := ip^.next;
        end;
end;

procedure menu_movetotop(menu: PWMMenu; ip: PWMmenuitem);
begin
	{ unlink }
	ip^.prev^.next := ip^.next;
	ip^.next^.prev := ip^.prev;

	{ put first }
	ip^.next := menu^.items.next;
	ip^.prev := @menu^.items;
	menu^.items.next^.prev := ip;
	menu^.items.next := ip;
end;

{
 * Repaint the menu.
 }
procedure menu_repaint(menu: PWMMenu);
var
	ip: PWMmenuitem;
	i: Integer;
begin
	{ clear }
	XSetForeground(display, menu^.gc, color_menu_bg.normal);
	XFillRectangle(display, menu^.pixmap, menu^.gc,
	    0, 0, menu^.widget.dim.width, menu^.widget.dim.height);

	{ repaint }
	XSetForeground(display, menu^.gc, color_menu_bg.bright2);
	XDrawLine(display, menu^.pixmap, menu^.gc,
	    0, 0, menu^.widget.dim.width - 2, 0);
	XDrawLine(display, menu^.pixmap, menu^.gc,
	    0, 1, 0, menu^.widget.dim.height - 2);
	XSetForeground(display, menu^.gc, color_menu_bg.shadow2);
	XDrawLine(display, menu^.pixmap, menu^.gc,
	    menu^.widget.dim.width - 1, 1, menu^.widget.dim.width - 1, menu^.widget.dim.height - 1);
	XDrawLine(display, menu^.pixmap, menu^.gc,
	    1, menu^.widget.dim.height - 1, menu^.widget.dim.width - 2, menu^.widget.dim.height - 1);

	i := 0;
        ip := menu^.items.next;

	while (ip <> @menu^.items) do
        begin
		if (i = menu^.current) then
                begin
			XSetForeground(display, menu^.gc,
			    color_menu_selection_bg.normal);
			XFillRectangle(display, menu^.pixmap, menu^.gc,
			    1, 1 + i * button_size,
			    menu^.widget.dim.width - 2, button_size);
			XSetForeground(display, menu^.gc,
			    color_menu_selection_fg.normal);
		end else
			XSetForeground(display, menu^.gc,
			    color_menu_fg.normal);
       
		XDrawString(display, menu^.pixmap, menu^.gc, button_size,
		    1 + (i + 1) * button_size - font^.descent - title_pad,
		    ip^.name, strlen(ip^.name));

		Inc(i);
                ip := ip^.next;
        end;

	{ show }
	XCopyArea(display, menu^.pixmap, menu^.widget.XWINDOW, menu^.gc,
	    0, 0, menu^.widget.dim.width, menu^.widget.dim.height, 0, 0);
end;

{
 * Pop up and activate the menu at position (x, y).
 }
procedure menu_popup(menu: PWMMenu; x, y, button: Integer);
var
	dw: Integer;
	dh: Integer;
begin
	dw := DisplayWidth(display, screen);
	dh := DisplayHeight(display, screen);

	if (menu^.items.next = @menu^.items) then
		Exit;	{ empty menu }

	if (x + menu^.widget.dim.width >= dw) then
		x := MAX(0, x - menu^.widget.dim.width - 1);

	if (y + menu^.widget.dim.height >= dh) then
		y := MAX(0, y - menu^.widget.dim.height - 1);

	menu^.button := button;

	widget_move(@menu^.widget, x, y);
	XRaiseWindow(display, menu^.widget.XWINDOW);
	widget_map(@menu^.widget);
	if (button <> -1) then
        begin
		XGrabPointer(display, menu^.widget.XWINDOW, False,
		    ButtonMotionMask or ButtonReleaseMask,
		    GrabModeAsync, GrabModeAsync, None, None, CurrentTime);
        end;
	menu_repaint(menu);
end;

{
 * Hide and inactivate the menu.
 }
procedure menu_hide(menu: PWMMenu);
begin
	widget_unmap(@menu^.widget);
	if (menu^.button <> -1) then
		XUngrabPointer(display, CurrentTime);
end;

{
 * Handle menu events.
 }
procedure menuevent(widget: PWMWidget; ep: PXEvent);
var
	self: PWMMenu;
	tmp: Integer;
begin
        self := PWMMenu(widget);

	case (ep^._type) of
	 MotionNotify:
         begin
		tmp := self^.current;
		if (ep^.xbutton.x < 1) or (ep^.xbutton.x >= self^.widget.dim.WIDTH - 1) or
		    (ep^.xbutton.y < 1) or (ep^.xbutton.y >= self^.widget.dim.HEIGHT - 1) then
			self^.current := -1
		else
			self^.current := (ep^.xbutton.y - 1) div button_size;

		if (tmp <> self^.current) then
			menu_repaint(self);
         end;
	 ButtonRelease:
         begin
		menu_hide(self);
		if (ep^.xbutton.button = self^.button) then
			menu_select(self);
         end;
	 Expose:
		XCopyArea(display, self^.pixmap, self^.widget.XWINDOW,
		    self^.gc, ep^.xexpose.x, ep^.xexpose.y,
		    ep^.xexpose.width, ep^.xexpose.height,
		    ep^.xexpose.x, ep^.xexpose.y);
        else
		WriteLn('menuevent: ', eventname(ep^._type), ' ', ep^._type);
        end;
end;

{
 * Create an empty menu.
 }
function menu_create(): PWMMenu;
var
	attr: TXSetWindowAttributes;
	gcval: TXGCValues;
	menu: PWMMenu;
begin
	menu := GetMem(sizeof(TWMMenu));

	widget_create(@menu^.widget, CLASS_MENU, root, 0, 0, 1, 1);
	attr.save_under := True;
	XChangeWindowAttributes(display, menu^.widget.XWINDOW, CWSaveUnder, @attr);
	XSetWindowBorderWidth(display, menu^.widget.XWINDOW, 1);
	XSetWindowBorder(display, menu^.widget.XWINDOW, color_border.normal);

	menu^.pixmap := XCreatePixmap(display, menu^.widget.XWINDOW,
	    menu^.widget.dim.width, menu^.widget.dim.height, DefaultDepth(display, screen));

	gcval.font := font^.fid;
	gcval.graphics_exposures := False;
	menu^.gc := XCreateGC(display, menu^.widget.XWINDOW,
	    GCFont or GCGraphicsExposures, @gcval);

	menu^.items.name := nil;
	menu^.items.ptr := nil;
	menu^.items.select := nil;
	menu^.items.prev := @menu^.items;
        menu^.items.next := @menu^.items;
	menu^.nitems := 0;
	menu^.current := -1;

	menu^.widget.event := @menuevent;
	XSelectInput(display, menu^.widget.XWINDOW,
	    ButtonMotionMask or ButtonReleaseMask or ExposureMask);
	Result := menu;
end;

{
 * Resize the menu so that all items fit.
 }
procedure trim(menu: PWMMenu);
var
	ip: PWMmenuitem;
	width: Integer;
	height: Integer;
begin
	width := 0;
	height := 2;
 
        ip := menu^.items.next;
        
	while (ip <> @menu^.items) do
        begin
		height += button_size;
		width := MAX(width, Length(ip^.name) + 2 * button_size);
  
                ip := ip^.next;
        end;
	width := MAX(width, 1);
	height := MAX(height, 1);

	widget_resize(@menu^.widget, width, height);

	XFreePixmap(display, menu^.pixmap);
	menu^.pixmap := XCreatePixmap(display, menu^.widget.XWINDOW,
	    menu^.widget.dim.width, menu^.widget.dim.height, DefaultDepth(display, screen));
end;

{
 * Add an item and automatically resize + repaint.
 }
function menu_additem(menu: PWMMenu; name: string;
    select: select_t; ptr: Pointer): PWMmenuitem;
var
	ip: PWMmenuitem;
begin
	if name = '' then name := '<< Anonymous >>';

	ip := GetMem(sizeof(TWMmenuitem));
	ip^.name := PChar(name);
	stringfit(ip^.name, DisplayWidth(display, screen) / 2);
	ip^.ptr := ptr;
	ip^.select := select;

	ip^.next := menu^.items.next;
	ip^.prev := @menu^.items;
	menu^.items.next^.prev := ip;
	menu^.items.next := ip;
	Inc(menu^.nitems);

	trim(menu);

	if (MAPPED(menu)) then
		menu_repaint(menu);

	Result := ip;
end;

procedure menu_renameitem(menu: PWMMenu; ip: PWMmenuitem; name: string);
begin
	if name = '' then name := '<< Anonymous >>';
 
	FreeMem(ip^.name);
	ip^.name := PChar(name);
	stringfit(ip^.name, DisplayWidth(display, screen) / 2);
	trim(menu);
	if (MAPPED(menu)) then
		menu_repaint(menu);
end;

{
 * Delete an item and automatically resize + repaint.
 }
procedure menu_delitem(menu: PWMMenu; item: PWMmenuitem);
begin
	item^.prev^.next := item^.next;
	item^.next^.prev := item^.prev;
	FreeMem(item^.name);
	FreeMem(item);
	Dec(menu^.nitems);

	trim(menu);

	if (MAPPED(menu)) then
		menu_repaint(menu);
end;

end.

