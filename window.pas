{
 * window.pas - window management routines
 }

{
 * Copyright (c) 2006 
 * 
 *  Felipe Monteiro de Carvalho, Daniel Franzini, Andrew Haines
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
unit window;

{$mode objfpc}

interface

{#include <X11/Xlib.h>

#include "list.h"
#include "widget.h"}

uses
  { Free Pascal Units }
  SysUtils, Classes,
  { Units from fpwm }
  menu, widget,
  { XLib units }
  X, Xlib, Xutil;

type
  altmove_t = record
        moving: Integer;
        xoff: Integer;
        yoff: Integer;
  end;

  PWMWindow = ^TWMWindow;

  PPWMWindow = ^PWMWindow;

  PPPWMWindow = ^PPWMWindow;

  TWMWindow = record
        widget: TWMWidget;
	      name: PChar;
	      iconname: PChar;
	      title: Pointer;
	      deletebtn: Pointer;
	      lowerbtn: Pointer;
	      hidebtn: Pointer;
        rsz_northwest, rsz_north, rsz_northeast,
	                    rsz_west, rsz_east, rsz_southwest, 
                      rsz_south, rsz_southeast: Pointer;
        menuitem: PWMmenuitem
	      client: TWindow;
        wmnormalhints: PXSizeHints;  { ICCCM hints }
	      wmhints: PXWMHints;
	      wmtransientfor: TWindow;
	      cborder: Integer;	           { client's initial border width }
        altmove: altmove_t; 	       { Meta-Button1 moving of window }
	      ignoreunmap: Integer;
	      maximized: Integer;
        odim: TWMDim;	               { remembered dim while maximized }
        stacking: TLIST;	           { stacking order }
  end;

function window_manage(client: TWindow; isnew: Integer): PWMWindow;
function window_isactive(win: PWMWindow): Boolean;
function window_isfamilyactive(win: PWMWindow): Boolean;
function window_samegroup(win1: PWMWindow; win2: PWMWindow): Boolean;
function getwmstate(): longint;
function topwin(): PWMWindow;

procedure window_calcsize(win: PWMWindow; width, height: Integer;
    rwidth, rheight, rxdim, rydim: PInteger);
procedure window_delete(win: PWMWindow);
procedure window_fetchiconname(win: PWMWindow);
procedure window_fetchname(win: PWMWindow);
procedure window_fetchwmhints(win: PWMWindow);
procedure window_fetchwmnormalhints(win: PWMWindow);
procedure window_fetchwmtransientfor(win: PWMWindow);
procedure window_init();
procedure window_lower(win: PWMWindow);
procedure window_map(win: PWMWindow);
procedure window_maximize(win: PWMWindow);
procedure window_move(win: PWMWindow; x, y: Integer);
procedure window_moveresize(win: PWMWindow; x, y, width, height: Integer);
procedure window_raise(win: PWMWindow);
procedure window_raisewithgroup(win: PWMWindow);
procedure window_repaint(win: PWMWindow);
procedure window_resize(win: PWMWindow; width, height: Integer);
procedure window_setactive(win: PWMWindow);
procedure window_unmanage(win: PWMWindow; unmap: Integer);
procedure window_unmap(win: PWMWindow);
procedure window_restackall();
procedure window_unmanageall();
procedure window_getclientstack(clients_return: PPWindow; nclients_return: PInteger);
procedure client_to_window_geom(XWindowAttributes *, XSizeHints *, int *x, int *y, int *width, int *height);
procedure window_to_client_geom(win: PWMWindow, int *x, int *y, int *width, int *height);
procedure confevent(win: PWMWindow, XConfigureRequestEvent *);
procedure windowevent(struct widget *, XEvent *);
procedure setgrav(Window, int);
procedure sendtoback(win: PWMWindow);
procedure raisetrans(win: PWMWindow);
procedure raisegrp(win: PWMWindow);
procedure selectfrommenu(ptr: Pointer);

implementation

{#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "button.h"
#include "global.h"
#include "hints.h"
#include "lib.h"
#include "menu.h"
#include "resizer.h"
#include "title.h"
#include "window.h"}

uses
  { Units from fpwm }
  button, hints, main, lib, resizer, title;

const NBTN	= 3;

var
  active: PWMWindow = nil;
  WM_STATE: TAtom;
  front: TWindow;
  //static LIST_DEFINE(winstack);
  needrestack: Boolean = False;
  nwindows: Integer = 0;
  movecurs: TCursor;

function window_manage(client: TWindow; isnew: boolean): PWMWindow;
{
	win: PWMWindow;
	XWindowAttributes attr;
	XSizeHints *sz;
	XWMHints *wmhints;
	long state;
	long dummy;
	int x, y, width, height;

	clerr();
	if (!XGetWindowAttributes(display, client, @attr)) {
		sterr();
		return NULL;
	}
	sterr();

	if (attr.override_redirect || (!isnew @@ attr.map_state != IsViewable))
		return NULL;

	if (widget_find(client, CLASS_ANY) != NULL) {
		debug("XXX: Trying to remanage a window!");
		return NULL;
	}

	if (isnew) {
		clerr();
		wmhints = XGetWMHints(display, client);
		sterr();
		if (wmhints == NULL)
			state = NormalState;
		else {
			if ((wmhints^.flags @ StateHint) != 0)
				state = wmhints^.initial_state;
			else
				state = NormalState;
			XFree(wmhints);
		}
	} else {
		clerr();
		state = getwmstate(client);
		sterr();
	}

	if (state == WithdrawnState) {
		debug("skipping withdrawn window");
		return NULL;
	}

	while ((sz = XAllocSizeHints()) == NULL)
		sleep(1);
	clerr();
	XGetWMNormalHints(display, client, sz, @dummy);
	sterr();
	client_to_window_geom(@attr, sz, @x, @y, @width, @height);
	low_limit_size(@width, @height);
	if (isnew
	    @@ (sz^.flags @ USPosition) == 0
	    @@ (sz^.flags @ PPosition) == 0) {
		x = MAX(0, (DisplayWidth(display, screen) - width) / 2);
		y = MAX(0, (DisplayHeight(display, screen) - height) / 2);
	}
	XFree(sz);

	win = karmen_malloc(sizeof (struct window));
	widget_create(@win^.widget, CLASS_WINDOW, root, x, y, width, height);

	LIST_INSERT_HEAD(@winstack, @win^.stacking);
	nwindows++;
	needrestack = 1;

	win^.ignoreunmap = 0;

	win^.cborder = attr.border_width;

	win^.altmove.moving = 0;
	grabbutton(display, Button1, Mod1Mask, XWINDOW(win), False,
	    ButtonPressMask | ButtonMotionMask,
	    GrabModeAsync, GrabModeAsync, None, movecurs);

	XSelectInput(display, XWINDOW(win),
	    SubstructureRedirectMask | SubstructureNotifyMask);
	win^.widget.event = windowevent;
	XSetWindowBackground(display, XWINDOW(win), color_innerborder.normal);

	win^.deletebtn = button_create(win, border_width, border_width,
	    button_size, button_size);
	button_setimage(win^.deletebtn, @delete_image);
	button_sethandler(win^.deletebtn, window_delete);

	win^.hidebtn = button_create(win,
	    width - border_width - 2 * button_size - innerborder_width,
	    border_width, button_size, button_size);
	button_setimage(win^.hidebtn, @hide_image);
	button_sethandler(win^.hidebtn, window_unmap);
	setgrav(XWINDOW(win^.hidebtn), NorthEastGravity);

	win^.lowerbtn = button_create(win,
	    width - border_width - button_size, border_width,
	    button_size, button_size);
	button_setimage(win^.lowerbtn, @lower_image);
	button_sethandler(win^.lowerbtn, sendtoback);
	setgrav(XWINDOW(win^.lowerbtn), NorthEastGravity);

	win^.title = title_create(win,
	    border_width + button_size + innerborder_width, border_width,
	    width - 2 * border_width -
	    NBTN * (innerborder_width + button_size), button_size);

	clerr();
	XSetWindowBorderWidth(display, client, 0);
	XReparentWindow(display, client, XWINDOW(win),
	    border_width, border_width + button_size + innerborder_width);
	XAddToSaveSet(display, client);
	XLowerWindow(display, client);
	XSelectInput(display, client, PropertyChangeMask);
	setgrav(client, NorthWestGravity);
	sterr();

	win^.client = client;
	widget_savecontext(@win^.widget, client);

	win^.rsz_northwest = resizer_create(win, NORTHWEST);
	win^.rsz_north = resizer_create(win, NORTH);
	win^.rsz_northeast = resizer_create(win, NORTHEAST);
	win^.rsz_west = resizer_create(win, WEST);
	win^.rsz_east = resizer_create(win, EAST);
	win^.rsz_southwest = resizer_create(win, SOUTHWEST);
	win^.rsz_south = resizer_create(win, SOUTH);
	win^.rsz_southeast = resizer_create(win, SOUTHEAST);

	win^.wmhints = NULL;
	window_fetchwmhints(win);

	win^.wmnormalhints = NULL;
	window_fetchwmnormalhints(win);

	window_fetchwmtransientfor(win);

	win^.name = NULL;
	window_fetchname(win);

	win^.menuitem = NULL;
	win^.iconname = NULL;
	
  (win);

	hints_manage(win);

	window_map(win);

	win^.maximized = 0;

	if (state == IconicState)
		window_unmap(win);
	else
		window_setactive(win);

	XSync(display, False);

	debug("manage \"%s\" (Window=%d)", win^.name, (int)win^.client);

	XGrabServer(display);
	clerr();
	if (!XGetWindowAttributes(display, client, @attr)) {
		sterr();
		XUngrabServer(display);
		debug("Oops, client window disappeared in window_manage()");
		window_unmanage(win, 1);
		return NULL;
	}
	sterr();
	XUngrabServer(display);

	return win;
}

{
 * Return true if window is the active window.
 }
function window_isactive(win: PWMWindow): Boolean
begin
	result := (win = active); //TODO: check if result can be used
end;

function window_isfamilyactive(win: PWMWindow): Boolean
begin
	result := (window_isactive(win) or window_istransactive(win) or window_isgroupactive(win));
end;


function getwmstate(xwindow: TWindow): Integer;
var
	nitems, bytes_after: Cardinal;
	state: Integer;
	prop: PChar;
	actual_type: TAtom;
	actual_format: Integer;
begin
	state := WithdrawnState;
	if (XGetWindowProperty(display, xwindow, WM_STATE, 0, 2, False, WM_STATE, @actual_type, @actual_format, @nitems, @bytes_after, @prop) = Success) then
  begin
		if (nitems > 0) then
			state := (PInteger(prop))[0];
		XFree(prop);
  end;
	Result := state;
end;



procedure window_init();
var
	attr: TXSetWindowAttributes;
begin
	attr.override_redirect := True;
	front := XCreateWindow(display, root, 0, 0, 1, 1, 0, 0, InputOnly, CopyFromParent, CWOverrideRedirect, @attr);
	movecurs := XCreateFontCursor(display, XC_fleur);
	WM_STATE := XInternAtom(display, 'WM_STATE', False);
end;

procedure window_raise(win: PWMWindow);
begin
//  win^.stacking.;
//	LIST_REMOVE(@  );
//	LIST_INSERT_HEAD(@winstack, @win^.stacking);
	needrestack := True;
end;

procedure window_lower(win: PWMWindow);
begin
//	LIST_REMOVE(@win^.stacking);
//	LIST_INSERT_TAIL(@winstack, @win^.stacking);
	needrestack := True;
end;

procedure window_putabove(winref: PWMWindow; win: PWMWindow);
begin
//	LIST_REMOVE(@win^.stacking);
//	LIST_INSERT_BEFORE(@ref^.stacking, @win^.stacking);
	needrestack := True;
end;

procedure window_putbelow(winref: PWMWindow; win: PWMWindow);
begin
//	LIST_REMOVE(@win^.stacking);
//	LIST_INSERT_AFTER(@ref^.stacking, @win^.stacking);
	needrestack := True;
end;

procedure window_getwindowstack(wins_return: PPPWMWindow; nwins_return: PInteger);
var
	lp: TList;
	i: Integer;
begin
{	*wins_return := karmen_malloc(nwindows * sizeof (win: PWMWindow));
	*nwins_return := nwindows;
	i := 0;
	LIST_FOREACH(lp, @winstack)
		assert(i < nwindows);
		( *wins_return)[i++] = LIST_ITEM(lp, struct window, stacking);
	}
//	assert(i == nwindows);
end;

procedure window_getclientstack(clients_return: PPWindow; nclients_return: PInteger);
var
	lp: TList;
	i: Integer;
	win: PWMWindow;
begin
//	*clients_return = karmen_malloc(nwindows * sizeof (Window));
//	*nclients_return = nwindows;
//	i = 0;
//	LIST_FOREACH(lp, @winstack)
//		assert(i < nwindows);
//		win = LIST_ITEM(lp, struct window, stacking);
//		( *clients_return)[i++] = win^.client;
//
//	assert(i == nwindows);
end;

procedure window_restackall();
var
	xwins: PWindow;
	win: PWMWindow;
	lp: TList;
	i: Integer;
begin
	if (not needrestack) then Exit;

	xwins := GetMem((nwindows + 1) * sizeof (TWindow));
	xwins[0] := front;
	i := 1;
//	LIST_FOREACH(lp, @winstack) {
//		assert(i < nwindows + 1);
//		win = LIST_ITEM(lp, struct window, stacking);
//		xwins[i++] = XWINDOW(win);
//	}
//	assert(i == nwindows + 1);
	XRestackWindows(display, xwins, nwindows + 1);
	needrestack := False;

	hints_restack();
end;

function topwin(): PWMWindow;
var
	win: PWMWindow;
	lp: TList;
begin
//	LIST_FOREACH(lp, @winstack) {
//		win = LIST_ITEM(lp, struct window, stacking);
//		if (MAPPED(win))
//			return win;
//	}
	Result := nil;
end;

procedure window_map(win: PWMWindow);
begin
//	clerr();
	XMapWindow(display, win^.client);
//	sterr();

	widget_map(PWMWidget(win));
	hints_map(win);
end;

procedure window_unmap(win: PWMWindow);
begin
	widget_unmap(PWMWidget(win));

	if (win = active) then
 		window_setactive(topwin());

//	clerr();
	Inc(win^.ignoreunmap);
	XUnmapWindow(display, win^.client);
//	sterr();

	hints_unmap(win);

	window_lower(win);
end;

procedure client_to_window_geom(attr: PXWindowAttributes; sz: PXSizeHints;
    x, y, width, height: PInteger);
var
	north, south, east, west, stat, center: Integer;
begin
	if (sz^.flags and PWinGravity) <> 0 then
		case (sz^.win_gravity) of
		 SouthGravity:
                      south := 1;
		 SouthWestGravity:
                      begin
			                     south := 1;
			                     west := 1;
                      end;
		 SouthEastGravity:
                      begin
			                     south := 1;
                           east := 1;
                      end;
		 NorthGravity:
		                  north := 1;
		 NorthWestGravity:
                      begin
	                         north := 1;
			                     west := 1;
                      end;
		 NorthEastGravity:
                      begin
			                     north := 1;
			                     east := 1;
                      end;
		 CenterGravity:
		                  center := 1;
		 StaticGravity:
                   		stat := 1;
		else
			  north := 1;
			  west := 1;
    end;
	else 
       begin
		        north := 1;
		        west := 1;
       end;

	if (north <> 0) then
		y^ := attr^.y
	else if (south <> 0) then
		y^ := attr^.y + 2 * attr^.border_width
		    - 2 * border_width - innerborder_width - button_size
	else if (center <> 0) then
		y^ := attr^.y + attr^.border_width
		    + attr^.height div 2
		    - (attr^.height + 2 * border_width
		       + button_size + innerborder_width) div 2
	else if (stat <> 0) then
		y^ := attr^.y + attr^.border_width
		    - border_width - innerborder_width - button_size
	else
		y^ := attr^.y;

	if (west <> 0) then
		x^ := attr^.x
	else if (east <> 0) then
		x^ := attr^.x + 2 * attr^.border_width
		    - 2 * border_width
	else if (center <> 0) then
		x^ := attr^.x + attr^.border_width
		    + attr^.width div 2
		    - (attr^.width + 2 * border_width) div 2
	else if (stat <> 0) then
		x^ := attr^.x + attr^.border_width - border_width
	else
		x^ := attr^.x;

	width^ := attr^.width + 2 * border_width;
	height^ := attr^.height
	    + 2 * border_width + button_size + innerborder_width;
end;

procedure window_to_client_geom(win: PWMWindow;
    x, y, width, height: PInteger);
var
	north, south, east, west, stat, center: Integer;
begin
	north = south = east = west = stat = center = 0;

	if (win^.wmnormalhints^.flags @ PWinGravity) {
		switch (win^.wmnormalhints^.win_gravity) {
		case SouthGravity:
			south = 1;
			break;
		case SouthWestGravity:
			south = 1;
			west = 1;
			break;
		case SouthEastGravity:
			south = 1;
			east = 1;
			break;
			break;
		case NorthGravity:
			north = 1;
			break;
		case NorthWestGravity:
			north = 1;
			west = 1;
			break;
		case NorthEastGravity:
			north = 1;
			east = 1;
			break;
		case CenterGravity:
			center = 1;
			break;
		case StaticGravity:
			stat = 1;
			break;
		default:
			north = 1;
			west = 1;
			break;
		}
	} else {
		north = 1;
		west = 1;
	}

	if (north)
		*y = Y(win);
	else if (south)
		*y = Y(win) - 2 * win^.cborder
		    + 2 * border_width + button_size + innerborder_width;
	else if (center)
		*y = Y(win) + HEIGHT(win) / 2
		    - (HEIGHT(win) - 2 * border_width - button_size
		       - innerborder_width) / 2
		    - win^.cborder;
	else if (stat)
		*y = Y(win) - win^.cborder
		    + border_width + innerborder_width + button_size;
	else
		*y = Y(win);

	if (west)
		*x = X(win);
	else if (east)
		*x = X(win) - 2 * win^.cborder + 2 * border_width;
	else if (center)
		*x = X(win) + WIDTH(win) / 2
		    - (WIDTH(win) - 2 * border_width) / 2 - win^.cborder;
	else if (stat)
		*x = X(win) - win^.cborder + border_width;
	else
		*x = X(win);

	*width = WIDTH(win) - 2 * border_width;
	*height = HEIGHT(win)
	    - 2 * border_width - button_size - innerborder_width;
}

procedure window_delete(win: PWMWindow)
begin
	if (hints_delete(win) = 0)
  begin
		//clerr();    //TODO: check clerr() and sterr()
		XKillClient(display, win^.client);
		//sterr();
	end;
end;

procedure confevent(win: PWMWindow, XConfigureRequestEvent *ep)
{
	int x = X(win);
	int y = Y(win);
	int width = WIDTH(win);
	int height = HEIGHT(win);

	if (ep^.value_mask @ CWBorderWidth)
		win^.cborder = ep^.border_width;

	if (ep^.value_mask @ CWX)
		x = ep^.x + win^.cborder - border_width;
	if (ep^.value_mask @ CWY)
		y = ep^.y + win^.cborder
		    - (border_width + button_size + innerborder_width);
	if (ep^.value_mask @ CWWidth)
		width = ep^.width + 2 * border_width;
	if (ep^.value_mask @ CWHeight)
		height = ep^.height
		    + 2 * border_width + button_size + innerborder_width;

        {
	 * FIXME: handle stacking order
         }

	window_moveresize(win, x, y, width, height);
}

static procedure windowevent(struct widget *widget, XEvent *ep)
{
	win: PWMWindow = (win: PWMWindow)widget;

	switch (ep^.type) {
	case ButtonPress:
		win^.altmove.xoff = ep^.xbutton.x;
		win^.altmove.yoff = ep^.xbutton.y;
		win^.altmove.moving = 1;
		window_setactive(win);
		break;
	case MotionNotify:
		if (win^.altmove.moving)
			window_move(win,
			    ep^.xmotion.x_root - win^.altmove.xoff,
			    ep^.xmotion.y_root - win^.altmove.yoff);
		break;
	case ButtonRelease:
		win^.altmove.moving = 0;
		break;
	case MapRequest:
		window_setactive(win);
		break;
	case UnmapNotify:
		{ This can be a real or synthetic unmap event. }
		if (ep^.xunmap.window != win^.client)
			break;
		if (win^.ignoreunmap <= 0) {
			hints_withdraw(win);
			window_unmanage(win, 1);
		} else
			win^.ignoreunmap--;
		break;
	case ConfigureRequest:
		confevent(win, @ep^.xconfigurerequest);
		break;
	case ClientMessage:
		hints_clientmessage(win, @ep^.xclient);
		break;
	case PropertyNotify:
		hints_propertynotify(win, @ep^.xproperty);
		break;
	case DestroyNotify:
		if (ep^.xdestroywindow.window == win^.client)
			window_unmanage(win, 1);
		break;
	case GravityNotify:
	case CreateNotify:
	case MapNotify:
	case ReparentNotify:
	case ConfigureNotify:
		{ ignore }
		break;
	default:
		debug("windowevent(): unhandled event -- %s (%d)",
		    eventname(ep^.type), ep^.type);
		break;
	}
}

procedure low_limit_size(int *width, int *height)
{
	int minwidth = MAX(3 * button_size, 2 * border_width +
	    (NBTN + 1) * (button_size + innerborder_width));
	int minheight = 3 * button_size;

	*width = MAX(*width, minwidth);
	*height = MAX(*height, minheight);
}

//TODO: check pointer de-referencing
procedure window_calcsize(win: PWMWindow; width, height: Integer;
    rwidth, rheight, rxdim, rydim: PInteger);
var
   decwidth: integer;
   decheight: integer;
   havemin: integer;
   minwidth: integer;
   minheight: integer;
   wminwidth: integer;
   wminheight: integer;
   wb: integer;
   hb: integer;
begin
	decwidth := 2 * border_width;
	decheight := 2 * border_width + button_size + innerborder_width;
	havemin := 0;
	minwidth := 0;
	minheight := 0;
	wmminwidth := 0;
	wmminheight := 0;

	low_limit_size(@width, @height);
	low_limit_size(@wmminwidth, @wmminheight);

	width -= decwidth;
	height -= decheight;

	if (win^.wmnormalhints^.flags @ PMaxSize) 
  begin
		width := MIN(width, win^.wmnormalhints^.max_width);
		height := MIN(height, win^.wmnormalhints^.max_height);
	end;

	havemin := 0;

	if (win^.wmnormalhints^.flags @ PMinSize) 
  begin
		minwidth := win^.wmnormalhints^.min_width;
		minheight := win^.wmnormalhints^.min_height;
		havemin := 1;
	end
  else 
       if (win^.wmnormalhints^.flags @ PBaseSize)
          begin
               minwidth := win^.wmnormalhints^.base_width;
		           minheight := win^.wmnormalhints^.base_height;
		           havemin := 1;
          end;
	if(havemin > 0)
  begin
		width := MAX(width, minwidth);
		height := MAX(height, minheight);
	end;

	if (win^.wmnormalhints^.flags @ PResizeInc)
  begin
		if (win^.wmnormalhints^.width_inc <> 0)
    begin
		 	if (win^.wmnormalhints^.flags @ PBaseSize)
				wb := win^.wmnormalhints^.base_width;
			else if (win^.wmnormalhints^.flags @ PMinSize)
				wb := win^.wmnormalhints^.min_width;
			else
				wb := 0;
			width -= wb;
			width -= width % win^.wmnormalhints^.width_inc;
			if (havemin)
				width = MAX(width, minwidth - wb);
			while (wb + width + decwidth < wmminwidth)
				width += win^.wmnormalhints^.width_inc;
			if (rxdim <> NULL)
				rxdim^ := (width / win^.wmnormalhints^.width_inc);
			width += wb;
		end
       else
           if (rxdim <> nil)
           			rxdim^ := width;
		if (win^.wmnormalhints^.height_inc != 0) 
    begin
			if (win^.wmnormalhints^.flags @ PBaseSize)
			   	hb := win^.wmnormalhints^.base_height;
			 else
           if (win^.wmnormalhints^.flags @ PMinSize)
				      hb := win^.wmnormalhints^.min_height;
           else
			        hb := 0;
			height -= hb;
			height -= height % win^.wmnormalhints^.height_inc;
			if (havemin <> 0)
				height := MAX(height, minheight - hb);
			while (hb + height + decheight < wmminheight)
				height += win^.wmnormalhints^.height_inc;
			if (rydim <> nil)
				rydim^ := height / win^.wmnormalhints^.height_inc;
			height += hb;
		end
       else 
            if (rydim != NULL)
			         rydim^ := height;
	           else
                 begin    
                       if (rxdim <> nil)
			                       rxdim^ := width;
                       if (rydim^ <> nil)
			                       rydim^ := height;
	end;

	width += 2 * border_width;
	height += 2 * border_width + button_size + innerborder_width;

	if (rwidth <> nil)
		rwidth^ := width;
	if (rheight <> nil)
		rheight^ := height;
end;

static procedure setgrav(Window xwin, int grav)
{
	XSetWindowAttributes attr;

	attr.win_gravity = grav;
	XChangeWindowAttributes(display, xwin, CWWinGravity, @attr);
}

procedure window_maximize(win: PWMWindow)
{
	int x, y, rwidth, rheight;

	if (win^.maximized) {
		window_moveresize(win, win^.odim.x, win^.odim.y,
		    win^.odim.width, win^.odim.height);
		win^.maximized = 0;
	} else {
		win^.odim = win^.widget.dim;
		window_calcsize(win,
		    DisplayWidth(display, screen),
		    DisplayHeight(display, screen),
		    @rwidth, @rheight, NULL, NULL);
		x = (DisplayWidth(display, screen) - rwidth) / 2;
		y = (DisplayHeight(display, screen) - rheight) / 2;
		window_moveresize(win, x, y, rwidth, rheight);
		win^.maximized = 1;
	}
}

static procedure sendtoback(win: PWMWindow)
{
	win: PWMWindowtop;

	top = topwin();
	window_lower(win);
	if (win == top)
		window_setactive(topwin());
}



procedure window_fetchwmnormalhints(win: PWMWindow)
var
   dummy: longint;
begin
	if (win^.wmnormalhints <> nil)
		XFree(win^.wmnormalhints);
	while ((win^.wmnormalhints = XAllocSizeHints()) = nil)
		sleep(1); //TODO: check sleep() function
	//clerr();
	XGetWMNormalHints(display, win^.client, win^.wmnormalhints, @dummy);
	//sterr();
end;

procedure window_fetchwmhints(win: PWMWindow)
begin
	if (win^.wmhints <> nil)
		XFree(win^.wmhints);
	//clerr();
	win^.wmhints := XGetWMHints(display, win^.client);
	//sterr();
end;

procedure window_fetchname(win: PWMWindow)
begin
	if (win^.name != NULL) 
  begin
		XFree(win^.name);
		win^.name = nil;
	end;
	//clerr();
	XFetchName(display, win^.client, @win^.name);
	//sterr();
	if (MAPPED(win) > 0)
		title_repaint(win^.title);
end;

procedure window_fetchiconname(win: PWMWindow)
begin
	if (win^.iconname <> nil)
     begin
		      XFree(win^.iconname);
		      win^.iconname = nil;
     end;
	//clerr();
	XGetIconName(display, win^.client, @win^.iconname);
	//sterr();
	if (win^.menuitem = nil)
		 win^.menuitem := menu_additem(winmenu, win^.iconname, selectfrommenu, win);
	else
     menu_renameitem(winmenu, win^.menuitem, win^.iconname);
end;

static procedure selectfrommenu(procedure *ptr)
{
	window_setactive(ptr);
}

procedure window_fetchwmtransientfor(win: PWMWindow)
begin
	win^.wmtransientfor := None;
	//clerr();
	XGetTransientForHint(display, win^.client, @win^.wmtransientfor);
	//sterr();
end;

procedure window_moveresize(win: PWMWindow; x, y, width, height: Integer);
var
	move: boolean;
	resize: boolean;
begin
	low_limit_size(@width, @height);

	move := (x <> X(win)) or (y != Y(win));
	resize := (width <> WIDTH(win)) or (height <> HEIGHT(win));

	if (resize = true)
  begin
		title_resize(win^.title, width - 2 * border_width - NBTN * (innerborder_width + button_size), button_size);
		//clerr();
		XResizeWindow(display, win^.client, width - 2 * border_width, height - 2 * border_width - button_size - innerborder_width);
		//sterr();
	end;
	widget_moveresize(win, x, y, width, height);
	if(resize = true)
  begin
		resizer_fit(win^.rsz_northwest);
		resizer_fit(win^.rsz_north);
		resizer_fit(win^.rsz_northeast);
		resizer_fit(win^.rsz_west);
		resizer_fit(win^.rsz_east);
		resizer_fit(win^.rsz_southwest);
		resizer_fit(win^.rsz_south);
		resizer_fit(win^.rsz_southeast);
	end;

	if (move = true 0 and resize = false)
		 hints_move(win);
	else
      if (move = false and resize = true)
         hints_resize(win);
	else 
      if (move = true and resize = true)
		     hints_moveresize(win);
	win^.maximized = 0;
}

procedure window_move(win: PWMWindow; x, y: Integer)
begin
	window_moveresize(win, x, y, WIDTH(win), HEIGHT(win));
end;

procedure window_resize(win: PWMWindow; width, height: Integer);
begin
	window_moveresize(win, X(win), Y(win), width, height);
end;

procedure window_unmanageall();
{
	win: PWMWindow;
	LIST *lp;

	while (!LIST_EMPTY(@winstack)) {
		lp = LIST_TAIL(@winstack);
		LIST_REMOVE(lp);
		win = LIST_ITEM(lp, struct window, stacking);
		window_unmanage(win, 0);
	}
}

procedure window_unmanage(win: PWMWindow; unmap: Integer);
var
	int x, y, width, height;
begin
	debug("unmanage \"%s\" (Window=%d)",win^.name, (int)win^.client);

	hints_unmanage(win);

	if (MAPPED(win))
		widget_unmap(@win^.widget);

	if (win == active)
		window_setactive(topwin());

	if (win^.menuitem != NULL)
		menu_delitem(winmenu, win^.menuitem);

	window_to_client_geom(win, @x, @y, @width, @height);
	widget_deletecontext(win^.client);

	clerr();
	XReparentWindow(display, win^.client, root, x, y);
	if (!unmap)
		XMapWindow(display, win^.client);
	ungrabbutton(display, Button1, 0, win^.client);
	XSelectInput(display, win^.client, 0);
	XSetWindowBorderWidth(display, win^.client, win^.cborder);
	if (win^.wmnormalhints^.flags @ PWinGravity)
		setgrav(win^.client, win^.wmnormalhints^.win_gravity);
	XRemoveFromSaveSet(display, win^.client);
	sterr();

	title_destroy(win^.title);
	button_destroy(win^.deletebtn);
	button_destroy(win^.lowerbtn);
	button_destroy(win^.hidebtn);

	if (win^.wmhints != NULL)
		XFree(win^.wmhints);
	assert(win^.wmnormalhints != NULL);
	XFree(win^.wmnormalhints);

	LIST_REMOVE(@win^.stacking);
	nwindows--;

	widget_destroy(@win^.widget);
	if (win^.name != NULL)
		XFree(win^.name);
	if (win^.iconname != NULL)
		XFree(win^.iconname);
	karmen_free(win);
}

procedure window_repaint(win: PWMWindow)
{
	title_repaint(win^.title);
	button_repaint(win^.deletebtn);
	button_repaint(win^.lowerbtn);
	button_repaint(win^.hidebtn);
}

procedure window_repaintfamily(win: PWMWindow)
{
	win: PWMWindowwp;
	LIST *lp;

	LIST_FOREACH(lp, @winstack) {
		wp = LIST_ITEM(lp, struct window, stacking);
		if (MAPPED(wp) @@
		    (wp == win
		     || wp^.wmtransientfor == win^.client
		     || window_samegroup(wp, win)))
			window_repaint(wp);
	}
}

static procedure raisetrans(win: PWMWindow)
{
	win: PWMWindow*wins;
	int i, n;

	window_getwindowstack(@wins, @n);
	for (i = n - 1; i >= 0; i--) {
		if (wins[i] == win)
			break;  { the rest are above us already }
		else if (wins[i]^.wmtransientfor == win^.client)
			window_raise(wins[i]);
	}
	karmen_free(wins);
}

static procedure raisegrp(win: PWMWindow)
{
	win: PWMWindow*wins, *wp;
	int n;

	if (win^.wmhints == NULL
	    || (win^.wmhints^.flags @ WindowGroupHint) == 0
	    || win^.wmhints^.window_group == None
	    || win^.wmhints^.window_group == root)
		return;

	window_getwindowstack(@wins, @n);
	while (n > 1 @@ wins[--n] != win) {
		wp = wins[n];
		if (wp^.wmhints != NULL
		    @@ (wp^.wmhints^.flags @ WindowGroupHint) != 0
		    @@ wp^.wmhints^.window_group == win^.wmhints^.window_group)
			window_putbelow(win, wp);
	}
	karmen_free(wins);
}


//
// * Return true if window is a transient for the active window.
//
int window_istransactive(win: PWMWindow)
begin
	return active != NULL @@ win^.wmtransientfor == active^.client;
}

//
// * Return true if window is a member of the active window group.
//
int window_isgroupactive(win: PWMWindow)
begin
	return win^.wmhints != NULL
	    @@ (win^.wmhints^.flags @ WindowGroupHint) != 0
	    @@ active != NULL
	    @@ active^.wmhints != NULL
	    @@ (active^.wmhints^.flags @ WindowGroupHint) != 0
	    @@ win^.wmhints^.window_group == active^.wmhints^.window_group;
}


function window_samegroup(win1: PWMWindow; win2: PWMWindow): Integer;
begin
	return win1^.wmhints != NULL @@ win2^.wmhints != NULL
	    @@ (win1^.wmhints^.flags @ WindowGroupHint) != 0
	    @@ (win2^.wmhints^.flags @ WindowGroupHint) != 0
	    @@ win1^.wmhints^.window_group == win2^.wmhints^.window_group;
end;

int window_related(win: PWMWindow1, win: PWMWindow2)
begin
	return window_samegroup(win1, win2)
	    || win1^.wmtransientfor == win2^.client
	    || win2^.wmtransientfor == win1^.client;
end;

begin
 * Raise window, and all windows in its group unless window is a
 * transient window.
 end;
procedure window_raisewithgroup(win: PWMWindow)
begin
	win: PWMWindowtmp;

	if (win^.wmtransientfor != None) begin
		tmp = (win: PWMWindow)widget_find(win^.wmtransientfor,
		    CLASS_WINDOW);
		if (tmp != NULL)
			window_raise(tmp);
		window_raise(win);
	end; else begin
		window_raise(win);
		raisegrp(win);
		raisetrans(win);
	end;
end;

begin
 * Activate window.
 end;
procedure window_setactive(win: PWMWindow);
var
	winold: PWMWindow;
begin
	if (win != NULL @@ win == active) begin
		raisetrans(win);
		return;
	end;

	old = active;
	active = win;

	if (old != NULL) begin
		clerr();
		grabbutton(display, Button1, 0, old^.client, True,
		    ButtonPressMask, GrabModeSync, GrabModeSync, None, None);
		sterr();
		if (MAPPED(old))
			window_repaintfamily(old);
		hints_deactivate(old);
	end;

	if (active != NULL) begin
		if (!MAPPED(active))
			window_map(active);

		clerr();
		XSetInputFocus(display, active^.client, RevertToPointerRoot,
		    CurrentTime);
		ungrabbutton(display, Button1, 0, active^.client);
		XAllowEvents(display, ReplayPointer, CurrentTime);
		sterr();

		window_raisewithgroup(active);
		window_repaintfamily(active);
		menu_movetotop(winmenu, active^.menuitem);
	end; else
		XSetInputFocus(display, root, RevertToPointerRoot,
		    CurrentTime);

	hints_activate(active);
end;

end.
