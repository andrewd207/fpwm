{
 * Inter-Client Communication Conventions Manual (ICCCM) hints
 *
 * NOTE: Since the ICCCM is so closely tied to window management, much
 *       of the standard is hardwired into "window.c".
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
unit icccm;

interface

{#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "global.h"
#include "hints.h"
#include "lib.h"
#include "window.h"}

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  hints,
  { XLib units }
  X, Xlib, Xutil;

{static Atom WM_CHANGE_STATE;
static Atom WM_DELETE_WINDOW;
static Atom WM_PROTOCOLS;
static Atom WM_STATE;}

procedure icccm_init();
{static void icccm_manage(struct window *);
static void icccm_map(struct window *);
static void icccm_unmap(struct window *);
static void icccm_withdraw(struct window *);
static void icccm_move(struct window *);
static int icccm_clientmessage(struct window *, XClientMessageEvent *);
static int icccm_propertynotify(struct window *, XPropertyEvent *);
static int icccm_delete(struct window *);
static int knowsproto(struct window *, Atom);
static void sendmesg(struct window *, Atom, long);
static void sendconf(struct window *);
static void setwmstate(struct window *, long);}

var
  icccm_hints: TWMHHints = (
	name: 'Inter-Client Communication Conventions Manual (ICCCM)';

	init: @icccm_init;
	manage: @icccm_manage;
	map: @icccm_map;
	unmap: @icccm_unmap;
	withdraw: @icccm_withdraw;
	move: @icccm_move;
	clientmessage: @icccm_clientmessage;
	propertynotify: @icccm_propertynotify;
	delete: @icccm_delete;
  );

implementation

procedure icccm_init(void)
begin
	WM_CHANGE_STATE = XInternAtom(display, "WM_CHANGE_STATE", False);
	WM_DELETE_WINDOW = XInternAtom(display, "WM_DELETE_WINDOW", False);
	WM_PROTOCOLS = XInternAtom(display, "WM_PROTOCOLS", False);
	WM_STATE = XInternAtom(display, "WM_STATE", False);
end;

procedure icccm_manage(struct window *win)
begin
	sendconf(win);
end;

procedure icccm_map(struct window *win)
begin
	setwmstate(win, NormalState);
end;

procedure icccm_unmap(struct window *win)
begin
	setwmstate(win, IconicState);
end;

procedure icccm_withdraw(struct window *win)
begin
	setwmstate(win, WithdrawnState);
end;

procedure icccm_move(struct window *win)
begin
	sendconf(win);
end;

function icccm_clientmessage(struct window *win, XClientMessageEvent *ep): Integer;
begin
	if (ep->message_type == WM_CHANGE_STATE && ep->format == 32) {
		switch (ep->data.l[0]) {
		case IconicState:
			window_unmap(win);
			return 1;
		case NormalState:
			window_map(win);
			return 1;
		}
	}
	return 0;
end;

function icccm_propertynotify(struct window *win, XPropertyEvent *ep): Integer;
begin
	switch (ep->atom) {
	case XA_WM_NAME:
		if (ep->state != PropertyDelete)
			window_fetchname(win);
		return 1;
	case XA_WM_ICON_NAME:
		if (ep->state != PropertyDelete)
			window_fetchiconname(win);
		return 1;
	case XA_WM_NORMAL_HINTS:
		window_fetchwmnormalhints(win);
		return 1;
	case XA_WM_HINTS:
		window_fetchwmhints(win);
		return 1;
	case XA_WM_TRANSIENT_FOR:
		window_fetchwmtransientfor(win);
		return 1;
	}
	return 0;
end;

function icccm_delete(win: PTWindow): Integer;
begin
	if (knowsproto(win, WM_DELETE_WINDOW)) then
        begin
		sendmesg(win, WM_PROTOCOLS, WM_DELETE_WINDOW);
		return 1;
        end
        else Result := 0;
end;

function knowsproto(win: PTWindow; Atom proto): Integer;
begin
	Atom *protocols;
	int i, n;
	int found;
var
	found = 0;
	clerr();
	if (XGetWMProtocols(display, win->client, &protocols, &n)) {
		for (i = 0; !found && i < n; i++) {
			if (protocols[i] == proto)
				found = 1;
		}
		if (protocols != NULL)
			XFree(protocols);
	}
	sterr();
	return found;
end;

procedure sendmesg(win: PTWindow; Atom type, long value)
var
	XEvent ev;
begin
	memset(&ev, 0, sizeof ev);
	ev.xclient.type = ClientMessage;
	ev.xclient.window = win->client;
	ev.xclient.message_type = type;
	ev.xclient.format = 32;
	ev.xclient.data.l[0] = value;
	ev.xclient.data.l[1] = CurrentTime;
	clerr();
	XSendEvent(display, win->client, False, 0L, &ev);
	sterr();
end;

procedure sendconf(win: PTWindow);
var
	XConfigureEvent conf;
begin
	conf.type = ConfigureNotify;
	conf.event = win->client;
	conf.window = win->client;
	conf.x = X(win) + border_width - win->cborder;
	conf.y = Y(win) + border_width + button_size + innerborder_width
	    - win->cborder;
	conf.width = WIDTH(win) - 2 * border_width;
	conf.height = HEIGHT(win)
	    - (2 * border_width + button_size + innerborder_width);
	conf.border_width = win->cborder;
	conf.above = None;
	conf.override_redirect = False;

	clerr();
	XSendEvent(display, win->client, False, StructureNotifyMask,
	    (XEvent *)&conf);
	sterr();
end;

procedure setwmstate(win: PTWindow; state: Integer);
var
	data: array[0..1] of Integer;
begin
	data[0] = state;
	data[1] = (long)None;
	clerr();
	XChangeProperty(display, win->client, WM_STATE, WM_STATE, 32,
	    PropModeReplace, (unsigned char *)data, 2);
	sterr();
end;

end.
