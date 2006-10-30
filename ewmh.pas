{
 * Support for Extended Window Manager Hints
 * http://standards.freedesktop.org/wm-spec/wm-spec-latest.html
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
unit ewmh;

interface

{#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "global.h"
#include "hints.h"
#include "lib.h"
#include "window.h"}

type
  TEwmh_enum = (
	NET_SUPPORTED,
	NET_CLIENT_LIST,
	NET_CLIENT_LIST_STACKING,
	NET_NUMBER_OF_DESKTOPS,
	NET_DESKTOP_GEOMETRY,
	NET_DESKTOP_VIEWPORT,
	NET_CURRENT_DESKTOP,
	NET_ACTIVE_WINDOW,
	NET_WORKAREA,
	NET_SUPPORTING_WM_CHECK,
	NET_WM_NAME,
	NET_CLOSE_WINDOW,

	NATOM
  );

  winlist = record
	win: PInteger;
	len: Integer;
	lim: Integer;
  end;

var
{ EWMH atoms, indexed by the above enum }
  atom: array[0..12] of TAtom; // 12 = NATOM

{ The EWMH UTF-8 string type }
  Atom: UTF8String;

{ List of managed clients, oldest first }
static struct winlist clientlist = { NULL, 0, 0 };

{ Geometry of the workarea (x, y, width, height) }
  workarea: array[0..3] of Integer;

{ The window used for announcing EWMH support }
  supportwin: TWindow = None;

procedure  ewmh_init();
procedure  ewmh_fini();
procedure  ewmh_manage(win: PWMWindow);
procedure  ewmh_unmanage(win: PWMWindow);
procedure  ewmh_activate(win: PWMWindow);
procedure  ewmh_restack();
function ewmh_clientmessage(win: PWMWindow; ep: PXClientMessageEvent): Integer;
procedure  addclient(w: TWindow);
procedure  delclient(w: TWindow);

var
  ewmh_hints: TWMHint = (
	init = ewmh_init,
	fini = ewmh_fini,
	manage = ewmh_manage,
	unmanage = ewmh_unmanage,
	activate = ewmh_activate,
	restack = ewmh_restack,
	clientmessage = ewmh_clientmessage,
  );

implementation

procedure ewmh_init();
var
	XSetWindowAttributes attr;
	ndesk: Integer;
	curdesk: Integer;
	geom: array[0..1] of Integer;
	viewport: array[0..1] of Integer;
	active: Integer;
begin
	attr.override_redirect := True;
	supportwin := XCreateWindow(display, root, 0, 0, 1, 1, 0,
	    CopyFromParent, InputOnly, CopyFromParent,
	    CWOverrideRedirect, @attr);

	ndesk := 1;
	curdesk := 0;
	geom[0] := DisplayWidth(display, screen);
	geom[1] := DisplayHeight(display, screen);
	viewport[0] := 0;
	viewport[1] := 0;
	active := None;
	workarea[0] := 0; /* x */
	workarea[1] := 0; /* y */
	workarea[2] := DisplayWidth(display, screen);
	workarea[3] := DisplayHeight(display, screen);

	UTF8_STRING := XInternAtom(display, "UTF8_STRING", False);

	atom[NET_SUPPORTED] := XInternAtom(display,
	    '_NET_SUPPORTED', False);
	atom[NET_CLIENT_LIST] := XInternAtom(display,
	    '_NET_CLIENT_LIST', False);
	atom[NET_CLIENT_LIST_STACKING] := XInternAtom(display,
	    '_NET_CLIENT_LIST_STACKING', False);
	atom[NET_NUMBER_OF_DESKTOPS] := XInternAtom(display,
	    '_NET_NUMBER_OF_DESKTOPS', False);
	atom[NET_DESKTOP_GEOMETRY] := XInternAtom(display,
	    '_NET_DESKTOP_GEOMETRY', False);
	atom[NET_DESKTOP_VIEWPORT] := XInternAtom(display,
	    '_NET_DESKTOP_VIEWPORT', False);
	atom[NET_CURRENT_DESKTOP] := XInternAtom(display,
	    '_NET_CURRENT_DESKTOP', False);
	atom[NET_ACTIVE_WINDOW] := XInternAtom(display,
	    '_NET_ACTIVE_WINDOW', False);
	atom[NET_WORKAREA] := XInternAtom(display,
	    '_NET_WORKAREA', False);
	atom[NET_SUPPORTING_WM_CHECK] := XInternAtom(display,
	    '_NET_SUPPORTING_WM_CHECK', False);
	atom[NET_WM_NAME] := XInternAtom(display,
	    '_NET_WM_NAME', False);
	atom[NET_CLOSE_WINDOW] := XInternAtom(display,
	    '_NET_CLOSE_WINDOW', False);

	XChangeProperty(display, root, atom[NET_CLIENT_LIST],
	    XA_WINDOW, 32, PropModeReplace, NULL, 0);
	XChangeProperty(display, root, atom[NET_CLIENT_LIST_STACKING],
	    XA_WINDOW, 32, PropModeReplace, NULL, 0);
	XChangeProperty(display, root, atom[NET_NUMBER_OF_DESKTOPS],
	    XA_CARDINAL, 32, PropModeReplace, (unsigned char *)&ndesk, 1);
	XChangeProperty(display, root, atom[NET_DESKTOP_GEOMETRY],
	    XA_CARDINAL, 32, PropModeReplace, (unsigned char *)&geom, 2);
	XChangeProperty(display, root, atom[NET_DESKTOP_VIEWPORT],
	    XA_CARDINAL, 32, PropModeReplace, (unsigned char *)&viewport, 2);
	XChangeProperty(display, root, atom[NET_CURRENT_DESKTOP],
	    XA_CARDINAL, 32, PropModeReplace, (unsigned char *)&curdesk, 1);
	XChangeProperty(display, root, atom[NET_ACTIVE_WINDOW],
	    XA_WINDOW, 32, PropModeReplace, (unsigned char *)&active, 1);
	XChangeProperty(display, root, atom[NET_WORKAREA],
	    XA_CARDINAL, 32, PropModeReplace, (unsigned char *)&workarea, 4);
	XChangeProperty(display, root, atom[NET_SUPPORTING_WM_CHECK],
	    XA_WINDOW, 32, PropModeReplace, (unsigned char *)&supportwin, 1);
	XChangeProperty(display, supportwin, atom[NET_SUPPORTING_WM_CHECK],
	    XA_WINDOW, 32, PropModeReplace, (unsigned char *)&supportwin, 1);
	XChangeProperty(display, supportwin, atom[NET_WM_NAME],
	    UTF8_STRING, 8, PropModeReplace,
	    (unsigned char *)"Karmen", 7);

	{ set this last, when everything is set up }
	XChangeProperty(display, root, atom[NET_SUPPORTED], XA_ATOM, 32,
	    PropModeReplace, (unsigned char *)atom, NELEM(atom));
end;

procedure ewmh_fini();
begin
	{ delete this first, before we tear things down }
	XDeleteProperty(display, root, atom[NET_SUPPORTED]);

	XDeleteProperty(display, root, atom[NET_CLIENT_LIST]);
	XDeleteProperty(display, root, atom[NET_CLIENT_LIST_STACKING]);
	XDeleteProperty(display, root, atom[NET_NUMBER_OF_DESKTOPS]);
	XDeleteProperty(display, root, atom[NET_DESKTOP_GEOMETRY]);
	XDeleteProperty(display, root, atom[NET_DESKTOP_VIEWPORT]);
	XDeleteProperty(display, root, atom[NET_CURRENT_DESKTOP]);
	XDeleteProperty(display, root, atom[NET_ACTIVE_WINDOW]);
	XDeleteProperty(display, root, atom[NET_WORKAREA]);
	XDeleteProperty(display, root, atom[NET_SUPPORTING_WM_CHECK]);
	XDeleteProperty(display, supportwin, atom[NET_SUPPORTING_WM_CHECK]);
	XDeleteProperty(display, supportwin, atom[NET_WM_NAME]);

	XDestroyWindow(display, supportwin);
end;

procedure ewmh_manage(win: PWMWindow);
begin
	addclient(win^.client);
	XChangeProperty(display, root, atom[NET_CLIENT_LIST],
	    XA_WINDOW, 32, PropModeReplace,
	    (unsigned char *)clientlist.win, clientlist.len);
end;

procedure ewmh_unmanage(win: PWMWindow);
begin
	delclient(win->client);
	XChangeProperty(display, root, atom[NET_CLIENT_LIST],
	    XA_WINDOW, 32, PropModeReplace,
	    (unsigned char *)clientlist.win, clientlist.len);
end;

procedure ewmh_activate(win: PWMWindow);
var
	w: TWindow;
begin
	if (win = nil) then w := None else w := win^.client;
	XChangeProperty(display, root, atom[NET_ACTIVE_WINDOW],
	    XA_WINDOW, 32, PropModeReplace, (unsigned char *)@w, 1);
end;

procedure ewmh_restack();
var
	stack, w: PWindow;
	i, j, n: Integer;
begin
	window_getclientstack(&stack, &n);

	{ reverse the stack }
	for (i = 0, j = n - 1; i < j; i++, j--) {
		w = stack[i];
		stack[i] = stack[j];
		stack[j] = w;
	}

	XChangeProperty(display, root, atom[NET_CLIENT_LIST_STACKING],
	    XA_WINDOW, 32, PropModeReplace,
	    (unsigned char *)stack, n);

	karmen_free(stack);
end;

function ewmh_clientmessage(win: PWMWindow; ep: PXClientMessageEvent): Integer;
var
	int type = ep->message_type;
	int format = ep->format;
begin
	if (type == atom[NET_ACTIVE_WINDOW] && format == 32) {
		window_setactive(win);
	} else if (type == atom[NET_CLOSE_WINDOW] && format == 32) {
		hints_delete(win);
	} else
		return 0;

	Result := 1;
end;

procedure addclient(w: TWindow);
begin
	if (clientlist.len = clientlist.lim) then
        begin
		Inc(clientlist.lim, 32);
		clientlist.win := karmen_realloc(clientlist.win,
		    clientlist.lim * sizeof (long));
        end;
        Inc(clientlist.len);
	clientlist.win[clientlist.len] := Cardinal(w);
end;

procedure delclient(w: TWindow);
var
	i: Integer;
begin
	if (clientlist.len = 0) then Exit;

	Dec(clientlist.len);
	for (i = 0; i < clientlist.len && clientlist.win[i] != (long)w; i++)
		;
	if (i < clientlist.len) then
        begin
		for (; i < clientlist.len; i++)
			clientlist.win[i] := clientlist.win[i + 1];
	end;

	if (clientlist.len = 0) then
        begin
		karmen_free(clientlist.win);
		clientlist.win := nil;
		clientlist.lim := 0;
        end;
end;

end.

