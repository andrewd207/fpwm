{
 * Modular hinting support
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
unit hints;

interface

{#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "hints.h"
#include "lib.h"}

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  { XLib units }
  X, Xlib, Xutil;

type
  void_t = procedure;
  
  window_t = procedure (p1: TWindow);

  clientmessage_t = function (p1: TWindow; p2: PXClientMessageEvent): Integer;

  propertynotify_t = function (p1: TWindow; p2: PXPropertyEvent): Integer;

  TWMHHints = record
	{ The name of the hints }
	name: PChar;

	{ Called at program start }
	init: void_t;

	{ Called before shutting down }
	fini: void_t;

	{ Called when a new window is managed }
	manage: window_t;

	{ Called when a window is unmanaged }
	unmanage: window_t;

	{ Called when a window is mapped }
	map: window_t;

	{ Called when a window is unmapped }
	unmap: window_t;

	{ Called when a client withdraws (unmaps) itself }
	withdraw: window_t;

	{ Called when a window becomes active, NULL if none }
	activate: window_t;

	{ Called when a window loses focus }
	deactivate: window_t;

	{ Called when a window was moved but not resized }
	move: window_t;

	{ Called when a window was resized but not moved }
	resize: window_t;

	{ Called when a window was moved AND resized }
	moveresize: window_t;

        {
	 * Called when a client message arrives for a window.
	 * A hint should return nonzero if the message was understood.
         }
	clientmessage: clientmessage_t;

        {
	 * Called when a property event arrives for a window.
	 * A hint should return nonzero if the property was understood.
         }
	propertynotify: propertynotify_t;

        {
	 * Called when a hint should try to gracefully delete the
	 * window.  A hint should return nonzero if it knows how
	 * to delete the window.
         }
	delete: window_t;

	{ Called after restacking windows }
	restack: void_t;
  end;

{procedure hints_init();
procedure hints_fini();
procedure hints_manage(struct window * );
procedure hints_unmanage(struct window * );
procedure hints_map(struct window * );
procedure hints_unmap(struct window *);
procedure hints_withdraw(struct window * );
procedure hints_activate(struct window * );
procedure hints_deactivate(struct window * );
procedure hints_move(struct window * );
procedure hints_resize(struct window * );
procedure hints_moveresize(struct window * );
procedure hints_clientmessage(struct window *, XClientMessageEvent * );
procedure hints_propertynotify(struct window *, XPropertyEvent * );
int hints_delete(struct window * );
procedure hints_restack();}

implementation

uses icccm;

{#if CONFIG_EWMH
extern struct hints ewmh_hints;
#endif}

//static struct hints *hints[] = {
//	&icccm_hints,

//#if CONFIG_EWMH
//	&ewmh_hints,
//#endif
//};

procedure hints_init();
var
	i: Integer;
begin
	for i := 0 to Length(hints) - 1 do
 		if (hints[i]^.init != nil)
			hints[i]^.init();
end;

procedure hints_fini()
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.fini != nil)
			hints[i]^.fini();
end;

procedure hints_manage(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.manage != nil)
			hints[i]^.manage(win);
end;

procedure hints_unmanage(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.unmanage != nil)
			hints[i]^.unmanage(win);
end;

procedure hints_map(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.map != nil)
			hints[i]^.map(win);
end;

procedure hints_unmap(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.unmap != nil)
			hints[i]^.unmap(win);
end;

procedure hints_withdraw(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.withdraw != nil)
			hints[i]^.withdraw(win);
end;

procedure hints_activate(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.activate != nil)
			hints[i]^.activate(win);
end;

procedure hints_deactivate(struct window *win)
var
	int i;

	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.deactivate != nil)
			hints[i]^.deactivate(win);
end;

procedure hints_move(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.move != nil)
			hints[i]^.move(win);
end;

procedure hints_resize(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.resize != nil)
			hints[i]^.resize(win);
end;

procedure hints_moveresize(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.moveresize != nil)
			hints[i]^.moveresize(win);
end;

procedure hints_clientmessage(struct window *win, XClientMessageEvent *ep)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.clientmessage != nil)
			if (hints[i]^.clientmessage(win, ep))
				return;
end;

procedure hints_propertynotify(struct window *win, XPropertyEvent *ep)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.propertynotify != nil)
			if (hints[i]^.propertynotify(win, ep))
				return;
end;

int hints_delete(struct window *win)
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.delete != nil)
			if (hints[i]^.delete(win))
				return 1;
	return 0;
end;

procedure hints_restack();
var
	i: Integer;
begin
	for (i = 0; i < NELEM(hints); i++)
		if (hints[i]^.restack != nil)
			hints[i]^.restack();
end;

end.

