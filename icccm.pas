{
 * Inter-Client Communication Conventions Manual (ICCCM) hints
 *
 * NOTE: Since the ICCCM is so closely tied to window management, much
 *       of the standard is hardwired into "window.c".
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
  SysUtils, ctypes,
  { Units from fpwm }
  hints, window,
  { XLib units }
  X, Xlib, Xutil, XAtom;

var
  WM_CHANGE_STATE: TAtom;
  WM_DELETE_WINDOW: TAtom;
  WM_PROTOCOLS: TAtom;
  WM_STATE: TAtom;

procedure icccm_init();
procedure icccm_manage(win: PWMWindow);
procedure icccm_map(win: PWMWindow);
procedure icccm_unmap(win: PWMWindow);
procedure icccm_withdraw(win: PWMWindow);
procedure icccm_move(win: PWMWindow);
function icccm_clientmessage(win: PWMWindow; ep: PXClientMessageEvent): Integer;
function icccm_propertynotify(win: PWMWindow; ep: PXPropertyEvent): Integer;
function icccm_delete(win: PWMWindow): Integer;
function knowsproto(win: PWMWindow; proto: TAtom): Integer;
procedure sendmesg(win: PWMWindow; type_: TAtom; value: Integer);
procedure sendconf(win: PWMWindow);
procedure setwmstate(win: PWMWindow; state: Integer);

var
  icccm_hints: TWMHints = (
	name: 'Inter-Client Communication Conventions Manual (ICCCM)';

	init: @icccm_init;
	fini: nil;
	manage: @icccm_manage;
        unmanage: nil;
	map: @icccm_map;
	unmap: @icccm_unmap;
	withdraw: @icccm_withdraw;
        activate: nil;
        deactivate: nil;
	move: @icccm_move;
        resize: nil;
        moveresize: nil;
	clientmessage: @icccm_clientmessage;
	propertynotify: @icccm_propertynotify;
	delete: @icccm_delete;
  );

implementation

uses
  { Units from fpwm }
  main;

procedure icccm_init();
begin
	WM_CHANGE_STATE := XInternAtom(display, 'WM_CHANGE_STATE', False);
	WM_DELETE_WINDOW := XInternAtom(display, 'WM_DELETE_WINDOW', False);
	WM_PROTOCOLS := XInternAtom(display, 'WM_PROTOCOLS', False);
	WM_STATE := XInternAtom(display, 'WM_STATE', False);
end;

procedure icccm_manage(win: PWMWindow);
begin
	sendconf(win);
end;

procedure icccm_map(win: PWMWindow);
begin
	setwmstate(win, NormalState);
end;

procedure icccm_unmap(win: PWMWindow);
begin
	setwmstate(win, IconicState);
end;

procedure icccm_withdraw(win: PWMWindow);
begin
	setwmstate(win, WithdrawnState);
end;

procedure icccm_move(win: PWMWindow);
begin
	sendconf(win);
end;

function icccm_clientmessage(win: PWMWindow; ep: PXClientMessageEvent): Integer;
begin
	Result := 0;

	if (ep^.message_type = WM_CHANGE_STATE) and (ep^.format = 32) then
        begin
		case ep^.data.l[0] of
		 IconicState:
                 begin
//			window_unmap(win);
			Result := 1;
                 end;
   	     	 NormalState:
                 begin
//			window_map(win);
			Result := 1;
                 end;
                end;
        end;
end;

function icccm_propertynotify(win: PWMWindow; ep: PXPropertyEvent): Integer;
begin
	Result := 1;

	case (ep^.atom) of
	 XA_WM_NAME:
		if (ep^.state <> PropertyDelete) then
			window_fetchname(win);
	 XA_WM_ICON_NAME:
		if (ep^.state <> PropertyDelete) then
			window_fetchiconname(win);
	 XA_WM_NORMAL_HINTS:
		window_fetchwmnormalhints(win);
	 XA_WM_HINTS:
		window_fetchwmhints(win);
	 XA_WM_TRANSIENT_FOR:
		window_fetchwmtransientfor(win);
        else
        	Result := 0;
        end;
end;

function icccm_delete(win: PWMWindow): Integer;
begin
	if (knowsproto(win, WM_DELETE_WINDOW) <> 0) then
        begin
		sendmesg(win, WM_PROTOCOLS, WM_DELETE_WINDOW);
		Result := 1;
        end
        else Result := 0;
end;

function knowsproto(win: PWMWindow; proto: TAtom): Integer;
var
	protocols: PAtom;
	i, n: Integer;
        found: Boolean = False;
begin
//	clerr();
	if (XGetWMProtocols(display, win^.client, @protocols, @n) <> 0) then
        begin
                i := 0;
		while (not found) and (i < n) do
                begin
			if (protocols[i] = proto) then found := True;
                        Inc(i);
                end;
		if (protocols <> nil) then XFree(protocols);
        end;
//	sterr();
	Result := Integer(found);
end;

procedure sendmesg(win: PWMWindow; type_: TAtom; value: Integer);
var
	ev: TXEvent;
begin
	FillChar(ev, sizeof(ev), #0);
	ev.xclient._type := ClientMessage;
	ev.xclient.window := win^.client;
	ev.xclient.message_type := type_;
	ev.xclient.format := 32;
	ev.xclient.data.l[0] := value;
	ev.xclient.data.l[1] := CurrentTime;
//	clerr();
	XSendEvent(display, win^.client, False, 0, @ev);
//	sterr();
end;

procedure sendconf(win: PWMWindow);
var
	conf: TXConfigureEvent;
begin
	conf._type := ConfigureNotify;
	conf.event := win^.client;
	conf.window := win^.client;
	conf.x := win^.widget.dim.x + border_width - win^.cborder;
	conf.y := win^.widget.dim.y + border_width + button_size + innerborder_width
	    - win^.cborder;
	conf.width := win^.widget.dim.width - 2 * border_width;
	conf.height := win^.widget.dim.height
	    - (2 * border_width + button_size + innerborder_width);
	conf.border_width := win^.cborder;
	conf.above := None;
	conf.override_redirect := False;

//	clerr();
	XSendEvent(display, win^.client, False, StructureNotifyMask,
	    PXEvent(@conf));
//	sterr();
end;

procedure setwmstate(win: PWMWindow; state: Integer);
var
	data: array[0..1] of Integer;
begin
	data[0] := state;
	data[1] := None;
//	clerr();
	XChangeProperty(display, win^.client, WM_STATE, WM_STATE, 32,
	    PropModeReplace, pcuchar(Pointer(@data)), 2);
//	sterr();
end;

end.
