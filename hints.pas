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

{$mode objfpc}{$H+}

interface

{#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "hints.h"
#include "lib.h"}

uses
  { Free Pascal Units }
  SysUtils,
  { Units from fpwm }
  window,
  { XLib units }
  X, Xlib, Xutil;

type
  void_t = procedure;
  
  window_t = procedure (p1: PWMWindow);

  clientmessage_t = function (p1: PWMWindow; p2: PXClientMessageEvent): Integer;

  propertynotify_t = function (p1: PWMWindow; p2: PXPropertyEvent): Integer;

  delete_t = function (p1: PWMWindow): Integer;

  TWMHints = record
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
	delete: delete_t;

	{ Called after restacking windows }
	restack: void_t;
  end;

procedure hints_init();
procedure hints_fini();
procedure hints_manage(win: PWMWindow );
procedure hints_unmanage(win: PWMWindow );
procedure hints_map(win: PWMWindow );
procedure hints_unmap(win: PWMWindow);
procedure hints_withdraw(win: PWMWindow );
procedure hints_activate(win: PWMWindow );
procedure hints_deactivate(win: PWMWindow );
procedure hints_move(win: PWMWindow );
procedure hints_resize(win: PWMWindow );
procedure hints_moveresize(win: PWMWindow );
procedure hints_clientmessage(win: PWMWindow; ep: PXClientMessageEvent);
procedure hints_propertynotify(win: PWMWindow; ep: PXPropertyEvent);
function hints_delete(win: PWMWindow): Integer;
procedure hints_restack();

implementation

uses icccm;

{#if CONFIG_EWMH
extern struct hints ewmh_hints;
#endif}

var
  vhints: array of TWMHints;

//#if CONFIG_EWMH
//	&ewmh_hints,
//#endif

procedure hints_init();
var
	i: Integer;
begin
        SetLength(vhints, 1);
        vhints[0] := icccm_hints;

	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].init <> nil) then
			vhints[i].init();
end;

procedure hints_fini();
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].fini <> nil) then
			vhints[i].fini();
end;

procedure hints_manage(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].manage <> nil) then
			vhints[i].manage(win);
end;

procedure hints_unmanage(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].unmanage <> nil) then
			vhints[i].unmanage(win);
end;

procedure hints_map(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].map <> nil) then
			vhints[i].map(win);
end;

procedure hints_unmap(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].unmap <> nil) then
			vhints[i].unmap(win);
end;

procedure hints_withdraw(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].withdraw <> nil) then
			vhints[i].withdraw(win);
end;

procedure hints_activate(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].activate <> nil) then
			vhints[i].activate(win);
end;

procedure hints_deactivate(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].deactivate <> nil) then
			vhints[i].deactivate(win);
end;

procedure hints_move(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].move <> nil) then
			vhints[i].move(win);
end;

procedure hints_resize(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].resize <> nil) then
			vhints[i].resize(win);
end;

procedure hints_moveresize(win: PWMWindow);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].moveresize <> nil) then
			vhints[i].moveresize(win);
end;

procedure hints_clientmessage(win: PWMWindow; ep: PXClientMessageEvent);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].clientmessage <> nil) then
                        if vhints[i].clientmessage(win, ep) <> 0 then Exit;
end;

procedure hints_propertynotify(win: PWMWindow; ep: PXPropertyEvent);
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].propertynotify <> nil) then
                        if vhints[i].propertynotify(win, ep) <> 0 then Exit;
end;

function hints_delete(win: PWMWindow): Integer;
var
	i: Integer;
begin
	Result := 1;

	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].delete <> nil) then
                        if vhints[i].delete(win) <> 0 then Exit;

	Result := 0;
end;

procedure hints_restack();
var
	i: Integer;
begin
	for i := 0 to Length(vhints) - 1 do
 		if (vhints[i].restack <> nil) then
                        vhints[i].restack();
end;

end.

