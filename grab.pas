{
 * grab.c - key/button grabbing
 *
 * The routines in this files are equivalent to their X counterparts,
 * except that they also grab/ungrab all combinations of Num Lock,
 * Caps Lock and Scroll Lock.
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
unit grab;

{$mode objfpc}{$H+}

interface

{#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "lib.h"}

uses
  { Free Pascal Units }
  SysUtils, Unix, BaseUnix, UnixType,
  { Units from fpwm }
  { XLib units }
  X, Xlib, Xutil, keysym;

procedure grabkey(display: PDisplay; keycode: Integer; modifiers: Cardinal;
    grab_window: TWindow; owner_events: Boolean; pointer_mode, keyboard_mode: Integer);
procedure ungrabkey(display: PDisplay; keycode: Integer; modifiers: Cardinal;
    grab_window: TWindow);
procedure grabbutton(display: PDisplay; button, modifiers: Cardinal;
    grab_window: TWindow; owner_events: Boolean; event_mask: Cardinal;
    pointer_mode, keyboard_mode: Integer; confine_to: TWindow; cursor: TCursor);
procedure ungrabbutton(display: PDisplay; button, modifiers: Cardinal;
    grab_window: TWindow);

implementation

{
 * All possible combinations of the lock masks.
 }
var
  lockmasks: array[0..7] of Cardinal = (
	0,
	LockMask,	{ Caps Lock   }
	Mod2Mask,	{ Num Lock    }
	Mod5Mask,	{ Scroll Lock }
	LockMask or Mod2Mask,
	LockMask or Mod5Mask,
	Mod2Mask or Mod5Mask,
	LockMask or Mod2Mask or Mod5Mask
  );

{
 * XGrabKey()
 }
procedure grabkey(display: PDisplay; keycode: Integer; modifiers: Cardinal;
    grab_window: TWindow; owner_events: Boolean; pointer_mode, keyboard_mode: Integer);
var
	i: Integer;
begin
	for i := 0 to Length(lockmasks) - 1 do
        begin
		XGrabKey(display, keycode, lockmasks[i] or modifiers,
		    grab_window, owner_events, pointer_mode, keyboard_mode);
        end;
end;

{
 * XUngrabKey()
 }
procedure ungrabkey(display: PDisplay; keycode: Integer; modifiers: Cardinal;
    grab_window: TWindow);
var
	i: Integer;
begin
	for i := 0 to Length(lockmasks) - 1 do
        begin
		XUngrabKey(display, keycode, lockmasks[i] or modifiers,
		    grab_window);
        end;
end;

{
 * XGrabButton()
 }
procedure grabbutton(display: PDisplay; button, modifiers: Cardinal;
    grab_window: TWindow; owner_events: Boolean; event_mask: Cardinal;
    pointer_mode, keyboard_mode: Integer; confine_to: TWindow; cursor: TCursor);
var
	i: Integer;
begin
	for i := 0 to Length(lockmasks) - 1 do
        begin
		XGrabButton(display, button,
		    lockmasks[i] or modifiers,
		    grab_window, owner_events, event_mask, pointer_mode,
		    keyboard_mode, confine_to, cursor);
        end;
end;

{
 * XUngrabButton()
 }
procedure ungrabbutton(display: PDisplay; button, modifiers: Cardinal;
    grab_window: TWindow);
var
	i: Integer;
begin
	for i := 0 to Length(lockmasks) - 1 do
        begin
		XUngrabButton(display, button, lockmasks[i] or modifiers,
		    grab_window);
        end;
end;

end.

