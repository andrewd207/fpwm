unit main;

{$mode objfpc}{$H+}

interface

{#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <X11/Xutil.h>

#include "global.h"
#include "lib.h"
#include "hints.h"
#include "menu.h"
#include "window.h"}

uses
  { Free Pascal Units }
  SysUtils, Unix, BaseUnix, UnixType,
  { Units from fpwm }
  button, lib, menu,
  { XLib units }
  X, Xlib, Xutil, Xresource, keysym;

{$include delete.xbm}
{$include lower.xbm}
{$include hide.xbm}

var
  errno: Integer;

  display: PDisplay;
  screen: Integer;
  root: TWindow;

  winmenu: PWMmenu;

  font: PXFontStruct;

  border_width: Integer = 3;
  button_size: Integer;
  innerborder_width: Integer = 1;
  title_pad: Integer;

  color_title_active_fg: TWMColor;
  color_title_active_bg: TWMColor;
  color_title_inactive_fg: TWMColor;
  color_title_inactive_bg: TWMColor;
  color_menu_fg: TWMColor;
  color_menu_bg: TWMColor;
  color_menu_selection_fg: TWMColor;
  color_menu_selection_bg: TWMColor;
  color_border: TWMColor;
  color_innerborder: TWMColor;

  delete_image: TWMImage = (
	data: @delete_bits;
       	width: delete_width;
	height: delete_height
  );
  lower_image: TWMImage = (
	data: @lower_bits;
       	width: lower_width;
	height: lower_height
  );
  hide_image: TWMImage = (
	data: @hide_bits;
       	width: hide_width;
	height: hide_height
  );

const
  DEFAULT_FONT = '-*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1';

var
  font_names: array[0..3] of PChar = (
	{ This will be overwritten by user supplied font name }
	DEFAULT_FONT,

	{ This is the one that looks best }
	DEFAULT_FONT,

	{ This is the one that exists }
	'fixed',

        nil
  );

{
 * These are the default background colors. They all have at least
 * 70% contrast to black, which is a common rule for readable text.
 }
const
  LIGHT_GRAY	= 'rgb:d3/d3/d3';
  BRIGHT_YELLOW	= 'rgb:d0/d0/a0';
  BRIGHT_BLUE	= 'rgb:b0/b0/e0';
  BRIGHT_GRAY	= 'rgb:c0/c0/c0';
  BRIGHT_GREEN	= 'rgb:a8/d8/a8';
  STEEL_GRAY	= 'rgb:d0/d0/d0';
  WHITE		= 'rgb:ff/ff/ff';
  GTK_LIGHTGRAY	= 'rgb:dc/da/d5';
  GTK_DARKGRAY	= 'rgb:b4/b2/ae';
  GTK_BLUE	= 'rgb:4b/69/83';

{
 * Foreground colors
 }
  BLACK		= 'rgb:00/00/00';

{ these are old colors }
  YELLOW	= 'rgb:b5/b5/80';
  BLUE 	        = 'rgb:90/90/b8';
  GREEN   	= 'rgb:87/b5/87';
  GRAY	        = 'rgb:a5/a5/a5';

var
  colorname_border: PChar = BLACK;
  colorname_innerborder: PChar = BLACK;
  colorname_title_active_fg: PChar = BLACK;
  colorname_title_active_bg: PChar = GTK_LIGHTGRAY;
  colorname_title_inactive_fg: PChar = BLACK;
  colorname_title_inactive_bg: PChar = GTK_DARKGRAY;
  colorname_menu_fg: PChar = BLACK;
  colorname_menu_bg: PChar = GTK_LIGHTGRAY;
  colorname_menu_selection_fg: PChar = WHITE;
  colorname_menu_selection_bg: PChar = GTK_BLUE;

{ The signals that we care about  }
  sigv: array[0..2] of Integer = ( SIGHUP, SIGINT, SIGTERM );

{ The original signal mask }
  origsigmask: Tsigset;

  noerr: Integer = 0;

procedure sighandler(signal: longint; info: psiginfo; context: psigcontext); cdecl;
function waitevent(): Integer;
procedure mkcolor(color: PWMColor; const name: string);
procedure initres();
procedure loadfont();
procedure init;
procedure mainloop();
procedure initclients();
procedure quit(status: Integer);

implementation

uses
  { Units from fpwm }
  grab, widget;

function error_handler(dpy: PDisplay; err: PXErrorEvent): Integer; cdecl;
var
  buf: array[0..255] of Char;
begin
       if ((err^.error_code = BadAccess) and (err^.resourceid = root)) then
       begin
		WriteLn('another wm is running');
		Halt(1);
       end;

	if (noerr = 0) then
        begin
		XGetErrorText(dpy, err^.error_code, buf, sizeof(buf));
//		perr(buf);
        end;
	Result := 0;
end;

procedure clerr;
begin
	assert(noerr >= 0);
	XGrabServer(display);
	Inc(noerr);
end;

procedure sterr;
begin
	XSync(display, False);
	XUngrabServer(display);
	Dec(noerr);
	assert(noerr >= 0);
end;

const signalled: Integer = 0;

procedure sighandler(signal: longint; info: psiginfo; context: psigcontext); cdecl;
begin
	signalled := signal;
end;

function waitevent(): Integer;
var
	savemask: Integer;
//	pfd: pollfd;
	res: Integer;
begin
//	pfd.fd := ConnectionNumber(display);
//	pfd.events := POLLIN;

{        repeat
		fpsigprocmask(SIG_SETMASK, @origsigmask, @savemask);
		res := poll(@pfd, 1, -1);
		fpsigprocmask(SIG_SETMASK, @savemask, nil);
	until ((res = -1) and (errno = EAGAIN) and (signalled = 0));

	if ((res = -1) or ((pfd.revents and POLLERR) <> 0) and (signalled <> 0)) then
		Result := -1
	else}
		Result := 0;
end;

{function nextevent(ep: PXEvent): Integer;
begin
	if (signalled) then
        begin
		errno := EINTR;
		Result := -1;
                Exit;
        end;
	if (XQLength(display) = 0) then
        begin
		XFlush(display);
		if (waitevent() = -1) then Result := -1;
        end;
	XNextEvent(display, ep);
	Result := 0;
end;}

{function maskevent(mask: Integer; ep: PXEvent): Integer;
begin
	Result := 0;

	if (signalled) then
        begin
		errno := EINTR;
		Result := -1;
                Exit;
        end;

	while (!XCheckMaskEvent(display, mask, ep)) do
        begin
		if (waitevent() = -1) then Result := -1;
        end;
end;}

procedure mkcolor(color: PWMColor; const name: string);
var
	tc, sc: TXColor;
begin
	XAllocNamedColor(display, DefaultColormap(display, screen),
	    PChar(name), @tc, @sc);

	color^.normal := sc.pixel;

	sc.red := MAX(0, tc.red - 65535 div 15);
	sc.green := MAX(0, tc.green - 65535 div 15);
	sc.blue := MAX(0, tc.blue - 65535 div 15);
	XAllocColor(display, DefaultColormap(display, screen), @sc);
	color^.shadow1 := sc.pixel;

	sc.red := MAX(0, tc.red - 65535 div 6);
	sc.green := MAX(0, tc.green - 65535 div 6);
	sc.blue := MAX(0, tc.blue - 65535 div 6);
	XAllocColor(display, DefaultColormap(display, screen), @sc);
	color^.shadow2 := sc.pixel;

	sc.red := MIN(65535, tc.red + 65535 div 15);
	sc.green := MIN(65535, tc.green + 65535 div 15);
	sc.blue := MIN(65535, tc.blue + 65535 div 15);
	XAllocColor(display, DefaultColormap(display, screen), @sc);
	color^.bright1 := sc.pixel;

	sc.red := MIN(65535, tc.red + 65535 div 6);
	sc.green := MIN(65535, tc.green + 65535 div 6);
	sc.blue := MIN(65535, tc.blue + 65535 div 6);
	XAllocColor(display, DefaultColormap(display, screen), @sc);
	color^.bright2 := sc.pixel;
end;

const options: array[0..11] of TXrmOptionDescRec = (
       	( option: '-afg'; specifier: 'title.active.foreground'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-abg'; specifier: 'title.active.background'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-ifg'; specifier: 'title.inactive.foreground'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-ibg'; specifier: 'title.inactive.background'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-mfg'; specifier: 'menu.foreground'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-mbg'; specifier: 'menu.background'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-msfg'; specifier: 'menu.selection.foreground'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-msbg'; specifier: 'menu.selection.background'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-bc'; specifier: 'border.color'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-bw'; specifier: 'border.width'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-fn'; specifier: 'font'; argKind: XrmoptionSepArg; value: nil ),
	( option: '-xrm'; specifier: nil; argKind: XrmoptionResArg; value: nil )
  );

procedure initres;
var
	db: TXrmDatabase;
	val: TXrmValue;
	dbstr, dummy: PChar;
begin
	XrmInitialize();

	db := nil;

	{ Load the default database. }
	if (dbstr = XResourceManagerString(display)) then
		db := XrmGetStringDatabase(dbstr);

	{ Add command line option resources. }
	XrmParseCommand(@db, options, Length(options), 'karmen', @argc, argv);

	if (XrmGetResource(db, 'karmen.title.active.foreground',
	    'Karmen.Active.Foreground', @dummy, @val)) then
		colorname_title_active_fg := val.addr;

	if (XrmGetResource(db, 'karmen.title.active.background',
	    'Karmen.Title.Active.Background', @dummy, @val)) then
		colorname_title_active_bg := val.addr;

	if (XrmGetResource(db, 'karmen.title.inactive.foreground',
	    'Karmen.Title.Inactive.Foreground', @dummy, @val)) then
		colorname_title_inactive_fg := val.addr;

	if (XrmGetResource(db, 'karmen.title.inactive.background',
	    'Karmen.Title.Inactive.Background', @dummy, @val)) then
		colorname_title_inactive_bg := val.addr;

	if (XrmGetResource(db, 'karmen.menu.foreground',
	    'Karmen.Menu.Foreground', @dummy, @val)) then
		colorname_menu_fg := val.addr;

	if (XrmGetResource(db, 'karmen.menu.background',
	    'Karmen.Menu.Background', @dummy, @val)) then
		colorname_menu_bg := val.addr;

	if (XrmGetResource(db, 'karmen.menu.selection.foreground',
	    'Karmen.Menu.Selection.Foreground', @dummy, @val)) then
		colorname_menu_selection_fg := val.addr;

	if (XrmGetResource(db, 'karmen.menu.selection.background',
	    'Karmen.Menu.Selection.Background', @dummy, @val)) then
		colorname_menu_selection_bg := val.addr;

	if (XrmGetResource(db, 'karmen.border.color',
	    'Karmen.Border.Color', @dummy, @val)) then
		colorname_border := val.addr;

{	if (XrmGetResource(db, 'karmen.border.width',
	    'Karmen.Border.Width', @dummy, @val)) then
		border_width := atoi((char *)val.addr);}

	if (XrmGetResource(db, 'karmen.innerborder.color',
	    'Karmen.InnerBorder.Color', @dummy, @val)) then
		colorname_innerborder := val.addr;

{	if (XrmGetResource(db, 'karmen.innerborder.width',
	    'Karmen.InnerBorder.Width', @dummy, @val)) then
		innerborder_width := atoi((char *)val.addr);}

	if (XrmGetResource(db, 'karmen.font', 'Karmen.Font', @dummy, @val)) then
		 font_names[0] := val.addr;
end;

procedure loadfont;
var
	i: Integer;
begin
        i := 0;
        
	while font_names[i] <> nil do
        begin
		font := XLoadQueryFont(display, font_names[i]);
  
		if (font <> nil) then Exit;
  
		WriteLn('Can''t load font ' + font_names[i]);
  
                Inc(i);
        end;
	WriteLn('No more fonts');
	Halt(1);
end;

procedure init;
var
	sigact, oldact: SigActionRec;
	sigmask: sigset_t;
	i: Integer;
begin
        {
	 * Block the signals that we plan to catch.
         }
	fpsigemptyset(sigmask);
	for i := 0 to Length(sigv) - 1 do
		fpsigaddset(sigmask, sigv[i]);
	fpsigprocmask(SIG_BLOCK, sigmask, origsigmask);

        {
	 * Set up signal handlers for those that were not ignored.
         }
	sigact.sa_handler := @sighandler;
	sigact.sa_flags := 0;
	fpsigfillset(sigact.sa_mask);
	for i := 0 to Length(sigv) - 1 do
        begin
		fpsigaction(sigv[i], nil, @oldact);
		if (Integer(oldact.sa_handler) <> SIG_IGN) then
			fpsigaction(sigv[i], @sigact, nil);
        end;

	XSetErrorHandler(@error_handler);
 
        display := XOpenDisplay(nil);
	if (display = nil) then
        begin
		WriteLn('can''t open display');
		Halt(1);
        end;
	screen := DefaultScreen(display);
	root := DefaultRootWindow(display);

	initres;
	if ( argc > 1) then
        begin
		for i := 1 to argc - 1 do
			WriteLn('unknown option: ' + argv[i]);
		Halt(1);
        end;

	XSelectInput(display, root, ButtonPressMask or
	    SubstructureRedirectMask or SubstructureNotifyMask or
	    KeyPressMask or KeyReleaseMask);

	grabkey(display, XKeysymToKeycode(display, XK_Tab), Mod1Mask,
	    root, True, GrabModeAsync, GrabModeAsync);
	grabkey(display, XKeysymToKeycode(display, XK_Tab),
	    ShiftMask or Mod1Mask, root, True, GrabModeAsync, GrabModeAsync);
	grabkey(display, XKeysymToKeycode(display, XK_Return), Mod1Mask,
	    root, True, GrabModeAsync, GrabModeAsync);
	grabkey(display, XKeysymToKeycode(display, XK_Escape), Mod1Mask,
	    root, True, GrabModeAsync, GrabModeAsync);
	grabkey(display, XKeysymToKeycode(display, XK_BackSpace), Mod1Mask,
	    root, True, GrabModeAsync, GrabModeAsync);
	grabkey(display, XKeysymToKeycode(display, XK_BackSpace),
	    ShiftMask or Mod1Mask, root, True, GrabModeAsync, GrabModeAsync);

	loadfont();

	title_pad := 1 + MAX(1, (font^.ascent + font^.descent) div 10);
	button_size := font^.ascent + font^.descent + 2 * title_pad;
	if ((button_size and 1) = 1) then Inc(button_size);

	mkcolor(@color_title_active_fg, colorname_title_active_fg);
	mkcolor(@color_title_active_bg, colorname_title_active_bg);
	mkcolor(@color_title_inactive_fg, colorname_title_inactive_fg);
	mkcolor(@color_title_inactive_bg, colorname_title_inactive_bg);
	mkcolor(@color_menu_fg, colorname_menu_fg);
	mkcolor(@color_menu_bg, colorname_menu_bg);
	mkcolor(@color_menu_selection_fg, colorname_menu_selection_fg);
	mkcolor(@color_menu_selection_bg, colorname_menu_selection_bg);
	mkcolor(@color_border, colorname_border);
	mkcolor(@color_innerborder, colorname_innerborder);
end;

procedure handlekey(ep: PXKeyEvent);
var
	cycling: Integer = 0;
begin
	case (XKeycodeToKeysym(display, ep^.keycode, 0)) of
	  XK_Meta_L,
	  XK_Meta_R,
	  XK_Alt_L,
	  XK_Alt_R:
          begin
		if (ep^._type = KeyRelease) then
                begin
			{ end window cycling }
			if (cycling) then
                        begin
				cycling := 0;
				menu_hide(winmenu);
				XUngrabKeyboard(display, CurrentTime);
				menu_select(winmenu);
                        end;
                end;
	  end;
	  XK_Tab:
		if (ep^._type = KeyPress) then
                begin
			cycling := 1;

			{ Listen for Alt/Meta release }
			XGrabKeyboard(display, root, True,
			    GrabModeAsync, GrabModeAsync, CurrentTime);

			if (not MAPPED(winmenu)) then
                        begin
				int x = DisplayWidth(display, screen) / 2
				    - WIDTH(winmenu) / 2;
				int y = DisplayHeight(display, screen) / 2
				    - HEIGHT(winmenu) / 2;
				menu_popup(winmenu, x, y, -1);
                        end;

			if (winmenu->current == -1)
				winmenu->current = 1;
			else
				winmenu->current +=
				    (ep->state & ShiftMask) == 0 ? 1 : -1;

			if (winmenu->current >= winmenu->nitems)
				winmenu->current = 0;
			else if (winmenu->current < 0)
				winmenu->current = winmenu->nitems - 1;

			menu_repaint(winmenu);
		end;
	  XK_Return:
		if (ep^._type = KeyPress and active <> nil) then
			window_maximize(active);
	  XK_Escape:
		if (cycling) then
                begin
			cycling := 0;
			winmenu^.current := -1;
			menu_hide(winmenu);
			XUngrabKeyboard(display, CurrentTime);
		end else if (ep^._type = KeyPress and active <> nil) then
			window_unmap(active);}
	  XK_BackSpace:
		if (ep^._type = KeyPress) and (active <> nil) then
                begin
			if ((ep^.state and ShiftMask) <> 0) then
//				clerr();
				XKillClient(display, active->client)
//				sterr();
			end else
				window_delete(active);
		end;
	else
		WriteLn('handlekey(): Unhandled key');
        end;
end;

procedure mainloop;
var
	widget: PWMWidget;
	e: TXEvent;
begin
        while true do
        begin
		window_restackall();
		if (nextevent(&e) == -1)
			quit(1);
		widget = widget_find(e.xany.window, CLASS_ANY);
		if (widget <> nil) then
                begin
			if (widget->event != NULL)
				widget->event(widget, &e);
		end
                else
                begin
			XWindowChanges wc;
			XConfigureRequestEvent *conf;

			case (e.type_) of
			 MapRequest:
				window_manage(e.xmaprequest.window, 1);
				break;
			 ConfigureRequest:
				conf := @e.xconfigurerequest;
				wc.x := conf^.x;
				wc.y := conf^.y;
				wc.width := conf^.width;
				wc.height := conf^.height;
				wc.border_width := conf^.border_width;
				wc.sibling := conf^.above;
				wc.stack_mode := conf^.detail;
				clerr();
				XConfigureWindow(display, conf^.window,
				    conf^.value_mask, @wc);
				sterr();
				break;
			 ButtonPress:
				if (e.xbutton.button == Button3)
					menu_popup(winmenu,
					    e.xbutton.x, e.xbutton.y,
					    e.xbutton.button);
			 KeyPress,
			 KeyRelease:
				handlekey(&e.xkey);
			 ClientMessage,
			 CreateNotify,
			 DestroyNotify,
			 ConfigureNotify,
			 ReparentNotify,
			 MapNotify,
			 UnmapNotify:
				{ ignore }
				Exit;
                        else
				WriteLn('wm: mainloop(): ',
				    'unhandled event -- ', eventname(e._type),
				    e._type);
                        end;
                end;
        end;
end;

procedure initclients;
var
	winlist: PWindow;
	d1, d2: TWindow;
	i, n: Cardinal;
begin
	if (XQueryTree(display, root, @d1, @d2, @winlist, @n) <> 0) then
        begin
		for i := 0 to n -1 do
			if (widget_find(winlist[i], CLASS_ANY) = nil) then
				window_manage(winlist[i], 0);
		if (winlist <> nil) then
			XFree(winlist);
        end;
end;

procedure quit(status: Integer);
var
	sigact: SigActionRec;
	mask: Tsigset;
begin
	window_unmanageall();
	hints_fini();

	XFreeFont(display, font);

	XSetInputFocus(display, PointerRoot, RevertToPointerRoot, CurrentTime);
	XCloseDisplay(display);

        {
	 * Reset signal behaviour and resend the signal. Fall back on exit().
         }
	if (signalled <> 0) then
        begin
		WriteLn('terminating on signal ' + signalled);

		sigact.sa_handler := SIG_DFL;
		fpsigfillset(sigact.sa_mask);
		sigact.sa_flags := 0;
		fpsigaction(signalled, @sigact, nil);

		fpsigemptyset(mask);
		fpsigaddset(mask, signalled);
		fpsigprocmask(SIG_UNBLOCK, @mask, nil);

//		raise(signalled);
        end;

	Halt(status);
end;

end.

