//
// * window.pas - window management routines
//
// * Copyright (c) 2006 Johan Veenhuizen
// *
// * Permission is hereby granted, free of charge, to any person obtaining
// * a copy of this software and associated documentation files (the
// * "Software"), to deal in the Software without restriction, including
// * without limitation the rights to use, copy, modify, merge, publish,
// * distribute, sublicense, and/or sell copies of the Software, and to
// * permit persons to whom the Software is furnished to do so, subject
// * to the following conditions:
// *
// * The above copyright notice and this permission notice shall be
// * included in all copies or substantial portions of the Software.
// *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
// * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
//
unit window;

interface
	uses X, Xlib, Xutil, button, global, hints, lib, menu, resizer, title, list, widget;
	
	function getwmstate(W: TWindow): longint;
	//	static longint getwmstate(Window);
	
	procedure client_to_window_geom(p1: pXWindowAttributes; p2: pXSizeHints, 
									p3, p4, p5, p6: pInteger);
	//static void client_to_window_geom(XWindowAttributes *, XSizeHints *,
	//								    int *x, int *y, int *width, int *height);
	
	procedure window_to_client_geom(p1: pWindow; p2, p3, p4, p5: pInteger);
	//	static void window_to_client_geom(struct window *, int *x, int *y, int *width, int *height);
	
	procedure confevent(p1: pWindow; p2: pXConfigureRequestEvent);
	//	static void confevent(struct window *, XConfigureRequestEvent *);
	
	procedure windowevent(p1: pWidget; p2: pXEvent);
	//	static void windowevent(struct widget *, XEvent *);
	
	procedure setgrav(p1: TWindow; p2: integer);
	//	static void setgrav(Window, int);
	
	procedure sendtoback(p1: pWindow);
	//	static void sendtoback(struct window *);
	
	procedure raisetrans(p1: pWindow);
	//	static void raisetrans(struct window *);
	
	procedure raisegrp(p1: pWindow);
	//	static void raisegrp(struct window *);
	
	procedure selectfrommenu(p1: Pointer);
	//	static void selectfrommenu(void *ptr);
	
	function topwin(): pWindow;
	//	static struct window *topwin(void);
	
	function window_manage(p1: TWindow; p2: integer): pWindow;
	//	struct window *window_manage(Window, int);
	
	function window_isactive(p1: pWindow): integer;
	//	int window_isactive(struct window *);
	
	function window_isfamilyactive(p1: pWindow): integer;
	//	int window_isfamilyactive(struct window *);
	
	procedure window_calcsize(p1: pWindow; p2,p3: integer; p4, p5, p6, p7: pInteger);
	//	void window_calcsize(struct window *, int, int, int *, int *, int *, int *);
	
	procedure window_delete(p1: pWindow);
	//	void window_delete(struct window *);
	
	procedure window_fetchiconname(p1: pWindow);
	//	void window_fetchiconname(struct window *);
	
	procedure window_fetchname(p1: pWindow);
	//	void window_fetchname(struct window *);
	
	procedure window_fetchwmhints(p1: pWindow);
	//	void window_fetchwmhints(struct window *);
	
	procedure window_fetchwmnormalhints(p1: pWindow);
	//	void window_fetchwmnormalhints(struct window *);
	
	procedure window_fetchwmtransientfor(p1: pWindow);
	//	void window_fetchwmtransientfor(struct window *);
	
	procedure window_init();	
	//	void window_init(void);
	
	procedure window_lower(p1: pWindow);
	//	void window_lower(struct window *);
	
	procedure window_map(p1: pWindow);
	//	void window_map(struct window *);
	
	procedure window_maximize(p1: pWindow);
	//	void window_maximize(struct window *);
	
	procedure window_move(p1: pWindow; p2, p3: integer);
	//	void window_move(struct window *, int, int);
	
	procedure window_moveresize(p1: pWindow; p2, p3, p4, p5: integer);
	//	void window_moveresize(struct window *, int, int, int, int);
	
	procedure window_raise(p1: pWindow);
	//	void window_raise(struct window *);
	
	procedure window_raisewithgroup(p1: pWindow);
	//	void window_raisewithgroup(struct window *);
	
	procedure window_repaint(p1: pWindow);
	//	void window_repaint(struct window *);
	
	procedure window_resize(p1: pWindow; p2, p3: integer);
	//	void window_resize(struct window *, int, int);
	
	procedure window_samegroup(p1, p2: pWindow);
	//	int window_samegroup(struct window *, struct window *);
	
	procedure window_setactive(p1: pWindow);
	//	void window_setactive(struct window *);
	
	procedure window_unmanage(p1: pWindow; p2: integer);
	//	void window_unmanage(struct window *, int);
	
	procedure window_unmap(p1: pWindow);
	//	void window_unmap(struct window *);
	
	procedure window_restackall();
	//	void window_restackall(void);
	
	procedure window_unmanageall();
	//	void window_unmanageall(void);
	
	
	//duvida: como fica ponteiro para ponteiro?!?!?!
	procedure window_getclientstack(
	//	void window_getclientstack(Window **, int *);

type
	// Meta-Button1 moving of window
	//	struct {
	//		int moving;
	//		int xoff;
	//		int yoff;
	//	} altmove;	
	TAltMove = record
		moving: integer;
		xoff: integer;
		yoff: integer;
	end;

	TWindow = record
		mywidget: 	TWidget;
		name: 		string;
		iconname: 	string;
		title: 		pTitle;
		deletebtn: 	pButton;
		lowerbtn: 	pButton;
		hidebtn: 	pButton;
		
		rsz_northwest, rsz_north, rsz_northeast, rsz_west, rsz_east, rsz_southwest,
			     rsz_south, rsz_southeast: pResizer;
		//struct resizer *rsz_northwest, *rsz_north, *rsz_northeast,
		//   	 *rsz_west, *rsz_east, *rsz_southwest, *rsz_south, *rsz_southeast;

        //struct menuitem *menuitem;
		menuitem: pMenuItem;

        //Window client;
		client: TWindow;

		// ICCCM hints
		wmnormalhints: pXSizeHints;
			//XSizeHints *wmnormalhints;
   
		wmhints: pXWMHints;
			//XWMHints *wmhints;
   
		wmtransientfor: TWindow;
			//Window wmtransientfor;

		// client's initial border width
		cborder: integer;

		// Meta-Button1 moving of window
		altmove: TAltMove;		

		ignoreunmap: integer;
		//int ignoreunmap;
		
		maximized: integer;
		//int maximized;
		
		// remembered dim while maximized
		odim: TDim;
			//struct dim odim;	

		// stacking order
		stacking: TList;
	end;



var
	const NBTN = 3
	
	active: pWindow;
	//struct window *active = NULL;

	WM_STATE: TAtom;
	//static Atom WM_STATE;	

	front: TWindow;
	//static Window front;

	//static LIST_DEFINE(winstack); TODO: verificar macros em list.c
	
	needrestack: integer;
	//static int needrestack = 0;
	
	nwindows: integer;
	//static int nwindows = 0;
	
	movecurs: TCursor;
	//static Cursor movecurs;

	//?!??!!?!? essas sao declaracoes sem nome?!?!?!
	//struct button;
	//struct menuitem;
	//struct resizer;
	//struct title;


implementation

	procedure window_init();
	var
		attr: XSetWindowAttributes;
	begin
		attr.override_redirect := True;
		front := XCreateWindow(display, root, 0, 0, 1, 1, 0, 0, InputOnly,
	    	CopyFromParent, CWOverrideRedirect, &attr);
		movecurs := XCreateFontCursor(display, XC_fleur);

		WM_STATE := XInternAtom(display, 'WM_STATE', False);
	end;

procedure window_raise(win: pWindow);
	begin
		LIST_REMOVE(&win->stacking); //como pegar o endereco?!?!?
		LIST_INSERT_HEAD(&winstack, &win->stacking);
		needrestack := 1;
	end;

procedure window_lower(struct window *win)
	begin
		LIST_REMOVE(&win->stacking);
		LIST_INSERT_TAIL(&winstack, &win->stacking);
		needrestack := 1;
	end;

procedure window_putabove(struct window *ref, struct window *win)
	begin
		LIST_REMOVE(&win->stacking);
		LIST_INSERT_BEFORE(&ref->stacking, &win->stacking);
		needrestack := 1;
	end;

void window_putbelow(struct window *ref, struct window *win)
begin
	LIST_REMOVE(&win->stacking);
	LIST_INSERT_AFTER(&ref->stacking, &win->stacking);
	needrestack = 1;
end;

void window_getwindowstack(struct window ***wins_return, int *nwins_return)
begin
	LIST *lp;
	int i;

	*wins_return = karmen_malloc(nwindows * sizeof (struct window *));
	*nwins_return = nwindows;
	i = 0;
	LIST_FOREACH(lp, &winstack) begin
		assert(i < nwindows);
		(*wins_return)[i++] = LIST_ITEM(lp, struct window, stacking);
	end;
	assert(i == nwindows);
end;

void window_getclientstack(Window **clients_return, int *nclients_return)
begin
	LIST *lp;
	struct window *win;
	int i;

	*clients_return = karmen_malloc(nwindows * sizeof (Window));
	*nclients_return = nwindows;
	i = 0;
	LIST_FOREACH(lp, &winstack) begin
		assert(i < nwindows);
		win = LIST_ITEM(lp, struct window, stacking);
		(*clients_return)[i++] = win->client;
	end;
	assert(i == nwindows);
end;

void window_restackall(void)
begin
	Window *xwins;
	struct window *win;
	LIST *lp;
	int i;

	if (!needrestack)
		return;

	xwins = karmen_malloc((nwindows + 1) * sizeof (Window));
	xwins[0] = front;
	i = 1;
	LIST_FOREACH(lp, &winstack) begin
		assert(i < nwindows + 1);
		win = LIST_ITEM(lp, struct window, stacking);
		xwins[i++] = XWINDOW(win);
	end;
	assert(i == nwindows + 1);
	XRestackWindows(display, xwins, nwindows + 1);
	needrestack = 0;

	hints_restack();
end;

static struct window *topwin(void)
begin
	struct window *win;
	LIST *lp;

	LIST_FOREACH(lp, &winstack) begin
		win = LIST_ITEM(lp, struct window, stacking);
		if (MAPPED(win))
			return win;
	end;
	return NULL;
end;

void window_map(struct window *win)
begin
	clerr();
	XMapWindow(display, win->client);
	sterr();

	widget_map((struct widget *)win);
	hints_map(win);
end;

void window_unmap(struct window *win)
begin
	widget_unmap((struct widget *)win);

	if (win == active)
		window_setactive(topwin());

	clerr();
	win->ignoreunmap++;
	XUnmapWindow(display, win->client);
	sterr();

	hints_unmap(win);

	window_lower(win);
end;

static longint getwmstate(Window xwindow)
begin
	unsigned longint nitems, bytes_after;
	longint state;
	unsigned char *prop;
	Atom actual_type;
	int actual_format;

	state = WithdrawnState;
	if (XGetWindowProperty(display, xwindow,
	    WM_STATE, 0L, 2L, False, WM_STATE, &actual_type, &actual_format,
	    &nitems, &bytes_after, &prop) == Success) begin
		if (nitems > 0)
			state = ((unsigned longint *)prop)[0];
		XFree(prop);
	end;
	return state;
end;

static void client_to_window_geom(XWindowAttributes *attr, XSizeHints *sz,
    int *x, int *y, int *width, int *height)
begin
	int north, south, east, west, stat, center;

	north = south = east = west = stat = center = 0;

	if (sz->flags & PWinGravity) begin
		switch (sz->win_gravity) begin
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
		end;
	end; else begin
		north = 1;
		west = 1;
	end;

	if (north)
		*y = attr->y;
	else if (south)
		*y = attr->y + 2 * attr->border_width
		    - 2 * border_width - innerborder_width - button_size;
	else if (center)
		*y = attr->y + attr->border_width
		    + attr->height / 2
		    - (attr->height + 2 * border_width
		       + button_size + innerborder_width) / 2;
	else if (stat)
		*y = attr->y + attr->border_width
		    - border_width - innerborder_width - button_size;
	else
		*y = attr->y;

	if (west)
		*x = attr->x;
	else if (east)
		*x = attr->x + 2 * attr->border_width
		    - 2 * border_width;
	else if (center)
		*x = attr->x + attr->border_width
		    + attr->width / 2
		    - (attr->width + 2 * border_width) / 2;
	else if (stat)
		*x = attr->x + attr->border_width - border_width;
	else
		*x = attr->x;

	*width = attr->width + 2 * border_width;
	*height = attr->height
	    + 2 * border_width + button_size + innerborder_width;
end;

static void window_to_client_geom(struct window *win,
    int *x, int *y, int *width, int *height)
begin
	int north, south, east, west, stat, center;

	north = south = east = west = stat = center = 0;

	if (win->wmnormalhints->flags & PWinGravity) begin
		switch (win->wmnormalhints->win_gravity) begin
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
		end;
	end; else begin
		north = 1;
		west = 1;
	end;

	if (north)
		*y = Y(win);
	else if (south)
		*y = Y(win) - 2 * win->cborder
		    + 2 * border_width + button_size + innerborder_width;
	else if (center)
		*y = Y(win) + HEIGHT(win) / 2
		    - (HEIGHT(win) - 2 * border_width - button_size
		       - innerborder_width) / 2
		    - win->cborder;
	else if (stat)
		*y = Y(win) - win->cborder
		    + border_width + innerborder_width + button_size;
	else
		*y = Y(win);

	if (west)
		*x = X(win);
	else if (east)
		*x = X(win) - 2 * win->cborder + 2 * border_width;
	else if (center)
		*x = X(win) + WIDTH(win) / 2
		    - (WIDTH(win) - 2 * border_width) / 2 - win->cborder;
	else if (stat)
		*x = X(win) - win->cborder + border_width;
	else
		*x = X(win);

	*width = WIDTH(win) - 2 * border_width;
	*height = HEIGHT(win)
	    - 2 * border_width - button_size - innerborder_width;
end;

void window_delete(struct window *win)
begin
	if (!hints_delete(win)) begin
		clerr();
		XKillClient(display, win->client);
		sterr();
	end;
end;

static void confevent(struct window *win, XConfigureRequestEvent *ep)
begin
	int x = X(win);
	int y = Y(win);
	int width = WIDTH(win);
	int height = HEIGHT(win);

	if (ep->value_mask & CWBorderWidth)
		win->cborder = ep->border_width;

	if (ep->value_mask & CWX)
		x = ep->x + win->cborder - border_width;
	if (ep->value_mask & CWY)
		y = ep->y + win->cborder
		    - (border_width + button_size + innerborder_width);
	if (ep->value_mask & CWWidth)
		width = ep->width + 2 * border_width;
	if (ep->value_mask & CWHeight)
		height = ep->height
		    + 2 * border_width + button_size + innerborder_width;

	begin
	 * FIXME: handle stacking order
	 end;

	window_moveresize(win, x, y, width, height);
end;

static void windowevent(struct widget *widget, XEvent *ep)
begin
	struct window *win = (struct window *)widget;

	switch (ep->type) begin
	case ButtonPress:
		win->altmove.xoff = ep->xbutton.x;
		win->altmove.yoff = ep->xbutton.y;
		win->altmove.moving = 1;
		window_setactive(win);
		break;
	case MotionNotify:
		if (win->altmove.moving)
			window_move(win,
			    ep->xmotion.x_root - win->altmove.xoff,
			    ep->xmotion.y_root - win->altmove.yoff);
		break;
	case ButtonRelease:
		win->altmove.moving = 0;
		break;
	case MapRequest:
		window_setactive(win);
		break;
	case UnmapNotify:
		begin This can be a real or synthetic unmap event. end;
		if (ep->xunmap.window != win->client)
			break;
		if (win->ignoreunmap <= 0) begin
			hints_withdraw(win);
			window_unmanage(win, 1);
		end; else
			win->ignoreunmap--;
		break;
	case ConfigureRequest:
		confevent(win, &ep->xconfigurerequest);
		break;
	case ClientMessage:
		hints_clientmessage(win, &ep->xclient);
		break;
	case PropertyNotify:
		hints_propertynotify(win, &ep->xproperty);
		break;
	case DestroyNotify:
		if (ep->xdestroywindow.window == win->client)
			window_unmanage(win, 1);
		break;
	case GravityNotify:
	case CreateNotify:
	case MapNotify:
	case ReparentNotify:
	case ConfigureNotify:
		begin ignore end;
		break;
	default:
		debug("windowevent(): unhandled event -- %s (%d)",
		    eventname(ep->type), ep->type);
		break;
	end;
end;

void low_limit_size(int *width, int *height)
begin
	int minwidth = MAX(3 * button_size, 2 * border_width +
	    (NBTN + 1) * (button_size + innerborder_width));
	int minheight = 3 * button_size;

	*width = MAX(*width, minwidth);
	*height = MAX(*height, minheight);
end;

void window_calcsize(struct window *win, int width, int height,
    int *rwidth, int *rheight, int *rxdim, int *rydim)
begin
	int decwidth = 2 * border_width;
	int decheight = 2 * border_width + button_size + innerborder_width;
	int havemin = 0;
	int minwidth = 0;
	int minheight = 0;
	int wmminwidth = 0;
	int wmminheight = 0;

	low_limit_size(&width, &height);
	low_limit_size(&wmminwidth, &wmminheight);

	width -= decwidth;
	height -= decheight;

	if (win->wmnormalhints->flags & PMaxSize) begin
		width = MIN(width, win->wmnormalhints->max_width);
		height = MIN(height, win->wmnormalhints->max_height);
	end;

	havemin = 0;
	if (win->wmnormalhints->flags & PMinSize) begin
		minwidth = win->wmnormalhints->min_width;
		minheight = win->wmnormalhints->min_height;
		havemin = 1;
	end; else if (win->wmnormalhints->flags & PBaseSize) begin
		minwidth = win->wmnormalhints->base_width;
		minheight = win->wmnormalhints->base_height;
		havemin = 1;
	end;
	if (havemin) begin
		width = MAX(width, minwidth);
		height = MAX(height, minheight);
	end;

	if (win->wmnormalhints->flags & PResizeInc) begin
		if (win->wmnormalhints->width_inc != 0) begin
			int wb;
			if (win->wmnormalhints->flags & PBaseSize)
				wb = win->wmnormalhints->base_width;
			else if (win->wmnormalhints->flags & PMinSize)
				wb = win->wmnormalhints->min_width;
			else
				wb = 0;
			width -= wb;
			width -= width % win->wmnormalhints->width_inc;
			if (havemin)
				width = MAX(width, minwidth - wb);
			while (wb + width + decwidth < wmminwidth)
				width += win->wmnormalhints->width_inc;
			if (rxdim != NULL)
				*rxdim = width / win->wmnormalhints->width_inc;
			width += wb;
		end; else if (rxdim != NULL)
			*rxdim = width;
		if (win->wmnormalhints->height_inc != 0) begin
			int hb;
			if (win->wmnormalhints->flags & PBaseSize)
				hb = win->wmnormalhints->base_height;
			else if (win->wmnormalhints->flags & PMinSize)
				hb = win->wmnormalhints->min_height;
			else
				hb = 0;
			height -= hb;
			height -= height % win->wmnormalhints->height_inc;
			if (havemin)
				height = MAX(height, minheight - hb);
			while (hb + height + decheight < wmminheight)
				height += win->wmnormalhints->height_inc;
			if (rydim != NULL)
				*rydim =
				    height / win->wmnormalhints->height_inc;
			height += hb;
		end; else if (rydim != NULL)
			*rydim = height;
	end; else begin
		if (rxdim != NULL)
			*rxdim = width;
		if (rydim != NULL)
			*rydim = height;
	end;

	width += 2 * border_width;
	height += 2 * border_width + button_size + innerborder_width;

	if (rwidth != NULL)
		*rwidth = width;
	if (rheight != NULL)
		*rheight = height;
end;

static void setgrav(Window xwin, int grav)
begin
	XSetWindowAttributes attr;

	attr.win_gravity = grav;
	XChangeWindowAttributes(display, xwin, CWWinGravity, &attr);
end;

void window_maximize(struct window *win)
begin
	int x, y, rwidth, rheight;

	if (win->maximized) begin
		window_moveresize(win, win->odim.x, win->odim.y,
		    win->odim.width, win->odim.height);
		win->maximized = 0;
	end; else begin
		win->odim = win->widget.dim;
		window_calcsize(win,
		    DisplayWidth(display, screen),
		    DisplayHeight(display, screen),
		    &rwidth, &rheight, NULL, NULL);
		x = (DisplayWidth(display, screen) - rwidth) / 2;
		y = (DisplayHeight(display, screen) - rheight) / 2;
		window_moveresize(win, x, y, rwidth, rheight);
		win->maximized = 1;
	end;
end;

static void sendtoback(struct window *win)
begin
	struct window *top;

	top = topwin();
	window_lower(win);
	if (win == top)
		window_setactive(topwin());
end;
	

struct window *window_manage(Window client, int isnew)
begin
	struct window *win;
	XWindowAttributes attr;
	XSizeHints *sz;
	XWMHints *wmhints;
	longint state;
	longint dummy;
	int x, y, width, height;

	clerr();
	if (!XGetWindowAttributes(display, client, &attr)) begin
		sterr();
		return NULL;
	end;
	sterr();

	if (attr.override_redirect || (!isnew && attr.map_state != IsViewable))
		return NULL;

	if (widget_find(client, CLASS_ANY) != NULL) begin
		debug("XXX: Trying to remanage a window!");
		return NULL;
	end;

	if (isnew) begin
		clerr();
		wmhints = XGetWMHints(display, client);
		sterr();
		if (wmhints == NULL)
			state = NormalState;
		else begin
			if ((wmhints->flags & StateHint) != 0)
				state = wmhints->initial_state;
			else
				state = NormalState;
			XFree(wmhints);
		end;
	end; else begin
		clerr();
		state = getwmstate(client);
		sterr();
	end;

	if (state == WithdrawnState) begin
		debug("skipping withdrawn window");
		return NULL;
	end;

	while ((sz = XAllocSizeHints()) == NULL)
		sleep(1);
	clerr();
	XGetWMNormalHints(display, client, sz, &dummy);
	sterr();
	client_to_window_geom(&attr, sz, &x, &y, &width, &height);
	low_limit_size(&width, &height);
	if (isnew
	    && (sz->flags & USPosition) == 0
	    && (sz->flags & PPosition) == 0) begin
		x = MAX(0, (DisplayWidth(display, screen) - width) / 2);
		y = MAX(0, (DisplayHeight(display, screen) - height) / 2);
	end;
	XFree(sz);

	win = karmen_malloc(sizeof (struct window));
	widget_create(&win->widget, CLASS_WINDOW, root, x, y, width, height);

	LIST_INSERT_HEAD(&winstack, &win->stacking);
	nwindows++;
	needrestack = 1;

	win->ignoreunmap = 0;

	win->cborder = attr.border_width;

	win->altmove.moving = 0;
	grabbutton(display, Button1, Mod1Mask, XWINDOW(win), False,
	    ButtonPressMask | ButtonMotionMask,
	    GrabModeAsync, GrabModeAsync, None, movecurs);

	XSelectInput(display, XWINDOW(win),
	    SubstructureRedirectMask | SubstructureNotifyMask);
	win->widget.event = windowevent;
	XSetWindowBackground(display, XWINDOW(win), color_innerborder.normal);

	win->deletebtn = button_create(win, border_width, border_width,
	    button_size, button_size);
	button_setimage(win->deletebtn, &delete_image);
	button_sethandler(win->deletebtn, window_delete);

	win->hidebtn = button_create(win,
	    width - border_width - 2 * button_size - innerborder_width,
	    border_width, button_size, button_size);
	button_setimage(win->hidebtn, &hide_image);
	button_sethandler(win->hidebtn, window_unmap);
	setgrav(XWINDOW(win->hidebtn), NorthEastGravity);

	win->lowerbtn = button_create(win,
	    width - border_width - button_size, border_width,
	    button_size, button_size);
	button_setimage(win->lowerbtn, &lower_image);
	button_sethandler(win->lowerbtn, sendtoback);
	setgrav(XWINDOW(win->lowerbtn), NorthEastGravity);

	win->title = title_create(win,
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

	win->client = client;
	widget_savecontext(&win->widget, client);

	win->rsz_northwest = resizer_create(win, NORTHWEST);
	win->rsz_north = resizer_create(win, NORTH);
	win->rsz_northeast = resizer_create(win, NORTHEAST);
	win->rsz_west = resizer_create(win, WEST);
	win->rsz_east = resizer_create(win, EAST);
	win->rsz_southwest = resizer_create(win, SOUTHWEST);
	win->rsz_south = resizer_create(win, SOUTH);
	win->rsz_southeast = resizer_create(win, SOUTHEAST);

	win->wmhints = NULL;
	window_fetchwmhints(win);

	win->wmnormalhints = NULL;
	window_fetchwmnormalhints(win);

	window_fetchwmtransientfor(win);

	win->name = NULL;
	window_fetchname(win);

	win->menuitem = NULL;
	win->iconname = NULL;
	window_fetchiconname(win);

	hints_manage(win);

	window_map(win);

	win->maximized = 0;

	if (state == IconicState)
		window_unmap(win);
	else
		window_setactive(win);

	XSync(display, False);

	debug("manage \"%s\" (Window=%d)", win->name, (int)win->client);

	XGrabServer(display);
	clerr();
	if (!XGetWindowAttributes(display, client, &attr)) begin
		sterr();
		XUngrabServer(display);
		debug("Oops, client window disappeared in window_manage()");
		window_unmanage(win, 1);
		return NULL;
	end;
	sterr();
	XUngrabServer(display);

	return win;
end;

void window_fetchwmnormalhints(struct window *win)
begin
	longint dummy;

	if (win->wmnormalhints != NULL)
		XFree(win->wmnormalhints);
	while ((win->wmnormalhints = XAllocSizeHints()) == NULL)
		sleep(1);
	clerr();
	XGetWMNormalHints(display, win->client, win->wmnormalhints, &dummy);
	sterr();
end;

void window_fetchwmhints(struct window *win)
begin
	if (win->wmhints != NULL)
		XFree(win->wmhints);
	clerr();
	win->wmhints = XGetWMHints(display, win->client);
	sterr();
end;

void window_fetchname(struct window *win)
begin
	if (win->name != NULL) begin
		XFree(win->name);
		win->name = NULL;
	end;
	clerr();
	XFetchName(display, win->client, &win->name);
	sterr();
	if (MAPPED(win))
		title_repaint(win->title);
end;

void window_fetchiconname(struct window *win)
begin
	if (win->iconname != NULL) begin
		XFree(win->iconname);
		win->iconname = NULL;
	end;
	clerr();
	XGetIconName(display, win->client, &win->iconname);
	sterr();

	if (win->menuitem == NULL)
		win->menuitem = menu_additem(winmenu, win->iconname,
		    selectfrommenu, win);
	else
		menu_renameitem(winmenu, win->menuitem, win->iconname);
end;

static void selectfrommenu(void *ptr)
begin
	window_setactive(ptr);
end;

void window_fetchwmtransientfor(struct window *win)
begin
	win->wmtransientfor = None;
	clerr();
	XGetTransientForHint(display, win->client, &win->wmtransientfor);
	sterr();
end;

void window_moveresize(struct window *win, int x, int y, int width, int height)
begin
	int move;
	int resize;

	low_limit_size(&width, &height);

	move = x != X(win) || y != Y(win);
	resize = width != WIDTH(win) || height != HEIGHT(win);

	if (resize) begin
		title_resize(win->title,
		    width - 2 * border_width
		     - NBTN * (innerborder_width + button_size),
		    button_size);
		clerr();
		XResizeWindow(display, win->client,
		    width - 2 * border_width,
		    height - 2 * border_width - button_size -
		    innerborder_width);
		sterr();
	end;

	widget_moveresize((struct widget *)win, x, y, width, height);

	if (resize) begin
		resizer_fit(win->rsz_northwest);
		resizer_fit(win->rsz_north);
		resizer_fit(win->rsz_northeast);
		resizer_fit(win->rsz_west);
		resizer_fit(win->rsz_east);
		resizer_fit(win->rsz_southwest);
		resizer_fit(win->rsz_south);
		resizer_fit(win->rsz_southeast);
	end;

	if (move && !resize)
		hints_move(win);
	else if (!move && resize)
		hints_resize(win);
	else if (move && resize)
		hints_moveresize(win);

	win->maximized = 0;
end;

void window_move(struct window *win, int x, int y)
begin
	window_moveresize(win, x, y, WIDTH(win), HEIGHT(win));
end;

void window_resize(struct window *win, int width, int height)
begin
	window_moveresize(win, X(win), Y(win), width, height);
end;

void window_unmanageall(void)
begin
	struct window *win;
	LIST *lp;

	while (!LIST_EMPTY(&winstack)) begin
		lp = LIST_TAIL(&winstack);
		LIST_REMOVE(lp);
		win = LIST_ITEM(lp, struct window, stacking);
		window_unmanage(win, 0);
	end;
end;

void window_unmanage(struct window *win, int unmap)
begin
	int x, y, width, height;

	debug("unmanage \"%s\" (Window=%d)",win->name, (int)win->client);

	hints_unmanage(win);

	if (MAPPED(win))
		widget_unmap(&win->widget);

	if (win == active)
		window_setactive(topwin());

	if (win->menuitem != NULL)
		menu_delitem(winmenu, win->menuitem);

	window_to_client_geom(win, &x, &y, &width, &height);
	widget_deletecontext(win->client);

	clerr();
	XReparentWindow(display, win->client, root, x, y);
	if (!unmap)
		XMapWindow(display, win->client);
	ungrabbutton(display, Button1, 0, win->client);
	XSelectInput(display, win->client, 0);
	XSetWindowBorderWidth(display, win->client, win->cborder);
	if (win->wmnormalhints->flags & PWinGravity)
		setgrav(win->client, win->wmnormalhints->win_gravity);
	XRemoveFromSaveSet(display, win->client);
	sterr();

	title_destroy(win->title);
	button_destroy(win->deletebtn);
	button_destroy(win->lowerbtn);
	button_destroy(win->hidebtn);

	if (win->wmhints != NULL)
		XFree(win->wmhints);
	assert(win->wmnormalhints != NULL);
	XFree(win->wmnormalhints);

	LIST_REMOVE(&win->stacking);
	nwindows--;

	widget_destroy(&win->widget);
	if (win->name != NULL)
		XFree(win->name);
	if (win->iconname != NULL)
		XFree(win->iconname);
	karmen_free(win);
end;

void window_repaint(struct window *win)
begin
	title_repaint(win->title);
	button_repaint(win->deletebtn);
	button_repaint(win->lowerbtn);
	button_repaint(win->hidebtn);
end;

void window_repaintfamily(struct window *win)
begin
	struct window *wp;
	LIST *lp;

	LIST_FOREACH(lp, &winstack) begin
		wp = LIST_ITEM(lp, struct window, stacking);
		if (MAPPED(wp) &&
		    (wp == win
		     || wp->wmtransientfor == win->client
		     || window_samegroup(wp, win)))
			window_repaint(wp);
	end;
end;

static void raisetrans(struct window *win)
begin
	struct window **wins;
	int i, n;

	window_getwindowstack(&wins, &n);
	for (i = n - 1; i >= 0; i--) begin
		if (wins[i] == win)
			break;  begin the rest are above us already end;
		else if (wins[i]->wmtransientfor == win->client)
			window_raise(wins[i]);
	end;
	karmen_free(wins);
end;

static void raisegrp(struct window *win)
begin
	struct window **wins, *wp;
	int n;

	if (win->wmhints == NULL
	    || (win->wmhints->flags & WindowGroupHint) == 0
	    || win->wmhints->window_group == None
	    || win->wmhints->window_group == root)
		return;

	window_getwindowstack(&wins, &n);
	while (n > 1 && wins[--n] != win) begin
		wp = wins[n];
		if (wp->wmhints != NULL
		    && (wp->wmhints->flags & WindowGroupHint) != 0
		    && wp->wmhints->window_group == win->wmhints->window_group)
			window_putbelow(win, wp);
	end;
	karmen_free(wins);
end;

begin
 * Return true if window is the active window.
 end;
int window_isactive(struct window *win)
begin
	return win == active;
end;

begin
 * Return true if window is a transient for the active window.
 end;
int window_istransactive(struct window *win)
begin
	return active != NULL && win->wmtransientfor == active->client;
end;

begin
 * Return true if window is a member of the active window group.
 end;
int window_isgroupactive(struct window *win)
begin
	return win->wmhints != NULL
	    && (win->wmhints->flags & WindowGroupHint) != 0
	    && active != NULL
	    && active->wmhints != NULL
	    && (active->wmhints->flags & WindowGroupHint) != 0
	    && win->wmhints->window_group == active->wmhints->window_group;
end;

int window_isfamilyactive(struct window *win)
begin
	return window_isactive(win)
	    || window_istransactive(win) || window_isgroupactive(win);
end;

int window_samegroup(struct window *win1, struct window *win2)
begin
	return win1->wmhints != NULL && win2->wmhints != NULL
	    && (win1->wmhints->flags & WindowGroupHint) != 0
	    && (win2->wmhints->flags & WindowGroupHint) != 0
	    && win1->wmhints->window_group == win2->wmhints->window_group;
end;

int window_related(struct window *win1, struct window *win2)
begin
	return window_samegroup(win1, win2)
	    || win1->wmtransientfor == win2->client
	    || win2->wmtransientfor == win1->client;
end;

begin
 * Raise window, and all windows in its group unless window is a
 * transient window.
 end;
void window_raisewithgroup(struct window *win)
begin
	struct window *tmp;

	if (win->wmtransientfor != None) begin
		tmp = (struct window *)widget_find(win->wmtransientfor,
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
void window_setactive(struct window *win)
begin
	struct window *old;

	if (win != NULL && win == active) begin
		raisetrans(win);
		return;
	end;

	old = active;
	active = win;

	if (old != NULL) begin
		clerr();
		grabbutton(display, Button1, 0, old->client, True,
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
		XSetInputFocus(display, active->client, RevertToPointerRoot,
		    CurrentTime);
		ungrabbutton(display, Button1, 0, active->client);
		XAllowEvents(display, ReplayPointer, CurrentTime);
		sterr();

		window_raisewithgroup(active);
		window_repaintfamily(active);
		menu_movetotop(winmenu, active->menuitem);
	end; else
		XSetInputFocus(display, root, RevertToPointerRoot,
		    CurrentTime);

	hints_activate(active);
end;
