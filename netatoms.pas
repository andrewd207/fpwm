unit NETAtoms;
{
  This Unit contains all the _NET atoms from the freedesktop 1.4 spec
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, X, XLib;

type
  TNETAtom = (
    //WM_ Atoms
    // _NET Atoms
    //Root Window Properties
    _SUPPORTED, // array of cardinal (_NET atoms that are supported)
    _CLIENT_LIST, // array of TWindow
    _CLIENT_LIST_STACKING, // array of TWindow (bottom to top)
    _NUMBER_OF_DESKTOPS, // cardinal
    _DESKTOP_GEOMETRY, // array [0..1] of cardinal (width height)
    _DESKTOP_VIEWPORT, // array of cardinal array [0..1] viewports (top left position)
    _CURRENT_DESKTOP, // cardinal (index of desktop)
    _DESKTOP_NAMES, // array of null terminated UTF8 Strings
    _ACTIVE_WINDOW, // TWindow
    _WORKAREA, // array [0..3] of cardinal (x, y, width, height)
    _SUPPORTING_WM_CHECK, // TWindow
    _VIRTUAL_ROOTS, // array of TWindow
    _DESKTOP_LAYOUT, // array [0..3] of cardinal (orientation, columns, rows, starting_corner)
    _SHOWING_DESKTOP, // cardinal (1 = Showing Desktop 0 = not Showing Desktop) boolean


    //Root Window Messages
    _CLOSE_WINDOW,
    _MOVERESIZE_WINDOW,
    _WM_MOVERESIZE,
    _RESTACK_WINDOW,
    _REQUEST_FRAME_EXTENTS,

    //Types
    UTF8_STRING,
    //Application Window Properties
    _WM_NAME, // UTF8 String
    _WM_VISIBLE_NAME, //UTF8 String
    _WM_ICON_NAME,  // UTF8 String
    _WM_VISIBLE_ICON_NAME, // UTF8 String
    _WM_DESKTOP, // cardinal (index of the desktop of the window) $FFFFFFFF for all desktops
    _WM_WINDOW_TYPE, // TAtom of the different types below
    _WM_WINDOW_TYPE_DESKTOP,
    _WM_WINDOW_TYPE_DOCK,
    _WM_WINDOW_TYPE_TOOLBAR,
    _WM_WINDOW_TYPE_MENU,
    _WM_WINDOW_TYPE_UTILITY,
    _WM_WINDOW_TYPE_SPLASH,
    _WM_WINDOW_TYPE_DIALOG,
    _WM_WINDOW_TYPE_NORMAL,
    _WM_STATE, // array of TAtoms. Possible members are listed below. others should be ignored
    _WM_STATE_MODAL,
    _WM_STATE_STICKY,
    _WM_STATE_MAXIMIZED_VERT,
    _WM_STATE_MAXIMIZED_HORZ,
    _WM_STATE_SHADED,
    _WM_STATE_SKIP_TASKBAR,
    _WM_STATE_SKIP_PAGER,
    _WM_STATE_HIDDEN,
    _WM_STATE_FULLSCREEN,
    _WM_STATE_ABOVE,
    _WM_STATE_BELOW,
    _WM_STATE_DEMANDS_ATTENTION,
    _WM_ALLOWED_ACTIONS, //array of TAtoms below. unknown atoms are ignored
    _WM_ACTION_MOVE,
    _WM_ACTION_RESIZE,
    _WM_ACTION_MINIMIZE,
    _WM_ACTION_SHADE,
    _WM_ACTION_STICK,
    _WM_ACTION_MAXIMIZE_HORZ,
    _WM_ACTION_MAXIMIZE_VERT,
    _WM_ACTION_FULLSCREEN,
    _WM_ACTION_CHANGE_DESKTOP,
    _WM_ACTION_CLOSE,
    _WM_STRUT, // array [0..3] of cardinal (left right top bottom)
    _WM_STRUT_PARTIAL, // array [0..11] of cardinal (left, right, top, bottom,
                      // left_start_y, left_end_y, right_start_y, right_end_y,
                      // top_start_x, top_end_x, bottom_start_x, bottom_end_x )
    _WM_ICON_GEOMETRY, // array [0..3] of cardinal (x, y, width, height)
    _WM_ICON, // array of cardinal the first two in the array are the width height
             // and the rest of the array is the icon data in BGRA order
    _WM_PID, // cardinal (process id of the window)
    _WM_HANDLED_ICONS,
    _WM_USER_TIME, // cardinal (XServer time of last user activity)
    _FRAME_EXTENTS, // array [0..3] of cardinal (left, right, top ,bottom)

    //Window Manager Protocols
    _WM_PING,
    _WM_SYNC_REQUEST
    );

const
  _NET_WM_ORIENTATION_HORZ = 0;
  _NET_WM_ORIENTATION_VERT = 1;

  _NET_WM_TOPLEFT     = 0;
  _NET_WM_TOPRIGHT    = 1;
  _NET_WM_BOTTOMRIGHT = 2;
  _NET_WM_BOTTOMLEFT  = 3;

  _NET_WM_MOVERESIZE_SIZE_TOPLEFT     = 0;
  _NET_WM_MOVERESIZE_SIZE_TOP         = 1;
  _NET_WM_MOVERESIZE_SIZE_TOPRIGHT    = 2;
  _NET_WM_MOVERESIZE_SIZE_RIGHT       = 3;
  _NET_WM_MOVERESIZE_SIZE_BOTTOMRIGHT = 4;
  _NET_WM_MOVERESIZE_SIZE_BOTTOM      = 5;
  _NET_WM_MOVERESIZE_SIZE_BOTTOMLEFT  = 6;
  _NET_WM_MOVERESIZE_SIZE_LEFT        = 7;
  _NET_WM_MOVERESIZE_MOVE             = 8;    // movement only
  _NET_WM_MOVERESIZE_SIZE_KEYBOARD    = 9;    // size via keyboard
  _NET_WM_MOVERESIZE_MOVE_KEYBOARD    = 10;   // move via keyboard
  _NET_WM_MOVERESIZE_CANCEL           = 11;   // cancel operation

  _NET_WM_STATE_REMOVE       = 0;    // remove/unset property
  _NET_WM_STATE_ADD          = 1;    // add/set property
  _NET_WM_STATE_TOGGLE       = 2;    // toggle property

type
  TNetAtoms = array [TNETAtom] of X.TAtom;

procedure Init_NETAtoms(const ADisplay: PXDisplay; out AAtoms: TNetAtoms);
function _NETAtomToString(Atom: TNetAtom): String;

implementation

procedure Init_NETAtoms(const ADisplay: PXDisplay; out AAtoms: TNetAtoms);
var
  NetAtom: TNetAtom;
  AtomStr: String;
begin
  for NetAtom := Low(TNetAtom) to High(TNetAtom) do begin
    AtomStr := _NETAtomToString(NetAtom);
    AAtoms[NetAtom] := XInternAtom(ADisplay, PChar(AtomStr), False);
  end;
end;

function _NETAtomToString(Atom: TNetAtom): String;
begin
  Result := '';
  case Atom of
    _SUPPORTED             : Result := '_NET_SUPPORTED';
    _CLIENT_LIST           : Result := '_NET_CLIENT_LIST';
    _CLIENT_LIST_STACKING  : Result := '_NET_CLIENT_LIST_STACKING';
    _NUMBER_OF_DESKTOPS    : Result := '_NET_NUMBER_OF_DESKTOPS';
    _DESKTOP_GEOMETRY      : Result := '_NET_DESKTOP_GEOMETRY';
    _DESKTOP_VIEWPORT      : Result := '_NET_DESKTOP_VIEWPORT';
    _CURRENT_DESKTOP       : Result := '_NET_CURRENT_DESKTOP';
    _DESKTOP_NAMES         : Result := '_NET_DESKTOP_NAMES';
    _ACTIVE_WINDOW         : Result := '_NET_ACTIVE_WINDOW';
    _WORKAREA              : Result := '_NET_WORKAREA';
    _SUPPORTING_WM_CHECK   : Result := '_NET_SUPPORTING_WM_CHECK';
    _VIRTUAL_ROOTS         : Result := '_NET_VIRTUAL_ROOTS';
    _DESKTOP_LAYOUT        : Result := '_NET_DESKTOP_LAYOUT';
    _SHOWING_DESKTOP       : Result := '_NET_SHOWING_DESKTOP';
    _CLOSE_WINDOW          : Result := '_NET_CLOSE_WINDOW';
    _MOVERESIZE_WINDOW     : Result := '_NET_MOVERESIZE_WINDOW';
    _WM_MOVERESIZE         : Result := '_NET_WM_MOVERESIZE';
    _RESTACK_WINDOW        : Result := '_NET_RESTACK_WINDOW';
    _REQUEST_FRAME_EXTENTS : Result := '_NET_REQUEST_FRAME_EXTENTS';
     UTF8_STRING           : Result :=  'UTF8_STRING';
    _WM_NAME               : Result := '_NET_WM_NAME';
    _WM_VISIBLE_NAME       : Result := '_NET_WM_VISIBLE_NAME';
    _WM_ICON_NAME          : Result := '_NET_WM_ICON_NAME';
    _WM_VISIBLE_ICON_NAME  : Result := '_NET_WM_VISIBLE_ICON_NAME';
    _WM_DESKTOP            : Result := '_NET_WM_DESKTOP';
    _WM_WINDOW_TYPE        : Result := '_NET_WM_WINDOW_TYPE';
    _WM_WINDOW_TYPE_DESKTOP: Result := '_NET_WM_WINDOW_TYPE_DESKTOP';
    _WM_WINDOW_TYPE_DOCK   : Result := '_NET_WM_WINDOW_TYPE_DOCK';
    _WM_WINDOW_TYPE_TOOLBAR: Result := '_NET_WM_WINDOW_TYPE_TOOLBAR';
    _WM_WINDOW_TYPE_MENU   : Result := '_NET_WM_WINDOW_TYPE_MENU';
    _WM_WINDOW_TYPE_UTILITY: Result := '_NET_WM_WINDOW_TYPE_UTILITY';
    _WM_WINDOW_TYPE_SPLASH : Result := '_NET_WM_WINDOW_TYPE_SPLASH';
    _WM_WINDOW_TYPE_DIALOG : Result := '_NET_WM_WINDOW_TYPE_DIALOG';
    _WM_WINDOW_TYPE_NORMAL : Result := '_NET_WM_WINDOW_TYPE_NORMAL';
    _WM_STATE              : Result := '_NET_WM_STATE';
    _WM_STATE_MODAL        : Result := '_NET_WM_STATE_MODAL';
    _WM_STATE_STICKY       : Result := '_NET_WM_STATE_STICKY';
    _WM_STATE_MAXIMIZED_VERT:Result := '_NET_WM_STATE_MAXIMIZED_VERT';
    _WM_STATE_MAXIMIZED_HORZ:Result := '_NET_WM_STATE_MAXIMIZED_HORZ';
    _WM_STATE_SHADED       : Result := '_NET_WM_STATE_SHADED';
    _WM_STATE_SKIP_TASKBAR : Result := '_NET_WM_STATE_SKIP_TASKBAR';
    _WM_STATE_SKIP_PAGER   : Result := '_NET_WM_STATE_SKIP_PAGER';
    _WM_STATE_HIDDEN       : Result := '_NET_WM_STATE_HIDDEN';
    _WM_STATE_FULLSCREEN   : Result := '_NET_WM_STATE_FULLSCREEN';
    _WM_STATE_ABOVE        : Result := '_NET_WM_STATE_ABOVE';
    _WM_STATE_BELOW        : Result := '_NET_WM_STATE_BELOW';
    _WM_STATE_DEMANDS_ATTENTION: Result := '_NET_WM_STATE_DEMANDS_ATTENTION';
    _WM_ALLOWED_ACTIONS    : Result := '_NET_WM_ALLOWED_ACTIONS';
    _WM_ACTION_MOVE        : Result := '_NET_WM_ACTION_MOVE';
    _WM_ACTION_RESIZE      : Result := '_NET_WM_ACTION_RESIZE';
    _WM_ACTION_MINIMIZE    : Result := '_NET_WM_ACTION_MINIMIZE';
    _WM_ACTION_SHADE       : Result := '_NET_WM_ACTION_SHADE';
    _WM_ACTION_STICK       : Result := '_NET_WM_ACTION_STICK';
    _WM_ACTION_MAXIMIZE_HORZ:Result := '_NET_WM_ACTION_MAXIMIZE_HORZ';
    _WM_ACTION_MAXIMIZE_VERT:Result := '_NET_WM_ACTION_MAXIMIZE_VERT';
    _WM_ACTION_FULLSCREEN  : Result := '_NET_WM_ACTION_FULLSCREEN';
    _WM_ACTION_CHANGE_DESKTOP:Result:= '_NET_WM_ACTION_CHANGE_DESKTOP';
    _WM_ACTION_CLOSE       : Result := '_NET_WM_ACTION_CLOSE';
    _WM_STRUT              : Result := '_NET_WM_STRUT';
    _WM_STRUT_PARTIAL      : Result := '_NET_WM_STRUT_PARTIAL';
    _WM_ICON_GEOMETRY      : Result := '_NET_WM_ICON_GEOMETRY';
    _WM_ICON               : Result := '_NET_WM_ICON';
    _WM_PID                : Result := '_NET_WM_PID';
    _WM_HANDLED_ICONS      : Result := '_NET_WM_HANDLED_ICONS';
    _WM_USER_TIME          : Result := '_NET_WM_USER_TIME';
    _FRAME_EXTENTS         : Result := '_NET_FRAME_EXTENTS';
    _WM_PING               : Result := '_NET_WM_PING';
    _WM_SYNC_REQUEST       : Result := '_NET_WM_SYNC_REQUEST';
  end;
end;

end.

