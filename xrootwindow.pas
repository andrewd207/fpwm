unit XRootWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, X, XLib, XAtom, XFrames, ctypes, BaseWM, NetAtoms;
  
type

  { TWMRootWindow }

  TWMRootWindow = class(TObject)
  private
    fIsRealRoot: Boolean;
    fOwner: TBaseWindowManager;
    fFrames: TXFrameList;
    fScreen: PScreen;
    fRootWindow: TWindow;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetICCCMHints;
    procedure SetNETHints;
    procedure UnSetICCCMHints;
    procedure UnSetNETHints;
  protected
    fSupportWindow: TWindow;
  public
    constructor Create(AOwner: TBaseWindowManager; AScreen: PScreen; AWindow: TWindow);
    destructor Destroy; override;
    function AddNewClient(AWindow: TWindow): TXFrame;
    procedure GrabExistingWindows;
    procedure SetHints;
    procedure UnSetHints;
    property Frames: TXFrameList read fFrames;
    property IsRealRoot: Boolean read fIsRealRoot;
    property Owner: TBaseWindowManager read fOwner;
    property RootWindow: TWindow read fRootWindow;
    property Screen: PScreen read fScreen;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    
  end;
  
  { TWMRootWindowList }

  TWMRootWindowList = class(TList)
  private
    function GetRootWindow(AIndex: Integer): TWMRootWindow;
    procedure SetRootWindow(AIndex: Integer; const AValue: TWMRootWindow);
  public
    procedure Clear; override;
    function FrameListFromWindow(const AWindow: TXFrame): TXFrameList;
    function RootWindowFromXWindow(const ARootWindow: TWindow): TWMRootWindow;
    property Windows[AIndex: Integer]: TWMRootWindow read GetRootWindow write SetRootWindow;
  end;
   

implementation

uses XWM;

type
  // a hack to get around a recursive uses
  TXWM = class(TXWindowManager)
  end;

{ TWMRootWindow }

// this should initialize
procedure TWMRootWindow.SetICCCMHints;
begin
  // TODO Set mandatory ICCCM hints on the rootwindow
end;

function TWMRootWindow.GetHeight: Integer;
var
  Attr: TXWindowAttributes;
begin
  Result := 0;
  if IsRealRoot then Exit(XHeightOfScreen(Screen));
  
  if XGetWindowAttributes(TXWM(Owner).Display, RootWindow, @Attr) <> 0 then
    Result := Attr.height;
end;

function TWMRootWindow.GetWidth: Integer;
var
  Attr: TXWindowAttributes;
begin
  Result := 0;
  if IsRealRoot then Exit(XWidthOfScreen(Screen));
  
  if XGetWindowAttributes(TXWM(Owner).Display, RootWindow, @Attr) <> 0 then
    Result := Attr.width;
end;

procedure TWMRootWindow.SetNETHints;
var
  WM: TXWM;
  DeskCount: Cardinal   = 1;
  DeskCurrent: Cardinal = None;
  DeskActive: TWindow = None;
  DeskViewPort: array[0..1] of Cardinal;
  DeskGeom: array [0..1] of Integer;
  DeskWorkArea: array [0..3] of Integer;
  
  Attr: TXSetWindowAttributes;
begin
  DeskViewPort[0] := 0;
  DeskViewPort[1] := 0;
  
  DeskWorkArea[0] := 0; // X
  DeskWorkArea[1] := 0; // Y
  DeskWorkArea[2] := Width;
  DeskWorkArea[3] := Height;
  
  Attr.override_redirect := True;
  fSupportWindow := XCreateWindow(TXWM(Owner).Display, RootWindow,
                             0, 0,         // X, Y
                             1, 1,         // Width, Height
                             0,            // border width
                             0,            // Depth
                             InputOnly,    // class
                             nil,          // visual
                             CWOverrideRedirect, // mask
                             @Attr);

  WM := TXWM(Owner);
  exit;
  with WM do begin
    // Set the atoms on the RootWindow
    ChangeWindowProperty(Self.RootWindow, _NET[_CLIENT_LIST],
           XA_WINDOW, 32, PropModeReplace, nil, 0);
    ChangeWindowProperty(Self.RootWindow, _NET[_CLIENT_LIST_STACKING],
          XA_WINDOW, 32, PropModeReplace, nil, 0);
    ChangeWindowProperty(Self.RootWindow, _NET[_NUMBER_OF_DESKTOPS],
          XA_CARDINAL, 32, PropModeReplace, @DeskCount, 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_DESKTOP_GEOMETRY],
          XA_CARDINAL, 32, PropModeReplace, @DeskGeom, 2);
    ChangeWindowProperty(Self.RootWindow, _NET[_DESKTOP_VIEWPORT],
          XA_CARDINAL, 32, PropModeReplace, @DeskViewPort, 2);
    ChangeWindowProperty(Self.RootWindow, _NET[_CURRENT_DESKTOP],
          XA_CARDINAL, 32, PropModeReplace, @DeskCurrent, 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_ACTIVE_WINDOW],
          XA_WINDOW, 32, PropModeReplace, @DeskActive, 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_WORKAREA],
          XA_CARDINAL, 32, PropModeReplace, @DeskWorkArea, 4);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTING_WM_CHECK],
          XA_WINDOW, 32, PropModeReplace, @fSupportWindow, 1);

    // Set the atoms on the support window
    ChangeWindowProperty(Self.fSupportWindow, _NET[_SUPPORTING_WM_CHECK],
          XA_WINDOW, 32, PropModeReplace, @fSupportWindow, 1);
    ChangeWindowProperty(fSupportWindow, _NET[_WM_NAME],
          _NET[UTF8_STRING], 8, PropModeReplace, PChar('fpwm'), 7);

    // Now mark that we support _NET atoms
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_CLIENT_LIST], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_CLIENT_LIST_STACKING], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_NUMBER_OF_DESKTOPS], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_DESKTOP_GEOMETRY], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_DESKTOP_VIEWPORT], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_CURRENT_DESKTOP], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_CURRENT_DESKTOP], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_ACTIVE_WINDOW], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_WORKAREA], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_SUPPORTING_WM_CHECK], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_WM_NAME], 1);
    ChangeWindowProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeAppend, @_NET[_CLOSE_WINDOW], 1);
  end;
end;

procedure TWMRootWindow.UnSetICCCMHints;
begin

end;

procedure TWMRootWindow.UnSetNETHints;
var
  WM: TXWM;
begin
  WM := TXWM(Owner);
  with WM do begin
    DeleteWindowProperty(RootWindow, _NET[_SUPPORTED]);
    DeleteWindowProperty(RootWindow, _NET[_CLIENT_LIST]);
    DeleteWindowProperty(RootWindow, _NET[_CLIENT_LIST_STACKING]);
    DeleteWindowProperty(RootWindow, _NET[_NUMBER_OF_DESKTOPS]);
    DeleteWindowProperty(RootWindow, _NET[_DESKTOP_GEOMETRY]);
    DeleteWindowProperty(RootWindow, _NET[_DESKTOP_VIEWPORT]);
    DeleteWindowProperty(RootWindow, _NET[_CURRENT_DESKTOP]);
    DeleteWindowProperty(RootWindow, _NET[_ACTIVE_WINDOW]);
    DeleteWindowProperty(RootWindow, _NET[_WORKAREA]);
    DeleteWindowProperty(RootWindow, _NET[_SUPPORTING_WM_CHECK]);
    DeleteWindowProperty(fSupportWindow, _NET[_SUPPORTING_WM_CHECK]);
    DeleteWindowProperty(fSupportWindow, _NET[_WM_NAME]);

    XDestroyWindow(Display, fSupportWindow);
  end;
end;

constructor TWMRootWindow.Create(AOwner: TBaseWindowManager; AScreen: PScreen; AWindow: TWindow);
begin
  fOwner := AOwner;
  fRootWindow := AWindow;
  fScreen := AScreen;
  fFrames := TXFrameList.Create;

  SetHints;
end;

destructor TWMRootWindow.Destroy;
begin
  fFrames.Free;
  inherited Destroy;
end;

function TWMRootWindow.AddNewClient(AWindow: TWindow): TXFrame;
begin
  Result := TXWM(fOwner).CreateNewWindowFrame(Self, fScreen, AWindow);
  if Result <> nil then Frames.Add(Result);
end;

procedure TWMRootWindow.GrabExistingWindows;
var
  I: Integer;
  WindowCount: cuint;
  WindowsRoot, WindowsParent: TWindow;
  ChildrenList: PWindow;
  WindowAttributes: TXWindowAttributes;
begin
  if XQueryTree(TXWM(fOwner).Display, fRootWindow, @WindowsRoot, @WindowsParent,
            @ChildrenList, @WindowCount) <> 0 then
  begin
    // find out if we are a real root window or a mdi root
    if fRootWindow = WindowsRoot then
      fIsRealRoot := True
    else
      fIsRealRoot := False;
    // this finds out if the window want's to be managed or not.
    for I := 0 to WindowCount-1 do begin
      XGetWindowAttributes(TXWM(fOwner).Display, ChildrenList[I], @WindowAttributes);
      // override_redirect are windows like menus or hints that appear with no border
      // we don't manage them
      if  (WindowAttributes.override_redirect = False)
      and (WindowAttributes.map_state = IsViewable)
      then AddNewClient(ChildrenList[I]);
    end;
    XFree(ChildrenList);
  end;
end;

procedure TWMRootWindow.SetHints;
begin
  SetICCCMHints;
  SetNETHints;
  // and any hints specific to our windowmanager here
end;

procedure TWMRootWindow.UnSetHints;
begin
  UnSetNETHints;
  UnSetICCCMHints;
  // and then our hints
end;

{ TWMRootWindowList }

function TWMRootWindowList.GetRootWindow(AIndex: Integer): TWMRootWindow;
begin
  Result := TWMRootWindow(Items[AIndex]);
end;

procedure TWMRootWindowList.SetRootWindow(AIndex: Integer;
  const AValue: TWMRootWindow);
begin
  Items[AIndex] := AValue;
end;

procedure TWMRootWindowList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count-1 do Windows[I].Free;
  inherited Clear;
end;

function TWMRootWindowList.RootWindowFromXWindow(const ARootWindow: TWindow
  ): TWMRootWindow;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do begin
    if ARootWindow = Windows[I].RootWindow then Exit(Windows[I]);
  end;
end;

function TWMRootWindowList.FrameListFromWindow(const AWindow: TXFrame
  ): TXFrameList;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do begin
    if Windows[I].Frames.IndexOf(AWindow) > -1 then begin
      Result := Windows[I].Frames;
      Exit;
    end;
  end;
end;

end.


