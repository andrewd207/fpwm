unit XRootWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, X, XLib, XAtom, XFrames, ctypes, BaseWM, NetAtoms, XAtoms;
  
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
    function AddNewClient(AWindow: TWindow; AX, AY, AWidth, AHeight: Integer; AOverrideDirect: TBool): TXFrame;
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
  Atoms: array [0..10] of TAtom;
begin
  DeskViewPort[0] := 0;
  DeskViewPort[1] := 0;
  
  DeskWorkArea[0] := 0; // X
  DeskWorkArea[1] := 0; // Y
  DeskWorkArea[2] := Width;
  DeskWorkArea[3] := Height;
  
  Attr.override_redirect := TBool(True);
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
    WindowChangeProperty(Self.RootWindow, _NET[_CLIENT_LIST],
           XA_WINDOW, 32, PropModeReplace, nil, 0);
    WindowChangeProperty(Self.RootWindow, _NET[_CLIENT_LIST_STACKING],
          XA_WINDOW, 32, PropModeReplace, nil, 0);
    WindowChangeProperty(Self.RootWindow, _NET[_NUMBER_OF_DESKTOPS],
          XA_CARDINAL, 32, PropModeReplace, @DeskCount, 1);
    WindowChangeProperty(Self.RootWindow, _NET[_DESKTOP_GEOMETRY],
          XA_CARDINAL, 32, PropModeReplace, @DeskGeom, 2);
    WindowChangeProperty(Self.RootWindow, _NET[_DESKTOP_VIEWPORT],
          XA_CARDINAL, 32, PropModeReplace, @DeskViewPort, 2);
    WindowChangeProperty(Self.RootWindow, _NET[_CURRENT_DESKTOP],
          XA_CARDINAL, 32, PropModeReplace, @DeskCurrent, 1);
    WindowChangeProperty(Self.RootWindow, _NET[_ACTIVE_WINDOW],
          XA_WINDOW, 32, PropModeReplace, @DeskActive, 1);
    WindowChangeProperty(Self.RootWindow, _NET[_WORKAREA],
          XA_CARDINAL, 32, PropModeReplace, @DeskWorkArea, 4);
    WindowChangeProperty(Self.RootWindow, _NET[_SUPPORTING_WM_CHECK],
          XA_WINDOW, 32, PropModeReplace, @fSupportWindow, 1);

    // Set the atoms on the support window
    WindowChangeProperty(Self.fSupportWindow, _NET[_SUPPORTING_WM_CHECK],
          XA_WINDOW, 32, PropModeReplace, @fSupportWindow, 1);
    WindowChangeProperty(fSupportWindow, _NET[_WM_NAME],
          UTF8_STRING, 8, PropModeReplace, PChar('fpwm'), 7);

    Atoms[0]  := _NET[_CLIENT_LIST];
    Atoms[1]  := _NET[_CLIENT_LIST_STACKING];
    Atoms[2]  := _NET[_NUMBER_OF_DESKTOPS];
    Atoms[3]  := _NET[_DESKTOP_GEOMETRY];
    Atoms[4]  := _NET[_DESKTOP_VIEWPORT];
    Atoms[5]  := _NET[_CURRENT_DESKTOP];
    Atoms[6]  := _NET[_ACTIVE_WINDOW];
    Atoms[7]  := _NET[_WORKAREA];
    Atoms[8]  := _NET[_SUPPORTING_WM_CHECK];
    Atoms[9]  := _NET[_WM_NAME];
    Atoms[10] := _NET[_CLOSE_WINDOW];

    // Now mark that we support _NET atoms
    WindowChangeProperty(Self.RootWindow, _NET[_SUPPORTED], XA_ATOM, 32,
          PropModeReplace, @Atoms, 11);
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
    WindowDeleteProperty(RootWindow, _NET[_SUPPORTED]);
    WindowDeleteProperty(RootWindow, _NET[_CLIENT_LIST]);
    WindowDeleteProperty(RootWindow, _NET[_CLIENT_LIST_STACKING]);
    WindowDeleteProperty(RootWindow, _NET[_NUMBER_OF_DESKTOPS]);
    WindowDeleteProperty(RootWindow, _NET[_DESKTOP_GEOMETRY]);
    WindowDeleteProperty(RootWindow, _NET[_DESKTOP_VIEWPORT]);
    WindowDeleteProperty(RootWindow, _NET[_CURRENT_DESKTOP]);
    WindowDeleteProperty(RootWindow, _NET[_ACTIVE_WINDOW]);
    WindowDeleteProperty(RootWindow, _NET[_WORKAREA]);
    WindowDeleteProperty(RootWindow, _NET[_SUPPORTING_WM_CHECK]);
    WindowDeleteProperty(fSupportWindow, _NET[_SUPPORTING_WM_CHECK]);
    WindowDeleteProperty(fSupportWindow, _NET[_WM_NAME]);

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
  UnSetHints;
  fFrames.Free;
  inherited Destroy;
end;

function TWMRootWindow.AddNewClient(AWindow: TWindow; AX, AY, AWidth, AHeight: Integer; AOverrideDirect: TBool): TXFrame;
begin
  WriteLn('Maybe Creating New Window Frame for; ', AWindow, 'X =' , Ax, 'Y = ', Ay, ' Width =', AWidth,' Height = ', AHeight);
  Result := TXWM(fOwner).CreateNewWindowFrame(Self, fScreen, AWindow, AX, AY, AWidth, AHeight, AOverrideDirect);
  if Result <> nil then Frames.Add(Result);
  if Result = nil then WriteLn('Didn''t create Frame');
end;

procedure TWMRootWindow.GrabExistingWindows;
var
  I: Integer;
  WindowCount: cuint;
  WindowsRoot, WindowsParent: TWindow;
  ChildrenList: PWindow;
  WindowAttributes: TXWindowAttributes;
  Frame: TXFrame;
 WM: TXWM;
begin
  WM := TXWM(Owner);
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
      //WriteLn('Possibly mapping window');
      XGetWindowAttributes(TXWM(fOwner).Display, ChildrenList[I], @WindowAttributes);
      // override_redirect are windows like menus or hints that appear with no border
      // we don't manage them
      if  (WindowAttributes.override_redirect = TBool(False))
      and (WindowAttributes.map_state = IsViewable)
      then
      begin
        Frame := AddNewClient(ChildrenList[I],
                          WindowAttributes.x,
                          WindowAttributes.y,
                          WindowAttributes.width,
                          WindowAttributes.height,
                          WindowAttributes.override_redirect);
        if Frame <> nil then
          Frame.MapWindow;
      end;
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
  //WriteLn('Window Count = ',Count);
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


