unit XFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, BaseWM, X, XLib, XUtil, XAtom, XAtoms, NetAtoms, ctypes;
  
type

  TXWindowState = (xwsNormal);

  TSize = record
    Width: Integer;
    Height: Integer;
  end;

  TXFrameInitialProp = (xfpSize, xfpPosition, xfpTitle, xfpResized, xfpMoved);
  TXFrameInitialProps = set of TXFrameInitialProp;
  
  { TXFrame }

  TXFrame = class(TBaseFrame)
  private
    fClientWindow: TWindow;

    fFrameBottomHeight: Integer;
    fFrameHeight: Integer;
    fFrameLeftWidth: Integer;
    fFrameRightWidth: Integer;
    fFrameTopHeight: Integer;
    fFrameWidth: Integer;
    fFrameWindow: TWindow;
    FClientWidth,
    FClientHeight: Integer;
    FClientLeft,
    FCLientTop: Integer;

    FMaxSize: TSize;
    FMinSize: TSize;
    fPID: Integer;
    procedure SetMaxSize ( const AValue: TSize ) ;
    procedure SetMinSize ( const AValue: TSize ) ;
    //fPosition
    //fSize
  protected
    Display: PXDisplay;
    RootWindow: TWindow;
    FFrameInitialProps: TXFrameInitialProps;
    procedure SetCaption(const AValue: String); override;
    procedure SetFrameBottomHeight(const AValue: Integer); virtual;
    procedure SetFrameHeight(const AValue: Integer); virtual;
    procedure SetFrameLeftWidth(const AValue: Integer); virtual;
    procedure SetFrameRightWidth(const AValue: Integer); virtual;
    procedure SetFrameTopHeight(const AValue: Integer); virtual;
    procedure SetFrameWidth(const AValue: Integer); virtual;

    procedure CheckInitialProps;
  public
    constructor Create(AOwner: TBaseWindowManager; AClientWindow: TWindow; AFrameWindow: TWindow; ARootWindow: TWindow; AX,AY,AW,AH: Integer); virtual;
    destructor Destroy; override;
    // procedures to handle windows
    procedure _ReadWMNormalHints; virtual;
    procedure UpdateWindowBoundsFromWindow; virtual;
    procedure BringWindowToFront; virtual;
    procedure FocusWindow; virtual;
    procedure MapWindow; virtual;
    procedure UnMapWindow; virtual;
    procedure MoveWindow(APosition: TPoint); virtual;
    procedure ResizeWindow(var AWidth, AHeight: Integer); virtual;
    function PaintWindowFrame: Boolean; virtual;
    function  CloseWindowNice: Boolean; virtual;
    procedure CloseWindowDirty; virtual;
    procedure SendClientFakeMoveEvent(AX, AY: Integer);
    // methods responding to TXEvents
    function ConfigureWindowEv(AConfigureEv: PXConfigureRequestEvent): Boolean; virtual;
    function PropertyChangeEv(const APropertyEv: TXPropertyEvent): Boolean; virtual;

    // methods responding to ClientMessages
    function HandleClientMessage(const AClientEv: TXClientMessageEvent): Boolean; virtual;
    procedure NetWMMoveResize(XOffset, YOffset: Integer; Direction: TNetMoveResizeSource; Button: Integer; FromApp: Boolean); virtual;
    procedure NetCloseWindow(ATimeStamp: Integer; FromApp: Boolean); virtual;


    property ClientWindow: TWindow read fClientWindow;
    property FrameWindow: TWindow read fFrameWindow;
    property FrameTopHeight: Integer read fFrameTopHeight write SetFrameTopHeight;
    property FrameBottomHeight: Integer read fFrameBottomHeight write SetFrameBottomHeight;
    property FrameLeftWidth: Integer read fFrameLeftWidth write SetFrameLeftWidth;
    property FrameRightWidth: Integer read fFrameRightWidth write SetFrameRightWidth;
    property FrameWidth: Integer read fFrameWidth write SetFrameWidth;
    property FrameHeight: Integer read fFrameHeight write SetFrameHeight;
    property PID: Integer read fPID write fPID;

    property MinSize: TSize read FMinSize write SetMinSize;
    property MaxSize: TSize read FMaxSize write SetMaxSize;
    property ClientHeight: Integer read FClientHeight write FClientHeight;
    property ClientWidth: Integer read FClientWidth write FClientWidth;
    property ClientTop: Integer read FClientTop write FCLientTop;
    property ClientLeft: Integer read FClientLeft write FClientLeft;

  end;
  
  { TXFrameList }

  TXFrameList = class(TList)
  private
    function GetFrame(AIndex: Integer): TXFrame;
    procedure SetFrame(AIndex: Integer; const AValue: TXFrame);
  public
    procedure Clear; override;
    property Frame[AIndex: Integer]: TXFrame read GetFrame write SetFrame;
    
  end;

  function Size(Width, Height: Integer): TSize;
  function Max(A,B: Integer): Integer;
  function Min(A,B: Integer): Integer;

implementation
uses XWM;

function Size(Width, Height: Integer): TSize;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

function Min(A,B: Integer): Integer;
begin
  if A < B then Result := A
  else Result := B;
end;

function Max(A,B: Integer): Integer;
begin
  if A > B then Result := A
  else Result := B;
end;

{ TXFrameList }

function TXFrameList.GetFrame(AIndex: Integer): TXFrame;
begin
  Result := TXFrame(Items[AIndex]);
end;

procedure TXFrameList.SetFrame(AIndex: Integer; const AValue: TXFrame);
begin
  Items[AIndex] := AValue;
end;

procedure TXFrameList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count-1 do Frame[I].Free;
  inherited Clear;
end;

{ TXFrame }

procedure TXFrame.SetMinSize ( const AValue: TSize ) ;
begin
  if (FMinSize.Width = AValue.Width) and (FMinSize.Height = AValue.Height) then exit;
  FMinSize := AValue;
end;

procedure TXFrame.SetMaxSize ( const AValue: TSize ) ;
begin
  if (FMaxSize.Width = AValue.Width) and (FMaxSize.Height = AValue.Height) then exit;
  FMaxSize := AValue;
end;

procedure TXFrame.SetCaption(const AValue: String);
begin
  Inherited SetCaption(AValue);
  // trigger repaint?
end;

procedure TXFrame.SetFrameBottomHeight(const AValue: Integer);
begin
  if fFrameBottomHeight=AValue then exit;
  fFrameBottomHeight:=AValue;
end;

procedure TXFrame.SetFrameHeight(const AValue: Integer);
begin
  if fFrameHeight=AValue then exit;
  fFrameHeight:=AValue;
end;

procedure TXFrame.SetFrameLeftWidth(const AValue: Integer);
begin
  if fFrameLeftWidth=AValue then exit;
  fFrameLeftWidth:=AValue;
end;

procedure TXFrame.SetFrameRightWidth(const AValue: Integer);
begin
  if fFrameRightWidth=AValue then exit;
  fFrameRightWidth:=AValue;
end;

procedure TXFrame.SetFrameTopHeight(const AValue: Integer);
begin
  if fFrameTopHeight=AValue then exit;
  fFrameTopHeight:=AValue;
end;

procedure TXFrame.SetFrameWidth(const AValue: Integer);
begin
  if fFrameWidth=AValue then exit;
  fFrameWidth:=AValue;
end;

procedure TXFrame.CheckInitialProps;
var
  WM: TXWindowManager absolute owner;
begin
  if xfpTitle in FFrameInitialProps = False then
  begin
    Caption := WM.WindowGetTitle(ClientWindow);
    Include(FFrameInitialProps, xfpTitle);
  end;
  if (xfpSize in FFrameInitialProps = False)
  or (xfpPosition in FFrameInitialProps = False)
  then
    UpdateWindowBoundsFromWindow;// ReadWMNormalHints;

  if (xfpResized in FFrameInitialProps = False) then
    ResizeWindow(FClientWidth, FClientHeight);

  if (xfpMoved in FFrameInitialProps = False) then
    MoveWindow(Point(FClientLeft, FCLientTop));
  if not (xfpSize in FFrameInitialProps) then
    WriteLn('Warning: Size for Window not set!');


end;

constructor TXFrame.Create(AOwner: TBaseWindowManager; AClientWindow: TWindow;
  AFrameWindow: TWindow; ARootWindow: TWindow; AX,AY,AW,AH: Integer);
var
  ATitle, AUnmappedTitle: String;
  WM : TXWindowManager;
begin
  WM := TXWindowManager(AOwner);;
  Display := WM.Display;
  Owner := AOwner;
  fClientWindow := AClientWindow;
  fFrameWindow := AFrameWindow;
  //fFrameWidth := AW;
  //fFrameHeight := AH;
  fFrameLeftWidth := 7;
  fFrameRightWidth := 7;
  fFrameTopHeight := 25;
  fFrameBottomHeight := 7;
  FClientWidth := AW;
  FClientHeight := AH;
  FClientLeft := AX;
  FClientTop := AY;
  RootWindow := ARootWindow;

  WM.WindowSetStandardEventMask(AClientWindow);

  if AX+AY >= 0 then
    Include(FFrameInitialProps, xfpPosition);
  if AW+AH > 0 then
    Include(FFrameInitialProps, xfpSize);


end;

destructor TXFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TXFrame.BringWindowToFront;
begin
  XRaiseWindow(Display, FrameWindow);
end;

procedure TXFrame.FocusWindow;
begin
  XSetInputFocus(Display, ClientWindow, RevertToPointerRoot, CurrentTime);
end;

procedure TXFrame.MapWindow;
var
  Width, Height: Integer;
  WM: TXWindowManager absolute Owner;
begin
  // this makes it so if we crash the child windows will be remapped to the screen
  XAddToSaveSet(Display, ClientWindow);

  CheckInitialProps;

  if WM.WindowGetParent(ClientWindow) <> FrameWindow then
  begin
    WriteLn('ERROR: Mapping window that isn''t in it''s frame');
    XReparentWindow(Display, ClientWindow, FrameWindow,
                  FrameLeftWidth, FrameTopHeight);
  end;

  WriteLN('MapWindow');
  XMapWindow(Display, ClientWindow);

  XRaiseWindow(Display, FrameWindow);
  XMapRaised(Display, FrameWindow);

  FocusWindow;
end;

procedure TXFrame.UnMapWindow;
begin
  XUnmapWindow(Display, FrameWindow);
end;

procedure TXFrame.MoveWindow(APosition: TPoint);
begin
  XMoveWindow(Display, FrameWindow, APosition.X, APosition.Y);
  FClientLeft := APosition.X;
  FCLientTop := APosition.Y;
  // TODO Send an event to the client so it knows it moved per the ICCCM
end;

procedure TXFrame.ResizeWindow(var AWidth, AHeight: Integer);
begin
  if MinSize.Width >= 0 then
    AWidth := Max(MinSize.Width, AWidth);
  if MinSize.Height >= 0 then
    AHeight := Max(MinSize.Height, AHeight);

  FrameWidth := AWidth+FrameLeftWidth+FrameRightWidth;
  FrameHeight := AHeight+FrameTopHeight+FrameBottomHeight;
  // resize the client to fit in the frame window
  XResizeWindow(Display, ClientWindow, AWidth, AHeight);

  // resize the Frame to fit the client's wanted size
  // it's important to resize this window after
  XResizeWindow(Display, FrameWindow, FrameWidth, FrameHeight);

  Include(FFrameInitialProps, xfpResized);
  FClientHeight := AHeight;
  FClientWidth :=  AWidth;
end;

function TXFrame.PaintWindowFrame: Boolean;
begin
  // :)
  Result := False;
end;

function TXFrame.CloseWindowNice: Boolean;
var
  WM: TXWindowManager absolute Owner;
begin
  Result := False;
  if WM.WindowSupportsProto(ClientWindow, WM_DELETE_WINDOW) then begin
    TXWindowManager(Owner).SendXSimpleMessage(ClientWindow, WM_PROTOCOLS, WM_DELETE_WINDOW);
    Result := True;
  end;
end;

procedure TXFrame.CloseWindowDirty;
begin
  // not very nice is it? >:D
  XKillClient(Display, ClientWindow);
end;

procedure TXFrame.SendClientFakeMoveEvent ( AX, AY: Integer ) ;
var
  WM: TXWindowManager absolute Owner;
  Msg: TXEvent;
begin
  msg.xconfigure._type := ConfigureNotify;
  msg.xconfigure.display := Display;
  msg.xconfigure.window := ClientWindow;
  msg.xconfigure.x := AX;
  msg.xconfigure.y := AY;
  msg.xconfigure.width := FClientWidth;
  msg.xconfigure.height := FClientHeight;
  msg.xconfigure.border_width := 0;
  msg.xconfigure.above := None;
  msg.xconfigure.override_redirect := 0;

  WM.SendXMessage(ClientWindow, @Msg, 0);
end;

function TXFrame.ConfigureWindowEv(AConfigureEv: PXConfigureRequestEvent): Boolean;
begin
  WriteLn('Configure');
  Result := False;
  if AConfigureEv^.window = FrameWindow then
    Exit;
  Result := True;
  // Move to the desired position and set the frame size, also the client will be resized
  if AConfigureEv^.value_mask and (CWY or CWX) > 0 then
    MoveWindow(Point(AConfigureEv^.x, AConfigureEv^.y));
  if AConfigureEv^.value_mask and (CWWidth or CWHeight) > 0 then
    ResizeWindow(AConfigureEv^.width, AConfigureEv^.height);
end;

function TXFrame.PropertyChangeEv(const APropertyEv: TXPropertyEvent): Boolean;
var
  Event: TXEvent;
  WM: TXWindowManager absolute Owner;
begin
  Result := True;//False;
  if APropertyEv.window = FrameWindow then
    Exit; //==>

  if ((APropertyEv.atom = WM._NET[_WM_NAME])
      or (APropertyEv.atom = XA_WM_NAME)
      or (APropertyEv.atom = WM._NET[_WM_ICON_NAME])
      or (APropertyEv.atom = XA_WM_ICON_NAME))
  and (APropertyEv.state <> PropertyDelete)
  then begin
    try
      Caption := WM.WindowGetTitle(APropertyEv.window);
      // We can get property notify events for windows that were being destroyed an now don't exist!!
    except end;
  end
  else if APropertyEv.atom = WM._NET[_WM_PID] then begin
    PID := WM.WindowGetPID(ClientWindow);
  end
  else if APropertyEv.atom = XA_WM_NORMAL_HINTS then begin
    _ReadWMNormalHints;
  end
  else begin
    Writeln('Got Unhandled property notify event: ', XGetAtomName(Display, APropertyEv.atom));
  end;
  Result := True;
end;

function TXFrame.HandleClientMessage(const AClientEv: TXClientMessageEvent): Boolean;
var
Atom: TAtom;
WM: TXWindowManager absolute Owner;
begin
  //Return False to not eat the event
  Result := False;
  if AClientEv.window = FrameWindow then
    Exit;
  Atom := AClientEv.message_type;
  if Atom = WM._NET[_MOVERESIZE_WINDOW] then
  begin
    WriteLn('_NET[_MOVERESIZE_WINDOW]');
  end
  else if Atom = WM._NET[_WM_MOVERESIZE] then
  begin
    NetWMMoveResize(
              AClientEv.data.l[0], // x_root
              AClientEv.data.l[1], // y_root
              AClientEv.data.l[2], // direction see
              AClientEv.data.l[3], // button
              AClientEv.data.l[4]=1);// source (1 for regular apps 2 for pagers or other special apps)
    Result := True;
  end
  else if Atom = WM._NET[_CLOSE_WINDOW] then
  begin
    WriteLn('_NET[_CLOSE_WINDOW]');
  end
  else if Atom = WM._NET[_RESTACK_WINDOW] then
  begin
  end
  else if Atom = WM._NET[_REQUEST_FRAME_EXTENTS] then
  begin
  end
  else WriteLn('Unhandled Client Message: ', XGetAtomName(Display, Atom));
end;

procedure TXFrame.NetWMMoveResize ( XOffset, YOffset: Integer;
  Direction: TNetMoveResizeSource; Button: Integer; FromApp: Boolean ) ;
begin
  // TODO Resize window
  // we can assume that the mouse is down, so we have to track the mouse
  // and either draw a rect to what the window will be resized to or actually
  // resize the window as we move the mouse

  // XOffset and YOffset is the mouse position inside the window
  // For Direction see the _NET_WM_MOVERESIZE consts in NetAtoms
end;

procedure TXFrame.NetCloseWindow ( ATimeStamp: Integer; FromApp: Boolean ) ;
begin
  // if WM_DELETE_WINDOW is in the windows supported protocols we should send it that
  // it will either close or not if the client doesn't want to.
  // if it doesn't support the WM_DELETE_WINDOW protocol we send XCLientKill
  if not CloseWindowNice then
    CloseWindowDirty
  else begin
    // CloseWindowNice worked but check in a few seconds if it responds to a ping
    // Add Timer to check on status
    // we can usw _NET_WM_PING if the window supports it to see if the
    // application is not responding then after a timeout
  end;
end;

procedure TXFrame._ReadWMNormalHints;
var
  SizeHints: TXSizeHints;
  SuppliedReturn: clong;
  AWidth: LongInt;
  AHeight: LongInt;

begin exit;//
  SuppliedReturn := $FFFFFFFF;
  if (XGetWMNormalHints(Display, ClientWindow, @SizeHints, @SuppliedReturn) <>0)
  or (XGetWMSizeHints(Display, ClientWindow,@SizeHints, @SuppliedReturn, XA_WM_SIZE_HINTS)<>0)
  then
  begin
    WriteLn('Got SizeHints');
    if SuppliedReturn and PBaseSize <> 0 then
    begin
      AWidth :=  SizeHints.base_width;
      AHeight := SizeHints.base_height;



      Include(FFrameInitialProps, xfpSize);
      Include(FFrameInitialProps, xfpPosition);
      WriteLN('BaseSize = ', AWidth,':', AHeight);
      FClientHeight := AHeight;
      FClientWidth := AWidth;
    end;
    {else} if SuppliedReturn and (PSize or USSize) <> 0 then //last resort since this is deprecated
    begin
      AWidth :=  SizeHints.width;
      AHeight := SizeHints.height;
    end; //}
    ResizeWindow(AWidth, AHeight);
    if SuppliedReturn and PMinSize <> 0 then
    begin
      MinSize := Size(SizeHints.min_width, SizeHints.min_height);
    end;

  end
  else
    WriteLn('Did not get SizeHints');
end;

procedure TXFrame.UpdateWindowBoundsFromWindow;
var
  GetAttr: TXWindowAttributes;
  X,Y: Integer;
begin
  if XGetWindowAttributes(Display, ClientWindow, @GetAttr)= 0 then
  begin
    FClientWidth := GetAttr.width;
    FClientHeight := GetAttr.height;
    X := GetAttr.x;
    Y := GetAttr.y;
    ResizeWindow(FClientWidth, FClientHeight);
    MoveWindow(Point(X, Y));
    Include(FFrameInitialProps, xfpSize);
    Include(FFrameInitialProps, xfpPosition);
  end;

end;



end.

