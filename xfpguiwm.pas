unit XfpGUIWM;

{$mode objfpc}{$H+}

interface

uses
  Classes, BaseWM, X, XLib, XUtil, XAtom, ctypes, XWM, XFrames, XRootWindow,
  fpGui, GfxBase, Gfx_X11, NetAtoms;
  
type

  { TfpGUIWindowManager }

  TfpGUIWindowManager = class(TXWindowManager)
  private
    App: TApplication;
    fGfxDisplay: TXDisplay;
    procedure CreateDisplay(ADisplayName: String); override;
    function CreateXWindow(ARootWindow: TWMRootWindow; const AScreen: PScreen; Geometry: TRect): TWindow;
  public
    function CreateNewWindowFrame(Sender: TWMRootWindow; const AScreen: PScreen; const AChild: TWindow): TXFrame; override;
    procedure DestroyWindowFrame(var AWindow: TXFrame); override;
    function PaintWindowFrame(AFrame: TXFrame): Boolean; override;
    // procedure for client events handling
    procedure ResizeWindow(const AWindow: TXFrame; AWidth, AHeight: Integer); override;
    procedure NetWMMoveResize(const AWindow: TXFrame; XOffset, YOffset: Integer; Direction: TNetMoveResizeSource; Button: Integer; FromApp: Boolean); override;
    procedure NetCloseWindow(const AWindow: TXFrame; ATimeStamp: Integer; FromApp: Boolean); override;
    property GfxDisplay: TXDisplay read fGfxDisplay;

  end;

implementation

uses XfpGUIFrame;

{ TfpGUIWindowManager }

procedure TfpGUIWindowManager.CreateDisplay(ADisplayName: String);
begin
  fGfxDisplay := TXDisplay(Application.Display);
  fGfxDisplay.EventFilter := @XEventCB;
  Display := fGfxDisplay.Handle;
  fGfxDisplay.DisplayName := ADisplayName;
end;


// IMPORTANT!! The Width and Height of the window must include the border size
function TfpGUIWindowManager.CreateXWindow(ARootWindow: TWMRootWindow; const AScreen: PScreen; Geometry: TRect): TWindow;
var
  FrameAttributes : TXSetWindowAttributes;

begin
  FrameAttributes.border_pixel := BlackPixelOfScreen(AScreen);
  // we wouldn't want our frame to have another frame made for it would we :)
  FrameAttributes.override_redirect := True;
  FrameAttributes.event_mask := WindowManagerEventMask or ExposureMask;

  Result := XCreateWindow(Display, ARootWindow.RootWindow,
                             Geometry.Left, Geometry.Top,
                             Geometry.Right-Geometry.Left, Geometry.Bottom-Geometry.Left,
                             0,
                             DefaultDepthOfScreen(AScreen), CopyFromParent,
                             DefaultVisualOfScreen(AScreen),
                             CWOverrideRedirect or CWBorderPixel or CWEventMask,
                             @FrameAttributes);

end;

function TfpGUIWindowManager.CreateNewWindowFrame(Sender: TWMRootWindow; const AScreen: PScreen;
  const AChild: TWindow): TXFrame;
var
  FrameWindow: TWindow;
  Colormap: QWord;
  Frame: TfpGUIFrame;
  SizeHints: TXSizeHints;
  SuppliedReturn: clong;
  Geom: TRect;
  Font: PXFontStruct;
  Form: TForm;
  GetAttr: TXWindowAttributes;
  SetAttr: TXSetWindowAttributes;
  Top, Left, Width, Height: Integer;
begin
  Result := nil;
  // This hints stuff should be moved to its own procedure
  //Xgets);
  WriteLn('Creating window frame');
  if (XGetWMSizeHints(Display, AChild, @SizeHints, @SuppliedReturn, XA_WM_SIZE_HINTS)<>0)
  or (XGetSizeHints(Display, AChild, @SizeHints, XA_WM_SIZE_HINTS)<>0) then
  begin
    Top := 50; //TODO use wingravity
    Left := 50; //TODO use wingravity
    Width :=  SizeHints.base_width;
    Height := SizeHints.base_height;
  end
  else begin
    Top := 50;
    Left := 50;
    // A Default window size
    Width := 400;
    Height := 300;
  end;
  
  Frame := TfpGUIFrame.Create(Self, AChild, None);

  FrameWindow := Frame.FrameWindow;
  
  Frame.Form.SetBounds(Left, Top, Width, Height);

  XGetWindowAttributes(Display, FrameWindow, @GetAttr);
  SetAttr.event_mask := WindowManagerEventMask or GetAttr.all_event_masks;
  XChangeWindowAttributes(Display, FrameWindow, CWEventMask , @SetAttr);
  
  XSetWindowBorderWidth(Display, Frame.ClientWindow, 0);
  XReparentWindow(Display, Frame.ClientWindow, Frame.FrameWindow,
                  Frame.FrameLeftWidth, Frame.FrameTopHeight);

  Result := Frame;
  WriteLn('Done Creating window frame');
end;

procedure TfpGUIWindowManager.DestroyWindowFrame(var AWindow: TXFrame);
var
  FrameList: TXFrameList;
  Frame: TfpGUIFrame;
begin
  if AWindow = nil then exit;
  Frame := TfpGUIFrame(AWindow);
  // first remove ourselves from the Rootwindow list
  FrameList := RootWindows.FrameListFromWindow(Frame);
  if FrameList <> nil then begin
    FrameList.Remove(Frame);
  end;
  // now free our FrameWindow
  Frame.Form.Free;
  //XDestroyWindow(Display, Frame.FrameWindow);
  // free the stuff we created when we created the window
  Frame.Free;
  AWindow := nil;
end;

function TfpGUIWindowManager.PaintWindowFrame(AFrame: TXFrame): Boolean;
var
  Frame: TfpGUIFrame;
  ARect: TRect;
begin
  Result := False;
  // we'll let fpgui do the painting for us
end;


procedure TfpGUIWindowManager.ResizeWindow(const AWindow: TXFrame; AWidth,
  AHeight: Integer);
var
  Frame: TfpGUIFrame;
  Width, Height: Integer;
  Ev : TXConfigureEvent;
begin
  {Width := AWidth + Frame.FrameLeftWidth + Frame.FrameRightWidth;
  Height := AHeight + Frame.FrameTopHeight + Frame.FrameBottomHeight;
  //Ev := GetCurrentEvent^.xconfigure;
  Ev._type := ConfigureNotify;
  Ev.display := Display;
  Ev.window := AWindow.FrameWindow;
  Ev.width := Width;
  Ev.height := Height;

  Writeln('hello');
  //if Ev.window = Frame.FrameWindow then
    TXWindow(Frame.Form.Wnd).Dispatch(Ev);
    //Frame.Form.SetBounds(Frame.Form.BoundsRect.TopLeft, Size(Width, Height));}
  inherited;
end;

procedure TfpGUIWindowManager.NetWMMoveResize(const AWindow: TXFrame; XOffset,
  YOffset: Integer; Direction: TNetMoveResizeSource; Button: Integer; FromApp: Boolean);
begin
  // TODO Resize window
  // we can assume that the mouse is down, so we have to track the mouse
  // and either draw a rect to what the window will be resized to or actually
  // resize the window as we move the mouse
  
  // XOffset and YOffset is the mouse position inside the window
  // For Direction see the _NET_WM_MOVERESIZE consts in NetAtoms
end;

procedure TfpGUIWindowManager.NetCloseWindow(const AWindow: TXFrame;
  ATimeStamp: Integer; FromApp: Boolean);
begin
  // if WM_DELETE_WINDOW is in the windows supported protocols we should send it that
  // it will either close or not if the client doesn't want to.
  // if it doesn't support the WM_DELETE_WINDOW protocol we send XCLientKill
  if not CloseWindowNice(AWindow.ClientWindow) then
   CloseWindowDirty(AWindow.ClientWindow)
  else begin
    // CloseWindowNice worked but check in a few seconds if it responds to a ping
    // Add Timer to check on status
    // we can usw _NET_WM_PING if the window supports it to see if the
    // application is not responding then after a timeout
  end;

end;

end.

