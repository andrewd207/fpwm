unit XWM;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, X, Xlib, XAtom, ctypes, BaseWM, XRootWindow, XFrames;
  
type

  { TXWindowManager }
  
  // This class will do the 'dirty' work of managing the window and will have
  // abstract methods for descendants to implement. i.e. TfpGUIWindowManager or so
  // all painting will be done in a descendant
  TXWindowManager = class(TBaseWindowManager)
  private
    fQuit: Boolean;
    fDisplay: PXDisplay;
    fRootWindowList: TWMRootWindowList;
    fLastErrorHandler: TXErrorHandler;
    function GrabRootWindows: Boolean;
  public
    constructor Create(const ADisplayName: String); virtual;
    destructor Destroy; override;
    function CreateNewWindowFrame(Sender: TWMRootWindow; const AScreen: PScreen; const AChild: TWindow): TXFrame; virtual; abstract;
    function WindowToFrame(const AWindow: TWindow): TXFrame;
    procedure CreateDisplay(ADisplayName: String); virtual;
    procedure InitWM(QueryWindows: Boolean = True); override;
    procedure MainLoop; override;
    procedure MapWindow(const AWindow: TXFrame); virtual;
    procedure PaintWindowFrame(AFrame: TXFrame); virtual; abstract;
    property Display: PXDisplay read fDisplay write fDisplay;
    property RootWindows: TWMRootWindowList read fRootWindowList;
  end;
  
const WindowManagerEventMask = SubstructureRedirectMask or SubstructureNotifyMask or
                                          ColormapChangeMask or EnterWindowMask;

implementation


// Creates an exception
function X11_Error(Display :PXDisplay; ErrEvent :PXErrorEvent):cint; cdecl;
var
 ErrorStr: String;
 e: Exception;
begin
  Result := 0;
  SetLength(ErrorStr, 1024);
  XGetErrorText(Display, ErrEvent^.error_code, @ErrorStr[1], 1024);
  e := Exception.Create(ErrorStr);
  raise E;
  e.Free;
end;

{ TXWindowManager }

{
  Returns True for success
}
function TXWindowManager.GrabRootWindows: Boolean;
var
  Attributes: TXSetWindowAttributes;
  I: Integer;
  AWindow: TWindow;
  ARootWindow: TWMRootWindow;
begin
  Result := False;
  for I := 0 to XScreenCount(fDisplay)-1 do begin
    AWindow := RootWindow(fDisplay, I);
    FillChar(Attributes, SizeOf(TXSetWindowAttributes), 0);
    // Set the event mask for the root window
    Attributes.event_mask := WindowManagerEventMask;
    try
      XChangeWindowAttributes(fDisplay, AWindow, CWEventMask, @Attributes);
      Result := True;
      ARootWindow := TWMRootWindow.Create(Self, XScreenOfDisplay(fDisplay, I), AWindow);
      fRootWindowList.Add(ARootWindow);
    except
      Result := False;
    end;
  end;
end;

procedure TXWindowManager.CreateDisplay(ADisplayName: String);
begin
  fDisplay := XOpenDisplay(PChar(ADisplayName));
end;

constructor TXWindowManager.Create(const ADisplayName: String);
begin
  fLastErrorHandler := XSetErrorHandler(@X11_Error);
  fRootWindowList := TWMRootWindowList.Create;
  CreateDisplay(ADisplayName);
end;

destructor TXWindowManager.Destroy;
begin
  fRootWindowList.Free;
  if Assigned(fDisplay) then XCloseDisplay(fDisplay);
  inherited Destroy;
end;


// finds our TXFrame from a TWindow
function TXWindowManager.WindowToFrame(const AWindow: TWindow): TXFrame;
var
  I: Integer;
  J: Integer;
  Frames: TXFrameList;
begin
  Result := nil;
  for I := 0 to RootWindows.Count-1 do begin
    Frames := RootWindows.Windows[I].Frames;
    for J := 0 to Frames.Count-1 do begin
      if (Frames.Frame[J].ClientWindow = AWindow)
      or (Frames.Frame[J].FrameWindow = AWindow)
      then Exit(Frames.Frame[J]);
    end;
  end;
end;

procedure TXWindowManager.InitWM(QueryWindows: Boolean = True);
var
  I: Integer;
begin
  if not GrabRootWindows then exit;

  // Look for windows which need to be managed and add them to the list;
  if QueryWindows then begin
    for I := 0 to fRootWindowList.Count-1 do
      fRootWindowList.Windows[I].GrabExistingWindows;
  end;
end;

procedure TXWindowManager.MainLoop;
var
  Event: TXEvent;
  Frame: TXFrame;
begin
  repeat
    WriteLn('Waiting For Event');
    XNextEvent(Display, @Event);
    WriteLn('GotEvent: ', Event.xany._type);
    case Event._type of
      ConfigureRequest:;
      MapRequest:
        begin
          Frame := RootWindows.Windows[0].AddNewClient(Event.xmaprequest.window);
          if Frame <> nil then MapWindow(Frame);
        end;

      MapNotify:;
      UnmapNotify:;
      CreateNotify:;
      DestroyNotify:
        begin
          Frame := WindowToFrame(Event.xdestroywindow.window);
          if (Frame <> nil) and (Frame.ClientWindow = Event.xany.window) then
             Frame.Free;
        end;
      Expose:
        begin
          Frame := WindowToFrame(Event.xexpose.window);
          if (Frame <> nil) and (Frame.FrameWindow = Event.xany.window) then
            PaintWindowFrame(Frame);
        end

    end;
    WriteLn('DoneEvent');
  until fQuit;
end;

procedure TXWindowManager.MapWindow(const AWindow: TXFrame);
var
 Width,
 Height: Integer;
begin
  XAddToSaveSet(Display, AWindow.ClientWindow);

  Width := AWindow.FrameWidth - AWindow.FrameLeftWidth - AWindow.FrameRightWidth;
  Height := AWindow.FrameHeight - AWindow.FrameTopHeight - AWindow.FrameBottomHeight;
  XMapWindow(Display, AWindow.ClientWindow);
  XResizeWindow(Display, AWindow.ClientWindow, Width, Height);
  XRaiseWindow(Display, AWindow.FrameWindow);
  XMapRaised(Display, AWindow.FrameWindow);
end;

end.

