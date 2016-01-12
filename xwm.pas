unit XWM;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, X, Xlib, XAtom, XUtil, ctypes, BaseWM, XRootWindow,
  XFrames, NETAtoms, XAtoms, fpg_base, fpg_main, cursorfont;
  
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
    fNetAtoms: TNetAtoms;
    fCurrentEvent: PXEvent;
    function GrabRootWindows: Boolean;
  public
    constructor Create(const ADisplayName: String); virtual;
    destructor Destroy; override;
    function WindowToFrame(const AWindow: TWindow): TXFrame;
    procedure CreateDisplay(ADisplayName: String); virtual;
    procedure InitWM(QueryWindows: Boolean = True); override;
    procedure MainLoop; override;

    // event related methods
    function XEventCB(const AEvent: TXEvent): Boolean; virtual;
    function GetCurrentEvent: PXEvent;

    // create and destroy frame windows
    function CreateNewWindowFrame(Sender: TWMRootWindow; const AScreen: PScreen; const AChild: TWindow; AX, AY, AWidth, AHeight: Integer; AOverrideDirect: TBool): TXFrame; virtual; abstract;
    procedure DestroyWindowFrame(var AWindow: TXFrame); virtual; abstract;

    // methods to send X messages
    procedure SendXSimpleMessage(const ADestWindow: TWindow; AMessage: TAtom; AValue: Integer);
    procedure SendXMessage(const ADestWindow: TWindow; AMessage: PXEvent; AMask: LongWord);
    procedure SendXClientMessage(const ADestWindow: TWindow; AType: TAtom);
    // manage window properties
    procedure WindowChangeProperty(AWindow: TWindow; AProperty: TAtom;
                AType: TAtom; AFormat: cint; AMode: cint; const AData: Pointer; Elements: cint);
    procedure WindowDeleteProperty(AWindow: TWindow; AProperty: TAtom);
    function WindowSupportsProto(AWindow: TWindow; AProto: TAtom): Boolean;
    function WindowGetTitle(const AWindow: TWindow): String;
    function WindowGetUserTime(const AWindow: TWindow): LongWord;
    function WindowGetPID(const AWindow: TWindow): Integer;
    procedure WindowSetStandardEventMask(const AClientWindow: TWindow); virtual;
    function  WindowGetParent(AWindow: TWindow): TWindow;
    // procedures to initialize the ICCCM and Extended WM hints
    procedure InitICCCMHints; virtual;
    procedure InitNETHints; virtual;
    procedure InitAtoms; virtual;
    property Display: PXDisplay read fDisplay write fDisplay;
    property _NET: TNetAtoms read fNetAtoms;
    property RootWindows: TWMRootWindowList read fRootWindowList;
  end;
  
const WindowManagerEventMask = SubstructureRedirectMask or SubstructureNotifyMask or
                                          ColormapChangeMask or EnterWindowMask;

implementation


// Creates an exception
function X11_Error(Display :PXDisplay; ErrEvent :PXErrorEvent):cint; cdecl;
var
 ErrorStr: String;
begin
  Result := 0;
  SetLength(ErrorStr, 1024);
  XGetErrorText(Display, ErrEvent^.error_code, @ErrorStr[1], 1024);
  WriteLn('<<<<>>>>>>>>>>> ', ErrorStr);
  //Raise Exception.Create(ErrorStr);
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
  xc: LongWord;
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
      xc := XCreateFontCursor(Display, XC_left_ptr);
      XDefineCursor(Display, AWindow, XC);
      XFreeCursor(Display, xc);
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
  InitAtoms;
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
  //Writeln('Looking for TXFrame for window: ', AWindow);
  Result := nil;
  for I := 0 to RootWindows.Count-1 do begin
    if Assigned(Result) then Break;
    Frames := RootWindows.Windows[I].Frames;
    for J := 0 to Frames.Count-1 do begin
      if (Frames.Frame[J].ClientWindow = AWindow)
      or (Frames.Frame[J].FrameWindow = AWindow)
      then begin
         Result := Frames.Frame[J];
         Break;
      end;
    end;
  end;
  //Writeln('Found TXFrame for : ', AWindow, ' = ', Assigned(Result));
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
begin
  fpgApplication.Run;
end;

function GetXEventName(Event: LongInt): String;
const
  EventNames: array[2..34] of String = (
    'KeyPress', 'KeyRelease', 'ButtonPress', 'ButtonRelease', 'MotionNotify',
    'EnterNotify', 'LeaveNotify', 'FocusIn', 'FocusOut', 'KeymapNotify',
    'Expose', 'GraphicsExpose', 'NoExpose', 'VisibilityNotify', 'CreateNotify',
    'DestroyNotify', 'UnmapNotify', 'MapNotify', 'MapRequest', 'ReparentNotify',
    'ConfigureNotify', 'ConfigureRequest', 'GravityNotify', 'ResizeRequest',
    'CirculateNotify', 'CirculateRequest', 'PropertyNotify', 'SelectionClear',
    'SelectionRequest', 'SelectionNotify', 'ColormapNotify', 'ClientMessage',
    'MappingNotify');
begin
  if (Event >= Low(EventNames)) and (Event <= High(EventNames)) then
    Result := EventNames[Event]
  else
    Result := '#' + IntToStr(Event);
end;

function TXWindowManager.XEventCB(const AEvent: TXEvent): Boolean;
var
  Frame: TXFrame;
  RootWindow: TWMRootWindow;
  Ev: TXConfigureEvent;
begin
  Result := False;
  //Return True to stop the event from being handled by the toolkit
  //WriteLn('BeginEvent: ', GetXEventName(AEvent.xany._type));
  fCurrentEvent := @AEvent;
  Frame := WindowToFrame(AEvent.xany.window);
  if (Frame <> nil) and (Frame.ClientWindow = AEvent.xany.window) then
  WriteLn('BeginEvent: ', GetXEventName(AEvent.xany._type));

  case AEvent._type of
    PropertyNotify:
      begin
        if Frame <> nil then
          Frame.PropertyChangeEv(AEvent.xproperty);
      end;

    ConfigureRequest:
      begin
        Frame := WindowToFrame(AEvent.xconfigurerequest.window);
        if Frame <> nil then
          Result := Frame.ConfigureWindowEv(@AEvent.xconfigurerequest);
      end;

    ConfigureNotify:
      begin
       {if Frame <> nil then
          if Frame.FrameWindow = AEvent.xconfigure.event then begin
            Ev := AEvent.xconfigure;
            Inc(Ev.width, 20);
            Inc(Ev.height, 40);
            XSendEvent(Display, Frame.FrameWindow, False, StructureNotifyMask, @Ev);
            Result := True;
          end;
       }
      end;
    MapRequest:
      begin

        Frame := WindowToFrame(AEvent.xmaprequest.window);
        if Frame <> nil then begin
          Frame.MapWindow;
          Result := True;
        end;
      end;
    MapNotify:
      begin

        Frame := WindowToFrame(AEvent.xmap.window);
        if Frame <> nil then begin
          Frame.MapWindow;
          Result := True;
        end;
      end;
    CreateNotify:
      begin
        Result := True;
        Frame := WindowToFrame(AEvent.xcreatewindow.window);
        RootWindow := RootWindows.RootWindowFromXWindow(AEvent.xcreatewindow.parent);
        if  (Frame = nil) and (RootWindow <> nil)
        then
          Frame := RootWindow.AddNewClient(AEvent.xcreatewindow.window,
                                           AEvent.xcreatewindow.x,
                                           AEvent.xcreatewindow.y,
                                           AEvent.xcreatewindow.width,
                                           AEvent.xcreatewindow.height,
                                           AEvent.xcreatewindow.override_redirect);
      end;
    ClientMessage:
      begin
        if Frame <> nil then begin
          Result := Frame.HandleClientMessage(AEvent.xclient);
        end;
        REsult := True;
      end;
    UnmapNotify:
      begin
        Frame := WindowToFrame(AEvent.xunmap.window);
        if (Frame <> nil) and (Frame.ClientWindow = AEvent.xunmap.window) then
        begin
          // do nothing
        end;
      end;
    //MapNotify:;
    //UnmapNotify:;
    //CreateNotify:;// do we need to do anything here?
    DestroyNotify:
      begin
        wRITElN('Destroy Notify');
        // only react to the destruction of client windows not of our frame windows
        Frame := WindowToFrame(AEvent.xdestroywindow.window);
        if (Frame <> nil) and (Frame.ClientWindow = AEvent.xdestroywindow.window) then begin
          DestroyWindowFrame(Frame);
          Result := True;
        end;
      end;
    Expose:
      begin
        if (Frame <> nil) and (Frame.FrameWindow = AEvent.xany.window) then begin
          Result := Frame.PaintWindowFrame;
        end;
      end;
    ButtonPress,
    ButtonRelease:
      begin
        {Frame := WindowToFrame(AEvent.xbutton.window);
        if Frame <> nil then
          Result := Frame.ClientWindow = AEvent.xbutton.window;
        if Frame.FrameWindow = Frame.ClientWindow then
            Result := True; // how is this possible?!}
      end;
    KeyPress,
    KeyRelease:
      begin
        WriteLn('Key Event');
        Frame := WindowToFrame(AEvent.xkey.window);
        if Frame <> nil then
        begin
          WriteLn('Frame  = ', Frame.FrameWindow);
          WriteLn('Client = ', Frame.ClientWindow);
          WriteLn('Event  = ', AEvent.xkey.window);
          Result := Frame.ClientWindow = AEvent.xkey.window;
          if Frame.FrameWindow = Frame.ClientWindow then
            Result := True; // how is this possible?!
        WriteLn('Drop Event = ', Result);
        end;
      end
    else
      begin
        //if AEvent._type <> MotionNotify then
       // WriteLn('Got Unhandled Event: ', GetXEventName(AEvent.xany._type));
        Result := False;
      end;
  end;
  //WriteLn('DoneEvent: ', GetXEventName(AEvent.xany._type));
  fCurrentEvent := nil;
  if (Result = False)
  and Assigned(Frame)
  and(AEvent.xany.window <> Frame.FrameWindow) then
    Result := True;


  //result := False
  //XSync(Display, false);
end;

function TXWindowManager.GetCurrentEvent: PXEvent;
begin
  Result := fCurrentEvent;
end;


procedure TXWindowManager.SendXSimpleMessage(const ADestWindow: TWindow;
  AMessage: TAtom; AValue: Integer);
var
  ClientMsg: TXClientMessageEvent;
begin
  FillChar(ClientMsg, sizeof(ClientMsg), #0);
  ClientMsg._type := ClientMessage;
  ClientMsg.window := ADestWindow;
  ClientMsg.message_type := AMessage;
  ClientMsg.format := 32;
  ClientMsg.data.l[0] := AValue;
  ClientMsg.data.l[1] := CurrentTime;
  SendXMessage(ADestWindow, @ClientMsg, 0);
end;

procedure TXWindowManager.SendXMessage(const ADestWindow: TWindow; AMessage: PXEvent; AMask: LongWord);
begin
  XSendEvent(Display, ADestWindow, False, AMask, AMessage);
end;

procedure TXWindowManager.SendXClientMessage(const ADestWindow: TWindow; AType: TAtom);
var
  Ev : TXClientMessageEvent;
begin
  Ev._type := ClientMessage;
  Ev.window := ADestWindow;
  Ev.message_type := AType;
  //Ev.data :=;
  //SendXmessage, AWindow, True);
  //XSendEvent(para1:PDisplay; para2:TWindow; para3:TBool; para4:clong; para5:PXEvent):TStatus;cdecl;external libX11;
end;

// AFormat is the size of the type of elements in bits. so a cardinal is 32 char is 8 etc
// Elements is how many items there are
// AMode is PropModeAppend, PropModePrepend, or PropModeReplace
procedure TXWindowManager.WindowChangeProperty(AWindow: TWindow; AProperty: TAtom;
  AType: TAtom; AFormat: cint; AMode: cint; const AData: Pointer; Elements: cint);
begin
  XChangeProperty(Display, AWindow, AProperty, AType, AFormat, AMode, AData, Elements);
end;

procedure TXWindowManager.WindowDeleteProperty(AWindow: TWindow; AProperty: TAtom);
begin
  XDeleteProperty(Display, AWindow, AProperty);
end;

function TXWindowManager.WindowSupportsProto(AWindow: TWindow; AProto: TAtom): Boolean;
var
  Protocols: PAtom;
  I, ProtoCount: Integer;
begin
  Result := False;
  if (XGetWMProtocols(display, AWindow, @Protocols, @ProtoCount) <> 0) then begin
    for I := 0 to ProtoCount-1 do begin
      if (Protocols[i] = AProto) then begin
        Result := True;
        Break;
      end;
    end;
    if (Protocols <> nil) then XFree(Protocols);
  end;
end;

function TXWindowManager.WindowGetTitle(const AWindow: TWindow): String;
var
  TypeAtom: TAtom;
  FormatAtom: TAtom;
  NumItems, BytesAfter: LongWord;
  ATitle: PChar;
begin
  Result := '';
  repeat
    BytesAfter := 0;
    if (XGetWindowProperty(Display, AWindow, _NET[_WM_NAME], 0, MaxINt, False, UTF8_STRING,
          @TypeAtom, @FormatAtom, @NumItems, @BytesAfter, @ATitle)=Success)
    or (XGetWindowProperty(Display, AWindow, XA_WM_NAME, 0, 24, False, XA_STRING,
          @TypeAtom, @FormatAtom, @NumItems, @BytesAfter, @ATitle)=Success)
    or (XGetWindowProperty(Display, AWindow, XA_WM_ICON_NAME, 0, 24, False, XA_STRING,
          @TypeAtom, @FormatAtom, @NumItems, @BytesAfter, @ATitle)=Success)
    then begin
      Result := Result + ATitle;
      XFree(ATitle);
    end;
  until BytesAfter = 0;
  if Result = '' then
    Result := 'Untitled';

end;

function TXWindowManager.WindowGetUserTime(const AWindow: TWindow): LongWord;
var
  TypeAtom: TAtom;
  FormatAtom: TAtom;
  NumItems, BytesAfter: LongWord;
begin
  // Return 0 if property is unset
  Result := 0;
  if XGetWindowProperty(Display, AWindow, _NET[_WM_USER_TIME], 0, 1, False, XA_CARDINAL,
        @TypeAtom, @FormatAtom, @NumItems, @BytesAfter, @Result)<>Success
  then Result := 0;
end;

function TXWindowManager.WindowGetPID(const AWindow: TWindow): Integer;
var
  TypeAtom: TAtom;
  FormatAtom: TAtom;
  NumItems, BytesAfter: LongWord;
begin
  // Return -1 if no PID available
  Result := -1;
  if XGetWindowProperty(Display, AWindow, _NET[_WM_PID], 0, MaxInt, False, XA_CARDINAL,
        @TypeAtom, @FormatAtom, @NumItems, @BytesAfter, @Result)<>Success
  then Result := -1;
end;

procedure TXWindowManager.WindowSetStandardEventMask(
  const AClientWindow: TWindow);
var
 GetAttr: TXWindowAttributes;
 SetAttr: TXSetWindowAttributes;
begin
  if XGetWindowAttributes(Display, AClientWindow,@GetAttr) = 0 then
  ;  SetAttr.event_mask := PropertyChangeMask
  ;//else
  //  SetAttr.event_mask := GetAttr.all_event_masks or PropertyChangeMask;
  XChangeWindowAttributes(Display, AClientWindow, CWEventMask, @SetAttr);
  //XSync(Display, False);
end;

function TXWindowManager.WindowGetParent ( AWindow: TWindow ) : TWindow;
var
  I: Integer;
  WindowCount: cuint;
  WindowsRoot, WindowsParent: TWindow;
  ChildrenList: PWindow;
begin
  if XQueryTree(Display,AWindow, @WindowsRoot, @WindowsParent,
            @ChildrenList, @WindowCount) <> 0 then
  XFree(ChildrenList);
  Result := WindowsParent;
end;

procedure TXWindowManager.InitICCCMHints;
begin
  // currently all ICCCM atoms are defined in XAtom;
end;

procedure TXWindowManager.InitNETHints;
begin
  Init_NETAtoms(fDisplay, fNetAtoms);
end;

procedure TXWindowManager.InitAtoms;
begin
  InitICCCMHints;
  InitNETHints;
  WM_DELETE_WINDOW := XInternAtom(Display, 'WM_DELETE_WINDOW', False);
  WM_PROTOCOLS     := XInternAtom(Display, 'WM_PROTOCOLS', False);
  UTF8_STRING      := XInternAtom(Display, 'UTF8_STRING', False);;
end;

end.

