unit XfpGUIWM;

{$mode objfpc}{$H+}

interface

uses
  Classes, BaseWM, X, XLib, XUtil, XAtom, ctypes, XWM, XFrames, XRootWindow,
  fpg_base, fpg_main, fpg_x11, fpg_form, NetAtoms;
  
type

  { TfpGUIWindowManager }

  TfpGUIWindowManager = class(TXWindowManager)
  private
    App: TfpgApplication;
    procedure CreateDisplay(ADisplayName: String); override;
    function CreateXWindow(ARootWindow: TWMRootWindow; const AScreen: PScreen; Geometry: TRect): TWindow;
  public
    function CreateNewWindowFrame(Sender: TWMRootWindow; const AScreen: PScreen; const AChild: TWindow; AX, AY, AWidth, AHeight: Integer; AOverrideDirect: TBool): TXFrame; override;
    procedure DestroyWindowFrame(var AWindow: TXFrame); override;
  end;

implementation

uses XfpGUIFrame;

{ TfpGUIWindowManager }

procedure TfpGUIWindowManager.CreateDisplay(ADisplayName: String);
begin
  fpgApplication.EventFilter := @XEventCB;
  Display := fpgApplication.Display;
end;


// IMPORTANT!! The Width and Height of the window must include the border size
function TfpGUIWindowManager.CreateXWindow(ARootWindow: TWMRootWindow; const AScreen: PScreen; Geometry: TRect): TWindow;
var
  FrameAttributes : TXSetWindowAttributes;

begin
  FrameAttributes.border_pixel := BlackPixelOfScreen(AScreen);
  // we wouldn't want our frame to have another frame made for it would we :)
  FrameAttributes.override_redirect := TBool(True);
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
  const AChild: TWindow; AX, AY, AWidth, AHeight: Integer; AOverrideDirect: TBool): TXFrame;
var
  FrameWindow: TWindow;
  Frame: TfpGUIFrame;
  GetAttr: TXWindowAttributes;
  SetAttr: TXSetWindowAttributes;
  //Top, Left, Width, Height: Integer;
begin
  Result := nil;
  // This hints stuff should be moved to its own procedure
  //Xgets);
  if AOverrideDirect > 0 then
    Exit;
  Frame := TfpGUIFrame.Create(Self, AChild, None, Sender.RootWindow, AX, AY, AWidth, AHeight);

  if AChild = Frame.FrameWindow then begin
    WriteLN('Warning: Made Window frame for window frame!! Destroying.');
    Frame.Free;
    Exit;
  end;
  FrameWindow := Frame.FrameWindow;
  Result := Frame;
  //WriteLn('Creating window frame:', AX + AY + AWidth + AHeight);


  if XGetWindowAttributes(Display, FrameWindow, @GetAttr)= 0 then
    SetAttr.event_mask := WindowManagerEventMask
  else
    SetAttr.event_mask := WindowManagerEventMask or GetAttr.all_event_masks;
  XChangeWindowAttributes(Display, FrameWindow, CWEventMask , @SetAttr);


  XSetWindowBorderWidth(Display, Frame.ClientWindow, 0);

  XReparentWindow(Display, Frame.ClientWindow, Frame.FrameWindow,
                  Frame.FrameLeftWidth, Frame.FrameTopHeight);

end;

procedure TfpGUIWindowManager.DestroyWindowFrame(var AWindow: TXFrame);
var
  FrameList: TXFrameList;
  Frame: TfpGUIFrame;
begin
  WriteLn('Destroying Frame-----------------');
  if AWindow = nil then exit;
  Frame := TfpGUIFrame(AWindow);
  // first remove ourselves from the Rootwindow list
  WriteLn('Frame = ', Frame.FrameWindow);
  WriteLn('Client= ', Frame.ClientWindow);
  repeat
    FrameList := RootWindows.FrameListFromWindow(Frame);
    if FrameList <> nil then begin
      WriteLn('Found Window Frame to Destroy');
      FrameList.Remove(Frame);
    end else WriteLn('DID NOT Find Window Frame to Destroy');
  // free the stuff we created when we created the window
  until FrameList = nil;
  Frame.Free;

  AWindow := nil;
end;


end.

