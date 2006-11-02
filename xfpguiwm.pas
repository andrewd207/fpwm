unit XfpGUIWM;

{$mode objfpc}{$H+}

interface

uses
  Classes, BaseWM, X, XLib, XUtil, XAtom, ctypes, XWM, XFrames, XRootWindow, GfxBase, Gfx_X11;
  
type

  { TfpGUIFrame }

  TfpGUIFrame = class(TXFrame)
  private
    fCanvas: TXWindowCanvas;
    fFont: TXFont;
  public
   constructor Create(AOwner: TBaseWindowManager; AClientWindow: TWindow; AFrameWindow: TWindow); override;
   property Canvas: TXWindowCanvas read fCanvas write fCanvas;
   property Font: TXFont read fFont write fFont;
  end;

  { TfpGUIWindowManager }

  TfpGUIWindowManager = class(TXWindowManager)
  private
    fDisplay: TXDisplay;
    procedure CreateDisplay(ADisplayName: String); override;
    function CreateXWindow(ARootWindow: TWMRootWindow; const AScreen: PScreen; Geometry: TRect): TWindow;
  public
    function CreateNewWindowFrame(Sender: TWMRootWindow; const AScreen: PScreen; const AChild: TWindow): TXFrame; override;
    procedure DestroyWindowFrame(var AWindow: TXFrame); override;
    procedure PaintWindowFrame(AFrame: TXFrame); override;
    property GfxDisplay: TXDisplay read fDisplay;

  end;

implementation

{ TfpGUIWindowManager }

procedure TfpGUIWindowManager.CreateDisplay(ADisplayName: String);
begin
  fDisplay := TXDisplay.Create;
  Display := fDisplay.Handle;
  fDisplay.DisplayName := ADisplayName;
end;


// IMPORTANT!! The Width and Height of the window must be the border size
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
                             Geometry.Right-Geometry.Left, Geometry.Bottom-Geometry.Left, 0,
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
begin
  Result := nil;
  // This hints stuff should be moved to its own procedure
  WriteLn('Creating window frame');
  if XGetWMSizeHints(Display, AChild, @SizeHints, @SuppliedReturn, XA_WM_SIZE_HINTS)<>0 then
  begin
    Geom.Top := 50; //TODO use wingravity
    Geom.Left := 50; //TODO use wingravity
    Geom.Right := Geom.Left + SizeHints.base_width;
    Geom.Bottom := Geom.Top + SizeHints.base_height;
  end
  else begin
    Geom.Top := 50;
    Geom.Left := 50;
    // A Default window size
    Geom.Right := Geom.Left + 400;
    Geom.Bottom := Geom.Top + 300;
  end;

  // create the TWindow that is the frame
  FrameWindow := CreateXWindow(Sender, AScreen, Geom);

  Frame := TfpGUIFrame.Create(Self, AChild, FrameWindow);

  Colormap := XDefaultColormap(Display, XScreenNumberOfScreen(AScreen));
  
  Frame.Font := TXFont.Create(Display, '-adobe-helvetica-medium-r-normal--*-120-*-*-*-*-iso8859-1');
  // create our canvas object to draw on our frame with
  Frame.Canvas := TXWindowCanvas.Create(ColorMap, GfxDisplay, FrameWindow, Frame.Font.FontStruct);

  
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
  FrameList := RootWindows.FindFrameListFromWindow(Frame);
  if FrameList <> nil then begin
    FrameList.Remove(Frame);
  end;
  // now free our FrameWindow
  XDestroyWindow(Display, Frame.FrameWindow);
  // free the stuff we created when we created the window
  Frame.Canvas.Free;
  Frame.Font.Free;
  Frame.Free;
  AWindow := nil;
end;

procedure TfpGUIWindowManager.PaintWindowFrame(AFrame: TXFrame);
var
  Frame: TfpGUIFrame;
  ARect: TRect;
begin
  WriteLn('Painting');
  Frame := TfpGUIFrame(AFrame);
  Arect.Top := 0;
  ARect.Left := 0;
  Arect.Right := AFrame.FrameWidth;
  ARect.Bottom := AFrame.FrameHeight;
  Frame.Canvas.SetColor(colBlack);
  Frame.Canvas.FillRect(ARect);
  Frame.Canvas.SetColor(colWhite);
  Frame.Canvas.TextOut(Point(7, 7),'fpwm Managed Window');

end;

{ TfpGUIFrame }

constructor TfpGUIFrame.Create(AOwner: TBaseWindowManager; AClientWindow: TWindow; AFrameWindow: TWindow);
begin
  inherited Create(AOwner, AClientWindow, AFrameWindow);
end;

end.

