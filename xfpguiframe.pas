unit XfpGUIFrame;

{$mode objfpc}{$H+}

interface

uses
  { FCL RTL }
  Classes,
  { X Stuff }
  X, XLib,
  { fpGUI units}
  fpg_base, fpg_main, fpg_x11, fpg_form, fpg_widget, fpg_label,fpg_button, ctypes,
  { program units }
  BaseWM,
  XfpGUIWM, XFrames, xfpwmwidgets, XRootWindow;

type
  { TfpGUIFrame }

  TfpGUIFrame = class(TXFrame)
    procedure FormPaint(Sender: TObject);
  private
    FOrigXY: TPoint;
    FStartDragPos: TPoint;
    FMouseIsDown: Boolean;
    FForm: TfpgForm;
    FTitleBar: TXfpgWMTitleBar;
    FfpgClientWindow: TfpgClientWindow;
    procedure FormResize(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure SetCaption(const AValue: String); override;
    procedure TitleBarMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure TitleBarMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure TitleBarMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure MoveWindow(APosition: TPoint); override;
    procedure ResizeWindow(Var AWidth, AHeight: Integer); override;
  public
   constructor Create(AOwner: TBaseWindowManager; AClientWindow: TWindow; AFrameWindow: TWindow; ARootWindow: TWindow; AX,AY,AW,AH: Integer); override;
   destructor  Destroy; override;
   property Form: TfpgForm read fForm;
  end;

implementation

{ TfpGUIFrame }

procedure TfpGUIFrame.FormPaint ( Sender: TObject ) ;
begin
  with FForm do
    fpgStyle.DrawControlFrame(Canvas, 0, 0, Width, Height );
end;

procedure TfpGUIFrame.FormResize ( Sender: TObject ) ;
begin
  FTitleBar.Width := FForm.Width;
  FTitleBar.UpdateWindowPosition;
end;

procedure TfpGUIFrame.CloseClick(Sender: TObject);
begin
  CloseWindowNice;
end;

procedure TfpGUIFrame.SetCaption(const AValue: String);
begin
  inherited SetCaption(AValue);
  FTitleBar.Title := AValue;
end;

procedure TfpGUIFrame.TitleBarMouseDown ( Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint ) ;
var
  WM: TfpGUIWindowManager absolute Owner;
  RootX, RootY: cint;
  CW: TWindow;
  CWX, CWY: cint;
  RW: TWindow;
  mask: cuint;
begin
  FMouseIsDown := True;
  //FOrigXY:= Point(FForm.Top, FForm.Left);
  //WriteLn('MOuse is down');

  XQueryPointer(Display, RootWindow, @RW, @CW, @RootX, @RootY, @CWx, @CWy, @mask);
  FOrigXY := Point(RootX, RootY);
  FStartDragPos := Point(FForm.Left, FForm.Top);

  FTitleBar.MouseCursor := mcMove;
  BringWindowToFront;
  FocusWindow;

end;

procedure TfpGUIFrame.TitleBarMouseUp ( Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint ) ;
begin
  FMouseIsDown := False;
  FTitleBar.MouseCursor := mcArrow;
  //WriteLn('MOuse is UP');
end;

procedure TfpGUIFrame.TitleBarMouseMove ( Sender: TObject; AShift: TShiftState;
  const AMousePos: TPoint ) ;
var
  NewPos: TPoint;
  RootX, RootY: cint;
  CW: TWindow;
  CWX, CWY: cint;
  RW: TWindow;
  mask: cuint;

begin
  if Not FMouseIsDown then
    Exit;
  XQueryPointer(Display, RootWindow, @RW, @CW, @RootX, @RootY, @CWx, @CWy, @mask);

  NewPos := FStartDragPos;

  NewPos.X := NewPos.X + (RootX-FOrigXY.X);
  NewPos.Y := NewPos.Y + (RootY-FOrigXY.Y);

  MoveWindow(NewPos);
end;

procedure TfpGUIFrame.MoveWindow ( APosition: TPoint ) ;
begin
  FForm.Top := APosition.Y;
  FForm.Left := APosition.X;
  FForm.UpdateWindowPosition;
  ClientLeft := APosition.X;
  ClientTop := APosition.Y;
  Include(FFrameInitialProps, xfpMoved);
  //WriteLn('Moving Window ',APosition.x,':', APosition.y);
end;

procedure TfpGUIFrame.ResizeWindow ( var AWidth, AHeight: Integer ) ;
begin
  if MinSize.Width >= 0 then
    AWidth := Max(MinSize.Width, AWidth);
  if MinSize.Height >= 0 then
    AHeight := Max(MinSize.Height, AHeight);

  if AWidth < (FTitleBar.Height*4)+FrameLeftWidth+FrameRightWidth then
    AWidth := (FTitleBar.Height*4)+FrameLeftWidth+FrameRightWidth;

  if AHeight < FTitleBar.Height+FrameBottomHeight+FrameTopHeight then
     AHeight := FTitleBar.Height+FrameBottomHeight+FrameTopHeight;

  FrameWidth := AWidth+FrameLeftWidth+FrameRightWidth;
  FrameHeight := AHeight+FrameTopHeight+FrameBottomHeight;
  // resize the client to fit in the frame window
  XResizeWindow(Display, ClientWindow, AWidth, AHeight);

  // resize the Frame to fit the client's wanted size
  // it's important to resize this window after
  FForm.Width := FrameWidth;
  FForm.Height := FrameHeight;
  FForm.UpdateWindowPosition;
  Include(FFrameInitialProps, xfpResized);
  WriteLn('ResizeWindow ', AWidth,';',AHeight);
  ClientHeight := AHeight;
  ClientWidth := AWidth;
end;

type
  TFPWMFormAccess = class(TfpgForm);

constructor TfpGUIFrame.Create(AOwner: TBaseWindowManager; AClientWindow: TWindow; AFrameWindow: TWindow; ARootWindow: TWindow; AX,AY,AW,AH: Integer);
begin
  // consider all buttons and labels in this routine as just the shortest route
  // to have the basic things a window needs. These must be changed later to be prettier.
  FForm := TfpgForm.Create(fpgApplication);
  FForm.WindowAttributes := [waBorderless,waUnblockableMessages, waX11SkipWMHints];

  FForm.OnResize := @FormResize;
  FForm.OnPaint := @FormPaint;

  TFPWMFormAccess(fForm).DoAllocateWindowHandle(nil);

  FfpgClientWindow := TfpgClientWindow.Create(FForm, AClientWindow);
  FTitleBar := TXfpgWMTitleBar.Create(FForm);
  FTitleBar.OnCloseButtonClick := @CloseClick;
  FTitleBar.Top := 0;
  FTitleBar.Left := 0;
  FTitleBar.Visible := True;
  FTitleBar.OnStartDrag := @TitleBarMouseDown;
  FTitleBar.OnMouseMove := @TitleBarMouseMove;
  FTitleBar.OnEndDrag :=   @TitleBarMouseUp;

  // create the TWindow that is the frame
  //Caption := TfpGUIWindowManager(AOwner).WindowGetTitle(AClientWindow);


  inherited Create(AOwner, AClientWindow, TWindow(fForm.WinHandle), ARootWindow, AX,AY,AW,AH);

end;

destructor TfpGUIFrame.Destroy;
begin
  fpgPostMessage(FForm, fpgApplication, FPGM_CLOSE);
  //fForm.Free;
  //fpgApplication.PopModalForm;
  inherited Destroy;
end;

end.

