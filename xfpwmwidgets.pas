unit xfpwmwidgets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_main, fpg_base, fpg_widget,fpg_label, fpg_button, fpg_impl;

type

  { TXfpgWMTitleBar }

  TXfpgWMTitleBar = class(TfpgWidget)
    FMenuButton: TfpgButton;
    FCloseButton: TfpgButton;
    FMinimizeButton: TfpgButton;
    FMaximizeRestoreButton: TfpgButton;
    //FTitleLabel: TfpgLabel;
  protected
    FTitleText: String;
    FCreating: Boolean;
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
  private
    FOnEndDrag: TMouseButtonEvent;
    FOnStartDrag: TMouseButtonEvent;
    function GetTitle: String;
    procedure Resized(Sender: TObject);
    function GetOnCloseButtonClick: TNotifyEvent;
    function GetOnMaxRestoreButtonClick: TNotifyEvent;
    function GetOnMinimizeButtonClick: TNotifyEvent;
    procedure SetOnCloseButtonClick ( const AValue: TNotifyEvent ) ;
    procedure SetOnMaxRestoreButtonClick ( const AValue: TNotifyEvent ) ;
    procedure SetOnMinimizeButtonClick ( const AValue: TNotifyEvent ) ;
    procedure SetTitle ( const AValue: String ) ;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property OnCloseButtonClick: TNotifyEvent read GetOnCloseButtonClick write SetOnCloseButtonClick;
    property OnMinimizeButtonClick: TNotifyEvent read GetOnMinimizeButtonClick write SetOnMinimizeButtonClick;
    property OnMaxRestoreButtonClick: TNotifyEvent read GetOnMaxRestoreButtonClick write SetOnMaxRestoreButtonClick;
    property OnStartDrag: TMouseButtonEvent read FOnStartDrag write FOnStartDrag;
    property OnEndDrag: TMouseButtonEvent read FOnEndDrag write FOnEndDrag;
    property Title: String read GetTitle write SetTitle;

    property OnMouseMove;
  end;

  { TfpgClientWindow }

  TfpgClientWindow = class(TfpgWidget)
  protected
    FWinHandle: TfpgWinHandle;
  public
    constructor Create(AOwner: TComponent; AWinHandle: TfpgWinHandle);
    //procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); override;
  end;

implementation

{ TXfpgWMTitleBar }

procedure TXfpgWMTitleBar.HandlePaint;
function SetTextWidth(AWidth: Integer; out ATextWidth: Integer): Integer;
begin
  ATextWidth := AWidth;
  Result := AWidth;
end;
var
  AvailSize: Integer;
  TheText: String;
  CopySize: Integer;
  PaintX, PaintY, PaintWidth: Integer;
begin
  if not Visible then exit;
  Canvas.Color := BackgroundColor;
  Canvas.FillRectangle(0,0,Width,Height);


  if FCreating then
    Exit;
   //exit;
  Canvas.TextColor := clWhite;
  TheText := Trim(FTitleText);
  AvailSize := FMinimizeButton.Left - FMenuButton.Width;
  CopySize := Length(TheText);

  while (SetTextWidth(Canvas.Font.TextWidth(TheText), PaintWidth) > AvailSize) and (CopySize > 2) do
  begin
    Dec(CopySize);
    TheText := Copy(FTitleText, 1, CopySize)+'...';
  end;

  PaintY := (Height div 2) - (Canvas.Font.Height div 2);
  PaintX := (AvailSize div 2) - (PaintWidth div 2);

  Canvas.DrawText(PaintX, PaintY, PaintWidth, Height, TheText);
  //Canvas.DrawText(0, 0 , AvailSize, Height, TheText);

end;

procedure TXfpgWMTitleBar.HandleLMouseDown ( x, y: integer;
  shiftstate: TShiftState ) ;
begin
  inherited HandleLMouseDown ( x, y, shiftstate ) ;
  if Assigned(FOnStartDrag) then
    FOnStartDrag(Self, mbLeft, shiftstate, Point(x,y));
end;

procedure TXfpgWMTitleBar.HandleLMouseUp ( x, y: integer;
  shiftstate: TShiftState ) ;
begin
  inherited HandleLMouseUp ( x, y, shiftstate ) ;
  if Assigned(FOnEndDrag) then
    FOnEndDrag(Self, mbLeft, shiftstate, Point(x,y));
end;

procedure TXfpgWMTitleBar.HandleMouseMove ( x, y: integer; btnstate: word;
  shiftstate: TShiftState ) ;
begin
  inherited HandleMouseMove ( x, y, btnstate, shiftstate ) ;
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, shiftstate, Point(x,y));
end;

function TXfpgWMTitleBar.GetTitle: String;
begin
  //Result := FTitleLabel.Text;
  Result := FTitleText;
end;

procedure TXfpgWMTitleBar.Resized ( Sender: TObject ) ;
begin
  FMenuButton.Left := 0;
  FMenuButton.Height := Height;

  //FTitleLabel.Left := FMenuButton.Left+FMenuButton.Width;

  FCloseButton.Left := Width - FCloseButton.Width;
  FMaximizeRestoreButton.Left := FCloseButton.Left- FMaximizeRestoreButton.Width;
  FMinimizeButton.Left := FMaximizeRestoreButton.Left - FMinimizeButton.Width;

  FCloseButton.UpdateWindowPosition;
  FMaximizeRestoreButton.UpdateWindowPosition;
  FMinimizeButton.UpdateWindowPosition;
  FMenuButton.UpdateWindowPosition;
end;

function TXfpgWMTitleBar.GetOnCloseButtonClick: TNotifyEvent;
begin
  Result := FCloseButton.OnClick;
end;

function TXfpgWMTitleBar.GetOnMaxRestoreButtonClick: TNotifyEvent;
begin
  Result := FMaximizeRestoreButton.OnClick;
end;

function TXfpgWMTitleBar.GetOnMinimizeButtonClick: TNotifyEvent;
begin
  Result := FMinimizeButton.OnClick;
end;

procedure TXfpgWMTitleBar.SetOnCloseButtonClick ( const AValue: TNotifyEvent  ) ;
begin
  FCloseButton.OnClick := AValue
end;

procedure TXfpgWMTitleBar.SetOnMaxRestoreButtonClick ( const AValue: TNotifyEvent ) ;
begin
  FMaximizeRestoreButton.OnClick := AValue
end;

procedure TXfpgWMTitleBar.SetOnMinimizeButtonClick ( const AValue: TNotifyEvent ) ;
begin
  FMinimizeButton.OnClick := AValue
end;

procedure TXfpgWMTitleBar.SetTitle ( const AValue: String ) ;
begin
  if AValue = FTitleText then exit;
  {if AValue <> '' then
    FTitleLabel.Text := AValue
  else
    FTitleLabel.Text := 'Untitled';}
 if AValue <> '' then
   FTitleText := AValue
 else
   FTitleText := 'Untitled';
 Invalidate;
end;

constructor TXfpgWMTitleBar.Create ( AOwner: TComponent ) ;
begin
  FCreating := True;
  inherited Create ( AOwner ) ;

  BackgroundColor := clBlue;
  Self.OnResize := @Resized;
  MouseCursor := mcArrow;


  FMenuButton := TfpgButton.Create(Self);
  with FMenuButton do begin
    Text := 'M';
    Embedded := True;
    Width := Height;
    Left := 0;
    Top := 0;
    Visible := True;
    MouseCursor := mcArrow;
  end;

  {FTitleLabel := TfpgLabel.Create(Self);
  with FTitleLabel do begin
    Parent := Self;
    Text := 'Untitled';
    TextColor := clWhite;
    Left := FMenuButton.Left+ FMenuButton.Width;
    Top := 0;
    AutoSize := True;
    Visible := True;
  end;}

  Height := FMenuButton.Height;

  FCloseButton := TfpgButton.Create(Self);
  with FCloseButton do begin
    Parent := Self;
    Text := 'X';
    Embedded := True;
    Width := Height;
    Left := Self.Width-Width;
    Top := 0;
    Visible := True;
    MouseCursor := mcArrow;
  end;
  FMaximizeRestoreButton := TfpgButton.Create(Self);
  with FMaximizeRestoreButton do begin
    Parent := Self;
    Text := '+';
    Embedded := True;
    Width := Height;
    Left := FCloseButton.Left-Width;
    Top := 0;
    Visible := True;
    MouseCursor := mcArrow;
  end;
  FMinimizeButton:= TfpgButton.Create(Self);
  with FMinimizeButton do begin
    Parent := Self;
    Text := '_';
    Embedded := True;
    Width := Height;
    Left := FMaximizeRestoreButton.Left-Width;
    Top := 0;
    Visible := True;
    MouseCursor := mcArrow;
  end;
  FCreating := False;
end;

destructor TXfpgWMTitleBar.Destroy;
begin
  inherited Destroy;
end;

{ TfpgClientWindow }

constructor TfpgClientWindow.Create ( AOwner: TComponent; AWinHandle: TfpgWinHandle ) ;
begin
  inherited Create(AOwner);
  FWinHandle := AWinHandle;
end;

{procedure TfpgClientWindow.DoAllocateWindowHandle ( AParent: TfpgWindowBase ) ;
begin
  inherited DoAllocateWindowHandle ( AParent ) ;
end;}

end.

