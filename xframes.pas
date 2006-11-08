unit XFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, BaseWM, X, XLib;
  
type

  TXWindowState = (xwsNormal);
  
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
    //fPosition
    //fSize
    procedure SetCaption(const AValue: String); override;
    procedure SetFrameBottomHeight(const AValue: Integer); virtual;
    procedure SetFrameHeight(const AValue: Integer); virtual;
    procedure SetFrameLeftWidth(const AValue: Integer); virtual;
    procedure SetFrameRightWidth(const AValue: Integer); virtual;
    procedure SetFrameTopHeight(const AValue: Integer); virtual;
    procedure SetFrameWidth(const AValue: Integer); virtual;
  public
    constructor Create(AOwner: TBaseWindowManager; AClientWindow: TWindow; AFrameWindow: TWindow); virtual;
    destructor Destroy; override;
    property ClientWindow: TWindow read fClientWindow;
    property FrameWindow: TWindow read fFrameWindow;
    property FrameTopHeight: Integer read fFrameTopHeight write SetFrameTopHeight;
    property FrameBottomHeight: Integer read fFrameBottomHeight write SetFrameBottomHeight;
    property FrameLeftWidth: Integer read fFrameLeftWidth write SetFrameLeftWidth;
    property FrameRightWidth: Integer read fFrameRightWidth write SetFrameRightWidth;
    property FrameWidth: Integer read fFrameWidth write SetFrameWidth;
    property FrameHeight: Integer read fFrameHeight write SetFrameHeight;
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

implementation
uses XWM;

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

constructor TXFrame.Create(AOwner: TBaseWindowManager; AClientWindow: TWindow;
  AFrameWindow: TWindow);
var
  ATitle, AUnmappedTitle: String;
begin
  Owner := AOwner;
  fClientWindow := AClientWindow;
  fFrameWindow := AFrameWindow;
  fFrameWidth := 400;
  fFrameHeight := 300;
  fFrameLeftWidth := 7;
  fFrameRightWidth := 7;
  fFrameTopHeight := 25;
  fFrameBottomHeight := 7;
  TXWindowManager(AOwner).GetWindowTitles(AClientWindow, ATitle,AUnmappedTitle);
  Caption := ATitle;
end;

destructor TXFrame.Destroy;
begin
  inherited Destroy;
end;

end.

