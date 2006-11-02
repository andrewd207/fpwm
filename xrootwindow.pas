unit XRootWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, X, XLib, XAtom, XFrames, ctypes, BaseWM;
  
type

  { TWMRootWindow }

  TWMRootWindow = class(TObject)
  private
    fOwner: TBaseWindowManager;
    fFrames: TXFrameList;
    fScreen: PScreen;
    fRootWindow: TWindow;
    procedure SetICCCMHints;
    procedure SetFreeDesktopHints;
  public
    constructor Create(AOwner: TBaseWindowManager; AScreen: PScreen; AWindow: TWindow);
    destructor Destroy; override;
    function AddNewClient(AWindow: TWindow): TXFrame;
    procedure GrabExistingWindows;
    procedure SetHints;
    property Frames: TXFrameList read fFrames;
    property Owner: TBaseWindowManager read fOwner;
    property RootWindow: TWindow read fRootWindow;
    property Screen: PScreen read fScreen;
    
  end;
  
  { TWMRootWindowList }

  TWMRootWindowList = class(TList)
  private
    function GetRootWindow(AIndex: Integer): TWMRootWindow;
    procedure SetRootWindow(AIndex: Integer; const AValue: TWMRootWindow);
  public
    procedure Clear; override;
    function FindFrameListFromWindow(const AWindow: TXFrame): TXFrameList;
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

procedure TWMRootWindow.SetFreeDesktopHints;
begin
  // TODO Set mandatory FreeDesktop.org hints on the rootwindow
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
  fFrames.Free;
  inherited Destroy;
end;

function TWMRootWindow.AddNewClient(AWindow: TWindow): TXFrame;
begin
  Result := TXWM(fOwner).CreateNewWindowFrame(Self, fScreen, AWindow);
  if Result <> nil then Frames.Add(Result);
end;

procedure TWMRootWindow.GrabExistingWindows;
var
  I: Integer;
  WindowCount: cuint;
  WindowsRoot, WindowsParent: TWindow;
  ChildrenList: PWindow;
  WindowAttributes: TXWindowAttributes;
begin
  if XQueryTree(TXWM(fOwner).Display, fRootWindow, @WindowsRoot, @WindowsParent,
            @ChildrenList, @WindowCount) <> 0 then
  begin //success!
    // this finds out if the window want's to be managed or not.
    for I := 0 to WindowCount-1 do begin
      XGetWindowAttributes(TXWM(fOwner).Display, ChildrenList[I], @WindowAttributes);
      // override_redirect are windows like menus or hints that appear with no border
      // we don't manage them
      if  (WindowAttributes.override_redirect = False)
      and (WindowAttributes.map_state = IsViewable)
      then AddNewClient(ChildrenList[I]);
    end;
    XFree(ChildrenList);
  end;
end;

procedure TWMRootWindow.SetHints;
begin
  SetICCCMHints;
  SetFreeDesktopHints;
  // and any hints specific to our windowmanager here
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

function TWMRootWindowList.FindFrameListFromWindow(const AWindow: TXFrame
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

