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
  public
    constructor Create(AOwner: TBaseWindowManager; AScreen: PScreen; AWindow: TWindow);
    destructor Destroy; override;
    function AddNewClient(AWindow: TWindow): TXFrame;
    procedure GrabExistingWindows;
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
    property Windows[AIndex: Integer]: TWMRootWindow read GetRootWindow write SetRootWindow;
  end;
   

implementation

uses XWM;

type
  // a hack to get around a recursive uses
  TXWM = class(TXWindowManager)
  end;

{ TWMRootWindow }

constructor TWMRootWindow.Create(AOwner: TBaseWindowManager; AScreen: PScreen; AWindow: TWindow);
begin
  fOwner := AOwner;
  fRootWindow := AWindow;
  fScreen := AScreen;
  fFrames := TXFrameList.Create;
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

end.

