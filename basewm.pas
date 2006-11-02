unit BaseWM;

{$mode objfpc}{$H+}

interface

uses
  Classes;
  
type

  // A base window manager. this might be useful later
  TBaseWindowManager = class(TObject)
  public
    procedure InitWM(QueryWindows: Boolean = True); virtual; abstract;
    procedure MainLoop; virtual; abstract;
  end;
  
  { TBaseFrame }

  TBaseFrame = class(TObject)
  private
    fOwner: TBaseWindowManager;
    fCaption: String;
  public
    procedure SetCaption(const AValue: String); virtual;
    property Caption: String read fCaption write SetCaption;
    property Owner: TBaseWindowManager read fOwner write fOwner;
  end;


implementation

{ TBaseFrame }

procedure TBaseFrame.SetCaption(const AValue: String);
begin
  fCaption := AValue;
end;

end.

