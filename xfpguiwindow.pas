unit xfpguiwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_window;

type

  { TfpguiWindow }

  TfpguiWindow=class(TfpgWindow)
  protected
    procedure DoAllocateWindowHandle; override;
  public
    property OnResize;
    property OnPaint;
    constructor Create(AOwner: TComponent); override;
  end;


implementation

{ TfpguiWindow }

procedure TfpguiWindow.DoAllocateWindowHandle;
begin
  inherited DoAllocateWindowHandle;
  Window.WindowType:=wtWindow;
  Window.WindowAttributes := [waBorderless,waUnblockableMessages, waX11SkipWMHints];
end;

constructor TfpguiWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllocateWindowHandle;
end;

end.

