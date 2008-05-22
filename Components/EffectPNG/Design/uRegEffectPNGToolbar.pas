unit uRegEffectPNGToolbar;

interface

uses
  Classes,
  uEffectPNGToolbar;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PNGControls', [TEffectPNGToolButton, TEffectPNGToolBar]);
end;

end.
