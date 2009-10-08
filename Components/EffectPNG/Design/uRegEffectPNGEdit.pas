unit uRegEffectPNGEdit;

interface

uses
  Classes,
  uEffectPNGEdit;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PNGControls', [TEffectPNGEdit]);
end;

end.
