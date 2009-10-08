unit uRegEffectPNGImage;

interface

uses
  Classes,
  uEffectPNGImage;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('PNGControls', [TEffectPNGImage]);
end;

end.
