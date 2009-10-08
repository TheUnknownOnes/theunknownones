unit uRegisterVisualComponents;

interface

uses
  Classes,
  uBassWaveView;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TBassWaveView]);
end;

end.
