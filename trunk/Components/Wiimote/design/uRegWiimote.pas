unit uRegWiimote;

interface

{$R 'images.res'}

uses
  Classes,
  uWiimote;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TWiimote]);
end;

end.
