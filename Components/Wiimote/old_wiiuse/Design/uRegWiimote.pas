unit uRegWiimote;

interface

uses
  Classes,
  uWiimote;


  
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TWiimoteManager]);
end;

end.
