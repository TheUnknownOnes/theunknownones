unit uRegVSTSearchEdit;

interface

uses
  Classes,
  uVSTSearchEdit;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TVSTSearchEdit]);
end;

end.
