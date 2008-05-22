unit uRegSearchEdit;

interface

uses
  Classes,
  uSearchEdit;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TSearchEdit]);
end;

end.
