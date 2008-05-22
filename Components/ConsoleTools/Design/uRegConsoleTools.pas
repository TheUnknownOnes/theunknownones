unit uRegConsoleTools;

interface

uses
  Classes,
  uConsoleTools;


procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('TUO',[TConsoleProcess]);
end;


end.
