unit uRegIBEScriptComponent;

interface

uses
  Classes, uIBEScriptComponent;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TIBEScriptComponent]);
end;

end.
