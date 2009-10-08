unit uRegVirtualActionTree;

interface

uses
  Classes,
  uVirtualActionTree;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Virtual Controls', [TVirtualActionTree]);
end;

end.
