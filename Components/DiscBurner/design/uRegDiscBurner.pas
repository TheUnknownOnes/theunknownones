unit uRegDiscBurner;

interface

uses
  Classes,
  uDiscBurner;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TDiscMaster]);
end;

end.
