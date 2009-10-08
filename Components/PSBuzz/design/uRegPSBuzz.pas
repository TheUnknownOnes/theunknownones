unit uRegPSBuzz;

interface

{$R 'images.res'}

uses
  Classes,
  uPSBuzz;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TPSBuzz]);
end;

end.
