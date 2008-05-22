unit uRegImageListProvider;

interface

uses
  Classes,
  uImageListProvider;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Imagelists',[TImageListProvider]);
end;

end.
