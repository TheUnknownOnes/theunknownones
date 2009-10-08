unit uRegPNGImageList;

interface

uses
  Classes,
  uPNGImageList;


  
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Imagelists',[TPNGImageList]);
end;

end.
