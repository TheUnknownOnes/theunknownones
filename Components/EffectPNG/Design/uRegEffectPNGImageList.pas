unit uRegEffectPNGImageList;

interface

uses
  Classes,
  uEffectPNGImageList;


  
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Imagelists',[TEffectPNGImageList]);
end;


end.
