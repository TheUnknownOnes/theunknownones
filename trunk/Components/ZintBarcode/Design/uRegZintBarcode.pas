unit uRegZintBarcode;

interface

uses
  Classes,
  uZintBarcode;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TZintBarcodeComponent]); 
end;

end.
