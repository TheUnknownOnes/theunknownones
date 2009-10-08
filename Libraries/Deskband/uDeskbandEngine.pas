//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uDeskbandEngine;

interface

uses
  uDeskbandTypes, dialogs;

function DeskBandForms : TDeskBandFormClassList;

procedure RegisterDeskBands;

implementation

var
  FDeskBandForms : TDeskBandFormClassList;

procedure RegisterDeskBands;
var
  Cls : TDeskBandFormClass;
begin
  for cls in DeskBandForms do
  begin
    TDeskBandFormFactory.Create(cls);
  end;
end;

function DeskBandForms : TDeskBandFormClassList;
begin
  if not Assigned(FDeskBandForms) then
    FDeskBandForms:=TDeskBandFormClassList.Create;

  Result:=FDeskBandForms;
end;

initialization
  FDeskBandForms:=nil;

finalization
  if Assigned(FDeskBandForms) then
    FDeskBandForms.Free;

end.
