//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCommonTypes;

interface

type
  TMonatJahrInt = Integer;

function EncodeMonatJahrInt(Monat, Jahr : Integer) : TMonatJahrInt;
procedure DecodeMonatJahrInt(AMonatJahrInt : TMonatJahrInt; var Monat, Jahr : Integer);

implementation

function EncodeMonatJahrInt(Monat, Jahr : Integer) : TMonatJahrInt;
begin
  Result:=(Jahr*100)+Monat;
end;

procedure DecodeMonatJahrInt(AMonatJahrInt : TMonatJahrInt; var Monat, Jahr : Integer);
var
  temp : Single;
begin
  temp:=AMonatJahrInt/100;
  Jahr:=Trunc(temp);
  Monat:=AMonatJahrInt-(Jahr*100);
end;

end.
