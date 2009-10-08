//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uStreamTools;

interface

uses
  Classes, SysUtils;

function SerializeStream(const AStream : TStream) : String;
procedure DeserializeToStream(const Data : String; const AStream : TStream);

implementation

function SerializeStream(const AStream : TStream) : String;
var
  SS : TStringStream;
  B : Byte;
begin
  SS:=TStringStream.Create('');
  try
    while AStream.Read(b,SizeOf(b))>0 do
      SS.WriteString(IntToHex(b,2));
    Result:=SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure DeserializeToStream(const Data : String; const AStream : TStream);
var
  SS : TStringStream;
  Ser : array [0..1] of AnsiChar;
  B : Byte;
begin
  SS:=TStringStream.Create(Data);
  try
    while ss.Read(Ser,2)>0 do
    begin
      B:=StrToInt('$'+Ser);
      AStream.Write(B,1);
    end;
  finally
    SS.Free;
  end;
end;

end.
