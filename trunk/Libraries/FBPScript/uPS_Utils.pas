unit uPS_Utils;

interface

uses
  Classes,
  TypInfo,
  uRTTIHelper,
  uPSCompiler,
  uPSRuntime;

procedure RegisterEnum(ACompiler : TPSPascalCompiler; ATypeInfo : PTypeInfo);

implementation

procedure RegisterEnum(ACompiler : TPSPascalCompiler; ATypeInfo : PTypeInfo);
var
  sl : TStringList;
  idx : Integer;
begin
  sl := TStringList.Create;
  try
    rttihEnumToList(ATypeInfo, sl);
    for idx := 0 to sl.Count - 1 do
      sl[idx] := sl.Names[idx];
    ACompiler.AddTypeS(ATypeInfo^.Name, '(' + sl.CommaText + ')');
  finally
    sl.Free;
  end;
end;

end.
