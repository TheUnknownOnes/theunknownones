//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uPosCrypt;

interface

uses
  Classes, SysUtils, Math;

type
  TPosCryptKey = array of AnsiChar;

procedure PosCrypt_Encode(const AData, AEncryptedData : TStream; AKey : TPosCryptKey); overload;
procedure PosCrypt_Encode(const AData : String; out AEncryptedData : String; AKey : TPosCryptKey); overload;

procedure PosCrypt_Decode(const AEncryptedData, AData : TStream; AKey : TPosCryptKey); overload;
procedure PosCrypt_Decode(const AEncryptedData : String; out AData : String; AKey : TPosCryptKey); overload;

function PosCrypt_KeyFromString(AString : AnsiString) : TPosCryptKey;

implementation

procedure CheckKey(AKey : TPosCryptKey); inline;
begin
  Assert(Length(AKey) >= 2, 'The key should not be shorter then 2 characters');
  Assert(Length(AKey) <= 256, 'The key should not be longer then 256 characters');
end;

function CalcCharsPerByte(AKeyLen : Cardinal) : Cardinal; inline;
var
  sum : Int64;
  chars : Cardinal;
  maxcardinal : Cardinal;
begin
  sum := 0;
  chars := 0;
  maxcardinal := Trunc(IntPower(2, 32) - 1);

  while sum < maxcardinal do
  begin
    sum := sum + (AKeyLen - 1) * Trunc(IntPower(AKeyLen, chars));
    inc(chars);
  end;

  Result := Ceil(Chars / 4);
end;

procedure Encode(ABuffer : Cardinal;
                 AResult : PAnsiChar;
                 const AKey : TPosCryptKey;
                 const AKeyLen : Cardinal); inline;
begin
  while ABuffer > 0 do
  begin
    AResult^ := AKey[ABuffer mod AKeyLen];
    Inc(AResult);
    ABuffer := ABuffer div AKeyLen;
  end;
end;

procedure PosCrypt_Encode(const AData, AEncryptedData : TStream; AKey : TPosCryptKey);
var
  InBuffer : Cardinal;
  OutBuffer : PAnsiChar;
  OutBufferSize,
  CharsPerByte : Cardinal;
  KeyLen : Cardinal;
  DataLen : Cardinal;
begin
  Assert(AData <> AEncryptedData, 'Data and EncryptedData should not be the same object');
  CheckKey(AKey);

  AEncryptedData.Seek(0, soFromBeginning);
  AData.Seek(0, soFromBeginning);

  KeyLen := Length(AKey);
  CharsPerByte := CalcCharsPerByte(KeyLen);
  OutBufferSize := CharsPerByte * SizeOf(InBuffer);
  GetMem(OutBuffer, OutBufferSize);
  try

    repeat

      InBuffer := 0;
      DataLen := AData.Read(InBuffer, SizeOf(InBuffer));
      FillChar(OutBuffer^, OutBufferSize, Akey[0]);
      Encode(InBuffer, OutBuffer, AKey, KeyLen);
      AEncryptedData.Write(OutBuffer^, CharsPerByte * DataLen);

    until AData.Position = AData.Size;

  finally
    FreeMem(OutBuffer, OutBufferSize);
  end;
end;

procedure PosCrypt_Encode(const AData : String; out AEncryptedData : String; AKey : TPosCryptKey);
var
  sFrom,
  sTo : TStringStream;
begin
  sFrom := TStringStream.Create;
  sTo := TStringStream.Create;
  try
    sFrom.WriteString(AData);
    PosCrypt_Encode(sFrom, sTo, AKey);
    AEncryptedData := sTo.DataString;
  finally
    sFrom.Free;
    sTo.Free;
  end;
end;

function PosOfCharInKey(AChar : AnsiChar; const AKey : TPosCryptKey) : Cardinal; inline;
var
  idx : Cardinal;
begin
  Result := 0;

  for idx := Low(AKey) to High(AKey) do
  begin
    if AKey[idx] = AChar then
    begin
      Result := idx;
      break;
    end;
  end;

end;

procedure Decode(ABuffer : PAnsiChar;
                 ADataLen : Cardinal;
                 out AResult : Cardinal;
                 const AKey : TPosCryptKey;
                 const AKeyLen : Cardinal); inline;
var
  p : Cardinal;
begin
  p := 0;
  AResult := 0;

  while p < ADataLen do
  begin
    Inc(AResult, PosOfCharInKey(ABuffer^, AKey) * Trunc(IntPower(AKeyLen, p)));
    Inc(ABuffer);
    Inc(p);
  end;
end;

procedure PosCrypt_Decode(const AEncryptedData, AData : TStream; AKey : TPosCryptKey);
var
  InBuffer : PAnsiChar;
  OutBuffer : Cardinal;
  InBufferSize,
  CharsPerByte : Cardinal;
  KeyLen : Cardinal;
  DataLen : Cardinal;
begin
  Assert(AData <> AEncryptedData, 'Data and EncryptedData should not be the same object');
  CheckKey(AKey);

  AEncryptedData.Seek(0, soFromBeginning);
  AData.Seek(0, soFromBeginning);

  KeyLen := Length(AKey);
  CharsPerByte := CalcCharsPerByte(KeyLen);
  InBufferSize := CharsPerByte * SizeOf(OutBuffer);
  GetMem(InBuffer, InBufferSize);
  try

    repeat
      FillChar(InBuffer^, InBufferSize, AKey[0]);
      DataLen := AEncryptedData.Read(InBuffer^, InBufferSize);
      Decode(InBuffer, DataLen, OutBuffer, AKey, KeyLen);
      AData.Write(OutBuffer, DataLen div CharsPerByte);

    until AEncryptedData.Position = AEncryptedData.Size;

  finally
    FreeMem(InBuffer, InBufferSize);
  end;
end;

procedure PosCrypt_Decode(const AEncryptedData : String; out AData : String; AKey : TPosCryptKey); overload;
var
  sFrom,
  sTo : TStringStream;
begin
  sFrom := TStringStream.Create;
  sTo := TStringStream.Create;
  try
    sFrom.WriteString(AEncryptedData);
    PosCrypt_Decode(sFrom, sTo, AKey);
    AData := sTo.DataString;
  finally
    sFrom.Free;
    sTo.Free;
  end;
end;

function KeyContainsChar(const AKey : TPosCryptKey; AChar : AnsiChar) : Boolean; inline;
var
  c : AnsiChar;
begin
  Result := false;

  for c in AKey do
  begin
    if c = AChar then
    begin
      Result := true;
      break;
    end;
  end;

end;

function PosCrypt_KeyFromString(AString : AnsiString) : TPosCryptKey;
var
  idx,
  idy : Integer;
begin
  SetLength(Result, Length(AString));

  idy := 0;
  for idx := 1 to Length(AString) do
  begin
    if not KeyContainsChar(Result, AString[idx]) then
    begin
      Result[idy] := AString[idx];
      Inc(idy);
    end;
  end;

  SetLength(Result, idy);
end;

end.
