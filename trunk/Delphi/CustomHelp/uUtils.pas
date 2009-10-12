unit uUtils;

interface

uses
  Types, ActiveX, SysUtils, Classes;

type
  TBytes = array of Byte;

{$if not Defined(PosStr)}
function PosStr(const subStr: string; const str: string; fromPos: cardinal = 1;
  toPos: cardinal = maxInt): integer;
{$ifend}

{$if not Defined(PosText)}
function PosText(const subStr: string; const str: string; fromPos: cardinal = 1;
  toPos: cardinal = maxInt): integer;
{$ifend}

function SafeArrayToBytes(const si: PSafeArray): TBytes;
function SafeArrayToIntArray(const si: PSafeArray): TIntegerDynArray;
function SafeArrayToCardArray(const si: PSafeArray): TCardinalDynArray;

function BytesToIntArray(const b: TBytes): TIntegerDynArray;
function BytesToCardArray(const b: TBytes): TCardinalDynArray;

function FileContainsText(AFileName : String; AText : AnsiString) : Boolean;

type
  TExpandEnvVarsOption = (eevoPreferSystemValues);
  TExpandEnvVarsOptions = set of TExpandEnvVarsOption;

procedure ExpandEnvVars(var AString : String; const AStartToken, AEndToken: String; const ACustomEnvVars: TStrings = nil; Options: TExpandEnvVarsOptions = []);

implementation

uses
  StrUtils, Windows, Dialogs;
           
procedure ExpandEnvVars(var AString : String; const AStartToken, AEndToken: String; const ACustomEnvVars: TStrings; Options: TExpandEnvVarsOptions);
var
  EnvVarStartIdx,
  EnvVarEndIdx   : Integer;
  StartTokenLen,
  EndTokenLen   : Integer;
  EnvVarName     : String;
  cevIndex : Integer;

  Buffer : Array [0..255] of Char;
  SysValue : String;
  CustomValue : String;

  Value : String;

  UseCustomValue: Boolean;
  UseSystemValue: Boolean;
begin
  UseCustomValue:=False;
  UseSystemValue:=False;

  StartTokenLen:=Length(AStartToken);
  EndTokenLen:=Length(AEndToken);

  if (StartTokenLen=0) or (EndTokenLen=0) then
    raise Exception.Create('No start token or end token supplied');

  //find the first occurence of StartToken
  EnvVarStartIdx:=Pos(AStartToken, AString);
  //yeah we found our start token
  while EnvVarStartIdx>0 do
  begin
    //is there an end token, too?
    EnvVarEndIdx:=PosEx(AEndToken, AString, EnvVarStartIdx);
    if EnvVarEndIdx>EnvVarStartIdx then
    begin
      //let's get the variable name
      EnvVarName:=Copy(AString, EnvVarStartIdx+StartTokenLen, EnvVarEndIdx-EnvVarStartIdx-StartTokenLen);

      UseSystemValue:=GetEnvironmentVariable(PChar(EnvVarName), PChar(@Buffer), Length(Buffer))>0;
      SysValue:=Buffer;
      if Assigned(ACustomEnvVars) then
      begin
        cevIndex:=ACustomEnvVars.IndexOfName(EnvVarName);
        if cevIndex>=0 then
        begin
          CustomValue:=ACustomEnvVars.ValueFromIndex[cevIndex];
          UseCustomValue:=True;
        end;
      end;

      if eevoPreferSystemValues in Options then
      begin
        if UseSystemValue then
          Value:=SysValue
        else
        if UseCustomValue then
          Value:=CustomValue
        else
          Value:='';
      end
      else
      begin
        if UseCustomValue then
          Value:=CustomValue
        else
        if UseSystemValue then
          Value:=SysValue
        else
          Value:='';
      end;

      Delete(AString, EnvVarStartIdx, EndTokenLen+EnvVarEndIdx-EnvVarStartIdx);
      Insert(Value, AString, EnvVarStartIdx);

      EnvVarStartIdx:=Pos(AStartToken, AString);
    end
    else
      break;  //we didn't find a matching end token... AString is broken
  end;
end;

{$if not Defined(PosStr) OR not Defined(PosText)}
function SearchBuf(const s: String; const delim: String;
  const IgnoreCase: boolean; fromPos: cardinal; toPos: cardinal): integer;
var
  rs: PChar;
  opt: TStringSearchOptions;
  i:  cardinal;
  sl,st: cardinal;
begin
  opt := [soDown];
  st := 0;
  if NOT IgnoreCase then
    Include(opt, soMatchCase);
  if fromPos > toPos then
  begin
    Exclude(opt, soDown);
    i := fromPos;
    fromPos := toPos;
    toPos := i;
  end;
  if toPos > cardinal(Length(s)) then
    toPos := Length(s);
  sl := toPos - fromPos + 1;
  if not (soDown in opt) then
    st := sl;

  rs := StrUtils.SearchBuf(@s[fromPos], sl, st, 0, delim, opt);
  if rs = NIL then
    Result := 0
  else
    Result := rs - @s[fromPos] + 1;
end;

{$if not Defined(PosStr)}
function PosStr(const subStr: string; const str: string; fromPos: cardinal = 1;
  toPos: cardinal = maxInt): integer;
begin
  Result := SearchBuf(str, subStr, False, fromPos, toPos);
end;

{$ifend}
{$if not Defined(PosText)}
function PosText(const subStr: string; const str: string; fromPos: cardinal = 1;
  toPos: cardinal = maxInt): integer;
begin
  Result := SearchBuf(str, subStr, True, fromPos, toPos);
end;

{$ifend}
{$ifend}

function SafeArrayToBytes(const si: PSafeArray): TBytes;
var
  nLow, nHigh, nSize: integer;
  pData: Pointer;
begin
  //Copy from Variant Array to Delphi array
  SafeArrayGetLBound(si, 1, nLow);
  SafeArrayGetUBound(si, 1, nHigh);
  nSize := SafeArrayGetElemsize(si);
  SetLength(Result, nSize * (nHigh - nLow + 1));

  SafeArrayAccessData(si, pData);
  CopyMemory(@Result[1], pData, Length(Result));
  SafeArrayUnaccessData(si);
end;

function BytesToIntArray(const b: TBytes): TIntegerDynArray;
var
  newLen: integer;
  elemSize: integer;
begin
  elemSize := SizeOf(integer);
  newLen := (Length(b) + elemSize - 1) DIV elemSize;
  SetLength(Result, newLen);
  ZeroMemory(@Result[newLen - 1], elemSize);
  CopyMemory(@Result[1], @b[1], Length(b));
end;

function BytesToCardArray(const b: TBytes): TCardinalDynArray;
var
  newLen: integer;
  elemSize: integer;
begin
  elemSize := SizeOf(cardinal);
  newLen := (Length(b) + elemSize - 1) DIV elemSize;
  SetLength(Result, newLen);
  ZeroMemory(@Result[newLen - 1], elemSize);
  CopyMemory(@Result[1], @b[1], Length(b));
end;

function SafeArrayToIntArray(const si: PSafeArray): TIntegerDynArray;
begin
  Result := BytesToIntArray(SafeArrayToBytes(si));
end;

function SafeArrayToCardArray(const si: PSafeArray): TCardinalDynArray;
begin
  Result := BytesToCardArray(SafeArrayToBytes(si));
end;

function FileContainsText(AFileName: String; AText: AnsiString): Boolean;
var
  MS : TMemoryStream;
  TextBuffer : PAnsiChar;
  TextLen : Integer;
  FileBuffer : PAnsiChar;
  FileBufferLastChar : PAnsiChar;
  PosInMemory : Integer;

  procedure LowerLastChar();
  begin
    if FileBufferLastChar^ in ['A'..'Z'] then
      FileBufferLastChar^ := AnsiChar(Ord(FileBufferLastChar^) + $20);
  end;
begin
  Result := false;

  Ms := TMemoryStream.Create;
  Ms.LoadFromFile(AFileName);

  TextLen := Length(AText);
  GetMem(TextBuffer, TextLen);
  try
    StrPCopy(TextBuffer, AnsiString(LowerCase(String(AText))));

    FileBufferLastChar := Ms.Memory;
    for PosInMemory := 0 to TextLen - 2 do
    begin
      LowerLastChar;
      Inc(FileBufferLastChar);
    end;

    FileBuffer := Ms.Memory;
    PosInMemory := TextLen - 1;
    FileBufferLastChar := Pointer(Integer(FileBuffer) + PosInMemory);

    while PosInMemory < Ms.Size do
    begin
      LowerLastChar;

      if CompareMem(TextBuffer, FileBuffer, TextLen) then
      begin
        Result := true;
        break;
      end;

      Inc(PosInMemory);
      Inc(FileBuffer);
      Inc(FileBufferLastChar);
    end;
    

  finally
    Ms.Free;
    FreeMem(TextBuffer, TextLen);
  end;
end;

end.
