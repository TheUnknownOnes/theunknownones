unit uUtils;

interface

uses
  Types,
  ActiveX,
  SysUtils,
  Classes,
  Windows;

{$if not Defined(TBytes)}
type
  TBytes = array of Byte;

{$ifend}

{$if not Defined(PosStr)}
function PosStr(const subStr: string; const str: string; fromPos: Cardinal = 1;
  toPos: Cardinal = maxInt): Integer;
{$ifend}

{$if not Defined(PosText)}
function PosText(const subStr: string; const str: string; fromPos: Cardinal = 1;
  toPos: Cardinal = maxInt): Integer;
{$ifend}

function LeftToken(var s: string; const delim: string;
  const IgnoreCase: Boolean): string;
function RightToken(var s: string; const delim: string;
  const IgnoreCase: Boolean): string;

function SafeArrayToBytes(const si: PSafeArray): TBytes;
function SafeArrayToIntArray(const si: PSafeArray): TIntegerDynArray;
function SafeArrayToCardArray(const si: PSafeArray): TCardinalDynArray;

function BytesToIntArray(const b: TBytes): TIntegerDynArray;
function BytesToCardArray(const b: TBytes): TCardinalDynArray;

function FileContainsText(AFileName: string; AText: string): Boolean;

type
  TExpandEnvVarsOption  = (eevoPreferSystemValues);
  TExpandEnvVarsOptions = set of TExpandEnvVarsOption;

const
  ENVVAR_TOKEN_START = '$(';
  ENVVAR_TOKEN_END   = ')';

function EnvVarToken(const Name: string; const AStartToken: string = ENVVAR_TOKEN_START;
  AEndToken: string = ENVVAR_TOKEN_END): string;
function GetEnvironmentVariable(const Name: string; out Value: string): Boolean;
  overload;

procedure ExpandEnvVars(var AString: string; const AStartToken, AEndToken: string;
  const ACustomEnvVars: TStrings = nil; Options: TExpandEnvVarsOptions = []); overload;
procedure ExpandEnvVars(var AString: string; const ACustomEnvVars: TStrings = nil;
  Options: TExpandEnvVarsOptions = []); overload;

function CtrlDown: Boolean;
function ShiftDown: Boolean;
function AltDown: Boolean;

function AnsiStartsText(const ASubTexts: array of string; const AText: string): Boolean;
  overload;
{$IFNDEF UNICODE}
function AnsiStartsText(const ASubText: string; const AText: string): Boolean; overload;
{$ENDIF}

function GetTempPath: WideString; overload;
function CreateTempFileName(FileName: WideString): WideString; overload;
function GetTempFileName(prefix: WideString): WideString; overload;

procedure DeleteRegKey(aRoot: HKey; aPath: string);

implementation

uses
  StrUtils,
  Dialogs,
  Registry;

procedure DeleteRegKey(aRoot: HKey; aPath: string);
var
  SL: TStringList;
  X:  Integer;
begin
  SL := TStringList.Create;
  try
    with TRegistry.Create do
      try
        RootKey := aRoot;
        if OpenKey(aPath, False) then
        begin
          GetKeyNames(SL);
          for X := 0 to SL.Count - 1 do
            DeleteRegKey(aRoot, aPath + '\' + SL[X]);
          CloseKey;
          DeleteKey(aPath);
        end;
      finally
        Free;
      end;
  finally
    SL.Free
  end;
end;

function GetTempPath: WideString;
var
  i: Integer;
begin
  SetLength(Result, MAX_PATH);
  i := Windows.GetTempPathW(MAX_PATH, PWideChar(Result));
  SetLength(Result, i);
  if (Result <> '') and (Result[i] <> '\') then
    Result := Result + '\';
end;

function GetTempFileName(prefix: WideString): WideString;
var
  tempPath: WideString;
begin
  tempPath := GetTempPath;
  SetLength(Result, MAX_PATH);
  SetLength(Result, Windows.GetTempFileNameW(PWideChar(TempPath),
    PWideChar(prefix), 1, PWideChar(Result)));
end;

function CreateTempFileName(FileName: WideString): WideString;
var
  i, i2: Integer;
  F:     THandle;
begin
  if Pos('\', FileName) = 0 then
    FileName := GetTempPath + FileName;
  if Pos('*', FileName) = 0 then
    FileName := ChangeFileExt(FileName, '*.' +
      Copy(ExtractFileExt(FileName), 2, MAX_PATH));
  i := GetTickCount;
  i2 := i;
  repeat
    if i - i2 > 10000 then
      raise Exception.CreateFmt('Temporäre Datei "%s" konnte nicht erstellt werden.',
        [StringReplace(FileName, '*', '{UniqueID}', [])]);
    Result := StringReplace(FileName, '*', IntToHex(i, 8), []);
    F      := CreateFileW(PWideChar(Result), GENERIC_WRITE, 0, nil,
      CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, 0);
    Inc(i);
  until F <> INVALID_HANDLE_VALUE;
  CloseHandle(F);
end;

{$IFNDEF UNICODE}
function AnsiStartsText(const ASubText: string; const AText: string): Boolean; overload;
begin
  Result := StrUtils.AnsiStartsText(ASubText, AText);
end;

{$ENDIF}

function AnsiStartsText(const ASubTexts: array of string; const AText: string): Boolean;
  overload;
var
  idx: Integer;
begin
  Result := False;
  for idx := 0 to Length(ASubTexts) - 1 do
    if AnsiStartsText(ASubTexts[idx], AText) then
    begin
      Result := True;
      Break;
    end;
end;

function CtrlDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Control] and 128) <> 0);
end;

function ShiftDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Shift] and 128) <> 0);
end;

function AltDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Menu] and 128) <> 0);
end;


function GetEnvironmentVariable(const Name: string; out Value: string): Boolean;
const
  BufSize = 1024;
var
  Len:    Integer;
  Buffer: array[0..BufSize - 1] of Char;
begin
  Result := False;
  Len    := GetEnvironmentVariable(PChar(Name), @Buffer, BufSize);
  if GetLastError = ERROR_ENVVAR_NOT_FOUND then
    Exit;
  if Len < BufSize then
    SetString(Value, PChar(@Buffer), Len)
  else
  begin
    SetLength(Value, Len - 1);
    GetEnvironmentVariable(PChar(Name), PChar(Value), Len);
  end;
  Result := True;
end;

function EnvVarToken(const Name: string; const AStartToken: string;
  AEndToken: string): string;
begin
  Result := AStartToken + Name + AEndToken;
end;

procedure ExpandEnvVars(var AString: string; const ACustomEnvVars: TStrings = nil;
  Options: TExpandEnvVarsOptions = []);
begin
  ExpandEnvVars(AString, ENVVAR_TOKEN_START, ENVVAR_TOKEN_END, ACustomEnvVars, Options);
end;

procedure ExpandEnvVars(var AString: string; const AStartToken, AEndToken: string;
  const ACustomEnvVars: TStrings; Options: TExpandEnvVarsOptions);
var
  EnvVarStartIdx, EnvVarEndIdx: Integer;
  StartTokenLen, EndTokenLen: Integer;
  EnvVarName:     string;
  cevIndex:       Integer;

  SysValue:       string;
  CustomValue:    string;

  Value:          string;

  UseCustomValue: Boolean;
  UseSystemValue: Boolean;
begin
  StartTokenLen := Length(AStartToken);
  EndTokenLen   := Length(AEndToken);

  if (StartTokenLen = 0) or (EndTokenLen = 0) then
    raise Exception.Create('No start token or end token supplied');

  //find the first occurence of StartToken
  EnvVarStartIdx := Pos(AStartToken, AString);
  //yeah we found our start token
  while EnvVarStartIdx > 0 do
  begin
    //is there an end token, too?
    EnvVarEndIdx := PosEx(AEndToken, AString, EnvVarStartIdx);
    if EnvVarEndIdx > EnvVarStartIdx then
    begin
      //let's get the variable name
      EnvVarName := Copy(AString, EnvVarStartIdx + StartTokenLen,
        EnvVarEndIdx - EnvVarStartIdx - StartTokenLen);

      UseSystemValue := GetEnvironmentVariable(EnvVarName, SysValue);
      UseCustomValue := False;
      if Assigned(ACustomEnvVars) then
      begin
        cevIndex       := ACustomEnvVars.IndexOfName(EnvVarName);
        UseCustomValue := cevIndex >= 0;
        if UseCustomValue then
          CustomValue := ACustomEnvVars.ValueFromIndex[cevIndex];
      end;

      if UseSystemValue and (eevoPreferSystemValues in Options) then
        Value := SysValue
      else if UseCustomValue then
        Value := CustomValue
      else if UseSystemValue then
        Value := SysValue
      else
        Value := '';

      Delete(AString, EnvVarStartIdx, EndTokenLen + EnvVarEndIdx - EnvVarStartIdx);
      Insert(Value, AString, EnvVarStartIdx);
      Inc(EnvVarStartIdx, Length(AString) + 1); // do not replace variables recursivly

      EnvVarStartIdx := PosEx(AStartToken, AString, EnvVarStartIdx);
    end
    else
      break;  //we didn't find a matching end token... AString is broken
  end;
end;

{$if not Defined(PosStr) OR not Defined(PosText)}
function SearchBuf(const s: string; const delim: string;
  const IgnoreCase: Boolean; fromPos: Cardinal; toPos: Cardinal): Integer;
var
  rs:     PChar;
  opt:    TStringSearchOptions;
  i:      Cardinal;
  sl, st: Cardinal;
begin
  opt := [soDown];
  st  := 0;
  if not IgnoreCase then
    Include(opt, soMatchCase);
  if fromPos > toPos then
  begin
    Exclude(opt, soDown);
    i       := fromPos;
    fromPos := toPos;
    toPos   := i;
  end;
  if toPos > Cardinal(Length(s)) then
    toPos := Length(s);
  sl := toPos - fromPos + 1;
  if not (soDown in opt) then
    st := sl;

  rs := StrUtils.SearchBuf(@s[fromPos], sl, st, 0, delim, opt);
  if rs = nil then
    Result := 0
  else
    Result := rs - @s[fromPos] + 1;
end;

{$if not Defined(PosStr)}
function PosStr(const subStr: string; const str: string; fromPos: Cardinal = 1;
  toPos: Cardinal = maxInt): Integer;
begin
  Result := SearchBuf(str, subStr, False, fromPos, toPos);
end;

{$ifend}
{$if not Defined(PosText)}
function PosText(const subStr: string; const str: string; fromPos: Cardinal = 1;
  toPos: Cardinal = maxInt): Integer;
begin
  Result := SearchBuf(str, subStr, True, fromPos, toPos);
end;

{$ifend}
{$ifend}

function LeftToken(var s: string; const delim: string;
  const IgnoreCase: Boolean): string;
var
  idx: Integer;
begin
  if IgnoreCase then
    idx := PosText(delim, s, 1, MaxInt)
  else
    idx := PosStr(delim, s, 1, MaxInt);
  if idx < 1 then
  begin
    Result := s;
    s      := '';
  end
  else
  begin
    Result := Copy(s, 1, idx - 1);
    Delete(s, 1, idx - 1 + Length(delim));
  end;
end;

function RightToken(var s: string; const delim: string;
  const IgnoreCase: Boolean): string;
var
  idx: Integer;
begin
  if IgnoreCase then
    idx := PosText(delim, s, MaxInt, 1)
  else
    idx := PosStr(delim, s, MaxInt, 1);
  if idx < 1 then
  begin
    Result := s;
    s      := '';
  end
  else
  begin
    Result := Copy(s, idx + Length(delim));
    Delete(s, idx, MaxInt);
  end;
end;

function SafeArrayToBytes(const si: PSafeArray): TBytes;
var
  nLow, nHigh, nSize: Integer;
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
  newLen:   Integer;
  elemSize: Integer;
begin
  elemSize := SizeOf(Integer);
  newLen   := (Length(b) + elemSize - 1) div elemSize;
  SetLength(Result, newLen);
  ZeroMemory(@Result[newLen - 1], elemSize);
  CopyMemory(@Result[1], @b[1], Length(b));
end;

function BytesToCardArray(const b: TBytes): TCardinalDynArray;
var
  newLen:   Integer;
  elemSize: Integer;
begin
  elemSize := SizeOf(Cardinal);
  newLen   := (Length(b) + elemSize - 1) div elemSize;
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

function FileContainsText(AFileName: string; AText: string): Boolean;
var
  MS:          TMemoryStream;
  TextBuffer:  PAnsiChar;
  TextLen:     Integer;
  FileBuffer:  PAnsiChar;
  FileBufferLastChar: PAnsiChar;
  PosInMemory: Integer;

  procedure LowerLastChar();
  begin
    if FileBufferLastChar^ in ['A'..'Z'] then
      FileBufferLastChar^ := AnsiChar(Ord(FileBufferLastChar^) + $20);
  end;

begin
  Result := False;

  Ms := TMemoryStream.Create;
  Ms.LoadFromFile(AFileName);

  TextLen := Length(AText);
  GetMem(TextBuffer, TextLen);
  try
    StrPCopy(TextBuffer, Ansistring(LowerCase(AText)));

    FileBufferLastChar := Ms.Memory;
    for PosInMemory := 0 to TextLen - 2 do
    begin
      LowerLastChar;
      Inc(FileBufferLastChar);
    end;

    FileBuffer         := Ms.Memory;
    PosInMemory        := TextLen - 1;
    FileBufferLastChar := Pointer(Integer(FileBuffer) + PosInMemory);

    while PosInMemory < Ms.Size do
    begin
      LowerLastChar;

      if CompareMem(TextBuffer, FileBuffer, TextLen) then
      begin
        Result := True;
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
