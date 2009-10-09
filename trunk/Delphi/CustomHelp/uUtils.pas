unit uUtils;

interface

uses
  Types,
  ActiveX,
  SysUtils;

type
  TBytes = array of Byte;


function LeftToken(var s: string; const delim: string;
  const IgnoreCase: boolean): string;

function RightToken(var s: string; const delim: string;
  const IgnoreCase: boolean): string;

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

implementation

uses
  StrUtils,
  Windows;

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

function LeftToken(var s: string; const delim: string;
  const IgnoreCase: boolean): string;
var
  idx: integer;
begin
  if IgnoreCase then
    idx := PosText(delim, s, 1, MaxInt)
  else
    idx := PosStr(delim, s, 1, MaxInt);
  if idx < 1 then
  begin
    Result := s;
    s := '';
  end
  else
  begin
    Result := Copy(s, 1, idx - 1);
    Delete(s, 1, idx - 1 + Length(delim));
  end;
end;

function RightToken(var s: string; const delim: string;
  const IgnoreCase: boolean): string;
var
  idx: integer;
begin
  if IgnoreCase then
    idx := PosText(delim, s, MaxInt, 1)
  else
    idx := PosStr(delim, s, MaxInt, 1);
  if idx < 1 then
  begin
    Result := s;
    s := '';
  end
  else
  begin
    Result := Copy(s, idx + Length(delim));
    Delete(s, idx, MaxInt);
  end;
end;

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

end.
