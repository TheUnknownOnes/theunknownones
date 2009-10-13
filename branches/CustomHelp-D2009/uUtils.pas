UNIT uUtils;

INTERFACE

USES
  Types,
  ActiveX,
  SysUtils;

FUNCTION LeftToken(VAR s: string; CONST delim: string;
  CONST IgnoreCase: boolean): string;

FUNCTION RightToken(VAR s: string; CONST delim: string;
  CONST IgnoreCase: boolean): string;

{$IF not Defined(PosStr)}
FUNCTION PosStr(CONST subStr: string; CONST str: string; fromPos: cardinal = 1;
  toPos: cardinal = maxInt): integer;
{$IFEND}

{$IF not Defined(PosText)}
FUNCTION PosText(CONST subStr: string; CONST str: string; fromPos: cardinal = 1;
  toPos: cardinal = maxInt): integer;
{$IFEND}

FUNCTION SafeArrayToBytes(CONST si: PSafeArray): TBytes;
FUNCTION SafeArrayToIntArray(CONST si: PSafeArray): TIntegerDynArray;
FUNCTION SafeArrayToCardArray(CONST si: PSafeArray): TCardinalDynArray;

FUNCTION BytesToIntArray(CONST b: TBytes): TIntegerDynArray;
FUNCTION BytesToCardArray(CONST b: TBytes): TCardinalDynArray;

IMPLEMENTATION

USES
  StrUtils,
  Windows;

{$IF not Defined(PosStr) OR not Defined(PosText)}
FUNCTION SearchBuf(CONST s: WideString; CONST delim: WideString;
  CONST IgnoreCase: boolean; fromPos: cardinal; toPos: cardinal): integer;
VAR
  rs: PWideChar;
  opt: TStringSearchOptions;
  i:  cardinal;
  sl,st: cardinal;
BEGIN
  opt := [soDown];
  st := 0;
  IF NOT IgnoreCase THEN
    Include(opt, soMatchCase);
  IF fromPos > toPos THEN
  BEGIN
    Exclude(opt, soDown);
    i := fromPos;
    fromPos := toPos;
    toPos := i;
  END;
  IF toPos > cardinal(Length(s)) THEN
    toPos := Length(s);
  sl := toPos - fromPos + 1;
  if not (soDown in opt) then
    st := sl;

  rs := StrUtils.SearchBuf(@s[fromPos], sl, st, 0, delim, opt);
  IF rs = NIL THEN
    Result := 0
  ELSE
    Result := rs - @s[fromPos] + 1;
END;

{$IF not Defined(PosStr)}
FUNCTION PosStr(CONST subStr: string; CONST str: string; fromPos: cardinal = 1;
  toPos: cardinal = maxInt): integer;
BEGIN
  Result := SearchBuf(str, subStr, False, fromPos, toPos);
END;

{$IFEND}
{$IF not Defined(PosText)}
FUNCTION PosText(CONST subStr: string; CONST str: string; fromPos: cardinal = 1;
  toPos: cardinal = maxInt): integer;
BEGIN
  Result := SearchBuf(str, subStr, True, fromPos, toPos);
END;

{$IFEND}
{$IFEND}

FUNCTION LeftToken(VAR s: string; CONST delim: string;
  CONST IgnoreCase: boolean): string;
VAR
  idx: integer;
BEGIN
  IF IgnoreCase THEN
    idx := PosText(delim, s, 1, MaxInt)
  ELSE
    idx := PosStr(delim, s, 1, MaxInt);
  IF idx < 1 THEN
  BEGIN
    Result := s;
    s := '';
  END
  ELSE
  BEGIN
    Result := Copy(s, 1, idx - 1);
    Delete(s, 1, idx - 1 + Length(delim));
  END;
END;

FUNCTION RightToken(VAR s: string; CONST delim: string;
  CONST IgnoreCase: boolean): string;
VAR
  idx: integer;
BEGIN
  IF IgnoreCase THEN
    idx := PosText(delim, s, MaxInt, 1)
  ELSE
    idx := PosStr(delim, s, MaxInt, 1);
  IF idx < 1 THEN
  BEGIN
    Result := s;
    s := '';
  END
  ELSE
  BEGIN
    Result := Copy(s, idx + Length(delim));
    Delete(s, idx, MaxInt);
  END;
END;

FUNCTION SafeArrayToBytes(CONST si: PSafeArray): TBytes;
VAR
  nLow, nHigh, nSize: integer;
  pData: Pointer;
BEGIN
  //Copy from Variant Array to Delphi array
  SafeArrayGetLBound(si, 1, nLow);
  SafeArrayGetUBound(si, 1, nHigh);
  nSize := SafeArrayGetElemsize(si);
  SetLength(Result, nSize * (nHigh - nLow + 1));

  SafeArrayAccessData(si, pData);
  CopyMemory(@Result[1], pData, Length(Result));
  SafeArrayUnaccessData(si);
END;

FUNCTION BytesToIntArray(CONST b: TBytes): TIntegerDynArray;
VAR
  newLen: integer;
  elemSize: integer;
BEGIN
  elemSize := SizeOf(integer);
  newLen := (Length(b) + elemSize - 1) DIV elemSize;
  SetLength(Result, newLen);
  ZeroMemory(@Result[newLen - 1], elemSize);
  CopyMemory(@Result[1], @b[1], Length(b));
END;

FUNCTION BytesToCardArray(CONST b: TBytes): TCardinalDynArray;
VAR
  newLen: integer;
  elemSize: integer;
BEGIN
  elemSize := SizeOf(cardinal);
  newLen := (Length(b) + elemSize - 1) DIV elemSize;
  SetLength(Result, newLen);
  ZeroMemory(@Result[newLen - 1], elemSize);
  CopyMemory(@Result[1], @b[1], Length(b));
END;

FUNCTION SafeArrayToIntArray(CONST si: PSafeArray): TIntegerDynArray;
BEGIN
  Result := BytesToIntArray(SafeArrayToBytes(si));
END;

FUNCTION SafeArrayToCardArray(CONST si: PSafeArray): TCardinalDynArray;
BEGIN
  Result := BytesToCardArray(SafeArrayToBytes(si));
END;

END.
