library KoelnerPhonetik;

{
DECLARE EXTERNAL FUNCTION koelnerphonetikequals
varchar(32676) null,
varchar(32676) null
RETURNS integer
ENTRY_POINT 'kphonetikeq' MODULE_NAME 'koelnerphonetik';

DECLARE EXTERNAL FUNCTION koelnerphonetikcontains
varchar(32676) null,
varchar(32676) null
RETURNS integer
ENTRY_POINT 'kphonetikcont' MODULE_NAME 'koelnerphonetik';
}


{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  uFBParamDescription,
  uIB2007;

{$R *.res}

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;


function _KoelnerKey(const AStr: String): String;
//Phonetische Suche nach Postel
//implementation according to Marc123
//taken from: http://forum.delphi-treff.de/showthread.php?28726-K%F6lner-Phonetik
const
  alphabet = ['a'..'z', #$E4{'ä'}, #$F6{'ö'}, #$FC{'ü'}, #$DF{'ß'}];
var
  key, s: String;
  pc, cc, nc, lc: char;
  i, p, l: Integer;

  function getcode: String;
  begin
    Result := '';
    if charinset(cc, ['a', 'e', 'i', 'j', 'o', 'u', 'y', #$E4{'ä'}, #$F6{'ö'}, #$FC{'ü'}]) then
      Result := '0'
    else if cc = 'h' then
      Result := ''
    else if cc = 'b' then
      Result := '1'
    else if charinset(cc, ['f', 'v', 'w']) then
      Result := '3'
    else if charinset(cc, ['g', 'k', 'q']) then
      Result := '4'
    else if cc = 'l' then
      Result := '5'
    else if charinset(cc, ['m', 'n']) then
      Result := '6'
    else if cc = 'r' then
      Result := '7'
    else if charinset(cc, ['s', 'z', #$DF{'ß'}]) then
      Result := '8'
    else if cc = 'c' then
    begin
      if pc = #0 then // Anlaut
      begin
        if charinset(nc, ['a', 'h', 'k', 'l', 'o', 'q', 'r', 'u', 'x']) then
          Result := '4'
        else
          Result := '8';
      end
      else
      begin
        if charinset(pc, ['s', 'z', #$DF{'ß'}]) then
          Result := '8'
        else if charinset(nc, ['a', 'h', 'k', 'o', 'q', 'u', 'x']) then
          Result := '4'
        else
          Result := '8'
      end;
    end
    else if charinset(cc, ['d', 't']) then
    begin
      if charinset(nc, ['c', 's', 'z', #$DF{'ß'}]) then
        Result := '8'
      else
        Result := '2';
    end
    else if cc = 'p' then
    begin
      if nc = 'h' then
        Result := '3'
      else
        Result := '1';
    end
    else if cc = 'x' then
    begin
      if charinset(pc, ['c', 'k', 'q']) then
        Result := '8'
      else
        Result := '48';
    end;
  end;

  procedure Shift;
  begin
    pc := cc;
    cc := nc;
    while (p <= l) and not charinset(s[p], alphabet) do
      inc(p);
    if p <= l then
    begin
      nc := s[p];
      inc(p)
    end
    else
      nc := #0;
  end;

begin
  pc := #0; // previous char
  cc := #0; // current char
  nc := #0; // next char
  key := '';

  p := 1;
  s := Lowercase(AStr);
  l := Length(s);

  Shift;
  Shift;
  while cc <> #0 do
  begin
    key := key + getcode;
    Shift;
  end;

  // doppelte und '0' löschen, Anlaut bestehen lassen
  Result := '';
  if key <> '' then
  begin
    Result := Result + key[1];
    lc := key[1];
    for i := 2 to Length(key) do
      if (key[i] <> lc) and (key[i] <> '0') then
      begin
        Result := Result + key[i];
        lc := key[i];
      end;
  end;
end;

function VarcharParamToString(AParam : PParamVary) : String;
begin
  SetLength(Result, AParam^.vary_length);
  Move(AParam^.vary_string, Result[1], AParam^.vary_length);
end;

function kphonetikeq(AString1, AString2 : PParamVary) : PInteger; export; cdecl;
begin
  if (Assigned(AString1)) and
     (Assigned(AString2)) then
  begin
    Result := ib_util_malloc(SizeOf(Integer));

    if _KoelnerKey(VarcharParamToString(AString1))=_KoelnerKey(VarcharParamToString(AString2)) then
      Result^ := 1
    else
      Result^ := 0;
  end
  else
  begin
    Result := nil;
  end;
end;

function kphonetikcont(AString1, AString2 : PParamVary) : PInteger; export; cdecl;
var
  i : Integer;
  sl: TStringList;
  testWord : String;
begin
  if (Assigned(AString1)) and
     (Assigned(AString2)) then
  begin
    Result := ib_util_malloc(SizeOf(Integer));
    Result^ := 0;

    sl:=TStringList.Create;
    sl.Text:=StringReplace(VarcharParamToString(AString1), ' ', #13#10, [rfReplaceAll]);
    testWord:=VarcharParamToString(AString2);

    for i:=0 to sl.Count-1 do
    begin
      if _KoelnerKey(sl[i])=_KoelnerKey(testWord) then
      begin
        Result^ := 1;
        break;
      end;
    end;

    sl.Free;
  end
  else
  begin
    Result := nil;
  end;
end;

exports
  kphonetikeq, kphonetikcont;

begin
  IsMultiThread := true;
end.

