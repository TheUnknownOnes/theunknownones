unit uKoelnerPhonetik;

interface

function KoelnerKey(const AStr: String): String;
function SoundsSimilar(const AStr1, AStr2: String): Boolean;

implementation

uses
  SysUtils;


function SoundsSimilar(const AStr1, AStr2: String): Boolean;
begin
  Result:=KoelnerKey(AStr1)=KoelnerKey(AStr2);
end;

function KoelnerKey(const AStr: String): String;
//Phonetische Suche nach Postel
//implementation according to Marc123
//taken from: http://forum.delphi-treff.de/showthread.php?28726-K%F6lner-Phonetik
const
  alphabet = ['a' .. 'z', 'ä', 'ö', 'ü', 'ß'];
var
  key, s: String;
  pc, cc, nc, lc: char;
  i, p, l: Integer;

  function getcode: String;
  begin
    Result := '';
    if charinset(cc, ['a', 'e', 'i', 'j', 'o', 'u', 'y', 'ä', 'ö', 'ü']) then
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
    else if charinset(cc, ['s', 'z', 'ß']) then
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
        if charinset(pc, ['s', 'z', 'ß']) then
          Result := '8'
        else if charinset(nc, ['a', 'h', 'k', 'o', 'q', 'u', 'x']) then
          Result := '4'
        else
          Result := '8'
      end;
    end
    else if charinset(cc, ['d', 't']) then
    begin
      if charinset(nc, ['c', 's', 'z', 'ß']) then
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

end.
