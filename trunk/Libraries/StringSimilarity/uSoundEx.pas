unit uSoundEx;

interface


function SoundexKey(const AStr: String): String;
function SoundsSimilar(const AStr1, AStr2: String): Boolean;

implementation

uses
  SysUtils;

function SoundsSimilar(const AStr1, AStr2: String): Boolean;
begin
  Result:=SoundexKey(AStr1)=SoundexKey(AStr2);
end;

function SoundexKey(const AStr: String): String;
// The string for which the soundex should be calculated is
// passed via the AStr argument. Function returns the
// resulting soundex.

// The Soundex algorithm is based on an article by Rod Stephens
// in the March, 1998 issue of Delphi Informant magazine.

// unmodified by TUO ... umlauts are ignored
var
  no_vowels, coded, out_str: String;
  ch: Char;
  ii: Integer;
  myStr : String;
begin
  myStr := Trim(UpperCase(AStr));

  if Length(myStr) > 0 then begin
    no_vowels := myStr[1];
    for ii := 2 to Length(myStr) do begin
      ch := myStr[ii];
      case ch of
        'A','E','I','O','U',' ','H','W','Y':   ;  //do nothing
      else
        no_vowels := no_vowels + ch;
      end;
    end;

    coded := '';
    for ii := 1 to Length(no_vowels) do begin
      ch := no_vowels[ii];
      case ch of
        'B','F','P','V':                  ch := '1';
        'C','G','J','K','Q','S','X','Z':  ch := '2';
        'D','T':                          ch := '3';
        'L':                              ch := '4';
        'M','N':                          ch := '5';
        'R':                              ch := '6';
      else
        ch := '0';
      end;
      coded := coded + ch;
    end;

    out_str := no_vowels[1];

    for ii := 2 to Length(no_vowels) do begin
      if (coded[ii] <> coded[ii-1]) then begin
        out_str := out_str + coded[ii];
        if (Length(out_str) >= 4) then
          break;
      end;
    end;
    Result := out_str;
  end else
    Result := '';
end;


end.
