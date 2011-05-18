unit uLevenshtein;

interface

function DamerauLevenshteinDistance(const Str1, Str2: String): Integer;
function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;

implementation

uses
  SysUtils;

function DamerauLevenshteinDistance(const Str1, Str2: String): Integer;
  function Min(const A, B, C: Integer): Integer; inline;
  begin
    Result := A;
    if B < A then
      Result := B;
    if C < Result then
      Result := C;
  end;

var
  LenStr1, LenStr2: Integer;
  I, J, T, Cost, PrevCost: Integer;
  pStr1, pStr2, S1, S2: PChar;
  D: PIntegerArray;
begin
  LenStr1 := Length(Str1);
  LenStr2 := Length(Str2);

  // to save some space, make sure the second index points to the shorter string
  if LenStr1 < LenStr2 then
  begin
    T := LenStr1;
    LenStr1 := LenStr2;
    LenStr2 := T;
    pStr1 := PChar(Str2);
    pStr2 := PChar(Str1);
  end
  else
  begin
    pStr1 := PChar(Str1);
    pStr2 := PChar(Str2);
  end;

  // to save some time and space, look for exact match
  while (LenStr2 <> 0) and (pStr1^ = pStr2^) do
  begin
    Inc(pStr1);
    Inc(pStr2);
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  while (LenStr2 <> 0) and ((pStr1+LenStr1-1)^ = (pStr2+LenStr2-1)^) do
  begin
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  if LenStr2 = 0 then
  begin
    Result := LenStr1;
    Exit;
  end;

  // calculate the edit distance
  T := LenStr2 + 1;

  GetMem(D, T * SizeOf(Integer));

  for I := 0 to T-1 do
    D[I] := I;

  S1 := pStr1;
  for I := 1 to LenStr1 do
  begin
    PrevCost := I-1;
    Cost := I;
    S2 := pStr2;
    for J := 1 to LenStr2 do
    begin
      if (S1^ = S2^) or ((I > 1) and (J > 1) and (S1^ = (S2 - 1)^) and (S2^ = (S1 - 1)^)) then
        Cost := PrevCost
      else
        Cost := 1 + min(Cost, PrevCost, D[J]);
      PrevCost := D[J];
      D[J] := Cost;
      Inc(S2);
    end;
    Inc(S1);
  end;
  Result := D[LenStr2];
  FreeMem(D);
end;



function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
var
  MaxLen: Integer;
  Distance: Integer;
begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);
  if MaxLen <> 0 then
  begin
    if IgnoreCase then
      Distance := DamerauLevenshteinDistance(LowerCase(Str1), LowerCase(Str2))
    else
      Distance := DamerauLevenshteinDistance(Str1, Str2);
    Result := Result - (Distance / MaxLen);
  end;
end;

end.
