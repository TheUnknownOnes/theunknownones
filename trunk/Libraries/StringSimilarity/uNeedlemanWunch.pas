unit uNeedlemanWunch;

interface

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
function NeedlemanWunchDistance(const Str1, Str2: String): Integer;

implementation

uses
  SysUtils, Math;

const
  gapCost = 2;

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
var
  needlemanWunch : Double;
  maxVal, minVal : Double;
begin
  if IgnoreCase then
    needlemanWunch := NeedlemanWunchDistance(AnsiUpperCase(Str1), AnsiUpperCase(Str2))
  else
    needlemanWunch := NeedlemanWunchDistance(Str1, Str2);

  //normalise into zero to one region from min max possible
  maxVal := Max(Length(str1), Length(Str2)) * MaxValue([gapCost, 1]);
  minVal := Max(Length(str1), Length(Str2)) * MinValue([gapCost, 0]);

  if minVal < 0 then
  begin
    maxVal:=maxVal-minVal;
    needlemanWunch:=needlemanWunch-minVal;
  end;

  if maxVal=0 then
    Result:=1  //as both strings identically zero length
  else
    Result:= 1 - (needlemanWunch / maxVal);
end;

function NeedlemanWunchDistance(const Str1, Str2: String): Integer;
var
  s, t : String;
  d : array of array of Integer;
  n : Integer;  // length of s
  m : Integer; // length of t
  i : Integer; // iterates through s
  j : Integer; // iterates through t
  cost : Integer; // cost

begin
  s:=Str1;
  t:=Str2;

  n:=Length(s);
  m:=Length(t);
  if (n=0) or (m=0) then
  begin
    Result:=m + n;
    exit;
  end;

  //create matrix
  //put row and column numbers in place
  SetLength(d, n + 1);
  for i := Low(d) to High(d) do
  begin
    SetLength(d[i], m + 1);
    d[i][0] := i;
  end;
  for j := Low(d[0]) to High(d[0]) do
    d[0][j] := j;

  // cycle through rest of table filling values from the lowest cost value of the three part cost function
  for i := 1 to n do
    for j := 1 to m do
    begin
      if s[i]=t[j] then
        cost:=0
      else
        cost:=1;

      d[i][j]:=MinIntValue([d[i - 1][j] + gapCost, d[i][j - 1] + gapCost, d[i - 1][j - 1] + cost])
    end;

  result:=d[n][m];
end;

end.
