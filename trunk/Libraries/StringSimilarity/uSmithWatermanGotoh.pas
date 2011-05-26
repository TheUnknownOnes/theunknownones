unit uSmithWatermanGotoh;

interface

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
function SmithWatermanGotoh(const AStr1, AStr2: String): Integer;

implementation

uses
  SysUtils, Math;

const
  CHAR_EXACT_MATCH_SCORE = +5;
  CHAR_APPROX_MATCH_SCORE = +3;
  CHAR_MISMATCH_MATCH_SCORE = -3;
  WINDOW_SIZE = 100;

  APPROX_LIST : array [0..6] of tsyscharset =
          (['d','t'],
           ['g', 'j'],
           ['l','r'],
           ['m','n'],
           ['b','p','v'],
           ['a','e','i','o','u'],
           [',','.']);

function GetGapCost(const stringIndexStartGap, stringIndexEndGap: Integer): Integer;
begin
  if (stringIndexStartGap >= stringIndexEndGap) then
    Result:=0
  else
    Result:=5 + ((stringIndexEndGap - 1) - stringIndexStartGap);
end;

function GetCost(const AStr1: String; const string1Index: Integer;const AStr2: String;const string2Index: Integer): Integer;
var
  si, ti : Char;
  i : Integer;
begin
  result:=CHAR_MISMATCH_MATCH_SCORE;
  //check within range
  if (Length(AStr1) >= string1Index) and (string1Index > 0) or
     (Length(AStr2) >= string2Index) or (string2Index > 0) then
  begin
    //check for approximate match
    si:=copy(AStr1,string1Index,1)[1];
    ti:=copy(AStr2,string2Index,1)[1];

    if (si = ti) then
      Result:=CHAR_EXACT_MATCH_SCORE
    else
    begin
      //check for approximate match
      for i := Low(APPROX_LIST) to High(APPROX_LIST) do
        if CharInSet(si, APPROX_LIST[i]) and CharInSet(ti, APPROX_LIST[i]) then
        begin
          Result:=CHAR_APPROX_MATCH_SCORE;
          break;;
        end;
    end;
  end;
end;

function SmithWatermanGotoh(const AStr1, AStr2: String): Integer;
var
  s, t : String;
  d : array of array of Integer;
  n : Integer;  // length of s
  m : Integer; // length of t
  i : Integer; // iterates through s
  j : Integer; // iterates through t
  k : Integer;
  cost : Integer; // cost

  maxSoFar : Integer;
  maxGapCost,
  maxGapCost1,
  maxGapCost2 : Integer;
  WindowStart: Integer;
begin
  s:=AStr1;
  t:=AStr2;

  n:=Length(s);
  m:=Length(t);
  if (n=0) or (m=0) then
  begin
    Result:=m + n;
    exit;
  end;

  SetLength(d, n);
  for i := Low(d) to High(d) do
  begin
    SetLength(d[i], m);
  end;

  maxSoFar := 0;

  for I := 0 to n - 1 do
  begin
    cost:=GetCost(s, i + 1, t, 1);

    if i = 0 then
      d[0][0]:=max(0, cost)
    else
    begin
      maxGapCost:=0;
      WindowStart:= i - WINDOW_SIZE;
      if WindowStart<1 then
        WindowStart := 1;

      for k := WindowStart to i - 1 do
      begin
        maxGapCost:=Max(maxGapCost, d[i - k][0] - GetGapCost(i - k, i))
      end;

      d[i][0] := MaxIntValue([0, maxGapCost, cost]);
    end;

    if d[i][0]>maxSoFar then
      maxSoFar := d[i][0];
  end;

  for j := 0 to m - 1 do
  begin
    cost := GetCost(s, 1, t, j + 1);

    if j = 0 then
      d[0][0] := Max(0, cost)
    else
    begin
      maxGapCost := 0;
      WindowStart := j - WINDOW_SIZE;
      if WindowStart<1 then
        WindowStart:=1;
      for k := WindowStart to j - 1 do
        maxGapCost := MaxIntValue([maxGapCost, d[0][j - k] - GetGapCost(j - k, j)]);
      d[0][j] := MaxIntValue([0, maxGapCost, cost]);
    end;
    if d[0][j]>maxSoFar then
      maxSoFar := d[0][j];
  end;

  for i:=1 to n-1 do
  begin
    for j := 1 to m - 1 do
    begin
      cost := GetCost(s, i + 1, t, j + 1);

      maxGapCost1 := 0;
      maxGapCost2 := 0;

      WindowStart:=i-WINDOW_SIZE;
      if WindowStart<1 then
        WindowStart:=1;

      for k := WindowStart to i - 1 do
        maxGapCost1 := Max(maxGapCost1, d[i - k][j] - GetGapCost(i - k, i));

      WindowStart:= j - WINDOW_SIZE;
      if WindowStart < 1 then
        WindowStart := 1;


      for k := WindowStart to j - 1 do
        maxGapCost2 := Max(maxGapCost2,  d[i][j - k] - getGapCost(j - k, j));

      d[i][j] := MaxIntValue([0, maxGapCost1, maxGapCost2, d[i - 1][j - 1] + cost]);

      if (d[i][j] > maxSoFar) then
        maxSoFar := d[i][j];
    end;
  end;

  Result:=maxSoFar;
end;

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
var
  swg : Integer;
  maxValue : Double;
begin
  if IgnoreCase then
    swg := SmithWatermanGotoh(AnsiUpperCase(Str1), AnsiUpperCase(Str2))
  else
    swg := SmithWatermanGotoh(Str1, Str2);

  maxValue := Min(Length(str1), Length(str2));

  if (CHAR_EXACT_MATCH_SCORE > -5) then
    maxValue := maxValue * CHAR_EXACT_MATCH_SCORE
  else
    maxValue := maxValue * -5;

  if (maxValue = 0) then
      result:=1
  else
      result:= (swg / maxValue);
end;

end.
