unit uSmithWatermanGotoh;

interface

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
function SmithWatermanGotohDistance(const AStr1, AStr2: String): Double;

implementation

uses
  SysUtils, Math;

const
  CHAR_EXACT_MATCH_SCORE = +5;
  CHAR_APPROX_MATCH_SCORE = +3;
  CHAR_MISMATCH_MATCH_SCORE = -3;

  MAX_GAP_COST = +5;
  WINDOW_SIZE = 100;

  APPROX_LIST : array [0..6] of tsyscharset =
          (['d','t'],
           ['g', 'j'],
           ['l','r'],
           ['m','n'],
           ['b','p','v'],
           ['a','e','i','o','u'],
           [',','.']);

function GetGapCost(const stringIndexStartGap, stringIndexEndGap: Integer): Double;
begin
  if (stringIndexStartGap >= stringIndexEndGap) then
    Result:=0
  else
    Result:=MAX_GAP_COST + ((stringIndexEndGap - 1) - stringIndexStartGap);
end;

function GetCost(const AStr1: String; const string1Index: Integer;const AStr2: String;const string2Index: Integer): Double;
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
          break;
        end;
    end;
  end;
end;

function SmithWatermanGotohDistance(const AStr1, AStr2: String): Double;
var
  s, t : String;
  d : array of array of Double;
  n : Integer;  // length of s
  m : Integer; // length of t
  i : Integer; // iterates through s
  j : Integer; // iterates through t
  k : Integer;
  cost : Double; // cost

  maxSoFar : Double;
  maxGapCost,
  maxGapCost1,
  maxGapCost2 : Double;
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

  //create matrix (n)x(m)
  SetLength(d, n);
  for i := Low(d) to High(d) do
  begin
    SetLength(d[i], m);
  end;

  //process first row and column first as no need to consider previous rows/columns
  maxSoFar := 0;
  for I := 0 to n - 1 do
  begin
    // get the substution cost
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

      d[i][0] := MaxValue([0, maxGapCost, cost]);
    end;

    //update max possible if available
    if d[i][0]>maxSoFar then
      maxSoFar := d[i][0];
  end;

  for j := 0 to m - 1 do
  begin
    // get the substution cost
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
        maxGapCost := MaxValue([maxGapCost, d[0][j - k] - GetGapCost(j - k, j)]);
      d[0][j] := MaxValue([0, maxGapCost, cost]);
    end;

    //update max possible if available
    if d[0][j]>maxSoFar then
      maxSoFar := d[0][j];
  end;

  // cycle through rest of table filling values from the lowest cost value of the three part cost function
  for i:=1 to n-1 do
  begin
    for j := 1 to m - 1 do
    begin
      cost := GetCost(s, i + 1, t, j + 1);

      // find lowest cost at point from three possible
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

      d[i][j] := MaxValue([0, maxGapCost1, maxGapCost2, d[i - 1][j - 1] + cost]);

      if (d[i][j] > maxSoFar) then
        maxSoFar := d[i][j];
    end;
  end;

  // return max value within matrix as holds the maximum edit score
  Result:=maxSoFar;
end;

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
var
  swg : Double;
  maxValue : Double;
begin
  if IgnoreCase then
    swg := SmithWatermanGotohDistance(AnsiUpperCase(Str1), AnsiUpperCase(Str2))
  else
    swg := SmithWatermanGotohDistance(Str1, Str2);

  maxValue := Min(Length(str1), Length(str2));

  if (CHAR_EXACT_MATCH_SCORE > -MAX_GAP_COST) then
    maxValue := maxValue * CHAR_EXACT_MATCH_SCORE
  else
    maxValue := maxValue * -MAX_GAP_COST;

  if (maxValue = 0) then
      result:=1
  else
      result:= (swg / maxValue);
end;

end.
