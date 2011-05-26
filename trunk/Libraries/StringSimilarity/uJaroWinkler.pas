unit uJaroWinkler;

interface

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;

implementation

uses
  math, SysUtils;

function JaroWinkler(const AStr1, AStr2: String; p:Double=0.1): Double;
Var
  MaxDeviation,
  len1,len2,
  MatchingCost,
  TranspositionCost,
  PrefixLength,i,j   : integer;
  c1,c2 : Char;
  t1Match,t2Match:string;
  b1,b2:array of Boolean;
  JaroDistance:Double;

  function FindMatches(prmTextInitial:string; b1:array of Boolean):string;
  var
    i:integer;
  begin
    Result:='';
    //Calculates the number of characters that match
    for i := 1 to Length(prmTextInitial) do
    begin
      if b1[i] then
      begin
        result:=result+prmTextInitial[i];
      end;
    end;
  end;

begin
  MaxDeviation:=round(Max(Length(AStr1), Length(AStr2))/2)-1;
  if ((AStr1='') or (AStr2='')) then
  begin
    Result:=0;
    exit;
  end;

  MatchingCost:=0;
  TranspositionCost:=0;
  len1:=Length(AStr1);
  len2:=Length(AStr2);

  Setlength(b1,len1+1);
  Setlength(b2,len2+1);

  for i := 0 to len1 do
  begin
    b1[i]:=false;
  end;

  for i := 0 to len2 do
  begin
    b2[i]:=false;
  end;

  for i := 1 to len1 do
  begin
    c1:=AStr1[i];
    if (i<=len2) then
      c2:=AStr2[i]
    else
      c2:=#0;

    for j := Max(i-MaxDeviation,1) to Min(i+MaxDeviation,len2) do
    begin
      c2:=AStr2[j];
      if c1=c2 then //MatchingCost with transposition
      begin
        b1[i]:=true;
        b2[j]:=true;
        //The character was matched, it is no longer available
        Inc(MatchingCost);
        break;
      end;
    end;
  end;

  if (MatchingCost=0) then
  begin
    Result:=0;
    exit;
  end;

  t1Match:=FindMatches(AStr1,b1);
  t2Match:=FindMatches(AStr2,b2);

  if t1Match<>t2Match then
  begin
    for i := 1 to length(t1Match) do
    begin
      if t1Match[i]<>t2Match[i] then
        Inc(TranspositionCost)
    end;
  end
  else
  begin
    TranspositionCost:=0;
  end;

  JaroDistance:=1/3*((MatchingCost/len1)+(MatchingCost/len2)+((MatchingCost-Int(TranspositionCost/2))/MatchingCost));

  //Calculate the Winkler distance
  PrefixLength:=0;

  for i := 1 to min(4,min(len1,len2)) do
  begin
    c1:=AStr1[i];
    c2:=AStr2[i];
    if c1=c2 then
      inc(PrefixLength)
    else
      break;
  end;

  Result:=JaroDistance+(PrefixLength*p*(1-JaroDistance));
end;


function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
begin
  if IgnoreCase then
    Result:=JaroWinkler(AnsiUpperCase(Str1), AnsiUpperCase(Str2))
  else
    Result:=JaroWinkler(Str1, Str2);
end;

end.
