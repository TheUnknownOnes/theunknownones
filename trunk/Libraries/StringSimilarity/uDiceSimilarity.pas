unit uDiceSimilarity;

interface

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;

implementation

uses
  SysUtils, Math;

type
  TBigram = string[2];
  TBigrams = array of TBigram;

function TokenizeString(Str: String; IgnoreCase: Boolean): TBigrams;
var
  i : Integer;
  l : Integer;
begin
  i:=0;
  l:=Length(Str);

  if IgnoreCase then
    Str:=AnsiUpperCase(Str);

  SetLength(Result, Ceil(l/2));

  if l=0 then
    Exit;

  while i<=High(Result) do
  begin
    Result[i]:=TBigram(copy(Str, (2*i)+1, 2));
    inc(i);
  end;
end;

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
var
  commonTerms: Integer;
  Bigrams1,
  Bigrams2 : TBigrams;
  idx : Integer;
begin
  commonTerms:=0;
  Bigrams1:=TokenizeString(Str1, IgnoreCase);
  Bigrams2:=TokenizeString(Str2, IgnoreCase);

  for idx := 0 to Min(High(Bigrams1), High(Bigrams2)) do
  begin
    if Bigrams1[idx]=Bigrams2[idx] then
      Inc(commonTerms);
  end;

  Result:=2 * commonTerms / (Length(Bigrams1) + Length(Bigrams2));
end;

end.
