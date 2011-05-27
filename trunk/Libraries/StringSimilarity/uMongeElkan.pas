unit uMongeElkan;

interface

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;

implementation

uses
  SysUtils, Classes, uSmithWatermanGotoh, Math;

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
var
  sl1, sl2 : TStringList;
  maxFound,
  sumMatches,
  found : Double;
  I, J: Integer;
begin
  Result:=0;

  sl1:=TStringList.Create;
  sl2:=TStringList.Create;
  try
    sl1.CommaText:=Str1;
    sl2.CommaText:=Str2;

    sumMatches:=0;
    for I := 0 to sl1.Count - 1 do
    begin
      maxFound:=0;
      for J := 0 to sl2.Count - 1 do
      begin
        found := uSmithWatermanGotoh.StringSimilarityRatio(sl1[I], sl2[J], IgnoreCase);
        maxFound:=MaxValue([found, maxFound]);
      end;
      sumMatches:=sumMatches+maxFound;
    end;

    Result:=sumMatches / sl1.Count;
  finally
    sl1.Free;
    sl2.Free;
  end;
end;

end.
