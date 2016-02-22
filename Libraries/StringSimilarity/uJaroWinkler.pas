unit uJaroWinkler;

// translation from: http://stackoverflow.com/questions/19123506/jaro-winkler-distance-algorithm-in-c-sharp
// from: leebickmtu
// translation by: Sebastian Klatte (2016)

interface

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;

implementation

uses
  Math, SysUtils;

const
  {$REGION 'Documentation'}
  /// <summary>
  ///   The Winkler modification will not be applied unless the percent match
  ///   was at or above the mWeightThreshold percent without the modification. <br />
  ///    Winkler's paper used a default value of 0.7
  /// </summary>
  {$ENDREGION}
  WEIGHT_THRESHOLD = 0.7;
  {$REGION 'Documentation'}
  /// <summary>
  ///   Size of the prefix to be concidered by the Winkler modification. <br />
  ///   Winkler's paper used a default value of 4
  /// </summary>
  {$ENDREGION}
  NUM_CHARS = 4;

{$REGION 'Documentation'}
/// <summary>
///   Returns the Jaro-Winkler distance between the specified strings. The
///   distance is symmetric and will fall in the range 0 (no match) to 1
///   (perfect match).
/// </summary>
{$ENDREGION}
function Proximity(const AStr1, AStr2: string): Double;
var
  LLen1, LLen2, LSearchRange, LIndex, LIndexInner, LMatches, LStart, LEnd, LNumTransposed, LNumHalfTransposed, LMax: Integer;
  LMatched1, LMatched2: Array of Boolean;
  LWeight: Double;
begin
  LLen1 := Length(AStr1);
  LLen2 := Length(AStr2);
  if (LLen1 = 0) then
    Exit(IfThen(LLen2 = 0, 1, 0));

  LSearchRange := Max(0, Round(Max(LLen1, LLen2) / 2) - 1);

  // default initialized to false
  SetLength(LMatched1, LLen1);
  SetLength(LMatched2, LLen2);

  LMatches := 0;
  for LIndex := 0 to LLen1 - 1 do
  begin
    LStart := Max(0, LIndex - LSearchRange);
    LEnd := Min(LIndex + LSearchRange + 1, LLen2);
    for LIndexInner := LStart to LEnd - 1 do
    begin
      if LMatched2[LIndexInner] then
        Continue;
      if not(AStr1[LIndex + 1] = AStr2[LIndexInner + 1]) then
        Continue;
      LMatched1[LIndex] := True;
      LMatched2[LIndexInner] := True;
      Inc(LMatches);
      break;
    end;
  end;

  if (LMatches = 0) then
    Exit(0);

  LNumHalfTransposed := 0;
  LIndexInner := 0;
  for LIndex := 0 to LLen1 - 1 do
  begin
    if not LMatched1[LIndex] then
      Continue;
    while not(LMatched2[LIndexInner]) do
      Inc(LIndexInner);

    if not(AStr1[LIndex + 1] = AStr2[LIndexInner + 1]) then
      Inc(LNumHalfTransposed);

    Inc(LIndexInner);
  end;

  LNumTransposed := LNumHalfTransposed div 2;

  LWeight := ((LMatches / LLen1) + (LMatches / LLen2) + ((LMatches - LNumTransposed) / LMatches)) * 1 / 3;

  if (LWeight <= WEIGHT_THRESHOLD) then
    Exit(LWeight);

  LMax := Min(NUM_CHARS, Min(LLen1, LLen2));
  LIndex := 0;
  while (LIndex < LMax) and (AStr1[LIndex + 1] = AStr2[LIndex + 1]) do
    Inc(LIndex);

  if (LIndex = 0) then
    Exit(LWeight);

  Result := LWeight + (0.1 * LIndex * (1 - LWeight));
end;

{$REGION 'Documentation'}
/// <summary>
///   Returns the Jaro-Winkler distance between the specified strings. The
///   distance is symmetric and will fall in the range 0 (perfect match) to 1
///   (no match).
/// </summary>
{$ENDREGION}
function Distance(const AStr1, AStr2: string): Double;
begin
  Result := 1 - Proximity(AStr1, AStr2);
end;

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
begin
  if IgnoreCase then
    Result := Proximity(AnsiUpperCase(Str1), AnsiUpperCase(Str2))
  else
    Result := Proximity(Str1, Str2);
end;

end.
