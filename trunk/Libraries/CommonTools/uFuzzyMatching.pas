unit uFuzzyMatching;

interface

function StringSimilarityRatio( const SearchIn, SearchStr : String ) : Double;

implementation

const
  MaxParLen = 255;

function PrepareTheString( const OriginStr : String; var ConvStr : String ) : Integer;
  var
    i : Integer;
  begin
    ConvStr := OriginStr;

    for i := 1 to Length( OriginStr ) do
      begin
        ConvStr[ i ] := UpCase( ConvStr[ i ] );
        if ( ConvStr[ i ] < '0' ) then
          ConvStr[ i ] := ' '
        else
          case ConvStr[ i ] of
            Chr( 196 ) :
              ConvStr[ i ] := Chr( 228 );
            Chr( 214 ) :
              ConvStr[ i ] := Chr( 246 );
            Chr( 220 ) :
              ConvStr[ i ] := Chr( 252 );
            Chr( 142 ) :
              ConvStr[ i ] := Chr( 132 );
            Chr( 153 ) :
              ConvStr[ i ] := Chr( 148 );
            Chr( 154 ) :
              ConvStr[ i ] := Chr( 129 );
            ':' :
              ConvStr[ i ] := ' ';
            ';' :
              ConvStr[ i ] := ' ';
            '<' :
              ConvStr[ i ] := ' ';
            '>' :
              ConvStr[ i ] := ' ';
            '=' :
              ConvStr[ i ] := ' ';
            '?' :
              ConvStr[ i ] := ' ';
            '[' :
              ConvStr[ i ] := ' ';
            ']' :
              ConvStr[ i ] := ' ';
          END;
      END;

    PrepareTheString := i;
  END;

function NGramMatch( const TextPara, SearchStr : String;
  SearchStrLen, NGramLen : Integer; VAR MaxMatch : Integer ) : Integer;

  VAR
    NGram : String[ 8 ];
    NGramCount : Integer;
    i, Count : Integer;

  BEGIN
    NGramCount := SearchStrLen - NGramLen + 1;
    Count := 0;
    MaxMatch := 0;

    i := 1;
    while i <= NGramCount DO
      BEGIN
        NGram := Copy( SearchStr, i, NGramLen );
        IF ( NGram[ NGramLen - 1 ] = ' ' ) AND ( NGram[ 1 ] <> ' ' ) THEN
          Inc( i, NGramLen - 3 ) (* Wird in der Schleife noch erhoeht! *)
        ELSE
          BEGIN
            Inc( MaxMatch, NGramLen );
            IF Pos( NGram, TextPara ) > 0 THEN
              Inc( Count );
          END;
        Inc( i );
      END;

    NGramMatch := Count * NGramLen;
  END;

function StringSimilarityRatio( const SearchIn, SearchStr : String ) : Double;
VAR
    SStr : string;
    TextPara : String;
    TextBuffer : String;
    TextLen : Integer;
    SearchStrLen : Integer;
    NGram1Len : Integer;
    NGram2Len : Integer;
    MatchCount1 : Integer;
    MatchCount2 : Integer;
    MaxMatch1 : Integer;
    MaxMatch2 : Integer;
    Similarity : extended;
    BestSim : extended;

  BEGIN

    BestSim := 0.0;

    if ( SearchIn <> '' ) and ( SearchStr <> '' ) then
      begin
        SearchStrLen := PrepareTheString( SearchStr, SStr );
        NGram1Len := 3;
        IF SearchStrLen < 7 THEN
          NGram2Len := 2
        ELSE
          NGram2Len := 5;

        TextBuffer := SearchIn;
        TextLen := PrepareTheString( TextBuffer, TextPara ) + 1;
        TextPara := Concat( ' ', TextPara );

        IF TextLen < MaxParLen - 2 THEN
          BEGIN
            MatchCount1 := NGramMatch( TextPara, SStr, SearchStrLen,
              NGram1Len, MaxMatch1 );
            MatchCount2 := NGramMatch( TextPara, SStr, SearchStrLen,
              NGram2Len, MaxMatch2 );
            Similarity := 100.0 * ( MatchCount1 + MatchCount2 ) /
              ( MaxMatch1 + MaxMatch2 );
            IF Similarity > BestSim THEN
              BestSim := Similarity;
          END;
      end;

    RESULT := BestSim;

  END;

end.
