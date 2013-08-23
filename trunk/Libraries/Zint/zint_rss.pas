unit zint_rss;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b work in progress
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

function general_rules(field : TArrayOfChar; _type: TArrayOfChar) : Integer;

implementation

uses
  zint_common, zint_composite;

const NUMERIC = 110;
const ALPHA	= 97;
const ISOIEC = 105;
const INVALID_CHAR = 100;
const ANY_ENC = 120;
const ALPHA_OR_ISO = 121;

{ Attempts to apply encoding rules from secions 7.2.5.5.1 to 7.2.5.5.3 of ISO/IEC 24724:2006 }
function general_rules(field : TArrayOfChar; _type: TArrayOfChar) : Integer;
var
  block : array[0..1] of array[0..199] of Integer;
  block_count, i, j, k : Integer;
  current, next, last : Byte;
begin
  block_count := 0;

  block[0][block_count] := 1;
  block[1][block_count] := Ord(_type[0]);

  for i := 1 to strlen(_type) - 1 do
  begin
    current := Ord(_type[i]);
    last := Ord(_type[i - 1]);

    if (current = last) then
      block[0][block_count] := block[0][block_count] + 1
    else
    begin
      Inc(block_count);
      block[0][block_count] := 1;
      block[1][block_count] := Ord(_type[i]);
    end;
  end;

  Inc(block_count);

  for i := 0 to block_count - 1 do
  begin
    current := block[1][i];
    next := (block[1][i + 1] and $FF);

    if ((current = ISOIEC) and (i <> (block_count - 1))) then
    begin
      if ((next = ANY_ENC) and (block[0][i + 1] >= 4)) then
        block[1][i + 1] := NUMERIC;

      if ((next = ANY_ENC) and (block[0][i + 1] < 4)) then
        block[1][i + 1] := ISOIEC;

      if ((next = ALPHA_OR_ISO) and (block[0][i + 1] >= 5)) then
        block[1][i + 1] := ALPHA;

      if ((next = ALPHA_OR_ISO) and (block[0][i + 1] < 5)) then
        block[1][i + 1] := ISOIEC;

    end;

    if (current = ALPHA_OR_ISO) then
      block[1][i] := ALPHA;


    if ((current = ALPHA) and (i <> (block_count - 1))) then
    begin
      if ((next = ANY_ENC) and (block[0][i + 1] >= 6)) then
        block[1][i + 1] := NUMERIC;

      if ((next = ANY_ENC) and (block[0][i + 1] < 6)) then
      begin
        if ((i = block_count - 2) and (block[0][i + 1] >= 4)) then
          block[1][i + 1] := NUMERIC
        else
          block[1][i + 1] := ALPHA;
      end;
    end;

    if (current = ANY_ENC) then
      block[1][i] := NUMERIC;
  end;

  if (block_count > 1) then
  begin
    i := 1;
    while(i < block_count) do
    begin
      if (block[1][i - 1] = block[1][i]) then
      begin
        { bring together }
        block[0][i - 1] := block[0][i - 1] + block[0][i];
        j := i + 1;

        { decreace the list }
        while(j < block_count) do
        begin
          block[0][j - 1] := block[0][j];
          block[1][j - 1] := block[1][j];
          Inc(j);
        end;
        Dec(block_count);
        Dec(i);
      end;
      Inc(i);
    end;
  end;

  for i := 0 to block_count - 2 do
  begin
    if ((block[1][i] = NUMERIC) and ((block[0][i] and 1) <> 0)) then
    begin
      { Odd size numeric block }
      block[0][i] := block[0][i] - 1;
      block[0][i + 1] := block[0][i + 1] + 1;
    end;
  end;

  j := 0;
  for i := 0 to block_count - 1 do
  begin
    for k := 0 to  block[0][i] - 1 do
    begin
      _type[j] := Chr(block[1][i]);
      Inc(j);
    end;
  end;

  if ((block[1][block_count - 1] = NUMERIC) and ((block[0][block_count - 1] and 1) <> 0)) then
  begin
    { If the last block is numeric and an odd size, further
    processing needs to be done outside this procedure }
    result := 1; exit
  end
  else
  begin
    result := 0; exit;
  end;
end;

end.

