unit zint_medical;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

function pharma_one(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
function pharma_two(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
function codabar(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
function code32(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;

implementation

uses
  zint_common, zint_code;

const CALCIUM	: AnsiString = '0123456789-$:/.+ABCD';

const CodaTable : array[0..19] of AnsiString = ('11111221', '11112211', '11121121', '22111111', '11211211', '21111211',
	'12111121', '12112111', '12211111', '21121111', '11122111', '11221111', '21112121', '21211121',
	'21212111', '11212121', '11221211', '12121121', '11121221', '11122211');

function pharma_one(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
  { 'Pharmacode can represent only a single integer from 3 to 131070. Unlike other
     commonly used one-dimensional barcode schemes, pharmacode does not store the data in a
     form corresponding to the human-readable digits; the number is encoded in binary, rather
     than decimal. Pharmacode is read from right to left: with n as the bar position starting
     at 0 on the right, each narrow bar adds 2n to the value and each wide bar adds 2(2^n).
     The minimum barcode is 2 bars and the maximum 16, so the smallest number that could
     be encoded is 3 (2 narrow bars) and the biggest is 131070 (16 wide bars).'
     - http://en.wikipedia.org/wiki/Pharmacode }

  { This code uses the One Track Pharamacode calculating algorithm as recommended by
     the specification at http://www.laetus.com/laetus.php?request=file&id=69 }
var
  tester : Cardinal;
  counter, error_number, h : Integer;
  inter : AnsiString; { 131070 . 17 bits }
  dest : AnsiString; { 17 * 2 + 1 }
begin
  error_number := 0;

  if (_length > 6) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  tester := StrToIntDef(source, 0);

  if ((tester < 3) or (tester > 131070)) then
  begin
    strcpy(symbol.errtxt, 'Data out of range');
    result := ZERROR_INVALID_DATA; exit;
  end;

  repeat
    if ((tester and 1) = 0) then
    begin
      concat(inter, 'W');
      tester := (tester - 2) div 2;
    end
    else
    begin
      concat(inter, 'N');
      tester := (tester - 1) div 2;
    end;
  until not (tester <> 0);

  h := strlen(inter);
  dest := '';
  for counter := h downto 1 do
  begin
    if (inter[counter] = 'W') then
      concat(dest, '32')
    else
      concat(dest, '12');
  end;

  expand(symbol, dest);

  result := error_number; exit;
end;

function pharma_two_calc(symbol : zint_symbol; source : AnsiString; out dest : AnsiString) : Integer;
  { This code uses the Two Track Pharamacode defined in the document at
     http://www.laetus.com/laetus.php?request=file&id=69 and using a modified
     algorithm from the One Track system. This standard accepts integet values
     from 4 to 64570080. }
var
  tester : Cardinal;
  counter, h : Integer;
  inter : AnsiString;
  error_number : Integer;
begin
  tester := StrToIntDef(source, 0);

  if ((tester < 4) or (tester > 64570080)) then
  begin
    strcpy(symbol.errtxt, 'Data out of range');
    result := ZERROR_INVALID_DATA; exit;
  end;
  error_number := 0;
  strcpy(inter, '');
  repeat
    case tester mod 3 of
      0:
      begin
        concat(inter, '3');
        tester := (tester - 3) div 3;
      end;
      1:
      begin
        concat(inter, '1');
        tester := (tester - 1) div 3;
      end;
      2:
      begin
        concat(inter, '2');
        tester := (tester - 2) div 3;
      end;
    end;
  until not (tester <> 0);

  h := strlen(inter);
  for counter := h downto 1 do
    dest := dest + inter[counter];

  result := error_number; exit;
end;

{ Draws the patterns for two track pharmacode }
function pharma_two(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  height_pattern : AnsiString;
  loopey, h : Cardinal;
  writer : Integer;
  error_number : Integer;
begin
  error_number := 0;;
  strcpy(height_pattern, '');

  if (_length > 8) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;
  error_number := pharma_two_calc(symbol, source, height_pattern);
  if (error_number <> 0) then
  begin
    result := error_number; exit;
  end;

  writer := 0;
  h := strlen(height_pattern);
  for loopey := 1 to h do
  begin
    if ((height_pattern[loopey] = '2') or (height_pattern[loopey] = '3')) then
    begin
      set_module(symbol, 0, writer);
    end;
    if ((height_pattern[loopey] = '1') or (height_pattern[loopey] = '3')) then
    begin
      set_module(symbol, 1, writer);
    end;
    Inc(writer, 2);
  end;
  symbol.rows := 2;
  symbol.width := writer - 1;


  result := error_number; exit;
end;

{ The Codabar system consisting of simple substitution }
function codabar(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  i, error_number : Integer;
  dest : AnsiString;
begin
  error_number := 0;
  strcpy(dest, '');

  if (_length > 60) then
  begin { No stack smashing please }
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(source);
  error_number := is_sane(CALCIUM, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;
  { Codabar must begin and end with the characters A, B, C or D }
  if ((source[1] <> 'A') and (source[1] <> 'B') and (source[1] <> 'C') and (source[1] <> 'D')) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := ZERROR_INVALID_DATA; exit;
  end;

  if ((source[_length] <> 'A') and (source[_length] <> 'B') and
        (source[_length] <> 'C') and (source[_length] <> 'D')) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := ZERROR_INVALID_DATA; exit;
  end;

  for i := 1 to _length do
    lookup(CALCIUM, CodaTable, source[i], dest);

  expand(symbol, dest);
  ustrcpy(symbol.text, source);
  result := error_number; exit;
end;

{ Italian Pharmacode }
function code32(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  i, zeroes, error_number, checksum, checkpart, checkdigit : Integer;
  localstr, risultante : AnsiString;
  pharmacode, remainder, devisor : Integer;
  codeword : array[0..5] of Integer;
  tabella : AnsiString;
begin
  { Validate the input }
  if (_length > 8) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  { Add leading zeros as required }
  zeroes := 8 - _length;
  SetLength(localstr, zeroes);
  FillChar(localstr[1], zeroes, '0');
  localstr := localstr + source;

  { Calculate the check digit }
  checksum := 0;
  checkpart := 0;
  for i := 0 to 3 do
  begin
    checkpart := ctoi(localstr[i * 2 + 1]);
    Inc(checksum, checkpart);
    checkpart := 2 * (ctoi(localstr[(i * 2) + 2]));
    if (checkpart >= 10) then
      Inc(checksum, (checkpart - 10) + 1)
    else
      Inc(checksum, checkpart);
  end;

  { Add check digit to data string }
  checkdigit := checksum mod 10;
  localstr := localstr + itoc(checkdigit);

  { Convert string into an integer value }
  pharmacode := StrToIntDef(localstr, 0);

  { Convert from decimal to base-32 }
  devisor := 33554432;
  for i := 5 downto 0 do
  begin
    codeword[i] := pharmacode div devisor;
    remainder := pharmacode mod devisor;
    pharmacode := remainder;
    devisor := devisor div 32;
  end;

  risultante := '';
  { Look up values in 'Tabella di conversione' }
  strcpy(tabella, '0123456789BCDFGHJKLMNPQRSTUVWXYZ');
  for i := 5 downto 0 do
    risultante := risultante + tabella[codeword[i] + 1];

  { Plot the barcode using Code 39 }
  error_number := c39(symbol, risultante, strlen(risultante));
  if (error_number <> 0) then begin result := error_number; exit; end;

  { Override the normal text output with the Pharmacode number }
  ustrcpy(symbol.text, 'A');
  uconcat(symbol.text, localstr);

  result := error_number; exit;
end;

end.

