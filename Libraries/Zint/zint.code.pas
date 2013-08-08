unit zint.code;

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
  SysUtils, zint.zint;

function code_11(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
function c39(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
function pharmazentral(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
function ec39(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
function c93(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;

implementation

uses
  zint.common;

const SODIUM : AnsiString	= '0123456789-';
const SILVER : AnsiString =	'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%abcd';

const C11Table : array[0..10] of AnsiString = ('111121', '211121', '121121', '221111', '112121', '212111', '122111',
	'111221', '211211', '211111', '112111');

{ Code 39 tables checked against ISO/IEC 16388:2007 }

{ Incorporates Table A1 }
const C39Table : array[0..42] of AnsiString = ( '1112212111', '2112111121', '1122111121', '2122111111', '1112211121',
	'2112211111', '1122211111', '1112112121', '2112112111', '1122112111', '2111121121',
	'1121121121', '2121121111', '1111221121', '2111221111', '1121221111', '1111122121',
	'2111122111', '1121122111', '1111222111', '2111111221', '1121111221', '2121111211',
	'1111211221', '2111211211', '1121211211', '1111112221', '2111112211', '1121112211',
	'1111212211', '2211111121', '1221111121', '2221111111', '1211211121', '2211211111',
	'1221211111', '1211112121', '2211112111', '1221112111', '1212121111', '1212111211',
	'1211121211', '1112121211');
{ Code 39 character assignments (Table 1) }

const EC39Ctrl : array[0..127] of AnsiString = ('%U', '$A', '$B', '$C', '$D', '$E', '$F', '$G', '$H', '$I', '$J', '$K',
	'$L', '$M', '$N', '$O', '$P', '$Q', '$R', '$S', '$T', '$U', '$V', '$W', '$X', '$Y', '$Z',
	'%A', '%B', '%C', '%D', '%E', ' ', '/A', '/B', '/C', '/D', '/E', '/F', '/G', '/H', '/I', '/J',
	'/K', '/L', '-', '.', '/O', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '/Z', '%F',
	'%G', '%H', '%I', '%J', '%V', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '%K', '%L', '%M', '%N', '%O',
	'%W', '+A', '+B', '+C', '+D', '+E', '+F', '+G', '+H', '+I', '+J', '+K', '+L', '+M', '+N', '+O',
	'+P', '+Q', '+R', '+S', '+T', '+U', '+V', '+W', '+X', '+Y', '+Z', '%P', '%Q', '%R', '%S', '%T');
{ Encoding the full ASCII character set in Code 39 (Table A2) }

const C93Ctrl : array[0..127] of AnsiString = ('bU', 'aA', 'aB', 'aC', 'aD', 'aE', 'aF', 'aG', 'aH', 'aI', 'aJ', 'aK',
	'aL', 'aM', 'aN', 'aO', 'aP', 'aQ', 'aR', 'aS', 'aT', 'aU', 'aV', 'aW', 'aX', 'aY', 'aZ',
	'bA', 'bB', 'bC', 'bD', 'bE', ' ', 'cA', 'cB', 'cC', 'cD', 'cE', 'cF', 'cG', 'cH', 'cI', 'cJ',
	'cK', 'cL', 'cM', 'cN', 'cO', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'cZ', 'bF',
	'bG', 'bH', 'bI', 'bJ', 'bV', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'bK', 'bL', 'bM', 'bN', 'bO',
	'bW', 'dA', 'dB', 'dC', 'dD', 'dE', 'dF', 'dG', 'dH', 'dI', 'dJ', 'dK', 'dL', 'dM', 'dN', 'dO',
	'dP', 'dQ', 'dR', 'dS', 'dT', 'dU', 'dV', 'dW', 'dX', 'dY', 'dZ', 'bP', 'bQ', 'bR', 'bS', 'bT');

const C93Table : array[0..46] of AnsiString = ('131112', '111213', '111312', '111411', '121113', '121212', '121311',
	'111114', '131211', '141111', '211113', '211212', '211311', '221112', '221211', '231111',
	'112113', '112212', '112311', '122112', '132111', '111123', '111222', '111321', '121122',
	'131121', '212112', '212211', '211122', '211221', '221121', '222111', '112122', '112221',
	'122121', '123111', '121131', '311112', '311211', '321111', '112131', '113121', '211131',
	'121221', '312111', '311121', '122211');

{ Global Variables for Channel Code }
{var
  S : array[0..10] of Integer;
  B : array[0..10] of Integer;
  value : Integer;
  target_value : Integer;
  pattern : AnsiString;}


{ *********************** CODE 11 ******************** }

{ Code 11 }
function code_11(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  i : Cardinal;
  h, c_digit, c_weight, c_count, k_digit, k_weight, k_count : Integer;
  weight : array[0..127] of Integer;
  error_number : Integer;
  dest : AnsiString; { 6 +  121 * 6 + 2 * 6 + 5 + 1 ~ 1024}
  checkstr : array[0..2] of AnsiChar;
begin
  error_number := 0;

  if (_length > 121) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(SODIUM, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;
  c_weight := 1;
  c_count := 0;
  k_weight := 1;
  k_count := 0;

  { start character }
  strcpy(dest, '112211');

  { Draw main body of barcode }
  for i := 1 to _length do
  begin
    lookup(SODIUM, C11Table, source[i], dest);
    if (source[i] = '-') then
      weight[i - 1] := 10
    else
      weight[i - 1] := ctoi(source[i]);
  end;

  { Calculate C checksum }
  for h := _length - 1 downto 0 do
  begin
    Inc(c_count, (c_weight * weight[h]));
    Inc(c_weight);

    if (c_weight > 10) then
      c_weight := 1;
  end;
  c_digit := c_count mod 11;

  weight[_length] := c_digit;

  { Calculate K checksum }
  for h := _length downto 0 do
  begin
    Inc(k_count, (k_weight * weight[h]));
    Inc(k_weight);

    if (k_weight > 9) then
      k_weight := 1;
  end;
  k_digit := k_count mod 11;

  checkstr[0] := itoc(c_digit);
  checkstr[1] := itoc(k_digit);
  if (checkstr[0] = 'A') then checkstr[0] := '-';
  if (checkstr[1] = 'A') then checkstr[1] := '-';
  checkstr[2] := #0;
  lookup(SODIUM, C11Table, checkstr[0], dest);
  lookup(SODIUM, C11Table, checkstr[1], dest);

  { Stop character }
  concat (dest, '11221');

  expand(symbol, dest);

  ustrcpy(symbol.text, source);
  uconcat(symbol.text, checkstr);
  result := error_number; exit;
end;

{ Code 39 }
function c39(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  i : Cardinal;
  counter : Cardinal;
  check_digit : AnsiChar;
  error_number : Integer;
  dest : AnsiString;
  localstr : AnsiString;
begin
  error_number := 0;
  counter := 0;

  if ((symbol.option_2 < 0) or (symbol.option_2 > 1)) then
    symbol.option_2 := 0;

  if ((symbol.symbology = BARCODE_LOGMARS) and (_length > 59)) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end
  else if (_length > 74) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  to_upper(source);
  error_number := is_sane(SILVER , source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  { Start character }
  strcpy(dest, '1211212111');

  for i := 1 to _length do
  begin
    lookup(SILVER, C39Table, source[i], dest);
    Inc(counter, posn(SILVER, source[i]));
  end;

  if ((symbol.symbology = BARCODE_LOGMARS) or (symbol.option_2 = 1)) then
  begin
    counter := counter mod 43;
    if (counter < 10) then
    begin
      check_digit := itoc(counter);
    end
    else
    begin
      if (counter < 36) then
      begin
        check_digit := AnsiChar((counter - 10) + Ord('A'));
      end
      else
      begin
        case counter of
          36: check_digit := '-';
          37: check_digit := '.';
          38: check_digit := ' ';
          39: check_digit := '$';
          40: check_digit := '/';
          41: check_digit := '+';
          42: check_digit := #37;
          else
            check_digit := ' ';
        end;
      end;
    end;
    lookup(SILVER, C39Table, check_digit, dest);

    { Display a space check digit as _, otherwise it looks like an error }
    if (check_digit = ' ') then
      check_digit := '_';

    localstr := check_digit;
  end;

  { Stop character }
  concat (dest, '121121211');

  if ((symbol.symbology = BARCODE_LOGMARS) or (symbol.symbology = BARCODE_HIBC_39)) then
  begin
    { LOGMARS uses wider 'wide' bars than normal Code 39 }
    counter := strlen(dest);
    for i := 1 to counter do
    begin
      if (dest[i] = '2') then
        dest[i] := '3';
    end;
  end;

  expand(symbol, dest);

  if (symbol.symbology = BARCODE_CODE39) then
  begin
    ustrcpy(symbol.text, '*');
    uconcat(symbol.text, source);
    uconcat(symbol.text, localstr);
    uconcat(symbol.text, '*');
  end
  else
  begin
    ustrcpy(symbol.text, source);
    uconcat(symbol.text, localstr);
  end;
  result := error_number; exit;
end;

{ Pharmazentral Nummer (PZN) }
function pharmazentral(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  i, error_number, zeroes : Integer;
  count, check_digit : Cardinal;
  localstr : AnsiString;
begin
  error_number := 0;

  count := 0;
  if (_length > 6) then
  begin
    strcpy(symbol.errtxt, 'Input wrong _length');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(NEON, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  localstr := '-';
  zeroes := 6 - _length + 1;
  for i := 1 to zeroes - 1 do
    localstr := localstr + '0';
  localstr := localstr + source;

  for i := 1 to 6 do
    Inc(count, (i + 1) * ctoi(localstr[i + 1]));

  check_digit := count  mod 11;
  if (check_digit = 11) then check_digit := 0;
  localstr := localstr + itoc(check_digit);

  if (localstr[7] = 'A') then
  begin
    strcpy(symbol.errtxt, 'Invalid PZN Data');
    result := ZERROR_INVALID_DATA; exit;
  end;
  error_number := c39(symbol, localstr, strlen(localstr));
  ustrcpy(symbol.text, 'PZN');
  uconcat(symbol.text, localstr);
  result := error_number; exit;
end;

{ ************** EXTENDED CODE 39 *************** }

{ Extended Code 39 - ISO/IEC 16388:2007 Annex A }
function ec39(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  buffer : AnsiString;
  i : Cardinal;
  error_number : Integer;
begin
  error_number := 0;

  if (_length > 74) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    Result := ZERROR_TOO_LONG; exit;
  end;

  { Creates a buffer string and places control characters into it }
  for i := 1 to _length do
  begin
    if (source[i] > #127) then
    begin
      { Cannot encode extended ASCII }
      strcpy(symbol.errtxt, 'Invalid characters in input data');
      result := ZERROR_INVALID_DATA; exit;
    end;
    concat(buffer, EC39Ctrl[Ord(source[i])]);
  end;

  { Then sends the buffer to the C39 function }
  error_number := c39(symbol, buffer, ustrlen(buffer));

  symbol.text := '';
  for i := 1 to _length do
    if source[i] <> #0 then
    symbol.text := symbol.text + source[i]
  else
    symbol.text := symbol.text + ' ';

  result := error_number; exit;
end;

{ ******************** CODE 93 ******************* }

{ Code 93 is an advancement on Code 39 and the definition is a lot tighter }
function c93(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  { SILVER includes the extra characters a, b, c and d to represent Code 93 specific
     shift characters 1, 2, 3 and 4 respectively. These characters are never used by
     c39() and ec39() }

  i : Integer;
  h, weight, c, k, error_number : Integer;
  values : array[0..127] of Integer;
  buffer : AnsiString;
  dest : AnsiString;
  set_copy : AnsiString;
begin
  set_copy := SILVER;
  error_number := 0;
  strcpy(buffer, '');

  if (_length > 107) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  symbol.text := '';
  { Message Content }
  for i := 1 to _length do
  begin
    if (source[i] > #127) then
    begin
      { Cannot encode extended ASCII }
      strcpy(symbol.errtxt, 'Invalid characters in input data');
      result := ZERROR_INVALID_DATA; exit;
    end;
    concat(buffer, C93Ctrl[Ord(source[i])]);

    if source[i] <> #0 then
      symbol.text := symbol.text + source[i]
    else
      symbol.text := symbol.text + ' ';
  end;

  { Now we can check the true _length of the barcode }
  h := strlen(buffer);
  if (h > 107) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;

  for i := 0 to h - 1 do
    values[i] := posn(SILVER, buffer[i + 1]) - 1;

  { Putting the data into dest[] is not done until after check digits are calculated }

  { Check digit C }
  c := 0;
  weight := 1;
  for i := h - 1 downto 0 do
  begin
    Inc(c, values[i] * weight);
    Inc(weight);
    if (weight = 21) then
      weight := 1;
  end;
  c := c mod 47;
  values[h] := c;
  buffer := buffer + set_copy[c + 1] ;

  { Check digit K }
  k := 0;
  weight := 1;
  for i := h downto 0 do
  begin
    Inc(k, values[i] * weight);
    Inc(weight);
    if (weight = 16) then
      weight := 1;
  end;
  k := k mod 47;
  Inc(h);
  buffer := buffer + set_copy[k + 1];
  Inc(h);

  { Start character }
  strcpy(dest, '111141');

  for i := 1 to h do
    lookup(SILVER, C93Table, buffer[i], dest);

  { Stop character }
  concat(dest, '1111411');
  expand(symbol, dest);

  symbol.text := symbol.text + set_copy[c + 1];
  symbol.text := symbol.text + set_copy[k + 1];

  result := error_number; exit;
end;


{ NextS() and NextB() are from ANSI/AIM BC12-1998 and are Copyright (c) AIM 1997 }
{ Their are used here on the understanding that they form part of the specification
   for Channel Code and therefore their use is permitted under the following terms
   set out in that document:

   'It is the intent and understanding of AIM [t]hat the symbology presented in this
   specification is entirely in the public domain and free of all use restrictions,
   licenses and fees. AIM USA, its memer companies, or individual officers
   assume no liability for the use of this document.' }



{procedure CheckCharacter();
var
  i : Integer;
  part : array[0..2] of AnsiChar;
begin
  if (value = target_value) then
  begin
     //Target reached - save the generated pattern
    strcpy(pattern, '11110');
    for i := 0 to 10 do
    begin
      part[0] := itoc(S[i]);
      part[1] := itoc(B[i]);
      part[2] := #0;
      concat(pattern, part);
    end;
  end;
end;

procedure NextB(Chan : Integer; i : Integer; MaxB : Integer; MaxS : Integer);
var
  _b : Integer;
begin
  if (S[i]+B[i-1]+S[i-1]+B[i-2] > 4) then
    _b := 1
  else
    _b := 2;

  if (i < Chan+2) then
  begin
    for _b := _b to MaxB do
    begin
      B[i] := _b;
      NextS(Chan,i+1,MaxS,MaxB+1-_b);
    end;
  end
  else
  if (_b <= MaxB) then
  begin
    B[i] := MaxB;
    CheckCharacter();
    Inc(value);
  end;
end;

procedure NextS(Chan : Integer; i : Integer; MaxS : Integer; MaxB : Integer);
var
  _s : Integer;
begin
  if (i < Chan+2) then _s := 1 else _s := MaxS;
  for _s := _s to MaxS do
  begin
    S[i] := _s;
    NextB(Chan,i,MaxB,MaxS+1-_s);
  end;
end;}

end.

