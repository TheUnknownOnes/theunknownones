unit zint.common;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b work in progress

  Notes:
    - currently missing: roundup, froundup, utf8toutf16 (maybe not all have to be implemented)
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
 SysUtils, zint.zint;

const
  _TRUE = 1;
  _FALSE = 0;

  { The most commonly used set }
  NEON : AnsiString = '0123456789';

// Pascal-specific things
procedure strcpy(out target : AnsiString; const source : AnsiString);
function strlen(const data : AnsiString) : Integer;
type
  TArrayOfByte = array of Byte;
  TArrayOfInteger = array of Integer;
  TArrayOfCardinal = array of Cardinal;



function ustrlen(data : AnsiString) : Integer;
procedure ustrcpy(var target : AnsiString; const source : AnsiString);
procedure concat(var dest : AnsiString; const source : AnsiString);
procedure uconcat(var dest : AnsiString; const source : AnsiString);
function ctoi(source : AnsiChar) : Integer;
function itoc(source : Integer) : AnsiChar;
procedure to_upper(var source : AnsiString);
function is_sane(const test_string : AnsiString; const source : AnsiString; _length : Integer) : Integer;
function posn(const set_string : AnsiString; const data : AnsiString) : Integer;
procedure lookup(const set_string : AnsiString; const table : array of AnsiString; const data : AnsiChar; var dest : AnsiString);
function module_is_set(var symbol : zint_symbol; y_coord : Integer; x_coord : Integer) : Integer;
procedure set_module(var symbol : zint_symbol; y_coord : Integer; x_coord : Integer);
procedure unset_module(var symbol : zint_symbol; y_coord : Integer; x_coord : Integer);
procedure expand(var symbol : zint_symbol; data : AnsiString);
function is_stackable(symbology : Integer) : Integer;
function is_extendable(symbology : Integer) : Integer;
function istwodigits(const source : AnsiString; position : Integer) : Integer;
function latin1_process(var symbol : zint_symbol; const source : AnsiString; var preprocessed : AnsiString; var _length : Integer) : Integer;

function nitems(a : TArrayOfInteger) : Integer; overload;

implementation

function ustrlen(data : AnsiString) : Integer;
{ Local replacement for strlen() with uint8_t strings }
begin
  Result := Length(data);
end;

procedure ustrcpy(var target : AnsiString; const source : AnsiString);
{ Local replacement for strcpy() with uint8_t strings }
begin
  target := source;
end;

procedure concat(var dest : AnsiString; const source : AnsiString);
{ Concatinates dest[] with the contents of source[], copying /0 as well }
begin
  dest := dest + source;
end;

procedure uconcat(var dest : AnsiString; const source : AnsiString);
{ Concatinates dest[] with the contents of source[], copying /0 as well }
begin
  dest := dest + source;
end;

function ctoi(source : AnsiChar) : Integer;
{ Converts a character 0-9 to its equivalent integer value }
begin
	if (source >= '0') and (source <= '9') then
		result := Ord(source) - Ord('0')
  else
	  result := Ord(source) - Ord('A') + 10;
end;

function itoc(source : Integer) : AnsiChar;
{ Converts an integer value to its hexadecimal character }
begin
  if (source >= 0) and (source <= 9) then
    Result := AnsiChar(Ord('0') + source)
  else
    Result := AnsiChar(Ord('A') + (source - 10));
end;

procedure to_upper(var source : AnsiString);
begin
  source := UpperCase(source);
end;

function is_sane(const test_string : AnsiString; const source : AnsiString; _length : Integer) : Integer;
{ Verifies that a string only uses valid characters }
var
  latch : Cardinal;
  i,j : Cardinal;
  lt : Integer;
begin
  lt := strlen(test_string);

	for i := 1 to _length do
  begin
		latch := _FALSE;
		for j:= 1 to lt do
    begin
			if (source[i] = test_string[j]) then
      begin
				latch := _TRUE;
				break;
			end;
    end;
		if not (latch = _TRUE) then
    begin
			result := ZERROR_INVALID_DATA; exit;
		end;
	end;

	result := 0; exit;
end;

function posn(const set_string : AnsiString; const data : AnsiString) : Integer;
{ Returns the position of data in set_string }
begin
  Result := Pos(data, set_string);
end;

{ Replaces huge switch statements for looking up in tables }
procedure lookup(const set_string : AnsiString; const table : array of AnsiString; const data : AnsiChar; var dest : AnsiString);
var
  n : Cardinal;
  i : Cardinal;
begin
  n := strlen(set_string);

	for i := 1 to n do
		if (data = set_string[i]) then
			dest := dest + table[i - 1];
end;


function module_is_set(var symbol : zint_symbol; y_coord : Integer; x_coord : Integer) : Integer;
begin
  result := (Ord(symbol.encoded_data[y_coord][x_coord div 7]) shr (x_coord mod 7)) and 1;
end;


procedure set_module(var symbol : zint_symbol; y_coord : Integer; x_coord : Integer);
begin
	symbol.encoded_data[y_coord][x_coord div 7] := ((symbol.encoded_data[y_coord][x_coord div 7]) or (1 shl (x_coord mod 7)));
end;

procedure unset_module(var symbol : zint_symbol; y_coord : Integer; x_coord : Integer);
begin
	symbol.encoded_data[y_coord][x_coord div 7] := ((symbol.encoded_data[y_coord][x_coord div 7]) or (not (1 shl (x_coord mod 7))));
end;

procedure expand(var symbol : zint_symbol; data : AnsiString);
{ Expands from a width pattern to a bit pattern */ }
var
  reader, n : Cardinal;
  writer, i : Integer;
  latch : AnsiChar;
begin
  n := strlen(data);
	writer := 0;
	latch := '1';

	for reader := 1 to n do
  begin
    for i := 0 to ctoi(data[reader]) - 1 do
    begin
			if (latch = '1') then set_module(symbol, symbol.rows, writer);
			Inc(writer);
		end;

    if latch = '1' then latch := '0' else latch := '1';
	end;

	if(symbol.symbology <> BARCODE_PHARMA) then
  begin
		if(writer > symbol.width) then
			symbol.width := writer;
  end
  else
  begin
		{ Pharmacode One ends with a space - adjust for this }
		if(writer > symbol.width + 2) then
			symbol.width := writer - 2;
	end;

	symbol.rows := symbol.rows + 1;
end;

function is_stackable(symbology : Integer) : Integer;
{ Indicates which symbologies can have row binding }
begin
  Result := 0;

	if(symbology < BARCODE_PDF417) then  Result := 1;
	if(symbology = BARCODE_CODE128B) then Result := 1;
	if(symbology = BARCODE_ISBNX) then Result := 1;
	if(symbology = BARCODE_EAN14) then Result := 1;
	if(symbology = BARCODE_NVE18) then Result := 1;
	if(symbology = BARCODE_KOREAPOST) then Result := 1;
	if(symbology = BARCODE_PLESSEY) then Result := 1;
	if(symbology = BARCODE_TELEPEN_NUM) then Result := 1;
	if(symbology = BARCODE_ITF14) then Result := 1;
	if(symbology = BARCODE_CODE32) then Result := 1;
end;

function is_extendable(symbology : Integer) : Integer;
{ Indicates which symbols can have addon }
begin
  Result := 0;

	if (symbology = BARCODE_EANX) then result := 1;
	if (symbology = BARCODE_UPCA) then result := 1;
	if (symbology = BARCODE_UPCE) then result := 1;
	if (symbology = BARCODE_ISBNX) then result := 1;
	if (symbology = BARCODE_UPCA_CC) then result := 1;
	if (symbology = BARCODE_UPCE_CC) then result := 1;
	if (symbology = BARCODE_EANX_CC) then result := 1;
end;

function istwodigits(const source : AnsiString; position : Integer) : Integer;
begin
  if ((source[position] >= '0') and (source[position] <= '9')) then
  begin
    if ((source[position + 1] >= '0') and (source[position + 1] <= '9')) then
    begin
      result := 1; exit;
    end;
  end;

  result := 0; exit;
end;

function latin1_process(var symbol : zint_symbol; const source : AnsiString; var preprocessed : AnsiString; var _length : Integer) : Integer;
{ Convert Unicode to Latin-1 for those symbologies which only support Latin-1 }
var
  i, next : Integer;
begin
  i := 1;

  preprocessed := '';

	repeat
		next := -1;
		if (source[i] < #128) then
    begin
			preprocessed := preprocessed + source[i];
			next := i + 1;
    end
    else
    begin
			if (source[i] = #$C2) then
      begin
				preprocessed := preprocessed + source[i + 1];
				next := i + 2;
      end;

			if(source[i] = #$C3) then
      begin
				preprocessed := preprocessed + Chr(Ord(source[i + 1]) + 64);
				next := i + 2;
      end;
		end;
		if(next = -1) then
    begin
			strcpy(symbol.errtxt, 'error: Invalid character in input string (only Latin-1 characters supported)');
			result := ZERROR_INVALID_DATA; exit;
    end;
		i := next;
	until not (i <= _length);

  _length := Length(preprocessed);

	result := 0; exit;
end;

function nitems(a : TArrayOfInteger) : Integer; overload;
begin
  Result := Length(a);
end;

procedure strcpy(out target : AnsiString; const source : AnsiString);
begin
  target := source;
end;

function strlen(const data : AnsiString) : Integer;
begin
  Result := Length(data);
end;

end.

