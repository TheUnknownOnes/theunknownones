unit zint_gs1;

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
  SysUtils, zint_common, zint;

function gs1_verify(symbol : zint_symbol; source : AnsiString; var reduced : AnsiString) : Integer;
function ugs1_verify(symbol : zint_symbol; source : AnsiString; var reduced : AnsiString) : Integer;

implementation

{ This code does some checks on the integrity of GS1 data. It is not intended
   to be bulletproof, nor does it report very accurately what problem was found
   or where, but should prevent some of the more common encoding errors }

procedure itostr(var ai_string : AnsiString; ai_value : Integer);
var
  thou, hund, ten, _unit : Integer;
begin
	ai_string := '(';
	thou := ai_value div 1000;
	hund := (ai_value - (1000 * thou)) div 100;
	ten := (ai_value - ((1000 * thou) + (100 * hund))) div 10;
	_unit := ai_value - ((1000 * thou) + (100 * hund) + (10 * ten));

	if(ai_value >= 1000) then begin ai_string := ai_string + itoc(thou); end;
	if(ai_value >= 100) then begin ai_string := ai_string + itoc(hund); end;
  ai_string := ai_string + itoc(ten);
  ai_string := ai_string + itoc(_unit);
	ai_string := ai_string + ')';
end;

function gs1_verify(symbol : zint_symbol; source : AnsiString; var reduced : AnsiString) : Integer;
var
  i, j, last_ai, ai_latch : Integer;
  ai_string : AnsiString;
  bracket_level, max_bracket_level, ai_length, max_ai_length, min_ai_length : Integer;
  ai_value, ai_location, data_location, data_length : array[0..99] of Integer;
  ai_count : Integer;
  error_latch : Integer;
begin
	{ Detect extended ASCII characters }
	for i := 1 to length(source) do
  begin
		if Ord(source[i]) >= 128 then
    begin
      symbol.errtxt := 'Extended ASCII characters are not supported by GS1';
			result := ZERROR_INVALID_DATA; exit;
		end;
		if Ord(source[i]) < 32 then
    begin
			symbol.errtxt := 'Control characters are not supported by GS1';
			result := ZERROR_INVALID_DATA; exit;
		end;
	end;

	if source[1] <> '[' then
  begin
		symbol.errtxt := 'Data does not start with an AI';
		result := ZERROR_INVALID_DATA; exit;
	end;

	{ Check the position of the brackets }
	bracket_level := 0;
	max_bracket_level := 0;
	ai_length := 0;
	max_ai_length := 0;
	min_ai_length := 5;
	j := 0;
	ai_latch := 0;
	for i := 1 to length(source) do
  begin
		Inc(ai_length, j);
		if (((j = 1) and (source[i] <> ']')) and ((source[i] < '0') or (source[i] > '9'))) then ai_latch := 1;
		if (source[i] = '[') then begin Inc(bracket_level); j := 1; end;
		if (source[i] = ']') then
    begin
			Dec(bracket_level);
			if (ai_length < min_ai_length) then min_ai_length := ai_length;
			j := 0;
			ai_length := 0;
		end;
		if (bracket_level > max_bracket_level) then max_bracket_level := bracket_level;
		if (ai_length > max_ai_length) then max_ai_length := ai_length;
	end;
	Dec(min_ai_length);

	if (bracket_level <> 0) then
  begin
		{ Not all brackets are closed }
		symbol.errtxt := 'Malformed AI in input data (brackets don''t match)';
		result := ZERROR_INVALID_DATA; exit;
	end;

	if (max_bracket_level > 1) then
  begin
		{ Nested brackets }
		symbol.errtxt := 'Found nested brackets in input data';
		result := ZERROR_INVALID_DATA; exit;
	end;

	if(max_ai_length > 4) then
  begin
		{ AI is too long }
		symbol.errtxt := 'Invalid AI in input data (AI too long)';
		result := ZERROR_INVALID_DATA; exit;
	end;

	if(min_ai_length <= 1) then
  begin
		{ AI is too short }
		symbol.errtxt := 'Invalid AI in input data (AI too short)';
		result := ZERROR_INVALID_DATA; exit;
	end;

	if(ai_latch = 1) then
  begin
		{ Non-numeric data in AI }
		symbol.errtxt := 'Invalid AI in input data (non-numeric characters in AI)';
		result := ZERROR_INVALID_DATA; exit;
	end;

	ai_count := 0;
	for i := 2 to length(source) do
  begin
    ai_string := '';
		if (source[i - 1] = '[') then
    begin
			ai_location[ai_count] := i;
			j := 0;
			repeat
				ai_string := ai_string + source[i + j];
        Inc(j)
			until not (source[i + j] <> ']');

			ai_value[ai_count] := StrToInt(ai_string);
			Inc(ai_count);
		end;
	end;

	for i := 0 to ai_count - 1 do
  begin
		data_location[i] := ai_location[i] + 3;
		if (ai_value[i] >= 100) then Inc(data_location[i]);
		if (ai_value[i] >= 1000) then Inc(data_location[i]);
		data_length[i] := 0;
		repeat
			Inc(data_length[i]);
		until not ((source[data_location[i] + data_length[i] - 1] <> '[') and (data_location[i] + data_length[i] - 1 <= Length(source)));
		Dec(data_length[i]);
	end;

	for i := 0 to ai_count - 1 do
  begin
		if(data_length[i] = 0) then
    begin
			{ No data for given AI }
			symbol.errtxt := 'Empty data field in input data';
			result := ZERROR_INVALID_DATA; exit;
		end;
	end;

	error_latch := 0;
	ai_string := '';
	for i := 0 to ai_count - 1 do
  begin
		case ai_value[i] of
			0: if(data_length[i] <> 18) then error_latch := 1;
			1,
			2,
			3: if(data_length[i] <> 14) then error_latch := 1;
			4: if(data_length[i] <> 16) then error_latch := 1;
			11,
			12,
			13,
			14,
			15,
			16,
			17,
			18,
			19: if(data_length[i] <> 6) then error_latch := 1;
			20: if(data_length[i] <> 2) then error_latch := 1;
			23,
			24,
			25,
			39,
			40,
			41,
			42,
			70,
			80,
			81: error_latch := 2;
		end;
		if (
			 ((ai_value[i] >= 100) and (ai_value[i] <= 179))
			 or ((ai_value[i] >= 1000) and (ai_value[i] <= 1799))
			 or ((ai_value[i] >= 200) and (ai_value[i] <= 229))
			 or ((ai_value[i] >= 2000) and (ai_value[i] <= 2299))
			 or ((ai_value[i] >= 300) and (ai_value[i] <= 309))
			 or ((ai_value[i] >= 3000) and (ai_value[i] <= 3099))
			 or ((ai_value[i] >= 31) and (ai_value[i] <= 36))
			 or ((ai_value[i] >= 310) and (ai_value[i] <= 369))
		) then
			error_latch := 2;
		if((ai_value[i] >= 3100) and (ai_value[i] <= 3699)) then
    begin
			if (data_length[i] <> 6) then
				error_latch := 1;
		end;
		if (
			 ((ai_value[i] >= 370) and (ai_value[i] <= 379))
			 or ((ai_value[i] >= 3700) and (ai_value[i] <= 3799))
		) then
			error_latch := 2;
		if ((ai_value[i] >= 410) and (ai_value[i] <= 415)) then
    begin
			if(data_length[i] <> 13) then
				error_latch := 1;
		end;
		if (
			 ((ai_value[i] >= 4100) and (ai_value[i] <= 4199))
			 or ((ai_value[i] >= 700) and (ai_value[i] <= 703))
			 or ((ai_value[i] >= 800) and (ai_value[i] <= 810))
			 or ((ai_value[i] >= 900) and (ai_value[i] <= 999))
			 or ((ai_value[i] >= 9000) and (ai_value[i] <= 9999))
		) then
			error_latch := 2;
		if((error_latch < 4) and (error_latch > 0)) then
    begin
			{ error has just been detected: capture AI }
			itostr(ai_string, ai_value[i]);
			Inc(error_latch, 4);
		end;
	end;

	if(error_latch = 5) then
  begin
    symbol.errtxt := 'Invalid data length for AI ' + ai_string;
		result := ZERROR_INVALID_DATA; exit;
	end;

	if(error_latch = 6) then
  begin
    symbol.errtxt := 'Invalid AI value ' + ai_string;
		result := ZERROR_INVALID_DATA; exit;
	end;

	{ Resolve AI data - put resulting string in 'reduced' }
	reduced := '';
	last_ai := 0;
	ai_latch := 1;
	for i := 1 to Length(source) do
  begin
		if ((source[i] <> '[') and (source[i] <> ']')) then
			reduced := reduced + source[i];
		if (source[i] = '[') then
    begin
			{ Start of an AI string }
			if(ai_latch = 0) then
      begin
				reduced := reduced + '[';
      end;
      SetLength(ai_string, 2);
			ai_string[1] := source[i + 1];
			ai_string[2] := source[i + 2];
			last_ai := StrToInt(ai_string);
			ai_latch := 0;
			{ The following values from "GS-1 General Specification version 8.0 issue 2, May 2008"
			figure 5.4.8.2.1 - 1 "Element Strings with Pre-Defined Length Using Application Identifiers" }
			if(
				((last_ai >= 0) and (last_ai <= 4))
				or ((last_ai >= 11) and (last_ai <= 20))
				or (last_ai = 23) { legacy support - see 5.3.8.2.2 }
				or ((last_ai >= 31) and (last_ai <= 36))
				or (last_ai = 41)
			) then
				ai_latch := 1;
		end;
		{ The ']' character is simply dropped from the input }
	end;

	{ the character '[' in the reduced string refers to the FNC1 character }
	result := 0; exit;
end;

function ugs1_verify(symbol : zint_symbol; source : AnsiString; var reduced : AnsiString) : Integer;
var
  temp : AnsiString;
  error_number : Integer;
begin
	error_number := gs1_verify(symbol, source, temp);
	if (error_number <> 0) then begin Result := error_number; exit; end;

	if (Length(temp) < Length(source) + 5) then
  begin
    reduced := temp;
		Result := 0; exit;
  end;
	symbol.errtxt := 'ugs1_verify overflow';
	result := ZERROR_INVALID_DATA; exit;
end;

end.

