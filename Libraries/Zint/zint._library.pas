unit zint._library;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete

  Notes:
    - only the used functions are implemented
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint.zint;

function ZBarcode_Create : zint_symbol;
procedure ZBarcode_Clear(var symbol : zint_symbol);
function ZBarcode_ValidID(symbol_id : Integer) : Integer;
function ZBarcode_Encode(var symbol : zint_symbol; source : AnsiString) : Integer;

implementation

uses zint.dmatrix, zint.code128, zint.gs1, zint.common, zint._2of5,
  zint.maxicode, zint.auspost, zint.aztec, zint.code, zint.medical;

const
  TECHNETIUM : AnsiString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%';

function ZBarcode_Create : zint_symbol;
begin
  FillChar(Result, SizeOf(zint_symbol), 0);

	Result.symbology := BARCODE_CODE128;
	Result.height := 0;
	Result.whitespace_width := 0;
	Result.border_width := 0;
	Result.output_options := 0;
	Result.rows := 0;
	Result.width := 0;
	Result.fgcolour := 'clBlack';
	Result.bgcolour := 'clWhite';
	Result.option_1 := -1;
	Result.option_2 := 0;
	Result.option_3 := 928; // PDF_MAX
	Result.show_hrt := 1; // Show human readable text
	Result.input_mode := DATA_MODE;
	Result.primary := '';
end;

procedure ZBarcode_Clear(var symbol : zint_symbol);
var
  i, j : Integer;
begin
	for i := 0 to symbol.rows - 1 do
		for j := 0 to symbol.width - 1 do
			unset_module(symbol, i, j);

	symbol.rows := 0;
	symbol.width := 0;
	symbol.text := '';
	symbol.errtxt := '';
end;

procedure error_tag(var error_string : AnsiString; error_number : Integer);
begin
	if (error_number <> 0) then
  begin
		if(error_number > 4) then
			error_string := 'error: ' + error_string
		else
			error_string := 'warning: ' + error_string
  end;
end;

function hibc(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  counter, error_number, i : Integer;
  to_process, temp : AnsiString;
  check_digit : AnsiChar;
begin
	if (_length > 36) then
  begin
		strcpy(symbol.errtxt, 'Data too long for HIBC LIC');
		result := ZERROR_TOO_LONG; exit;
	end;
	to_upper(source);
	error_number := is_sane(TECHNETIUM, source, _length);
	if (error_number = ZERROR_INVALID_DATA) then
  begin
		strcpy(symbol.errtxt, 'Invalid characters in data');
		result := error_number; exit;
	end;

	strcpy(to_process, '+');
	counter := 41;
	for i := 1 to _length do
		Inc(counter, Pos(source[i], TECHNETIUM) - 1);

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
				42: check_digit := '%';
				else check_digit := ' '; { Keep compiler happy }
			end;
		end;
	end;

	temp := check_digit;
  concat(to_process, source);
  concat(to_process, temp);
  _length := strlen(to_process);

	case symbol.symbology of
		BARCODE_HIBC_128:
    begin
			error_number := code_128(symbol, to_process, _length);
      ustrcpy(symbol.text, '*');
      uconcat(symbol.text, to_process);
      uconcat(symbol.text, '*');
    end;
    BARCODE_HIBC_39:
    begin
			symbol.option_2 := 0;
			error_number := c39(symbol, to_process, _length);
      ustrcpy(symbol.text, '*');
      uconcat(symbol.text, to_process);
      uconcat(symbol.text, '*');
    end;
    BARCODE_HIBC_DM:
			error_number := dmatrix(symbol, to_process, _length);
		{BARCODE_HIBC_QR:
			error_number := qr_code(symbol, to_process, _length);
		BARCODE_HIBC_PDF:
			error_number := pdf417enc(symbol, to_process, _length);
		BARCODE_HIBC_MICPDF:
			error_number := micro_pdf417(symbol, to_process, _length);}
		BARCODE_HIBC_AZTEC:
			error_number := aztec(symbol, to_process, _length);
	end;

	Result := error_number; exit;
end;

function gs1_compliant(symbology : Integer) : Integer;
{ Returns 1 if symbology supports GS1 data }
begin
  result := 0;

	case symbology of
		BARCODE_EAN128,
		BARCODE_RSS_EXP,
		BARCODE_RSS_EXPSTACK,
		BARCODE_EANX_CC,
		BARCODE_EAN128_CC,
		BARCODE_RSS14_CC,
		BARCODE_RSS_LTD_CC,
		BARCODE_RSS_EXP_CC,
		BARCODE_UPCA_CC,
		BARCODE_UPCE_CC,
		BARCODE_RSS14STACK_CC,
		BARCODE_RSS14_OMNI_CC,
		BARCODE_RSS_EXPSTACK_CC,
		BARCODE_CODE16K,
		BARCODE_AZTEC,
		BARCODE_DATAMATRIX,
		BARCODE_CODEONE,
		BARCODE_CODE49,
		BARCODE_QRCODE:
			result := 1;
	end;
end;

function ZBarcode_ValidID(symbol_id : Integer) : Integer;
{ Checks whether a symbology is supported }
begin
	result := 0;

	case symbol_id of
		BARCODE_CODE11,
		BARCODE_C25MATRIX,
		BARCODE_C25INTER,
		BARCODE_C25IATA,
		BARCODE_C25LOGIC,
		BARCODE_C25IND,
		BARCODE_CODE39,
		BARCODE_EXCODE39,
		BARCODE_EANX,
		BARCODE_EAN128,
		BARCODE_CODABAR,
		BARCODE_CODE128,
		BARCODE_DPLEIT,
		BARCODE_DPIDENT,
		BARCODE_CODE16K,
		BARCODE_CODE49,
		BARCODE_CODE93,
		BARCODE_FLAT,
		BARCODE_RSS14,
		BARCODE_RSS_LTD,
		BARCODE_RSS_EXP,
		BARCODE_TELEPEN,
		BARCODE_UPCA,
		BARCODE_UPCE,
		BARCODE_POSTNET,
		BARCODE_MSI_PLESSEY,
		BARCODE_FIM,
		BARCODE_LOGMARS,
		BARCODE_PHARMA,
		BARCODE_PZN,
		BARCODE_PHARMA_TWO,
		BARCODE_PDF417,
		BARCODE_PDF417TRUNC,
		BARCODE_MAXICODE,
		BARCODE_QRCODE,
		BARCODE_CODE128B,
		BARCODE_AUSPOST,
		BARCODE_AUSREPLY,
		BARCODE_AUSROUTE,
		BARCODE_AUSREDIRECT,
		BARCODE_ISBNX,
		BARCODE_RM4SCC,
		BARCODE_DATAMATRIX,
		BARCODE_EAN14,
		BARCODE_NVE18,
		BARCODE_JAPANPOST,
		BARCODE_KOREAPOST,
		BARCODE_RSS14STACK,
		BARCODE_RSS14STACK_OMNI,
		BARCODE_RSS_EXPSTACK,
		BARCODE_PLANET,
		BARCODE_MICROPDF417,
		BARCODE_ONECODE,
		BARCODE_PLESSEY,
		BARCODE_TELEPEN_NUM,
		BARCODE_ITF14,
		BARCODE_KIX,
		BARCODE_AZTEC,
		BARCODE_DAFT,
		BARCODE_MICROQR,
		BARCODE_HIBC_128,
		BARCODE_HIBC_39,
		BARCODE_HIBC_DM,
		BARCODE_HIBC_QR,
		BARCODE_HIBC_PDF,
		BARCODE_HIBC_MICPDF,
		BARCODE_HIBC_AZTEC,
		BARCODE_AZRUNE,
		BARCODE_CODE32,
		BARCODE_EANX_CC,
		BARCODE_EAN128_CC,
		BARCODE_RSS14_CC,
		BARCODE_RSS_LTD_CC,
		BARCODE_RSS_EXP_CC,
		BARCODE_UPCA_CC,
		BARCODE_UPCE_CC,
		BARCODE_RSS14STACK_CC,
		BARCODE_RSS14_OMNI_CC,
		BARCODE_RSS_EXPSTACK_CC,
		BARCODE_CHANNEL,
		BARCODE_CODEONE,
		BARCODE_GRIDMATRIX:
			result := 1;
  end;
end;

function extended_charset(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
var
  error_number : Integer;
begin
  error_number := 0;

	{ These are the "elite" standards which can support multiple character sets }
	{case symbol.symbology of
		BARCODE_QRCODE: error_number = qr_code(symbol, source, _length);
		BARCODE_MICROQR: error_number = microqr(symbol, source, _length);
		BARCODE_GRIDMATRIX: error_number = grid_matrix(symbol, source, _length);
	end;}

	Result := error_number; exit;
end;

function reduced_charset(var symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
{ These are the "norm" standards which only support Latin-1 at most }
var
  error_number : Integer;
  preprocessed : AnsiString;
begin
  error_number := 0;

	if (symbol.symbology = BARCODE_CODE16K) then
  begin
		symbol.whitespace_width := 16;
		symbol.border_width := 2;
		symbol.output_options := BARCODE_BIND;
	end;

	if (symbol.symbology = BARCODE_ITF14) then
  begin
		symbol.whitespace_width := 20;
		symbol.border_width := 8;
		symbol.output_options := BARCODE_BOX;
	end;

	case symbol.input_mode of
		DATA_MODE,
		GS1_MODE:
			preprocessed := source;
		UNICODE_MODE:
    begin
			error_number := latin1_process(symbol, source, preprocessed, _length);
			if (error_number <> 0) then begin result := error_number; exit; end;
    end;
	end;

	case symbol.symbology of
		BARCODE_C25MATRIX: error_number := matrix_two_of_five(symbol, preprocessed, _length);
		BARCODE_C25IND: error_number := industrial_two_of_five(symbol, preprocessed, _length);
		BARCODE_C25INTER: error_number := interleaved_two_of_five(symbol, preprocessed, _length);
		BARCODE_C25IATA: error_number := iata_two_of_five(symbol, preprocessed, _length);
		BARCODE_C25LOGIC: error_number := logic_two_of_five(symbol, preprocessed, _length);
		BARCODE_DPLEIT: error_number := dpleit(symbol, preprocessed, _length);
		BARCODE_DPIDENT: error_number := dpident(symbol, preprocessed, _length);
		//BARCODE_UPCA: error_number := eanx(symbol, preprocessed, _length);
		//BARCODE_UPCE: error_number := eanx(symbol, preprocessed, _length);
		//BARCODE_EANX: error_number := eanx(symbol, preprocessed, _length);
		BARCODE_EAN128: error_number := ean_128(symbol, preprocessed, _length);
		BARCODE_CODE39: error_number := c39(symbol, preprocessed, _length);
		BARCODE_PZN: error_number := pharmazentral(symbol, preprocessed, _length);
		BARCODE_EXCODE39: error_number := ec39(symbol, preprocessed, _length);
		BARCODE_CODABAR: error_number := codabar(symbol, preprocessed, _length);
		BARCODE_CODE93: error_number := c93(symbol, preprocessed, _length);
	  BARCODE_LOGMARS: error_number := c39(symbol, preprocessed, _length);
		BARCODE_CODE128: error_number := code_128(symbol, preprocessed, _length);
		BARCODE_CODE128B: error_number := code_128(symbol, preprocessed, _length);
		BARCODE_NVE18: error_number := nve_18(symbol, preprocessed, _length);
		BARCODE_CODE11: error_number := code_11(symbol, preprocessed, _length);
		//BARCODE_MSI_PLESSEY: error_number := msi_handle(symbol, preprocessed, _length);
		//BARCODE_TELEPEN: error_number := telepen(symbol, preprocessed, _length);
		//BARCODE_TELEPEN_NUM: error_number := telepen_num(symbol, preprocessed, _length);
		BARCODE_PHARMA: error_number := pharma_one(symbol, preprocessed, _length);
		//BARCODE_PLESSEY: error_number := plessey(symbol, preprocessed, _length);
		BARCODE_ITF14: error_number := itf14(symbol, preprocessed, _length);
		//BARCODE_FLAT: error_number := flattermarken(symbol, preprocessed, _length);
		//BARCODE_FIM: error_number := fim(symbol, preprocessed, _length);
		//BARCODE_POSTNET: error_number := post_plot(symbol, preprocessed, _length);
		//BARCODE_PLANET: error_number := planet_plot(symbol, preprocessed, _length);
		//BARCODE_RM4SCC: error_number := royal_plot(symbol, preprocessed, _length);
		BARCODE_AUSPOST: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSREPLY: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSROUTE: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSREDIRECT: error_number := australia_post(symbol, preprocessed, _length);
		//BARCODE_CODE16K: error_number := code16k(symbol, preprocessed, _length);
		BARCODE_PHARMA_TWO: error_number := pharma_two(symbol, preprocessed, _length);
		//BARCODE_ONECODE: error_number := imail(symbol, preprocessed, _length);
		//BARCODE_ISBNX: error_number := eanx(symbol, preprocessed, _length);
		//BARCODE_RSS14: error_number := rss14(symbol, preprocessed, _length);
		//BARCODE_RSS14STACK: error_number := rss14(symbol, preprocessed, _length);
		//BARCODE_RSS14STACK_OMNI: error_number := rss14(symbol, preprocessed, _length);
		//BARCODE_RSS_LTD: error_number := rsslimited(symbol, preprocessed, _length);
		//BARCODE_RSS_EXP: error_number := rssexpanded(symbol, preprocessed, _length);
		//BARCODE_RSS_EXPSTACK: error_number := rssexpanded(symbol, preprocessed, _length);
		//BARCODE_EANX_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_EAN128_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_RSS14_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_RSS_LTD_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_RSS_EXP_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_UPCA_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_UPCE_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_RSS14STACK_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_RSS14_OMNI_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_RSS_EXPSTACK_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_KIX: error_number := kix_code(symbol, preprocessed, _length);
		BARCODE_CODE32: error_number := code32(symbol, preprocessed, _length);
		//BARCODE_DAFT: error_number := daft_code(symbol, preprocessed, _length);
		BARCODE_EAN14: error_number := ean_14(symbol, preprocessed, _length);
		BARCODE_AZRUNE: error_number := aztec_runes(symbol, preprocessed, _length);
		//BARCODE_KOREAPOST: error_number := korea_post(symbol, preprocessed, _length);
		BARCODE_HIBC_128: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_39: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_DM: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_QR: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_PDF: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_MICPDF: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_AZTEC: error_number := hibc(symbol, preprocessed, _length);
		//BARCODE_JAPANPOST: error_number := japan_post(symbol, preprocessed, _length);
		//BARCODE_CODE49: error_number := code_49(symbol, preprocessed, _length);
		//BARCODE_CHANNEL: error_number := channel_code(symbol, preprocessed, _length);
		//BARCODE_CODEONE: error_number := code_one(symbol, preprocessed, _length);
		BARCODE_DATAMATRIX: error_number := dmatrix(symbol, preprocessed, _length);
		//BARCODE_PDF417: error_number := pdf417enc(symbol, preprocessed, _length);
		//BARCODE_PDF417TRUNC: error_number := pdf417enc(symbol, preprocessed, _length);
		//BARCODE_MICROPDF417: error_number := micro_pdf417(symbol, preprocessed, _length);
		BARCODE_MAXICODE: error_number := maxicode(symbol, preprocessed, _length);
		BARCODE_AZTEC: error_number := aztec(symbol, preprocessed, _length);
  end;

	result := error_number; exit;
end;

function ZBarcode_Encode(var symbol : zint_symbol; source : AnsiString) : Integer;
var
  _length : Integer;
  error_number, error_buffer, i : Integer;
  local_source : AnsiString;
begin
  _length := Length(source);
  error_number := 0;

	if (_length = 0) then
  begin
		symbol.errtxt := 'No input data';
		error_tag(symbol.errtxt, ZERROR_INVALID_DATA);
		Result := ZERROR_INVALID_DATA; exit;
	end;

	{ First check the symbology field }
	if (symbol.symbology < 1) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;

	{ symbol.symbologys 1 to 86 are defined by tbarcode }
	if (symbol.symbology = 5) then begin symbol.symbology := BARCODE_C25MATRIX; end;
	if ((symbol.symbology >= 10) and (symbol.symbology <= 12)) then begin symbol.symbology := BARCODE_EANX; end;
	if ((symbol.symbology = 14) or (symbol.symbology = 15)) then begin symbol.symbology := BARCODE_EANX; end;
	if (symbol.symbology = 17) then begin symbol.symbology := BARCODE_UPCA; end;
	if (symbol.symbology = 19) then begin strcpy(symbol.errtxt, 'Codabar 18 not supported, using Codabar'); symbol.symbology := BARCODE_CODABAR; error_number := ZWARN_INVALID_OPTION; end;
	if (symbol.symbology = 26) then begin symbol.symbology := BARCODE_UPCA; end;
	if (symbol.symbology = 27) then begin strcpy(symbol.errtxt, 'UPCD1 not supported'); error_number := ZERROR_INVALID_OPTION; end;
	if (symbol.symbology = 33) then begin symbol.symbology := BARCODE_EAN128; end;
	if ((symbol.symbology = 35) or (symbol.symbology = 36)) then begin symbol.symbology := BARCODE_UPCA; end;
	if ((symbol.symbology = 38) or (symbol.symbology = 39)) then begin symbol.symbology := BARCODE_UPCE; end;
	if ((symbol.symbology >= 41) and (symbol.symbology <= 45)) then begin symbol.symbology := BARCODE_POSTNET; end;
	if (symbol.symbology = 46) then begin symbol.symbology := BARCODE_PLESSEY; end;
	if (symbol.symbology = 48) then begin symbol.symbology := BARCODE_NVE18; end;
	if (symbol.symbology = 54) then begin strcpy(symbol.errtxt, 'General Parcel Code not supported, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	if ((symbol.symbology = 59) or (symbol.symbology = 61)) then begin symbol.symbology := BARCODE_CODE128; end;
	if (symbol.symbology = 62) then begin symbol.symbology := BARCODE_CODE93; end;
	if ((symbol.symbology = 64) or (symbol.symbology = 65)) then begin symbol.symbology := BARCODE_AUSPOST; end;
	if (symbol.symbology = 73) then begin strcpy(symbol.errtxt, 'Codablock E not supported'); error_number := ZERROR_INVALID_OPTION; end;
	if (symbol.symbology = 78) then begin symbol.symbology := BARCODE_RSS14; end;
	if (symbol.symbology = 83) then begin symbol.symbology := BARCODE_PLANET; end;
	if (symbol.symbology = 88) then begin symbol.symbology := BARCODE_EAN128; end;
	if (symbol.symbology = 91) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	if ((symbol.symbology >= 94) and (symbol.symbology <= 96)) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	if (symbol.symbology = 100) then begin symbol.symbology := BARCODE_HIBC_128; end;
	if (symbol.symbology = 101) then begin symbol.symbology := BARCODE_HIBC_39; end;
	if (symbol.symbology = 103) then begin symbol.symbology := BARCODE_HIBC_DM; end;
	if (symbol.symbology = 105) then begin symbol.symbology := BARCODE_HIBC_QR; end;
	if (symbol.symbology = 107) then begin symbol.symbology := BARCODE_HIBC_PDF; end;
	if (symbol.symbology = 109) then begin symbol.symbology := BARCODE_HIBC_MICPDF; end;
	if (symbol.symbology = 111) then begin symbol.symbology := BARCODE_HIBC_BLOCKF; end;
	if ((symbol.symbology >= 113) and (symbol.symbology <= 127)) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	{ Everything from 128 up is Zint-specific }
	if (symbol.symbology >= 143) then begin strcpy(symbol.errtxt, 'Symbology out of range, using Code 128'); symbol.symbology := BARCODE_CODE128; error_number := ZWARN_INVALID_OPTION; end;
	if ((symbol.symbology = BARCODE_CODABLOCKF) or (symbol.symbology = BARCODE_HIBC_BLOCKF)) then begin strcpy(symbol.errtxt, 'Codablock F not supported'); error_number := ZERROR_INVALID_OPTION; end;

	if (error_number > 4) then
  begin
		error_tag(symbol.errtxt, error_number);
		result := error_number; exit;
	end
  else
		error_buffer := error_number;

	if ((symbol.input_mode < 0) or (symbol.input_mode > 2)) then begin symbol.input_mode := DATA_MODE; end;

	if (symbol.input_mode = GS1_MODE) then
  begin
		for i := 1 to _length do
    begin
			if (source[i] = #0) then
      begin
				strcpy(symbol.errtxt, 'NULL characters not permitted in GS1 mode');
				result := ZERROR_INVALID_DATA; exit;
			end;
		end;
		if (gs1_compliant(symbol.symbology) = 1) then
    begin
			error_number := ugs1_verify(symbol, source, local_source);
			if (error_number <> 0) then begin result := error_number; exit; end;
			_length := ustrlen(local_source);
		end
    else
    begin
			strcpy(symbol.errtxt, 'Selected symbology does not support GS1 mode');
			result := ZERROR_INVALID_OPTION; exit;
    end;
  end
  else
		local_source := source;

	case symbol.symbology of
		BARCODE_QRCODE,
		BARCODE_MICROQR,
		BARCODE_GRIDMATRIX:
			error_number := extended_charset(symbol, local_source, _length);
    else
			error_number := reduced_charset(symbol, local_source, _length);
	end;

	if ((symbol.symbology = BARCODE_CODE128) or (symbol.symbology = BARCODE_CODE128B)) then
  begin
		for i := 1 to _length do
    begin
			if (local_source[i] = #0) then
				symbol.text := symbol.text + ' '
      else
				symbol.text := symbol.text + local_source[i];
		end;
	end;

	if (error_number = 0) then
		error_number := error_buffer;

	error_tag(symbol.errtxt, error_number);
	{printf('%s\n',symbol.text);}
	result := error_number; exit;
end;

end.

