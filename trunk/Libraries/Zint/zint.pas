unit zint;
{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete

  Notes:
    - char-arrays are implemented as AnsiStrings -> take care of then 1-based index!
    - the code of library.c is implemented here as part of TZintSymbol
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils;

const
  ZINT_ROWS_MAX = 178;
  ZINT_COLS_MAX = 178;

type
  Pzint_render_line = ^zint_render_line;
  zint_render_line = record
    x, y, length, width : Single;
    next : Pzint_render_line;      { Pointer to next line }
  end;

  Pzint_render_string = ^zint_render_string;
  zint_render_string = record
    x, y, fsize : Single;
    width : Single;                { Suggested string width, may be 0 if none recommended }
    length : Integer;
    text : AnsiString;
    next : Pzint_render_string;    { Pointer to next character }
  end;

  Pzint_render_ring = ^zint_render_ring;
  zint_render_ring  = record
    x, y, radius, line_width : Single;
    next : Pzint_render_ring;      { Pointer to next ring }
  end;

  Pzint_render_hexagon = ^zint_render_hexagon;
  zint_render_hexagon = record
    x, y, width, height : Single;
    next : Pzint_render_hexagon;   { Pointer to next hexagon }
  end;

  Pzint_render = ^zint_render;
  zint_render = record
    width, height : Single;
    lines : Pzint_render_line;          { Pointer to first line }
    strings : Pzint_render_string;      { Pointer to first string }
    rings : Pzint_render_ring;          { Pointer to first ring }
    hexagons : Pzint_render_hexagon;    { Pointer to first hexagon }
  end;

  TZintCustomRenderTarget = class;

  zint_symbol = class(TPersistent)
  public
    symbology : Integer;
    height : Integer;
    whitespace_width : Integer;
    border_width : Integer;
    output_options : Integer;
    option_1 : Integer;
    option_2 : Integer;
    option_3 : Integer;
    show_hrt : Integer;
    input_mode : Integer;
    text : AnsiString;
    rows : Integer;
    width : Integer;
    primary : AnsiString;
    errtxt : AnsiString;
    encoded_data : array[0..ZINT_ROWS_MAX - 1] of array[0..ZINT_COLS_MAX - 1] of Byte;
    row_height : array[0..ZINT_ROWS_MAX - 1] of Integer; { Largest symbol is 177x177 QR Code }
    rendered : Pzint_render;

    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure ClearRendered; virtual;

    procedure Encode(AData : AnsiString; ARaiseExceptions : Boolean = true); virtual;
    procedure Render(ATarget : TZintCustomRenderTarget); virtual;

    //These are the functions from library.c
    class function gs1_compliant(_symbology : Integer) : Integer;
    class procedure error_tag(var error_string : AnsiString; error_number : Integer);
    class function hibc(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
    class function extended_charset(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
    class function reduced_charset(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
    class function ZBarcode_Encode(symbol : zint_symbol; source : AnsiString) : Integer;
  end;

  TZintSymbol = zint_symbol;

  TZintRenderAdjustMode = (ramNone, ramScaleBarcode, ramInflateImage);

  TZintCustomRenderTarget = class(TObject)
  protected
    FHexagonScale: Single;
    FTransparent: Boolean;
    FRenderAdjustMode : TZintRenderAdjustMode;
    FHeightDesired : Single;
    FWidthDesired  : Single;
    FTop           : Single;
    FLeft          : Single;
    FMultiplikator : Single;
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    procedure Render(ASymbol : TZintSymbol); virtual;
    property Top: Single read FTop write FTop;
    property Left: Single read FLeft write FLeft;
    property HeightDesired: Single read FHeightDesired write FHeightDesired;
    property WidthDesired: Single read FWidthDesired write FWidthDesired;
    property RenderAdjustMode : TZintRenderAdjustMode read FRenderAdjustMode write FRenderAdjustMode;
    property Transparent : Boolean read FTransparent write FTransparent;
    property HexagonScale : Single read FHexagonScale write FHexagonScale;
  end;

const
  BARCODE_CODE11 = 1;
  BARCODE_C25MATRIX = 2;
  BARCODE_C25INTER = 3;
  BARCODE_C25IATA = 4;
  BARCODE_C25LOGIC = 6;
  BARCODE_C25IND = 7;
  BARCODE_CODE39 = 8;
  BARCODE_EXCODE39 = 9;
  BARCODE_EANX = 13;
  BARCODE_EAN128 = 16;
  BARCODE_CODABAR = 18;
  BARCODE_CODE128 = 20;
  BARCODE_DPLEIT = 21;
  BARCODE_DPIDENT = 22;
  BARCODE_CODE16K = 23;
  BARCODE_CODE49 = 24;
  BARCODE_CODE93 = 25;
  BARCODE_FLAT = 28;
  BARCODE_RSS14 = 29;
  BARCODE_RSS_LTD = 30;
  BARCODE_RSS_EXP = 31;
  BARCODE_TELEPEN = 32;
  BARCODE_UPCA = 34;
  BARCODE_UPCE = 37;
  BARCODE_POSTNET = 40;
  BARCODE_MSI_PLESSEY = 47;
  BARCODE_FIM = 49;
  BARCODE_LOGMARS = 50;
  BARCODE_PHARMA = 51;
  BARCODE_PZN = 52;
  BARCODE_PHARMA_TWO = 53;
  BARCODE_PDF417 = 55;
  BARCODE_PDF417TRUNC = 56;
  BARCODE_MAXICODE = 57;
  BARCODE_QRCODE = 58;
  BARCODE_CODE128B = 60;
  BARCODE_AUSPOST = 63;
  BARCODE_AUSREPLY = 66;
  BARCODE_AUSROUTE = 67;
  BARCODE_AUSREDIRECT = 68;
  BARCODE_ISBNX = 69;
  BARCODE_RM4SCC = 70;
  BARCODE_DATAMATRIX = 71;
  BARCODE_EAN14 = 72;
  BARCODE_CODABLOCKF = 74;
  BARCODE_NVE18 = 75;
  BARCODE_JAPANPOST = 76;
  BARCODE_KOREAPOST = 77;
  BARCODE_RSS14STACK = 79;
  BARCODE_RSS14STACK_OMNI = 80;
  BARCODE_RSS_EXPSTACK = 81;
  BARCODE_PLANET = 82;
  BARCODE_MICROPDF417 = 84;
  BARCODE_ONECODE = 85;
  BARCODE_PLESSEY = 86;

{ Tbarcode 8 codes  }
  BARCODE_TELEPEN_NUM = 87;
  BARCODE_ITF14 = 89;
  BARCODE_KIX = 90;
  BARCODE_AZTEC = 92;
  BARCODE_DAFT = 93;
  BARCODE_MICROQR = 97;

{ Tbarcode 9 codes  }
  BARCODE_HIBC_128 = 98;
  BARCODE_HIBC_39 = 99;
  BARCODE_HIBC_DM = 102;
  BARCODE_HIBC_QR = 104;
  BARCODE_HIBC_PDF = 106;
  BARCODE_HIBC_MICPDF = 108;
  BARCODE_HIBC_BLOCKF = 110;
  BARCODE_HIBC_AZTEC = 112;

{ Zint specific  }
  BARCODE_AZRUNE = 128;
  BARCODE_CODE32 = 129;
  BARCODE_EANX_CC = 130;
  BARCODE_EAN128_CC = 131;
  BARCODE_RSS14_CC = 132;
  BARCODE_RSS_LTD_CC = 133;
  BARCODE_RSS_EXP_CC = 134;
  BARCODE_UPCA_CC = 135;
  BARCODE_UPCE_CC = 136;
  BARCODE_RSS14STACK_CC = 137;
  BARCODE_RSS14_OMNI_CC = 138;
  BARCODE_RSS_EXPSTACK_CC = 139;
  BARCODE_CHANNEL = 140;
  BARCODE_CODEONE = 141;
  BARCODE_GRIDMATRIX = 142;

  BARCODE_NO_ASCII = 1;
  BARCODE_BIND = 2;
  BARCODE_BOX = 4;
  BARCODE_STDOUT = 8;
  READER_INIT = 16;
  SMALL_TEXT = 32;

  DATA_MODE = 0;
  UNICODE_MODE = 1;
  GS1_MODE = 2;
  KANJI_MODE = 3;
  SJIS_MODE = 4;

  DM_SQUARE = 100;

  ZWARN_INVALID_OPTION = 2;
  ZERROR_TOO_LONG = 5;
  ZERROR_INVALID_DATA = 6;
  ZERROR_INVALID_CHECK = 7;
  ZERROR_INVALID_OPTION = 8;
  ZERROR_ENCODING_PROBLEM = 9;
  ZERROR_FILE_ACCESS = 10;
  ZERROR_MEMORY = 11;

implementation

uses zint_dmatrix, zint_code128, zint_gs1, zint_common, zint_2of5,
  zint_maxicode, zint_auspost, zint_aztec, zint_code, zint_medical,
  zint_code16k, zint_code49, zint_render_;

const
  TECHNETIUM : AnsiString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%';

{ TZintSymbol }

procedure zint_symbol.Clear;
var
  i, j : Integer;
begin
	for i := 0 to rows - 1 do
		for j := 0 to width - 1 do
			unset_module(Self, i, j);

	rows := 0;
	width := 0;
	text := '';
	errtxt := '';
end;

procedure zint_symbol.ClearRendered;
var
  current_line, next_line : Pzint_render_line;
  current_string, next_string : Pzint_render_string;
  current_ring, next_ring : Pzint_render_ring;
  current_hexagon, next_hexagon : Pzint_render_hexagon;
begin
  if Assigned(rendered) then
  begin
    next_line := rendered^.lines;
    while Assigned(next_line) do
    begin
      current_line := next_line;
      next_line := current_line^.next;
      Dispose(current_line);
    end;

    next_string := rendered^.strings;
    while Assigned(next_string) do
    begin
      current_string := next_string;
      next_string := current_string^.next;
      current_string^.text := '';
      Dispose(current_string);
    end;

    next_ring := rendered^.rings;
    while Assigned(next_ring) do
    begin
      current_ring := next_ring;
      next_ring := current_ring^.next;
      Dispose(current_ring);
    end;

    next_hexagon := rendered^.hexagons;
    while Assigned(next_hexagon) do
    begin
      current_hexagon := next_hexagon;
      next_hexagon := current_hexagon^.next;
      Dispose(current_hexagon);
    end;

    Dispose(rendered);
  end;
end;

constructor zint_symbol.Create;
begin
  inherited;

  symbology := BARCODE_CODE128;
	height := 0;
	whitespace_width := 0;
	border_width := 0;
  output_options := 0;
	rows := 0;
	width := 0;
	option_1 := -1;
	option_2 := 0;
	option_3 := 928; // PDF_MAX
	show_hrt := 1; // Show human readable text
	input_mode := DATA_MODE;
	primary := '';
  rendered := nil;
end;

destructor zint_symbol.Destroy;
begin
  Clear;
  ClearRendered;
  
  inherited;
end;

procedure zint_symbol.Encode(AData: AnsiString; ARaiseExceptions: Boolean);
begin
  if (ZBarcode_Encode(Self, AData) >= ZERROR_TOO_LONG) and ARaiseExceptions then
    raise Exception.Create(self.errtxt);
end;

class procedure zint_symbol.error_tag(var error_string : AnsiString; error_number : Integer);
begin
	if (error_number <> 0) then
  begin
		if(error_number > 4) then
			error_string := 'error: ' + error_string
		else
			error_string := 'warning: ' + error_string
  end;
end;

class function zint_symbol.hibc(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
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

class function zint_symbol.gs1_compliant(_symbology : Integer) : Integer;
{ Returns 1 if symbology supports GS1 data }
begin
  result := 0;

	case _symbology of
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

class function zint_symbol.extended_charset(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
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

class function zint_symbol.reduced_charset(symbol : zint_symbol; source : AnsiString; _length : Integer) : Integer;
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
		BARCODE_CODE16K: error_number := code16k(symbol, preprocessed, _length);
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
		BARCODE_CODE49: error_number := code_49(symbol, preprocessed, _length);
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

procedure zint_symbol.Render(ATarget : TZintCustomRenderTarget);
begin
  ClearRendered;
  render_plot(Self, ATarget.FWidthDesired, ATarget.FHeightDesired);
  ATarget.Render(Self);
end;

class function zint_symbol.ZBarcode_Encode(symbol : zint_symbol; source : AnsiString) : Integer;
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

{ TZintCustomRenderTarget }

constructor TZintCustomRenderTarget.Create();
begin
  FTransparent:=False;
  FHexagonScale:=0.9;
end;

destructor TZintCustomRenderTarget.Destroy;
begin
  inherited;
end;

procedure TZintCustomRenderTarget.Render(ASymbol: TZintSymbol);
begin
  if FRenderAdjustMode=ramScaleBarcode then
  begin
    FMultiplikator:=FHeightDesired/ASymbol.rendered^.height;

    if FMultiplikator*ASymbol.rendered^.width>FWidthDesired then
      FMultiplikator:=FWidthDesired/ASymbol.rendered^.width;
  end
  else
    FMultiplikator := 1;
end;

end.

