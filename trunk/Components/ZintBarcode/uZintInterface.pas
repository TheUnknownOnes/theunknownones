{----------------------------------------------------------------------------- 
 Purpose: Useful classes and components to work with the zint library
 Created: 03.12.2009 14:29:10

 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uZintInterface;

interface 

{$M+}

uses
  Windows,
  Classes,
  SysUtils,
  Graphics;

type
  TZSymbol = record
    symbology : Integer;
    height : Integer;
    whitespace_width : Integer;
    border_width : Integer;
    output_options : Integer;
    fgcolour : array[0..9] of AnsiChar;
    bgcolour : array[0..9] of AnsiChar;
    outfile : array [0..255] of AnsiChar;
    scale : Single;
    option_1 : Integer;
    option_2 : Integer;
    option_3 : Integer;
    input_mode : Integer;
    text : array[0..127] of AnsiChar;
    rows : Integer;
    width : Integer;
    primary : array [0..127] of AnsiChar;
    encoded_data : array[0..177] of array[0..142] of AnsiChar;
    row_height : array[0..177] of Integer; // Largest symbol is 177x177 QR Code
    errtxt : array[0..99] of AnsiChar;
    bitmap : PAnsiChar;
    bitmap_width : Integer;
    bitmap_height : Integer;
    show_human_readable_text : Integer;
  end;
  PZSymbol = ^TZSymbol;

const
  { Tbarcode 7 codes }
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

  { Tbarcode 8 codes }
  BARCODE_TELEPEN_NUM = 87;
  BARCODE_ITF14 = 89;
  BARCODE_KIX = 90;
  BARCODE_AZTEC = 92;
  BARCODE_DAFT = 93;
  BARCODE_MICROQR = 97;

  { Tbarcode 9 codes }
  BARCODE_HIBC_128 = 98;
  BARCODE_HIBC_39 = 99;
  BARCODE_HIBC_DM = 102;
  BARCODE_HIBC_QR = 104;
  BARCODE_HIBC_PDF = 106;
  BARCODE_HIBC_MICPDF = 108;
  BARCODE_HIBC_BLOCKF = 110;
  BARCODE_HIBC_AZTEC = 112;

  { Zint specific }
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

type
  TZOutputOptions = (
    NONE = 0,
    BARCODE_NO_ASCII	= 1,
    BARCODE_BIND		= 2,
    BARCODE_BOX		= 4);

  TZInputMode = (
    DATA_MODE	= 0,
    UNICODE_MODE	= 1,
    GS1_MODE	= 2,
    KANJI_MODE	= 3,
    SJIS_MODE	= 4);

  TZErrorCode = (
    WARN_INVALID_OPTION	= 2,
    ERROR_TOO_LONG		= 5,
    ERROR_INVALID_DATA	= 6,
    ERROR_INVALID_CHECK	= 7,
    ERROR_INVALID_OPTION	= 8,
    ERROR_ENCODING_PROBLEM	= 9,
    ERROR_FILE_ACCESS	= 10,
    ERROR_MEMORY		= 11);

const
  LibName = 'zint.dll';


  function ZBarcode_Create() : PZSymbol; cdecl; external LibName;
  procedure ZBarcode_Delete(symbol : PZSymbol); cdecl; external LibName;
  procedure ZBarcode_Clear(symbol : PZSymbol); cdecl; external LibName;

  function ZBarcode_Buffer(symbol : PZSymbol; rotate_angle : Integer) : Integer; cdecl; external LibName;
  function ZBarcode_Encode_and_Buffer(symbol : PZSymbol; input : PAnsiChar; length : Integer; rotate_angle : Integer) : Integer; cdecl; external LibName;

  procedure ZBarcodeToBitmap(ASymbol : PZSymbol; const ABitmap : TBitmap);

implementation

procedure ZBarcodeToBitmap(ASymbol : PZSymbol; const ABitmap : TBitmap);
var
  //bmpinfo : TBitmapInfo;
  myp : PRGBTriple;
  row : Integer;
  rowwidth : Integer;
begin
  ABitmap.PixelFormat := pf24bit;
  ABitmap.SetSize(ASymbol.bitmap_width, ASymbol.bitmap_height);

  {FillMemory(@bmpinfo.bmiHeader, sizeof(bmpinfo.bmiHeader), 0);
  bmpinfo.bmiHeader.biSize := SizeOf(bmpinfo.bmiHeader);
  bmpinfo.bmiHeader.biWidth := ASymbol.bitmap_width;
  bmpinfo.bmiHeader.biHeight := -ASymbol.bitmap_height;
  bmpinfo.bmiHeader.biPlanes := 1;
  bmpinfo.bmiHeader.biBitCount := 24;
  bmpinfo.bmiHeader.biCompression := BI_RGB;}

  myp := Pointer(ASymbol.bitmap);
  rowwidth := Asymbol.bitmap_width * 3;

  for row := 0 to ASymbol.bitmap_height - 1 do
  begin
    CopyMemory(ABitmap.ScanLine[row], myp, rowwidth);
    Inc(myp, Asymbol.bitmap_width);
  end;



  //Normaly StretchDIBits should do this work. But it has problems with odd image_width
  //SetBitmapBits(ABitmap.Handle, ASymbol.bitmap_width * ASymbol.bitmap_height * 3, ASymbol.bitmap);

  {StretchDIBits(ABitmap.Canvas.Handle,
                0, 0, ASymbol.bitmap_width, ASymbol.bitmap_height,
                0, 0, ASymbol.bitmap_width, ASymbol.bitmap_height,
                ASymbol.bitmap, bmpinfo, DIB_RGB_COLORS, SRCCOPY);}
end;

end.
