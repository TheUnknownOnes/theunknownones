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
    - the code of library.c is implemented here as part of TZintSymbol
}

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}


interface

uses
  Classes,
  SysUtils;

{$IF declared(TEncoding)}
  {$DEFINE UseTEncoding}
{$IFEND}

const
  ZINT_ROWS_MAX = 178;
  ZINT_COLS_MAX = 178;

type
  {$IF not declared(TBytes)}
  TBytes = array of Byte;
  {$IFEND}
  TArrayOfByte = TBytes;
  TArrayOfInteger = array of Integer;
  TArrayOfCardinal = array of Cardinal;
  TArrayOfWord = array of Word;
  TArrayOfChar = array of Char;
  TArrayOfArrayOfChar = array of array of Char;
  TArrayOfSmallInt = array of SmallInt;

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
    text : String;
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
  TZintSymbol = class;

  { TCustomZintSymbolOptions }

  TCustomZintSymbolOptions = class(TPersistent)
  protected
    FSymbol : TZintSymbol;
    function GetBooleanOption(AIndex : Integer) : Boolean;
    procedure SetBooleanOption(AIndex : Integer; AValue : Boolean);
  public const
    DefaultValue_Option_1 = -1;
    DefaultValue_Option_2 = 0;
    DefaultValue_Option_3 = 928;
  public
    constructor Create(ASymbol : TZintSymbol); virtual;
  end;

  { TZintMSIPlessyOptions }

  TZintMSIPlessyOptions = class(TCustomZintSymbolOptions)
  public type
    TmpCheckDigitType = (cdtNone, cdtMod10, cdtMod1010, cdtMod11, cdtMod1110);
  private
    function GetCheckDigitType: TmpCheckDigitType;
    procedure SetCheckDigitType(AValue: TmpCheckDigitType);
  published
    property CheckDigitType : TmpCheckDigitType read GetCheckDigitType write SetCheckDigitType default cdtNone;
  end;

  { TZintExtCode39Options }

  TZintExtCode39Options = class(TCustomZintSymbolOptions)
  published
    property AddCheckDigit : Boolean index 2 read GetBooleanOption write SetBooleanOption default false;
  end;

  { TZintCompositeOptions }

  TZintCompositeOptions = class(TCustomZintSymbolOptions)
  public type
    TCompositeType = (ctAuto, ctCC_A, ctCC_B, ctCC_C);
  protected
    function GetCompositeType: TCompositeType;
    procedure SetCompositeType(AValue: TCompositeType);
  published
    property CompositeType : TCompositeType read GetCompositeType write SetCompositeType default ctAuto;
  end;

  { TZintGridMatrixOptions }

  TZintGridMatrixOptions = class(TCustomZintSymbolOptions)
  public type
    TgmSize = (gmsAuto, gms18, gms30, gms42, gms54, gms66, gms78, gms90, gms102, gms114, gms126, gms138, gms150, gms162);
    TgmErrorCorrectCapacity = (gmeccAuto, gmecc10Percent, gmecc20Percent, gmecc30Percent, gmecc40Percent, gmecc50Percent);
  protected
    function GetErrorCorrectionCapacity: TgmErrorCorrectCapacity;
    function GetSize: TgmSize;
    procedure SetErrorCorrectionCapacity(AValue: TgmErrorCorrectCapacity);
    procedure SetSize(AValue: TgmSize);
  published
    property Size : TgmSize read GetSize write SetSize default gmsAuto;
    property ErrorCorrectionCapacity : TgmErrorCorrectCapacity read GetErrorCorrectionCapacity write SetErrorCorrectionCapacity default gmeccAuto;
  end;

  { TZintPDF417Options }

  TZintPDF417Options = class(TCustomZintSymbolOptions)
  public type
    TpdfCheckDigitCount = -1..8;
    TpdfColumns = 0..30;
  protected
    function GetCheckDigitCount: TpdfCheckDigitCount;
    function GetColumns: TpdfColumns;
    procedure SetCheckDigitCount(AValue: TpdfCheckDigitCount);
    procedure SetColumns(AValue: TpdfColumns);
  published
    property CheckDigitCount : TpdfCheckDigitCount read GetCheckDigitCount write SetCheckDigitCount default -1;
    property Columns : TpdfColumns read GetColumns write SetColumns default 0;
  end;

  { TZintAztecOptions }

  TZintAztecOptions = class(TCustomZintSymbolOptions)
  public type
    TatErrorCorrectCapacity = (ateccAuto, atecc10Percent, atecc23Percent, atecc36Percent, atecc50Percent);
    TatSize = (atsAuto, ats15Compact, ats19Compact, ats23Compact, ats27Compact, ats19, ats23, ats27, ats31, ats37, ats41, ats45, ats49, ats53, ats57, ats61, ats67, ats71, ats75, ats79, ats83, ats87, ats91, ats95, ats101, ats105, ats109, ats113, ats117, ats121, ats125, ats131, ats135, ats139, ats143, ats147, ats151);
  protected
    function GetErrorCorrectCapacity: TatErrorCorrectCapacity;
    function GetSize: TatSize;
    procedure SetGetErrorCorrectCapacity(AValue: TatErrorCorrectCapacity);
    procedure SetSize(AValue: TatSize);
  published
    property ErrorCorrectCapacity : TatErrorCorrectCapacity read GetErrorCorrectCapacity write SetGetErrorCorrectCapacity;
    property Size : TatSize read GetSize write SetSize;
  end;

  { TZintMaxicodeOptions }

  TZintMaxicodeOptions = class(TCustomZintSymbolOptions)
  public type
    TmcMode = (mcmAuto, mcmMode2, mcmMode3, mcmMode4, mcmMode5, mcmMode6);
  protected
    function GetMode: TmcMode;
    procedure SetMode(AValue: TmcMode);
  published
    property Mode : TmcMode read GetMode write SetMode;
  end;

  { TZintDatamatrixOptions }

  TZintDatamatrixOptions = class(TCustomZintSymbolOptions)
  public type
    TdmSize = (dmsAuto, dms10x10, dms12x12, dms14x14, dms16x16, dms18x18, dms20x20, dms22x22, dms24x24, dms26x26, dms32x32, dms36x36, dms40x40, dms44x44, dms48x48, dms52x52, dms64x64, dms72x72, dms80x80, dms88x88, dms96x96, dms104x104, dms120x120, dms132x132, dms144x144, dms8x18, dms8x32, dms12x26, dms12x36, dms16x36, dms16x48);
  protected
    function GetForceSquare: Boolean;
    function GetSize: TdmSize;
    procedure SetForceSquare(AValue: Boolean);
    procedure SetSize(AValue: TdmSize);
  published
    property Size : TdmSize read GetSize write SetSize default dmsAuto;
    property ForceSquare : Boolean read GetForceSquare write SetForceSquare default false;
  end;

  { TZintQRCodeOptions }

  TZintQRCodeOptions = class(TCustomZintSymbolOptions)
  public type
    TqrECCLevel = (qreAuto, qreLevelL, qreLevelM, qreLevelQ, qreLevelH);
    TqrSize = (qrsAuto, qrs21, qrs25, qrs29, qrs33, qrs37, qrs41, qrs45, qrs49, qrs53, qrs57, qrs61, qrs65, qrs69, qrs73, qrs77, qrs81, qrs85, qrs89, qrs93, qrs97, qrs101, qrs105, qrs109, qrs113, qrs117, qrs121, qrs125, qrs129, qrs133, qrs137, qrs141, qrs145, qrs149, qrs153, qrs157, qrs161, qrs165, qrs169, qrs173, qrs177);
  private
    function GetECCLevel: TqrECCLevel;
    function GetSize: TqrSize;
    procedure SetECCLevel(AValue: TqrECCLevel);
    procedure SetSize(AValue: TqrSize);
  published
    property ECCLevel : TqrECCLevel read GetECCLevel write SetECCLevel default qreAuto;
    property Size : TqrSize read GetSize write SetSize default qrsAuto;
  end;

  { TZintMicroQROptions }

  TZintMicroQROptions = class(TCustomZintSymbolOptions)
  public type
    TmqVersion = (mqvAuto, mqv1, mqv2, mqv3, mqv4);
  private
    function GetVersion: TmqVersion;
    procedure SetVersion(AValue: TmqVersion);
  published
    property Version : TmqVersion read GetVersion write SetVersion default mqvAuto;
  end;

  { TZintCode1Options }

  TZintCode1Options = class(TCustomZintSymbolOptions)
  public type
    Tc1Version = (c1vAuto, c1vA, c1vB, c1vC, c1vD, c1vE, c1vF, c1vG, c1vH, c1vS);
  private
    function GetVersion: Tc1Version;
    procedure SetVersion(AValue: Tc1Version);
  published
    property Version : Tc1Version read GetVersion write SetVersion;
  end;

  { TZintSymbol }

  TZintSymbol = class(TPersistent)
  protected
    FMSIPlesseyOptions: TZintMSIPlessyOptions;
    FExtCode39Options: TZintExtCode39Options;
    FCompositeOptions : TZintCompositeOptions;
    FGridMatrixOptions : TZintGridMatrixOptions;
    FPDF417Options : TZintPDF417Options;
    FAztecOptions : TZintAztecOptions;
    FMaxicodeOptions : TZintMaxicodeOptions;
    FDatamatrixOptions : TZintDatamatrixOptions;
    FMicroQROptions : TZintMicroQROptions;
    FCode1Options : TZintCode1Options;
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
    text : TArrayOfByte;
    rows : Integer;
    width : Integer;
    primary : TArrayOfChar;
    errtxt : TArrayOfChar;
    encoded_data : array[0..ZINT_ROWS_MAX - 1] of array[0..ZINT_COLS_MAX - 1] of Byte;
    row_height : array[0..ZINT_ROWS_MAX - 1] of Integer; { Largest symbol is 177x177 QR Code }
    rendered : Pzint_render;

    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure ClearRendered; virtual;

    procedure Encode(AData : TArrayOfByte; ALength : Integer; ARaiseExceptions : Boolean = true); overload; virtual;
    procedure Encode(AData : String; ARaiseExceptions : Boolean = true); overload; virtual;
    procedure Render(ATarget : TZintCustomRenderTarget); virtual;

    //These are the functions from library.c
    class function gs1_compliant(_symbology : Integer) : Integer;
    class procedure error_tag(var error_string : TArrayOfChar; error_number : Integer);
    class function hibc(symbol : TZintSymbol; source : TArrayOfByte; _length : Integer) : Integer;
    class function extended_charset(symbol : TZintSymbol; source : TArrayOfByte; _length : Integer) : Integer;
    class function reduced_charset(symbol : TZintSymbol; source : TArrayOfByte; _length : Integer) : Integer;
    class function ZBarcode_Encode(symbol : TZintSymbol; source : TArrayOfByte; _length : Integer) : Integer;

  published
    property MSIPlesseyOptions : TZintMSIPlessyOptions read FMSIPlesseyOptions;
    property ExtCode39Options : TZintExtCode39Options read FExtCode39Options;
    property CompositeOptions : TZintCompositeOptions read FCompositeOptions;
    property GridMatrixOptions : TZintGridMatrixOptions read FGridMatrixOptions;
    property PDF417Options : TZintPDF417Options read FPDF417Options;
    property AztecOptions : TZintAztecOptions read FAztecOptions;
    property MaxiCodeOptions : TZintMaxicodeOptions read FMaxicodeOptions;
    property DatamatrixOptions : TZintDatamatrixOptions read FDatamatrixOptions;
    property MicroQROptions : TZintMicroQROptions read FMicroQROptions;
    property Code1Option : TZintCode1Options read FCode1Options;
  end;

  zint_symbol = TZintSymbol;

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

type
  TZintBarcodeSymbologyEntry = record
    DisplayName : String;
    Symbology : Integer;
  end;

const
  ZintSymbologies : array[0..83] of TZintBarcodeSymbologyEntry =
     ((DisplayName : 'CODE11'; Symbology : BARCODE_CODE11),
      (DisplayName : 'C25MATRIX'; Symbology : BARCODE_C25MATRIX),
      (DisplayName : 'C25INTER'; Symbology : BARCODE_C25INTER),
      (DisplayName : 'C25IATA'; Symbology : BARCODE_C25IATA),
      (DisplayName : 'C25LOGIC'; Symbology : BARCODE_C25LOGIC),
      (DisplayName : 'C25IND'; Symbology : BARCODE_C25IND),
      (DisplayName : 'CODE39'; Symbology : BARCODE_CODE39),
      (DisplayName : 'EXCODE39'; Symbology : BARCODE_EXCODE39),
      (DisplayName : 'EANX'; Symbology : BARCODE_EANX),
      (DisplayName : 'EAN128'; Symbology : BARCODE_EAN128),
      (DisplayName : 'CODABAR'; Symbology : BARCODE_CODABAR),
      (DisplayName : 'CODE128'; Symbology : BARCODE_CODE128),
      (DisplayName : 'DPLEIT'; Symbology : BARCODE_DPLEIT),
      (DisplayName : 'DPIDENT'; Symbology : BARCODE_DPIDENT),
      (DisplayName : 'CODE16K'; Symbology : BARCODE_CODE16K),
      (DisplayName : 'CODE49'; Symbology : BARCODE_CODE49),
      (DisplayName : 'CODE93'; Symbology : BARCODE_CODE93),
      (DisplayName : 'FLAT'; Symbology : BARCODE_FLAT),
      (DisplayName : 'RSS14'; Symbology : BARCODE_RSS14),
      (DisplayName : 'RSS_LTD'; Symbology : BARCODE_RSS_LTD),
      (DisplayName : 'RSS_EXP'; Symbology : BARCODE_RSS_EXP),
      (DisplayName : 'TELEPEN'; Symbology : BARCODE_TELEPEN),
      (DisplayName : 'UPCA'; Symbology : BARCODE_UPCA),
      (DisplayName : 'UPCE'; Symbology : BARCODE_UPCE),
      (DisplayName : 'POSTNET'; Symbology : BARCODE_POSTNET),
      (DisplayName : 'MSI_PLESSEY'; Symbology : BARCODE_MSI_PLESSEY),
      (DisplayName : 'FIM'; Symbology : BARCODE_FIM),
      (DisplayName : 'LOGMARS'; Symbology : BARCODE_LOGMARS),
      (DisplayName : 'PHARMA'; Symbology : BARCODE_PHARMA),
      (DisplayName : 'PZN'; Symbology : BARCODE_PZN),
      (DisplayName : 'PHARMA_TWO'; Symbology : BARCODE_PHARMA_TWO),
      (DisplayName : 'PDF417'; Symbology : BARCODE_PDF417),
      (DisplayName : 'PDF417TRUNC'; Symbology : BARCODE_PDF417TRUNC),
      (DisplayName : 'MAXICODE'; Symbology : BARCODE_MAXICODE),
      (DisplayName : 'QRCODE'; Symbology : BARCODE_QRCODE),
      (DisplayName : 'CODE128B'; Symbology : BARCODE_CODE128B),
      (DisplayName : 'AUSPOST'; Symbology : BARCODE_AUSPOST),
      (DisplayName : 'AUSREPLY'; Symbology : BARCODE_AUSREPLY),
      (DisplayName : 'AUSROUTE'; Symbology : BARCODE_AUSROUTE),
      (DisplayName : 'AUSREDIRECT'; Symbology : BARCODE_AUSREDIRECT),
      (DisplayName : 'ISBNX'; Symbology : BARCODE_ISBNX),
      (DisplayName : 'RM4SCC'; Symbology : BARCODE_RM4SCC),
      (DisplayName : 'DATAMATRIX'; Symbology : BARCODE_DATAMATRIX),
      (DisplayName : 'EAN14'; Symbology : BARCODE_EAN14),
      (DisplayName : 'CODABLOCKF'; Symbology : BARCODE_CODABLOCKF),
      (DisplayName : 'NVE18'; Symbology : BARCODE_NVE18),
      (DisplayName : 'JAPANPOST'; Symbology : BARCODE_JAPANPOST),
      (DisplayName : 'KOREAPOST'; Symbology : BARCODE_KOREAPOST),
      (DisplayName : 'RSS14STACK'; Symbology : BARCODE_RSS14STACK),
      (DisplayName : 'RSS14STACK_OMNI'; Symbology : BARCODE_RSS14STACK_OMNI),
      (DisplayName : 'RSS_EXPSTACK'; Symbology : BARCODE_RSS_EXPSTACK),
      (DisplayName : 'PLANET'; Symbology : BARCODE_PLANET),
      (DisplayName : 'MICROPDF417'; Symbology : BARCODE_MICROPDF417),
      (DisplayName : 'ONECODE'; Symbology : BARCODE_ONECODE),
      (DisplayName : 'PLESSEY'; Symbology : BARCODE_PLESSEY),
      (DisplayName : 'TELEPEN_NUM'; Symbology : BARCODE_TELEPEN_NUM),
      (DisplayName : 'ITF14'; Symbology : BARCODE_ITF14),
      (DisplayName : 'KIX'; Symbology : BARCODE_KIX),
      (DisplayName : 'AZTEC'; Symbology : BARCODE_AZTEC),
      (DisplayName : 'DAFT'; Symbology : BARCODE_DAFT),
      (DisplayName : 'MICROQR'; Symbology : BARCODE_MICROQR),
      (DisplayName : 'HIBC_128'; Symbology : BARCODE_HIBC_128),
      (DisplayName : 'HIBC_39'; Symbology : BARCODE_HIBC_39),
      (DisplayName : 'HIBC_DM'; Symbology : BARCODE_HIBC_DM),
      (DisplayName : 'HIBC_QR'; Symbology : BARCODE_HIBC_QR),
      (DisplayName : 'HIBC_PDF'; Symbology : BARCODE_HIBC_PDF),
      (DisplayName : 'HIBC_MICPDF'; Symbology : BARCODE_HIBC_MICPDF),
      (DisplayName : 'HIBC_BLOCKF'; Symbology : BARCODE_HIBC_BLOCKF),
      (DisplayName : 'HIBC_AZTEC'; Symbology : BARCODE_HIBC_AZTEC),
      (DisplayName : 'AZRUNE'; Symbology : BARCODE_AZRUNE),
      (DisplayName : 'CODE32'; Symbology : BARCODE_CODE32),
      (DisplayName : 'EANX_CC'; Symbology : BARCODE_EANX_CC),
      (DisplayName : 'EAN128_CC'; Symbology : BARCODE_EAN128_CC),
      (DisplayName : 'RSS14_CC'; Symbology : BARCODE_RSS14_CC),
      (DisplayName : 'RSS_LTD_CC'; Symbology : BARCODE_RSS_LTD_CC),
      (DisplayName : 'RSS_EXP_CC'; Symbology : BARCODE_RSS_EXP_CC),
      (DisplayName : 'UPCA_CC'; Symbology : BARCODE_UPCA_CC),
      (DisplayName : 'UPCE_CC'; Symbology : BARCODE_UPCE_CC),
      (DisplayName : 'RSS14STACK_CC'; Symbology : BARCODE_RSS14STACK_CC),
      (DisplayName : 'RSS14_OMNI_CC'; Symbology : BARCODE_RSS14_OMNI_CC),
      (DisplayName : 'RSS_EXPSTACK_CC'; Symbology : BARCODE_RSS_EXPSTACK_CC),
      (DisplayName : 'CHANNEL'; Symbology : BARCODE_CHANNEL),
      (DisplayName : 'CODEONE'; Symbology : BARCODE_CODEONE),
      (DisplayName : 'GRIDMATRIX'; Symbology : BARCODE_GRIDMATRIX));


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
  zint_render_, zint_helper, zint_aztec, zint_qr, zint_upcean,
  zint_maxicode, zint_auspost, zint_code, zint_medical,
  zint_code16k, zint_code49, zint_pdf417, zint_composite, zint_gridmtx,
  zint_plessey, zint_code1;

const
  TECHNETIUM : String = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%';

{ TZintCode1Options }

function TZintCode1Options.GetVersion: Tc1Version;
begin
  case FSymbol.option_2 of
    1: Result := c1vA;
    2: Result := c1vB;
    3: Result := c1vC;
    4: Result := c1vD;
    5: Result := c1vE;
    6: Result := c1vF;
    7: Result := c1vG;
    8: Result := c1vH;
    9: Result := c1vS;
    else
      Result := c1vAuto;
  end;
end;

procedure TZintCode1Options.SetVersion(AValue: Tc1Version);
begin
  case AValue of
    c1vAuto : FSymbol.option_2 := DefaultValue_Option_2;
    c1vA : FSymbol.option_2 := 1;
    c1vB : FSymbol.option_2 := 2;
    c1vC : FSymbol.option_2 := 3;
    c1vD : FSymbol.option_2 := 4;
    c1vE : FSymbol.option_2 := 5;
    c1vF : FSymbol.option_2 := 6;
    c1vG : FSymbol.option_2 := 7;
    c1vH : FSymbol.option_2 := 8;
    c1vS : FSymbol.option_2 := 9;
  end;
end;

{ TZintMicroQROptions }

function TZintMicroQROptions.GetVersion: TmqVersion;
begin
  case FSymbol.option_2 of
    1: Result := mqv1;
    2: Result := mqv2;
    3: Result := mqv3;
    4: Result := mqv4;
    else
      Result :=mqvAuto;
  end;
end;

procedure TZintMicroQROptions.SetVersion(AValue: TmqVersion);
begin
  case AValue of
    mqvAuto : FSymbol.option_2 := DefaultValue_Option_2;
    mqv1 : FSymbol.option_2 := 1;
    mqv2 : FSymbol.option_2 := 2;
    mqv3 : FSymbol.option_2 := 3;
    mqv4 : FSymbol.option_2 := 4;
  end;
end;

{ TZintQRCodeOptions }

function TZintQRCodeOptions.GetECCLevel: TqrECCLevel;
begin
  case FSymbol.option_1 of
    1 : Result := qreLevelL;
    2 : Result := qreLevelM;
    3 : Result := qreLevelQ;
    4 : Result := qreLevelH;
    else
      Result := qreAuto;
  end;
end;

function TZintQRCodeOptions.GetSize: TqrSize;
begin
  case FSymbol.option_2 of
    1 : Result := qrs21;
    2 : Result := qrs25;
    3 : Result := qrs29;
    4 : Result := qrs33;
    5 : Result := qrs37;
    6 : Result := qrs41;
    7 : Result := qrs45;
    8 : Result := qrs49;
    9 : Result := qrs53;
    10 : Result := qrs57;
    11 : Result := qrs61;
    12 : Result := qrs65;
    13 : Result := qrs69;
    14 : Result := qrs73;
    15 : Result := qrs77;
    16 : Result := qrs81;
    17 : Result := qrs85;
    18 : Result := qrs89;
    19 : Result := qrs93;
    20 : Result := qrs97;
    21 : Result := qrs101;
    22 : Result := qrs105;
    23 : Result := qrs109;
    24 : Result := qrs113;
    25 : Result := qrs117;
    26 : Result := qrs121;
    27 : Result := qrs125;
    28 : Result := qrs129;
    29 : Result := qrs133;
    30 : Result := qrs137;
    31 : Result := qrs141;
    32 : Result := qrs145;
    33 : Result := qrs149;
    34 : Result := qrs153;
    35 : Result := qrs157;
    36 : Result := qrs161;
    37 : Result := qrs165;
    38 : Result := qrs169;
    39 : Result := qrs173;
    40 : Result := qrs177;
    else
      Result := qrsAuto;
  end;
end;

procedure TZintQRCodeOptions.SetECCLevel(AValue: TqrECCLevel);
begin
  case AValue of
    qreAuto : FSymbol.option_1 := DefaultValue_Option_1;
    qreLevelL : FSymbol.option_1 := 1;
    qreLevelM : FSymbol.option_1 := 2;
    qreLevelQ : FSymbol.option_1 := 3;
    qreLevelH : FSymbol.option_1 := 4;
  end;
end;

procedure TZintQRCodeOptions.SetSize(AValue: TqrSize);
begin
  case AValue of
    qrsAuto : FSymbol.option_2 := DefaultValue_Option_2;
    qrs21 : FSymbol.option_2 := 1;
    qrs25 : FSymbol.option_2 := 2;
    qrs29 : FSymbol.option_2 := 3;
    qrs33 : FSymbol.option_2 := 4;
    qrs37 : FSymbol.option_2 := 5;
    qrs41 : FSymbol.option_2 := 6;
    qrs45 : FSymbol.option_2 := 7;
    qrs49 : FSymbol.option_2 := 8;
    qrs53 : FSymbol.option_2 := 9;
    qrs57 : FSymbol.option_2 := 10;
    qrs61 : FSymbol.option_2 := 11;
    qrs65 : FSymbol.option_2 := 12;
    qrs69 : FSymbol.option_2 := 13;
    qrs73 : FSymbol.option_2 := 14;
    qrs77 : FSymbol.option_2 := 15;
    qrs81 : FSymbol.option_2 := 16;
    qrs85 : FSymbol.option_2 := 17;
    qrs89 : FSymbol.option_2 := 18;
    qrs93 : FSymbol.option_2 := 19;
    qrs97 : FSymbol.option_2 := 20;
    qrs101 : FSymbol.option_2 := 21;
    qrs105 : FSymbol.option_2 := 22;
    qrs109 : FSymbol.option_2 := 23;
    qrs113 : FSymbol.option_2 := 24;
    qrs117 : FSymbol.option_2 := 25;
    qrs121 : FSymbol.option_2 := 26;
    qrs125 : FSymbol.option_2 := 27;
    qrs129 : FSymbol.option_2 := 28;
    qrs133 : FSymbol.option_2 := 29;
    qrs137 : FSymbol.option_2 := 30;
    qrs141 : FSymbol.option_2 := 31;
    qrs145 : FSymbol.option_2 := 32;
    qrs149 : FSymbol.option_2 := 33;
    qrs153 : FSymbol.option_2 := 34;
    qrs157 : FSymbol.option_2 := 35;
    qrs161 : FSymbol.option_2 := 36;
    qrs165 : FSymbol.option_2 := 37;
    qrs169 : FSymbol.option_2 := 38;
    qrs173 : FSymbol.option_2 := 39;
    qrs177 : FSymbol.option_2 := 40;
  end;
end;

{ TZintDatamatrixOptions }

function TZintDatamatrixOptions.GetForceSquare: Boolean;
begin
  Result := FSymbol.option_3 = DM_SQUARE;
end;

function TZintDatamatrixOptions.GetSize: TdmSize;
begin
  case FSymbol.option_2 of
    1 : Result := dms10x10;
    2 : Result := dms12x12;
    3 : Result := dms14x14;
    4 : Result := dms16x16;
    5 : Result := dms18x18;
    6 : Result := dms20x20;
    7 : Result := dms22x22;
    8 : Result := dms24x24;
    9 : Result := dms26x26;
    10 : Result := dms32x32;
    11 : Result := dms36x36;
    12 : Result := dms40x40;
    13 : Result := dms44x44;
    14 : Result := dms48x48;
    15 : Result := dms52x52;
    16 : Result := dms64x64;
    17 : Result := dms72x72;
    18 : Result := dms80x80;
    19 : Result := dms88x88;
    20 : Result := dms96x96;
    21 : Result := dms104x104;
    22 : Result := dms120x120;
    23 : Result := dms132x132;
    24 : Result := dms144x144;
    25 : Result := dms8x18;
    26 : Result := dms8x32;
    27 : Result := dms12x26;
    28 : Result := dms12x36;
    29 : Result := dms16x36;
    30 : Result := dms16x48;
    else
      Result := dmsAuto;
  end;
end;

procedure TZintDatamatrixOptions.SetForceSquare(AValue: Boolean);
begin
  if AValue then
    FSymbol.option_3 := DM_SQUARE
  else
    FSymbol.option_3 := DefaultValue_Option_3;
end;

procedure TZintDatamatrixOptions.SetSize(AValue: TdmSize);
begin
  case AValue of
    dmsAuto : FSymbol.option_2 := DefaultValue_Option_2;
    dms10x10 : FSymbol.option_2 := 1;
    dms12x12 : FSymbol.option_2 := 2;
    dms14x14 : FSymbol.option_2 := 3;
    dms16x16 : FSymbol.option_2 := 4;
    dms18x18 : FSymbol.option_2 := 5;
    dms20x20 : FSymbol.option_2 := 6;
    dms22x22 : FSymbol.option_2 := 7;
    dms24x24 : FSymbol.option_2 := 8;
    dms26x26 : FSymbol.option_2 := 9;
    dms32x32 : FSymbol.option_2 := 10;
    dms36x36 : FSymbol.option_2 := 11;
    dms40x40 : FSymbol.option_2 := 12;
    dms44x44 : FSymbol.option_2 := 13;
    dms48x48 : FSymbol.option_2 := 14;
    dms52x52 : FSymbol.option_2 := 15;
    dms64x64 : FSymbol.option_2 := 16;
    dms72x72 : FSymbol.option_2 := 17;
    dms80x80 : FSymbol.option_2 := 18;
    dms88x88 : FSymbol.option_2 := 19;
    dms96x96 : FSymbol.option_2 := 20;
    dms104x104 : FSymbol.option_2 := 21;
    dms120x120 : FSymbol.option_2 := 22;
    dms132x132 : FSymbol.option_2 := 23;
    dms144x144 : FSymbol.option_2 := 24;
    dms8x18 : FSymbol.option_2 := 25;
    dms8x32 : FSymbol.option_2 := 26;
    dms12x26 : FSymbol.option_2 := 27;
    dms12x36 : FSymbol.option_2 := 28;
    dms16x36 : FSymbol.option_2 := 29;
    dms16x48 : FSymbol.option_2 := 30;
  end;
end;

{ TZintMaxicodeOptions }

function TZintMaxicodeOptions.GetMode: TmcMode;
begin
  case FSymbol.option_1 of
    2 : Result := mcmMode2;
    3 : Result := mcmMode3;
    4 : Result := mcmMode4;
    5 : Result := mcmMode5;
    6 : Result := mcmMode6;
    else
      Result := mcmAuto;
  end;
end;

procedure TZintMaxicodeOptions.SetMode(AValue: TmcMode);
begin
  case AValue of
    mcmAuto : FSymbol.option_1 := DefaultValue_Option_1;
    mcmMode2 : FSymbol.option_1 := 2;
    mcmMode3 : FSymbol.option_1 := 3;
    mcmMode4 : FSymbol.option_1 := 4;
    mcmMode5 : FSymbol.option_1 := 5;
    mcmMode6 : FSymbol.option_1 := 6;
  end;
end;

{ TZintAztecOptions }

function TZintAztecOptions.GetErrorCorrectCapacity: TatErrorCorrectCapacity;
begin
  case FSymbol.option_1 of
    1 : Result := atecc10Percent;
    2 : Result := atecc23Percent;
    3 : Result := atecc36Percent;
    4 : Result := atecc50Percent;
    else
      Result := ateccAuto;
  end;
end;

function TZintAztecOptions.GetSize: TatSize;
begin
  case FSymbol.option_2 of
    1 : Result := ats15Compact;
    2 : Result := ats19Compact;
    3 : Result := ats23Compact;
    4 : Result := ats27Compact;
    5 : Result := ats19;
    6 : Result := ats23;
    7 : Result := ats27;
    8 : Result := ats31;
    9 : Result := ats37;
    10 : Result := ats41;
    11 : Result := ats45;
    12 : Result := ats49;
    13 : Result := ats53;
    14 : Result := ats57;
    15 : Result := ats61;
    16 : Result := ats67;
    17 : Result := ats71;
    18 : Result := ats75;
    19 : Result := ats79;
    20 : Result := ats83;
    21 : Result := ats87;
    22 : Result := ats91;
    23 : Result := ats95;
    24 : Result := ats101;
    25 : Result := ats105;
    26 : Result := ats109;
    27 : Result := ats113;
    28 : Result := ats117;
    29 : Result := ats121;
    30 : Result := ats125;
    31 : Result := ats131;
    32 : Result := ats135;
    33 : Result := ats139;
    34 : Result := ats143;
    35 : Result := ats147;
    36 : Result := ats151;
    else
      Result := atsAuto;
  end;
end;

procedure TZintAztecOptions.SetGetErrorCorrectCapacity(
  AValue: TatErrorCorrectCapacity);
begin
  case AValue of
    ateccAuto : FSymbol.option_1 := DefaultValue_Option_1;
    atecc10Percent : FSymbol.option_1 := 1;
    atecc23Percent : FSymbol.option_1 := 2;
    atecc36Percent : FSymbol.option_1 := 3;
    atecc50Percent : FSymbol.option_1 := 4;
  end;
end;

procedure TZintAztecOptions.SetSize(AValue: TatSize);
begin
  case AValue of
    atsAuto : FSymbol.option_2 := DefaultValue_Option_2;
    ats15Compact : FSymbol.option_2 := 1;
    ats19Compact : FSymbol.option_2 := 2;
    ats23Compact : FSymbol.option_2 := 3;
    ats27Compact : FSymbol.option_2 := 4;
    ats19 : FSymbol.option_2 := 5;
    ats23 : FSymbol.option_2 := 6;
    ats27 : FSymbol.option_2 := 7;
    ats31 : FSymbol.option_2 := 8;
    ats37 : FSymbol.option_2 := 9;
    ats41 : FSymbol.option_2 := 10;
    ats45 : FSymbol.option_2 := 11;
    ats49 : FSymbol.option_2 := 12;
    ats53 : FSymbol.option_2 := 13;
    ats57 : FSymbol.option_2 := 14;
    ats61 : FSymbol.option_2 := 15;
    ats67 : FSymbol.option_2 := 16;
    ats71 : FSymbol.option_2 := 17;
    ats75 : FSymbol.option_2 := 18;
    ats79 : FSymbol.option_2 := 19;
    ats83 : FSymbol.option_2 := 20;
    ats87 : FSymbol.option_2 := 21;
    ats91 : FSymbol.option_2 := 22;
    ats95 : FSymbol.option_2 := 23;
    ats101 : FSymbol.option_2 := 24;
    ats105 : FSymbol.option_2 := 25;
    ats109 : FSymbol.option_2 := 26;
    ats113 : FSymbol.option_2 := 27;
    ats117 : FSymbol.option_2 := 28;
    ats121 : FSymbol.option_2 := 29;
    ats125 : FSymbol.option_2 := 30;
    ats131 : FSymbol.option_2 := 31;
    ats135 : FSymbol.option_2 := 32;
    ats139 : FSymbol.option_2 := 33;
    ats143 : FSymbol.option_2 := 34;
    ats147 : FSymbol.option_2 := 35;
    ats151 : FSymbol.option_2 := 36;

  end;
end;

{ TZintPDF417Options }

function TZintPDF417Options.GetCheckDigitCount: TpdfCheckDigitCount;
begin
  if (FSymbol.option_1 >= Low(TpdfCheckDigitCount)) and (FSymbol.option_1 <= High(TpdfCheckDigitCount)) then
    Result := FSymbol.option_1
  else
    Result := Low(TpdfCheckDigitCount);
end;

function TZintPDF417Options.GetColumns: TpdfColumns;
begin
  if (FSymbol.option_2 >= Low(TpdfColumns)) and (FSymbol.option_2 <= High(TpdfColumns)) then
    Result := FSymbol.option_2
  else
    Result := Low(TpdfColumns);
end;

procedure TZintPDF417Options.SetCheckDigitCount(AValue: TpdfCheckDigitCount);
begin
  FSymbol.option_1 := AValue;
end;

procedure TZintPDF417Options.SetColumns(AValue: TpdfColumns);
begin
  FSymbol.option_2 := AValue;
end;

{ TZintGridMatrixOptions }

function TZintGridMatrixOptions.GetErrorCorrectionCapacity: TgmErrorCorrectCapacity;
begin
  case FSymbol.option_1 of
    1: Result := gmecc10Percent;
    2: Result := gmecc20Percent;
    3: Result := gmecc30Percent;
    4: Result := gmecc40Percent;
    5: Result := gmecc50Percent;
    else
      Result := gmeccAuto;
  end;
end;

function TZintGridMatrixOptions.GetSize: TgmSize;
begin
  case FSymbol.option_2 of
    1: Result := gms18;
    2: Result := gms30;
    3: Result := gms42;
    4: Result := gms54;
    5: Result := gms66;
    6: Result := gms78;
    7: Result := gms90;
    8: Result := gms102;
    9: Result := gms114;
    10: Result := gms126;
    11: Result := gms138;
    12: Result := gms150;
    13: Result := gms162
    else
      Result := gmsAuto;
  end;
end;

procedure TZintGridMatrixOptions.SetErrorCorrectionCapacity(
  AValue: TgmErrorCorrectCapacity);
begin
  case AValue of
    gmeccAuto : FSymbol.option_1 := DefaultValue_Option_1;
    gmecc10Percent : FSymbol.option_1 := 1;
    gmecc20Percent : FSymbol.option_1 := 2;
    gmecc30Percent : FSymbol.option_1 := 3;
    gmecc40Percent : FSymbol.option_1 := 4;
    gmecc50Percent : FSymbol.option_1 := 5;
  end;
end;

procedure TZintGridMatrixOptions.SetSize(AValue: TgmSize);
begin
  case AValue of
    gmsAuto : FSymbol.option_2 := DefaultValue_Option_2;
    gms18 : FSymbol.option_2 := 1;
    gms30 : FSymbol.option_2 := 2;
    gms42 : FSymbol.option_2 := 3;
    gms54 : FSymbol.option_2 := 4;
    gms66 : FSymbol.option_2 := 5;
    gms78 : FSymbol.option_2 := 6;
    gms90 : FSymbol.option_2 := 7;
    gms102 : FSymbol.option_2 := 8;
    gms114 : FSymbol.option_2 := 9;
    gms126 : FSymbol.option_2 := 10;
    gms138 : FSymbol.option_2 := 11;
    gms150 : FSymbol.option_2 := 12;
    gms162 : FSymbol.option_2 := 13;
  end;
end;

{ TZintCompositeOptions }

function TZintCompositeOptions.GetCompositeType: TCompositeType;
begin
  case FSymbol.option_1 of
    1: Result := ctCC_A;
    2: Result := ctCC_B;
    3: Result := ctCC_C;
    else
      Result := ctAuto;
  end;
end;

procedure TZintCompositeOptions.SetCompositeType(AValue: TCompositeType);
begin
  case AValue of
    ctAuto : FSymbol.option_1 := DefaultValue_Option_1;
    ctCC_A : FSymbol.option_1 := 1;
    ctCC_B : FSymbol.option_1 := 2;
    ctCC_C : FSymbol.option_1 := 3;
  end;
end;

{ TZintMSIPlessyOptions }

function TZintMSIPlessyOptions.GetCheckDigitType: TmpCheckDigitType;
begin
  case FSymbol.option_2 of
    0: Result := cdtNone;
    1: Result := cdtMod10;
    2: Result := cdtMod1010;
    3: Result := cdtMod11;
    4: Result := cdtMod1110;
    else
      Result := cdtNone;
  end;
end;

procedure TZintMSIPlessyOptions.SetCheckDigitType(AValue: TmpCheckDigitType);
begin
  case AValue of
    cdtNone : FSymbol.option_2 := DefaultValue_Option_2;
    cdtMod10 : FSymbol.option_2 := 1;
    cdtMod1010 : FSymbol.option_2 := 2;
    cdtMod11 : FSymbol.option_2 := 3;
    cdtMod1110 : FSymbol.option_2 := 4;
  end;
end;

{ TCustomZintSymbolOptions }

function TCustomZintSymbolOptions.GetBooleanOption(AIndex: Integer): Boolean;
begin
  case AIndex of
    1 : Result := FSymbol.option_1 <> 0;
    2 : Result := FSymbol.option_2 <> 0;
    3 : Result := FSymbol.option_3 <> 0;
    else
      Result := false;
  end;
end;

procedure TCustomZintSymbolOptions.SetBooleanOption(AIndex: Integer;
  AValue: Boolean);
var
  v : Integer;
begin
  if AValue then v := 1 else v := 0;

  case AIndex of
    1 : FSymbol.option_1 := v;
    2 : FSymbol.option_2 := v;
    3 : FSymbol.option_3 := v;
  end;
end;

constructor TCustomZintSymbolOptions.Create(ASymbol: TZintSymbol);
begin
  FSymbol := ASymbol;
end;

{ TZintSymbol }

procedure TZintSymbol.Clear;
var
  i, j : Integer;
begin
	for i := 0 to rows - 1 do
		for j := 0 to width - 1 do
			unset_module(Self, i, j);

	rows := 0;
	width := 0;
	ustrcpy(text, '');
	strcpy(errtxt, '');
end;

procedure TZintSymbol.ClearRendered;
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

constructor TZintSymbol.Create;
begin
  inherited;

  SetLength(text, 128);
  SetLength(primary, 128);
  SetLength(errtxt, 100);

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
	strcpy(primary, '');
  rendered := nil;

  FMSIPlesseyOptions := TZintMSIPlessyOptions.Create(Self);
  FExtCode39Options := TZintExtCode39Options.Create(Self);
  FCompositeOptions := TZintCompositeOptions.Create(Self);
  FGridMatrixOptions := TZintGridMatrixOptions.Create(Self);
  FPDF417Options := TZintPDF417Options.Create(Self);
  FAztecOptions := TZintAztecOptions.Create(Self);
  FMaxicodeOptions := TZintMaxicodeOptions.Create(Self);
  FDatamatrixOptions := TZintDatamatrixOptions.Create(Self);
  FMicroQROptions := TZintMicroQROptions.Create(Self);
  FCode1Options := TZintCode1Options.Create(Self);
end;

destructor TZintSymbol.Destroy;
begin
  Clear;
  ClearRendered;

  FMSIPlesseyOptions.Free;
  FExtCode39Options.Free;
  FCompositeOptions.Free;
  FGridMatrixOptions.Free;
  FPDF417Options.Free;
  FAztecOptions.Free;
  FMaxicodeOptions.Free;
  FDatamatrixOptions.Free;
  FMicroQROptions.Free;
  FCode1Options.Free;
  
  inherited;
end;

procedure TZintSymbol.Encode(AData: TArrayOfByte; ALength : Integer; ARaiseExceptions : Boolean);
begin
  if (ZBarcode_Encode(Self, AData, ALength) >= ZERROR_TOO_LONG) and ARaiseExceptions then
    raise Exception.Create(PChar(@self.errtxt[0]));
end;

procedure TZintSymbol.Encode(AData: String; ARaiseExceptions: Boolean);
var
  b : TArrayOfByte;

  {$IFDEF UseTEncoding}
  e : TEncoding;
  {$ENDIF}
begin
  if (input_mode and UNICODE_MODE) <> 0 then
  begin
  {$IFDEF UseTEncoding}
    {$IFDEF FPC}
    e := TEncoding.ANSI;
    {$ELSE}
    e := TEncoding.UTF8;
    {$ENDIF}
    b := e.GetBytes(AData);
    SetLength(b, Length(b) + 1);
    b[High(b)] := 0;
  {$ELSE}            
    b := StrToArrayOfByte(UTF8Encode(AData));
  {$ENDIF}
  end
  else
    b := StrToArrayOfByte(AData);

  Encode(b, ustrlen(b), ARaiseExceptions);
end;

class procedure TZintSymbol.error_tag(var error_string : TArrayOfChar; error_number : Integer);
var
  error_buffer : TArrayOfChar;
begin
  SetLength(error_buffer, 100);

  if (error_number <> 0) then
  begin
    strcpy(error_buffer, error_string);

    if (error_number > 4) then
      strcpy(error_string, 'error: ')
    else
      strcpy(error_string, 'warning: ');

    concat(error_string, error_buffer);
  end;
end;

class function TZintSymbol.hibc(symbol : TZintSymbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  counter, error_number, i : Integer;
  to_process, temp : TArrayOfChar;
  check_digit : Char;
begin
  SetLength(to_process, 40); SetLength(temp, 2);

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
		Inc(counter, posn(TECHNETIUM, source[i])) ;

	counter := counter mod 43;

	if (counter < 10) then
  begin
		check_digit := itoc(counter);
	end
  else
  begin
		if (counter < 36) then
    begin
			check_digit := Chr((counter - 10) + Ord('A'));
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

	temp[0] := check_digit;
  temp[1] := #0;
  concat(to_process, source);
	concat(to_process, temp);
  _length := strlen(to_process);

	case symbol.symbology of
		BARCODE_HIBC_128:
    begin
			error_number := code_128(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
      ustrcpy(symbol.text, '*');
      uconcat(symbol.text, to_process);
      uconcat(symbol.text, '*');
    end;
    BARCODE_HIBC_39:
    begin
			symbol.option_2 := 0;
			error_number := c39(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
      ustrcpy(symbol.text, '*');
      uconcat(symbol.text, to_process);
      uconcat(symbol.text, '*');
    end;
    BARCODE_HIBC_DM:
			error_number := dmatrix(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
		BARCODE_HIBC_QR:
			error_number := qr_code(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
		BARCODE_HIBC_PDF:
			error_number := pdf417enc(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
		BARCODE_HIBC_MICPDF:
			error_number := micro_pdf417(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
		BARCODE_HIBC_AZTEC:
			error_number := aztec(symbol, ArrayOfCharToArrayOfByte(to_process), _length);
	end;

	Result := error_number; exit;
end;

class function TZintSymbol.gs1_compliant(_symbology : Integer) : Integer;
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

class function TZintSymbol.extended_charset(symbol : TZintSymbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number : Integer;
begin
  error_number := 0;

	{ These are the "elite" standards which can support multiple character sets }
	case symbol.symbology of
	  BARCODE_QRCODE: error_number := qr_code(symbol, source, _length);
	 	BARCODE_MICROQR: error_number := microqr(symbol, source, _length);
		BARCODE_GRIDMATRIX: error_number := grid_matrix(symbol, source, _length);
	end;

	Result := error_number; exit;
end;

class function TZintSymbol.reduced_charset(symbol : TZintSymbol; source : TArrayOfByte; _length : Integer) : Integer;
{ These are the "norm" standards which only support Latin-1 at most }
var
  error_number : Integer;
  preprocessed : TArrayOfByte;
begin
  SetLength(preprocessed, _length + 1);
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
		BARCODE_UPCA: error_number := eanx(symbol, preprocessed, _length);
		BARCODE_UPCE: error_number := eanx(symbol, preprocessed, _length);
		BARCODE_EANX: error_number := eanx(symbol, preprocessed, _length);
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
		BARCODE_MSI_PLESSEY: error_number := msi_handle(symbol, preprocessed, _length);
		//BARCODE_TELEPEN: error_number := telepen(symbol, preprocessed, _length);
		//BARCODE_TELEPEN_NUM: error_number := telepen_num(symbol, preprocessed, _length);
		BARCODE_PHARMA: error_number := pharma_one(symbol, preprocessed, _length);
		BARCODE_PLESSEY: error_number := plessey(symbol, preprocessed, _length);
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
		BARCODE_ISBNX: error_number := eanx(symbol, preprocessed, _length);
		//BARCODE_RSS14: error_number := rss14(symbol, preprocessed, _length);
		//BARCODE_RSS14STACK: error_number := rss14(symbol, preprocessed, _length);
		//BARCODE_RSS14STACK_OMNI: error_number := rss14(symbol, preprocessed, _length);
		//BARCODE_RSS_LTD: error_number := rsslimited(symbol, preprocessed, _length);
		//BARCODE_RSS_EXP: error_number := rssexpanded(symbol, preprocessed, _length);
		//BARCODE_RSS_EXPSTACK: error_number := rssexpanded(symbol, preprocessed, _length);
		BARCODE_EANX_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_EAN128_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS14_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS_LTD_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS_EXP_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_UPCA_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_UPCE_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS14STACK_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS14_OMNI_CC: error_number := composite(symbol, preprocessed, _length);
		BARCODE_RSS_EXPSTACK_CC: error_number := composite(symbol, preprocessed, _length);
		//BARCODE_KIX: error_number := kix_code(symbol, preprocessed, _length);
		BARCODE_CODE32: error_number := code32(symbol, preprocessed, _length);
		//BARCODE_DAFT: error_number := daft_code(symbol, preprocessed, _length);
		BARCODE_EAN14: error_number := ean_14(symbol, preprocessed, _length);
		BARCODE_AZRUNE: error_number := aztec_runes(symbol, preprocessed, _length);
		//BARCODE_KOREAPOST: error_number := korea_post(symbol, preprocessed, _length);
		//BARCODE_HIBC_128: error_number := hibc(symbol, preprocessed, _length);
		//BARCODE_HIBC_39: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_DM: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_QR: error_number := hibc(symbol, preprocessed, _length);
		//BARCODE_HIBC_PDF: error_number := hibc(symbol, preprocessed, _length);
		//BARCODE_HIBC_MICPDF: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_AZTEC: error_number := hibc(symbol, preprocessed, _length);
		//BARCODE_JAPANPOST: error_number := japan_post(symbol, preprocessed, _length);
		BARCODE_CODE49: error_number := code_49(symbol, preprocessed, _length);
		BARCODE_CHANNEL: error_number := channel_code(symbol, preprocessed, _length);
		BARCODE_CODEONE: error_number := code_one(symbol, preprocessed, _length);
		BARCODE_DATAMATRIX: error_number := dmatrix(symbol, preprocessed, _length);
		BARCODE_PDF417: error_number := pdf417enc(symbol, preprocessed, _length);
		BARCODE_PDF417TRUNC: error_number := pdf417enc(symbol, preprocessed, _length);
		BARCODE_MICROPDF417: error_number := micro_pdf417(symbol, preprocessed, _length);
		BARCODE_MAXICODE: error_number := maxicode(symbol, preprocessed, _length);
		BARCODE_AZTEC: error_number := aztec(symbol, preprocessed, _length);
  end;

	result := error_number; exit;
end;

procedure TZintSymbol.Render(ATarget : TZintCustomRenderTarget);
begin
  ClearRendered;
  render_plot(Self, ATarget.FWidthDesired, ATarget.FHeightDesired);
  ATarget.Render(Self);
end;

class function TZintSymbol.ZBarcode_Encode(symbol : TZintSymbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  error_number, error_buffer, i : Integer;
  local_source : TArrayOfByte;
begin
  error_number := 0;

	if (_length = 0) then
  begin
		strcpy(symbol.errtxt, 'No input data');
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
			if (source[i] = 0) then
      begin
				strcpy(symbol.errtxt, 'NULL characters not permitted in GS1 mode');
				result := ZERROR_INVALID_DATA; exit;
			end;
		end;
		if (gs1_compliant(symbol.symbology) = 1) then
    begin
			error_number := ugs1_verify(symbol, source, _length, local_source);
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
		for i := 0 to _length - 1 do
    begin
			if (local_source[i] = 0) then
				symbol.text[i] := 32
      else
				symbol.text[i] := local_source[i];
		end;
    symbol.text[_length] := 0;
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

