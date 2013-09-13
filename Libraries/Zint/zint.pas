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
    //please use the following vars *ONLY* if you *REALLY* know, what you're doing
    //otherwise use the properties of the RenderTarget or the TZintSymbol.???Options - properties
    symbology : Integer;
    whitespace_width : Integer;
    border_width : Integer;
    output_options : Integer;
    option_1 : Integer;
    option_2 : Integer;
    option_3 : Integer;
    input_mode : Integer;
    text : TArrayOfByte;
    rows : Integer;
    width : Integer;
    primary : TArrayOfChar;
    errtxt : TArrayOfChar;
    encoded_data : array[0..ZINT_ROWS_MAX - 1] of array[0..ZINT_COLS_MAX - 1] of Byte;
    row_height : array[0..ZINT_ROWS_MAX - 1] of Integer; { Largest symbol is 177x177 QR Code }

    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Encode(AData : TArrayOfByte; ALength : Integer; ARaiseExceptions : Boolean = true); overload; virtual;
    procedure Encode(AData : String; ARaiseExceptions : Boolean = true); overload; virtual;
    procedure Render(ATarget : TZintCustomRenderTarget); virtual;

    procedure InsertModuleRow(AIndex : Integer; ACount : Integer = 1; ASet : Boolean = false);
    procedure InsertModuleCol(AIndex : Integer; ACount : Integer = 1; ASet : Boolean = false);

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

  TZintRenderAdjustMode = (ramScale, ramInflate);

  { TZintRenderValue }

  TZintRenderValue = class
  protected
    FTargetUnits : Single; //depends on the target; may be pixels, ...
    FModules : Single; //will be used as multiplicator with the module[height|width]
  public
    constructor Create(); overload; virtual;

    procedure IncTargetUnits(AValue : Single); virtual;
    procedure IncModules(AValue : Single); virtual;
    procedure DecTargetUnits(AValue : Single); virtual;
    procedure DecModules(AValue : Single); virtual;
  published
    property TargetUnits : Single read FTargetUnits write FTargetUnits;
    property Modules : Single read FModules write FModules;
  end;

  { TZintRenderBox }

  TZintRenderBox = class
  protected
    FTop, FBottom, FLeft, FRight : TZintRenderValue;

    function GetSum(AIndex : Integer) : Single;
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    procedure SetModules(AValue : Single); virtual;
    procedure SetTargetUnits(AValue : Single); virtual;
    procedure AddModulesToTargetUnits(AModuleWidth, AModuleHeight : Single;
                                      ATop : Boolean = true;
                                      ABottom : Boolean = true;
                                      ALeft : Boolean = true;
                                      ARight : Boolean = true); virtual;
    procedure RemoveModulesFromTargetUnits(AModuleWidth, AModuleHeight : Single;
                                           ATop : Boolean = true;
                                           ABottom : Boolean = true;
                                           ALeft : Boolean = true;
                                           ARight : Boolean = true); virtual;

    property TopAndBottomTargetUnits : Single index 0 read GetSum;
    property LeftAndRightTargetUnits : Single index 1 read GetSum;
    property TopAndBottomModules : Single index 2 read GetSum;
    property LeftAndRightModules : Single index 3 read GetSum;
  published
    property Top : TZintRenderValue read FTop;
    property Bottom : TZintRenderValue read FBottom;
    property Left : TZintRenderValue read FLeft;
    property Right : TZintRenderValue read FRight;
  end;

  TZintRenderRect = record
    X, Y, Width, Height : Single;
  end;

  TZintHAlign = (haLeft, haCenter, haRight);
  TZintVAlign = (vaTop, vaCenter, vaBottom);

  TZintClearBackgroundParams = TZintRenderRect;

  TZintDrawRectParams = TZintRenderRect;

  TZintDrawHexagonParams = TZintRenderRect;

  TZintDrawRingParams = record
    X, Y, OuterRadius, InnerRadius : Single;
  end;

  TZintDrawTextParams = record
    X, Y, Width, Height : Single;
    Text : String;
  end;

  TZintCalcTextHeightParams = record
    Text : String;
  end;

  TZintCalcTextWidthParams = TZintCalcTextHeightParams;

  TZintEANUPCFlag = (euEAN8, euEAN13, euUPCA, euUPCE, euAddon2, euAddon5);
  TZintEANUPCFlags = set of TZintEANUPCFlag;

  { TZintCustomRenderTarget }

  TZintCustomRenderTarget = class(TObject)
  protected
    FSymbol : TZintSymbol;

    FRowHeights : Integer; //sum of all rowheights measured in modules
    FModuleWidth, FModuleHeight : Single;
    FLargeBarCount : Integer; //count of rows, which height should be maximied
    FLargeBarHeight : Single; //barheight of the rows, which height should be maximied
    FTextSpacing : TZintRenderBox;
    FHasText, FHasAddonText : Boolean;
    FText, FAddonText : String;
    FWhitespace : TZintRenderBox;
    FMargin, FPadding, FBorder: TZintRenderBox;
    FMarginBox, FBorderBox, FPaddingBox, FWhitespaceBox, FBarcodeBox, FTextSpacingBox, FTextBox : TZintRenderRect;
    FHexagonScale: Single;
    FTransparent: Boolean;
    FRenderAdjustMode : TZintRenderAdjustMode;
    FHeightDesired, FWidthDesired, FWidth, FHeight : Single;
    FYDesired, FXDesired, FY, FX : Single;
    FTextHeight    : Single;
    FMinModuleWidth : Single;
    FHAlign : TZintHAlign;
    FVAlign : TZintVAlign;
    FStartTextBar : TZintRenderRect;
    FTextDone : Boolean;
    FEANUPCFlags : TZintEANUPCFlags;
    FShowText : Boolean;
    FLeadingTextWidth, FTrailingTextWidth : Single;

    //these functions calculates the zero-based values to absolute values based on the ...Desired-Values and FWidth & FHeight
    function CalcX(AValue : Single) : Single;
    function CalcY(AValue : Single) : Single;

    procedure AddSymbolOptions; virtual; //adds options from the symbol to this render target (border, whitespace, ...)
    procedure RemoveSymbolOptions; virtual; //removes options from this render target previously added by AddSymbolOptions
    procedure AddBoxModulesToTargetUnits; virtual;
    procedure RemoveBoxModulesFromTargetUnits; virtual;
    procedure FetchRowInfos; virtual; //search for large bars and sum up the heights of the rows
    procedure CalcSize; virtual;
    procedure CalcText; virtual;
    procedure CalcTextEANUPC; virtual;
    procedure CheckEANUPC; virtual;
    procedure CalcLargeBarHeight; virtual;
    procedure CalcBoxes; virtual;
    procedure DrawBorder; virtual;
    procedure DrawMaxiRings; virtual;
    procedure DrawMaxiModules; virtual;
    procedure DrawModules; virtual;
    procedure DrawTexts; virtual;
    procedure RenderStart; virtual;
    procedure RenderStop; virtual;
    procedure DrawStart; virtual;
    procedure DrawStop; virtual;
    procedure HandleSpecialBarsEANUPC(ABarIndex : Integer; var ABar : TZintDrawRectParams); virtual;
    procedure Inflate(const ANewWidth, ANewHeight : Single); virtual; abstract;
    procedure ClearBackground(const AParams : TZintClearBackgroundParams); virtual; abstract;
    procedure DrawRect(const AParams : TZintDrawRectParams); virtual; abstract;
    procedure DrawHexagon(const AParams : TZintDrawHexagonParams); virtual; abstract;
    procedure DrawRing(const AParams : TZintDrawRingParams); virtual; abstract;
    procedure DrawText(const AParams : TZintDrawTextParams); virtual; abstract;
    function CalcTextHeight(const AParams : TZintCalcTextHeightParams) : Single; virtual; abstract;
    function CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single; virtual; abstract;
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    procedure Render(ASymbol : TZintSymbol); virtual;

    property XDesired: Single read FXDesired write FXDesired;
    property YDesired: Single read FYDesired write FYDesired;
    property HeightDesired: Single read FHeightDesired write FHeightDesired;
    property WidthDesired: Single read FWidthDesired write FWidthDesired;

    property Y : Single read FY;
    property X : Single read FX;
    property Height : Single read FHeight;
    property Width : Single read FWidth;

  published
    property RenderAdjustMode : TZintRenderAdjustMode read FRenderAdjustMode write FRenderAdjustMode;
    property Transparent : Boolean read FTransparent write FTransparent;
    property HexagonScale : Single read FHexagonScale write FHexagonScale;
    property Margin : TZintRenderBox read FMargin write FMargin;
    property Padding : TZintRenderBox read FPadding write FPadding;
    property Border : TZintRenderBox read FBorder write FBorder;
    property TextSpacing : TZintRenderBox read FTextSpacing write FTextSpacing;
    property Whitespace : TZintRenderBox read FWhitespace write FWhitespace;
    property HAlign : TZintHAlign read FHAlign write FHAlign;
    property VAlign : TZintVAlign read FVAlign write FVAlign;
    property MinModuleWidth : Single read FMinModuleWidth write FMinModuleWidth; //will only be applied if RenderAdjustMode = ramInflate
    property ShowText : Boolean read FShowText write FShowText;
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
     ((DisplayName : 'Code 11'; Symbology : BARCODE_CODE11),
      (DisplayName : 'Standard Code 2 of 5'; Symbology : BARCODE_C25MATRIX),
      (DisplayName : 'Interleaved 2 of 5'; Symbology : BARCODE_C25INTER),
      (DisplayName : 'Code 2 of 5 IATA'; Symbology : BARCODE_C25IATA),
      (DisplayName : 'Code 2 of 5 Data Logic'; Symbology : BARCODE_C25LOGIC),
      (DisplayName : 'Code 2 of 5 Industrial'; Symbology : BARCODE_C25IND),
      (DisplayName : 'Code 3 of 9 (Code 39)'; Symbology : BARCODE_CODE39),
      (DisplayName : 'Extended Code 3 of 9 (Code 39+)'; Symbology : BARCODE_EXCODE39),
      (DisplayName : 'EAN'; Symbology : BARCODE_EANX),
      (DisplayName : 'GS1-128 (UCC.EAN-128)'; Symbology : BARCODE_EAN128),
      (DisplayName : 'Codabar'; Symbology : BARCODE_CODABAR),
      (DisplayName : 'Code 128 (automatic subset switching)'; Symbology : BARCODE_CODE128),
      (DisplayName : 'Deutsche Post Leitcode'; Symbology : BARCODE_DPLEIT),
      (DisplayName : 'Deutsche Post Identcode'; Symbology : BARCODE_DPIDENT),
      (DisplayName : 'Code 16K'; Symbology : BARCODE_CODE16K),
      (DisplayName : 'Code 49'; Symbology : BARCODE_CODE49),
      (DisplayName : 'Code 93'; Symbology : BARCODE_CODE93),
      (DisplayName : 'Flattermarken'; Symbology : BARCODE_FLAT),
      (DisplayName : 'GS1 DataBar-14'; Symbology : BARCODE_RSS14),
      (DisplayName : 'GS1 DataBar Limited'; Symbology : BARCODE_RSS_LTD),
      (DisplayName : 'GS1 DataBar Extended'; Symbology : BARCODE_RSS_EXP),
      (DisplayName : 'Telepen Alpha'; Symbology : BARCODE_TELEPEN),
      (DisplayName : 'UPC A'; Symbology : BARCODE_UPCA),
      (DisplayName : 'UPC E'; Symbology : BARCODE_UPCE),
      (DisplayName : 'PostNet'; Symbology : BARCODE_POSTNET),
      (DisplayName : 'MSI Plessey'; Symbology : BARCODE_MSI_PLESSEY),
      (DisplayName : 'FIM'; Symbology : BARCODE_FIM),
      (DisplayName : 'LOGMARS'; Symbology : BARCODE_LOGMARS),
      (DisplayName : 'Pharmacode One-Track'; Symbology : BARCODE_PHARMA),
      (DisplayName : 'PZN'; Symbology : BARCODE_PZN),
      (DisplayName : 'Pharmacode Two-Track'; Symbology : BARCODE_PHARMA_TWO),
      (DisplayName : 'PDF417'; Symbology : BARCODE_PDF417),
      (DisplayName : 'PDF417 Truncated'; Symbology : BARCODE_PDF417TRUNC),
      (DisplayName : 'Maxicode'; Symbology : BARCODE_MAXICODE),
      (DisplayName : 'QR Code'; Symbology : BARCODE_QRCODE),
      (DisplayName : 'Code 128 (Subset B)'; Symbology : BARCODE_CODE128B),
      (DisplayName : 'Australia Post Standard Customer'; Symbology : BARCODE_AUSPOST),
      (DisplayName : 'Australia Post Reply Paid'; Symbology : BARCODE_AUSREPLY),
      (DisplayName : 'Australia Post Routing'; Symbology : BARCODE_AUSROUTE),
      (DisplayName : 'Australia Post Redirection'; Symbology : BARCODE_AUSREDIRECT),
      (DisplayName : 'ISBN (EAN-13 with verification stage)'; Symbology : BARCODE_ISBNX),
      (DisplayName : 'Royal Mail 4 State (RM4SCC)'; Symbology : BARCODE_RM4SCC),
      (DisplayName : 'Data Matrix'; Symbology : BARCODE_DATAMATRIX),
      (DisplayName : 'EAN-14'; Symbology : BARCODE_EAN14),
      (DisplayName : 'CODABLOCKF'; Symbology : BARCODE_CODABLOCKF),
      (DisplayName : 'NVE-18'; Symbology : BARCODE_NVE18),
      (DisplayName : 'Japanese Postal Code'; Symbology : BARCODE_JAPANPOST),
      (DisplayName : 'Korea Post'; Symbology : BARCODE_KOREAPOST),
      (DisplayName : 'GS1 DataBar-14 Stacked'; Symbology : BARCODE_RSS14STACK),
      (DisplayName : 'GS1 DataBar-14 Stacked Omnidirectional'; Symbology : BARCODE_RSS14STACK_OMNI),
      (DisplayName : 'GS1 DataBar Expanded Stacked'; Symbology : BARCODE_RSS_EXPSTACK),
      (DisplayName : 'PLANET'; Symbology : BARCODE_PLANET),
      (DisplayName : 'MicroPDF417'; Symbology : BARCODE_MICROPDF417),
      (DisplayName : 'USPS OneCode'; Symbology : BARCODE_ONECODE),
      (DisplayName : 'Plessey Code'; Symbology : BARCODE_PLESSEY),
      (DisplayName : 'Telepen Numeric'; Symbology : BARCODE_TELEPEN_NUM),
      (DisplayName : 'ITF-14'; Symbology : BARCODE_ITF14),
      (DisplayName : 'Dutch Post KIX Code'; Symbology : BARCODE_KIX),
      (DisplayName : 'Aztec Code'; Symbology : BARCODE_AZTEC),
      (DisplayName : 'DAFT Code'; Symbology : BARCODE_DAFT),
      (DisplayName : 'Micro QR Code'; Symbology : BARCODE_MICROQR),
      (DisplayName : 'HIBC Code 128'; Symbology : BARCODE_HIBC_128),
      (DisplayName : 'HIBC Code 39'; Symbology : BARCODE_HIBC_39),
      (DisplayName : 'HIBC Data Matrix'; Symbology : BARCODE_HIBC_DM),
      (DisplayName : 'HIBC QR Code'; Symbology : BARCODE_HIBC_QR),
      (DisplayName : 'HIBC PDF417'; Symbology : BARCODE_HIBC_PDF),
      (DisplayName : 'HIBC MicroPDF417'; Symbology : BARCODE_HIBC_MICPDF),
      (DisplayName : 'HIBC_BLOCKF'; Symbology : BARCODE_HIBC_BLOCKF),
      (DisplayName : 'HIBC Aztec Code'; Symbology : BARCODE_HIBC_AZTEC),
      (DisplayName : 'Aztec Runes'; Symbology : BARCODE_AZRUNE),
      (DisplayName : 'Code 32'; Symbology : BARCODE_CODE32),
      (DisplayName : 'Composite Symbol with EAN linear component'; Symbology : BARCODE_EANX_CC),
      (DisplayName : 'Composite Symbol with GS1-128 linear component'; Symbology : BARCODE_EAN128_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar-14 linear component'; Symbology : BARCODE_RSS14_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar Limited component'; Symbology : BARCODE_RSS_LTD_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar Extended component'; Symbology : BARCODE_RSS_EXP_CC),
      (DisplayName : 'Composite Symbol with UPC A linear component'; Symbology : BARCODE_UPCA_CC),
      (DisplayName : 'Composite Symbol with UPC E linear component'; Symbology : BARCODE_UPCE_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar-14 Stacked component'; Symbology : BARCODE_RSS14STACK_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar-14 Stacked Omnidirectional component'; Symbology : BARCODE_RSS14_OMNI_CC),
      (DisplayName : 'Composite Symbol with GS1 DataBar Expanded Stacked component'; Symbology : BARCODE_RSS_EXPSTACK_CC),
      (DisplayName : 'Channel Code'; Symbology : BARCODE_CHANNEL),
      (DisplayName : 'Code One'; Symbology : BARCODE_CODEONE),
      (DisplayName : 'Grid Matrix'; Symbology : BARCODE_GRIDMATRIX));


  BARCODE_BIND = 2;
  BARCODE_BOX = 4;
  READER_INIT = 16;

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

implementation

uses zint_dmatrix, zint_code128, zint_gs1, zint_common, zint_2of5,
  zint_helper, zint_aztec, zint_qr, zint_upcean,
  zint_maxicode, zint_auspost, zint_code, zint_medical,
  zint_code16k, zint_code49, zint_pdf417, zint_composite, zint_gridmtx,
  zint_plessey, zint_code1, zint_telepen, zint_postal, zint_imail, zint_rss;

const
  TECHNETIUM : String = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%';

const
  EDesiredWithTooSmall = 'The desired width is too small.';
  EDesiredHeightTooSmall = 'The desired height is too small.';

{ TZintRenderValue }

constructor TZintRenderValue.Create;
begin
  inherited;
  Modules := 0;
  TargetUnits := 0;
end;

procedure TZintRenderValue.IncTargetUnits(AValue: Single);
begin
  FTargetUnits := FTargetUnits + AValue;
end;

procedure TZintRenderValue.IncModules(AValue: Single);
begin
  FModules := FModules + AValue;
end;

procedure TZintRenderValue.DecTargetUnits(AValue: Single);
begin
  FTargetUnits := FTargetUnits - AValue;
end;

procedure TZintRenderValue.DecModules(AValue: Single);
begin
  FModules := FModules - AValue;
end;

{ TZintRenderBox }

function TZintRenderBox.GetSum(AIndex: Integer): Single;
begin
  case AIndex of
    0: Result := FTop.TargetUnits + FBottom.TargetUnits;
    1: Result := FLeft.TargetUnits + FRight.TargetUnits;
    2: Result := FTop.Modules + FBottom.Modules;
    3: Result := FLeft.Modules + FRight.Modules;
    else
      Result := 0;
  end;
end;

constructor TZintRenderBox.Create();
begin
  FTop := TZintRenderValue.Create();
  FBottom := TZintRenderValue.Create();
  FLeft := TZintRenderValue.Create();
  FRight := TZintRenderValue.Create();
end;

destructor TZintRenderBox.Destroy;
begin
  FTop.Free;
  FBottom.Free;
  FLeft.Free;
  FRight.Free;

  inherited;
end;

procedure TZintRenderBox.SetModules(AValue: Single);
begin
  Top.Modules := AValue;
  Bottom.Modules := AValue;
  Left.Modules := AValue;
  Right.Modules := AValue;
end;

procedure TZintRenderBox.SetTargetUnits(AValue: Single);
begin
  Top.TargetUnits := AValue;
  Bottom.TargetUnits := AValue;
  Left.TargetUnits := AValue;
  Right.TargetUnits := AValue;
end;

procedure TZintRenderBox.AddModulesToTargetUnits(AModuleWidth,
  AModuleHeight: Single; ATop, ABottom, ALeft, ARight : Boolean);
begin
  //the modules stays untouched, because we need them later to rollback this action
  if ATop then Top.IncTargetUnits(Top.Modules * AModuleHeight);
  if ABottom then Bottom.IncTargetUnits(Bottom.Modules * AModuleHeight);
  if ALeft then Left.IncTargetUnits(Left.Modules * AModuleWidth);
  if ARight then Right.IncTargetUnits(Right.Modules * AModuleWidth);
end;

procedure TZintRenderBox.RemoveModulesFromTargetUnits(AModuleWidth,
  AModuleHeight: Single; ATop, ABottom, ALeft, ARight : Boolean);
begin
  //rollback what we've done in TransferFromModulesToTargetUnits
  if ATop then Top.DecTargetUnits(Top.Modules * AModuleHeight);
  if ABottom then Bottom.DecTargetUnits(Bottom.Modules * AModuleHeight);
  if ALeft then Left.DecTargetUnits(Left.Modules * AModuleWidth);
  if ARight then Right.DecTargetUnits(Right.Modules * AModuleWidth);
end;

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

  for i := Low(row_height) to High(row_height) do
    row_height[i] := 0;

	rows := 0;
	width := 0;
	ustrcpy(text, '');
	strcpy(errtxt, '');
end;

constructor TZintSymbol.Create;
begin
  inherited;

  SetLength(text, 128);
  SetLength(primary, 128);
  SetLength(errtxt, 100);

  symbology := BARCODE_CODE128;
	whitespace_width := 0;
	border_width := 0;
  output_options := 0;
	rows := 0;
	width := 0;
	option_1 := -1;
	option_2 := 0;
	option_3 := 928; // PDF_MAX
	input_mode := DATA_MODE;
	strcpy(primary, '');

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
  if (ZBarcode_Encode(Self, AData, ALength) >= ZERROR_TOO_LONG) then
  begin
    if ARaiseExceptions then
      raise Exception.Create(PChar(@self.errtxt[0]));
  end;
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
		BARCODE_TELEPEN: error_number := telepen(symbol, preprocessed, _length);
		BARCODE_TELEPEN_NUM: error_number := telepen_num(symbol, preprocessed, _length);
		BARCODE_PHARMA: error_number := pharma_one(symbol, preprocessed, _length);
		BARCODE_PLESSEY: error_number := plessey(symbol, preprocessed, _length);
		BARCODE_ITF14: error_number := itf14(symbol, preprocessed, _length);
		BARCODE_FLAT: error_number := flattermarken(symbol, preprocessed, _length);
		BARCODE_FIM: error_number := fim(symbol, preprocessed, _length);
		BARCODE_POSTNET: error_number := post_plot(symbol, preprocessed, _length);
		BARCODE_PLANET: error_number := planet_plot(symbol, preprocessed, _length);
		BARCODE_RM4SCC: error_number := royal_plot(symbol, preprocessed, _length);
		BARCODE_AUSPOST: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSREPLY: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSROUTE: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_AUSREDIRECT: error_number := australia_post(symbol, preprocessed, _length);
		BARCODE_CODE16K: error_number := code16k(symbol, preprocessed, _length);
		BARCODE_PHARMA_TWO: error_number := pharma_two(symbol, preprocessed, _length);
		BARCODE_ONECODE: error_number := imail(symbol, preprocessed, _length);
		BARCODE_ISBNX: error_number := eanx(symbol, preprocessed, _length);
		BARCODE_RSS14: error_number := rss14(symbol, preprocessed, _length);
		BARCODE_RSS14STACK: error_number := rss14(symbol, preprocessed, _length);
		BARCODE_RSS14STACK_OMNI: error_number := rss14(symbol, preprocessed, _length);
		BARCODE_RSS_LTD: error_number := rsslimited(symbol, preprocessed, _length);
		BARCODE_RSS_EXP: error_number := rssexpanded(symbol, preprocessed, _length);
		BARCODE_RSS_EXPSTACK: error_number := rssexpanded(symbol, preprocessed, _length);
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
		BARCODE_KIX: error_number := kix_code(symbol, preprocessed, _length);
		BARCODE_CODE32: error_number := code32(symbol, preprocessed, _length);
		BARCODE_DAFT: error_number := daft_code(symbol, preprocessed, _length);
		BARCODE_EAN14: error_number := ean_14(symbol, preprocessed, _length);
		BARCODE_AZRUNE: error_number := aztec_runes(symbol, preprocessed, _length);
		BARCODE_KOREAPOST: error_number := korea_post(symbol, preprocessed, _length);
		BARCODE_HIBC_128: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_39: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_DM: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_QR: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_PDF: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_MICPDF: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_HIBC_AZTEC: error_number := hibc(symbol, preprocessed, _length);
		BARCODE_JAPANPOST: error_number := japan_post(symbol, preprocessed, _length);
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
  ATarget.Render(Self);
end;

procedure TZintSymbol.InsertModuleRow(AIndex: Integer; ACount : Integer; ASet: Boolean);
var
  i, j : Integer;
begin
  for i := rows - 1 downto AIndex do
  begin
    for j := 0 to width - 1 do
    begin
      if module_is_set(Self, i, j) <> 0 then
        set_module(Self, i + ACount, j)
      else
        unset_module(Self, i + ACount, j);
    end;
  end;

  for i := AIndex to AIndex + ACount - 1 do
  begin
    for j := 0 to width - 1 do
    begin
      if ASet then
        set_module(Self, i, j)
      else
        unset_module(Self, i, j);
    end;
  end;

  Inc(rows, ACount);
end;

procedure TZintSymbol.InsertModuleCol(AIndex: Integer; ACount: Integer; ASet: Boolean);
var
  i, j : Integer;
begin
  for i := 0 to rows - 1 do
  begin
    for j := width - 1 downto AIndex do
    begin
      if module_is_set(Self, i, j) <> 0 then
        set_module(Self, i, j + ACount)
      else
        unset_module(Self, i, j + ACount);
    end;
  end;

  for i := 0 to rows - 1 do
  begin
    for j := AIndex to AIndex + ACount - 1 do
    begin
      if ASet then
        set_module(Self, i, j)
      else
        unset_module(Self, i, j);
    end;
  end;

  Inc(width, ACount);
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
	result := error_number; exit;
end;

{ TZintCustomRenderTarget }

function TZintCustomRenderTarget.CalcX(AValue: Single): Single;
begin
  case FHAlign of
    haLeft : Result := FXDesired + AValue;
    haCenter : Result := (FWidthDesired - FWidth) / 2 + AValue;
    haRight : Result := FWidthDesired - FWidth + AValue;
  end;
end;

function TZintCustomRenderTarget.CalcY(AValue: Single): Single;
begin
  case FVAlign of
    vaTop : Result := FYDesired + AValue;
    vaCenter : Result := (FHeightDesired - FHeight) / 2 + AValue;
    vaBottom : Result := FHeightDesired - FHeight + AValue;
  end;
end;

procedure TZintCustomRenderTarget.AddSymbolOptions;
begin
  FWhitespace.Left.IncModules(FSymbol.whitespace_width);
  FWhitespace.Right.IncModules(FSymbol.whitespace_width);

  if FSymbol.output_options and (BARCODE_BIND or BARCODE_BOX) <> 0 then
  begin
    FBorder.Top.IncModules(FSymbol.border_width);
    FBorder.Bottom.IncModules(FSymbol.border_width);
  end;

  if FSymbol.output_options and BARCODE_BOX <> 0 then
  begin
    FBorder.Left.IncModules(FSymbol.border_width);
    FBorder.Right.IncModules(FSymbol.border_width);
  end;
end;

procedure TZintCustomRenderTarget.RemoveSymbolOptions;
begin
  FWhitespace.Left.DecModules(FSymbol.whitespace_width);
  FWhitespace.Right.DecModules(FSymbol.whitespace_width);

  if FSymbol.output_options and (BARCODE_BIND or BARCODE_BOX) <> 0 then
  begin
    FBorder.Top.DecModules(FSymbol.border_width);
    FBorder.Bottom.DecModules(FSymbol.border_width);
  end;

  if FSymbol.output_options and BARCODE_BOX <> 0 then
  begin
    FBorder.Left.DecModules(FSymbol.border_width);
    FBorder.Right.DecModules(FSymbol.border_width);
  end;
end;

procedure TZintCustomRenderTarget.AddBoxModulesToTargetUnits;
begin
  FMargin.AddModulesToTargetUnits(FModuleWidth, FModuleHeight);
  FBorder.AddModulesToTargetUnits(FModuleWidth, FModuleHeight);
  FPadding.AddModulesToTargetUnits(FModuleWidth, FModuleHeight);
  FWhitespace.AddModulesToTargetUnits(FModuleWidth, FModuleHeight);
  //i'm sorry, but left and right textspace can only be set in targetunits
  FTextSpacing.AddModulesToTargetUnits(FModuleWidth, FModuleHeight, true, true, false, false);
end;

procedure TZintCustomRenderTarget.RemoveBoxModulesFromTargetUnits;
begin
  FMargin.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight);
  FBorder.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight);
  FPadding.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight);
  FWhitespace.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight);
  FTextSpacing.RemoveModulesFromTargetUnits(FModuleWidth, FModuleHeight, true, true, false, false);
end;

procedure TZintCustomRenderTarget.FetchRowInfos;
var
  idx : Integer;
begin
  FRowHeights := 0;
  FLargeBarCount := 0;

  for idx := 0 to FSymbol.rows - 1 do
  begin
    FRowHeights := FRowHeights + FSymbol.row_height[idx];
    if FSymbol.row_height[idx] = 0 then
      Inc(FLargeBarCount)
  end;
end;

procedure TZintCustomRenderTarget.CalcSize;
var
  BarcodeSpace,
  Modules,
  ExtraModules: Single;
  FModuleHWRatio : Single;

  function CalcModulesWidth : Single;
  begin
    Result := FSymbol.width +
              FWhitespace.LeftAndRightModules +
              FPadding.LeftAndRightModules +
              FBorder.LeftAndRightModules +
              FMargin.LeftAndRightModules;
  end;

begin
  if FSymbol.symbology = BARCODE_MAXICODE then
    FModuleHWRatio := 2 / sqrt(3)
  else
    FModuleHWRatio := 1;

  FWidth := FWidthDesired;
  Modules := CalcModulesWidth;

  BarcodeSpace := (FWidthDesired -
                   FWhitespace.LeftAndRightTargetUnits -
                   FPadding.LeftAndRightTargetUnits -
                   FBorder.LeftAndRightTargetUnits -
                   FMargin.LeftAndRightTargetUnits);

  if BarcodeSpace <= 0 then //if the desired width is too small ...
  begin
    if FRenderAdjustMode <> ramInflate then //... and we can't inflate the image ...
      raise Exception.Create(EDesiredWithTooSmall) //... then we can go home ;)
    else
    begin
      FWidth := FMargin.LeftAndRightTargetUnits +
                FBorder.LeftAndRightTargetUnits +
                FPadding.LeftAndRightTargetUnits +
                FWhitespace.LeftAndRightTargetUnits;
      if FMinModuleWidth > 0 then //if there is a MinModuleWidth, we care about it, in order to waive a Inflate()
        BarcodeSpace := Modules * FMinModuleWidth
      else
        BarcodeSpace := Modules;
      FWidth := FWidth + BarcodeSpace;
      Inflate(FWidth, FHeightDesired);
      FWidthDesired := FWidth;
    end;
  end;

  FModuleWidth := BarcodeSpace / Modules;

  //if there is a minimum ModuleWidth defined, we have to care about it
  if (FMinModuleWidth > 0) and
     (FModuleWidth < FMinModuleWidth) and
     (FRenderAdjustMode = ramInflate) then
  begin
    FWidth := FWidth * (FMinModuleWidth / FModuleWidth);
    Inflate(FWidth, FHeightDesired);
    FWidthDesired := FWidth;
    FModuleWidth := FMinModuleWidth;
  end;

  //lets go on with the height
  FModuleHeight := FModuleWidth * FModuleHWRatio;

  //we need 2 vars, because Maxicode has a special height calculation
  Modules := FRowHeights + FLargeBarCount;
  ExtraModules := FWhitespace.TopAndBottomModules +
                  FPadding.TopAndBottomModules +
                  FBorder.TopAndBottomModules +
                  FMargin.TopAndBottomModules;
  if FHasText and FShowText then
    ExtraModules := ExtraModules +
                    FTextSpacing.TopAndBottomModules;

  BarcodeSpace := FHeightDesired -
                  FWhitespace.TopAndBottomTargetUnits -
                  FPadding.TopAndBottomTargetUnits -
                  FBorder.TopAndBottomTargetUnits -
                  FMargin.TopAndBottomTargetUnits;
  if FHasText and FShowText then
    BarcodeSpace := BarcodeSpace -
                    FTextSpacing.TopAndBottomTargetUnits -
                    FTextHeight;

  //calc the minium height
  if FSymbol.symbology = BARCODE_MAXICODE then
  begin
    FHeight := Modules * FModuleHeight * 0.75 +
               FModuleHeight * 0.25 +
               ExtraModules * FModuleHeight +
               FWhitespace.TopAndBottomTargetUnits +
               FPadding.TopAndBottomTargetUnits +
               FBorder.TopAndBottomTargetUnits +
               FMargin.TopAndBottomTargetUnits;
  end
  else
  begin
    FHeight := (Modules + ExtraModules) * FModuleHeight +
               FWhitespace.TopAndBottomTargetUnits +
               FPadding.TopAndBottomTargetUnits +
               FBorder.TopAndBottomTargetUnits +
               FMargin.TopAndBottomTargetUnits;
    if FHasText and FShowText then
      FHeight := FHeight +
                 FTextHeight +
                 FTextSpacing.TopAndBottomTargetUnits;
  end;

  if BarcodeSpace <= 0 then //if the desired height is too small ...
  begin
    if FRenderAdjustMode <> ramInflate then //... and we can't inflate the image ...
      raise Exception.Create(EDesiredHeightTooSmall) //... then we can go home ;)
    else
    begin
      Inflate(FWidth, FHeight);
      FHeightDesired := FHeight;
      BarcodeSpace := Modules * FModuleHeight;
    end;
  end;

  if FHeight > FHeightDesired then
  begin
    case FRenderAdjustMode of
      ramInflate:
      begin
        Inflate(FWidth, FHeight);
        FHeightDesired := FHeight;
      end;
      ramScale:
      begin
        //starting from the height, we have to recalc the width
        if FSymbol.symbology = BARCODE_MAXICODE then
          FModuleHeight := BarcodeSpace / (Modules * 0.75 + 0.25 + ExtraModules)
        else
          FModuleHeight := BarcodeSpace / (Modules + ExtraModules);
        FModuleWidth := FModuleHeight / FModuleHWRatio;
        Modules := CalcModulesWidth;
        FWidth := Modules * FModuleWidth +
                  FWhitespace.LeftAndRightTargetUnits +
                  FPadding.LeftAndRightTargetUnits +
                  FBorder.LeftAndRightTargetUnits +
                  FMargin.LeftAndRightTargetUnits;
        FHeight := FHeightDesired;
      end;
    end;
  end
  else
  if (FSymbol.symbology <> BARCODE_MAXICODE) and (FLargeBarCount > 0) then
    FHeight := FHeightDesired;
end;

procedure TZintCustomRenderTarget.CalcText;
var
  idx : Integer;
  CTHP : TZintCalcTextHeightParams;
  CTWP : TZintCalcTextHeightParams;
begin
  FHasText := ustrlen(FSymbol.text) > 0;
  if FHasText then
    FText := ArrayOfByteToString(FSymbol.text)
  else
    FText := '';

  idx := Pos('+', FText);
  FHasAddonText := (is_extendable(FSymbol.symbology) <> 0) and (idx > 0);
  if FHasAddonText then
  begin
    FAddonText := Copy(FText, idx + 1, Length(FText) - idx);
    FText := Copy(FText, 1, idx - 1);
  end
  else
    FAddonText := '';

  if FShowText and FHasText then
  begin
    CTHP.Text := FText;
    FTextHeight := CalcTextHeight(CTHP);
  end
  else
  begin
    FTextHeight := 0;
  end;
end;

procedure TZintCustomRenderTarget.CalcTextEANUPC;
var
  CTWP : TZintCalcTextWidthParams;
begin
  if (euEAN13 in FEANUPCFlags) or
     (euUPCA in FEANUPCFlags) or
     (euUPCE in FEANUPCFlags) then
  begin
    CTWP.Text := Copy(FText, 1, 1);
    FLeadingTextWidth := CalcTextWidth(CTWP);
    if FWhitespace.Left.TargetUnits < FLeadingTextWidth + FTextSpacing.Right.TargetUnits then
      FWhitespace.Left.TargetUnits := FLeadingTextWidth + FTextSpacing.Right.TargetUnits;
  end
  else
    FLeadingTextWidth := 0;

  if (euUPCA in FEANUPCFlags) or
     (euUPCE in FEANUPCFlags) then
  begin
    CTWP.Text := Copy(FText, Length(FText), 1);
    FTrailingTextWidth := CalcTextWidth(CTWP);
    //if there is no addon, we have to increase the whitespace (maybe)
    if (not ((euAddon2 in FEANUPCFlags) or (euAddon5 in FEANUPCFlags))) and
       (FWhitespace.Right.TargetUnits < FTrailingTextWidth + FTextSpacing.Left.TargetUnits) then
      FWhitespace.Right.TargetUnits := FTrailingTextWidth + FTextSpacing.Left.TargetUnits;
  end
  else
    FTrailingTextWidth := 0;
end;

procedure TZintCustomRenderTarget.CheckEANUPC;
begin
  FEANUPCFlags := [];

  if ((FSymbol.symbology in [BARCODE_EANX, BARCODE_UPCA, BARCODE_UPCE]) and (FSymbol.rows = 1)) or
     (FSymbol.symbology in [BARCODE_EANX_CC, BARCODE_ISBNX, BARCODE_UPCA_CC, BARCODE_UPCE_CC]) then
  begin
    if FHasText then
    begin
      case FSymbol.symbology of
        BARCODE_EANX, BARCODE_EANX_CC, BARCODE_ISBNX:
        begin
          if Length(FText) = 8 then Include(FEANUPCFlags, euEAN8);
          if Length(FText) = 13 then Include(FEANUPCFlags, euEAN13);
        end;
        BARCODE_UPCA, BARCODE_UPCA_CC: Include(FEANUPCFlags, euUPCA);
        BARCODE_UPCE, BARCODE_UPCE_CC: Include(FEANUPCFlags, euUPCE);
      end;
    end;
    if FHasAddonText then
    begin
      if Length(FAddonText) = 2 then Include(FEANUPCFlags, euAddon2);
      if Length(FAddonText) = 5 then Include(FEANUPCFlags, euAddon5);
    end;
  end;
end;

procedure TZintCustomRenderTarget.CalcLargeBarHeight;
begin
  if FLargeBarCount > 0 then
    FLargeBarHeight := (FBarcodeBox.Height - FRowHeights * FModuleHeight) / FLargeBarCount;
end;

procedure TZintCustomRenderTarget.CalcBoxes;
begin
  FX := CalcX(0);
  FY := CalcY(0);

  FMarginBox.X := FX;
  FMarginBox.Y := FY;
  FMarginBox.Width := FWidth;
  FMarginBox.Height := FHeight;

  FBorderBox.X := FMarginBox.X + FMargin.Left.TargetUnits;
  FBorderBox.Y := FMarginBox.Y + FMargin.Top.TargetUnits;
  FBorderBox.Width := FMarginBox.Width - FMargin.Left.TargetUnits - FMargin.Right.TargetUnits;
  FBorderBox.Height := FMarginBox.Height - FMargin.Top.TargetUnits - FMargin.Bottom.TargetUnits;

  FPaddingBox.X := FBorderBox.X + FBorder.Left.TargetUnits;
  FPaddingBox.Y := FBorderBox.Y + FBorder.Top.TargetUnits;
  FPaddingBox.Width := FBorderBox.Width - FBorder.Left.TargetUnits - FBorder.Right.TargetUnits;
  FPaddingBox.Height := FBorderBox.Height - FBorder.Top.TargetUnits - FBorder.Bottom.TargetUnits;

  FWhitespaceBox.X := FPaddingBox.X + FPadding.Left.TargetUnits;
  FWhitespaceBox.Y := FPaddingBox.Y + FPadding.Top.TargetUnits;
  FWhitespaceBox.Width := FPaddingBox.Width - FPadding.Left.TargetUnits - FPadding.Right.TargetUnits;
  FWhitespaceBox.Height := FPaddingBox.Height - FPadding.Top.TargetUnits - FPadding.Bottom.TargetUnits -
                           FTextHeight - FTextSpacing.Bottom.TargetUnits - FTextSpacing.Top.TargetUnits;

  FTextspacingBox.X := FWhitespaceBox.X;
  FTextspacingBox.Y := FWhitespaceBox.Y + FWhitespaceBox.Height;
  FTextspacingBox.Width := FWhitespaceBox.Width;
  FTextspacingBox.Height := FTextSpacing.Top.TargetUnits + FTextHeight + FTextSpacing.Bottom.TargetUnits;

  FTextBox.X := FTextspacingBox.X + FTextSpacing.Left.TargetUnits;
  FTextBox.Y := FTextspacingBox.Y + FTextSpacing.Top.TargetUnits;
  FTextBox.Width := FTextspacingBox.Width - FTextSpacing.Left.TargetUnits - FTextSpacing.Right.TargetUnits;
  FTextBox.Height := FTextSpacingBox.Height - FTextSpacing.Top.TargetUnits - FTextSpacing.Bottom.TargetUnits;

  FBarcodeBox.X := FWhitespaceBox.X + FWhitespace.Left.TargetUnits;
  FBarcodeBox.Y := FWhitespaceBox.Y + FWhitespace.Top.TargetUnits;
  FBarcodeBox.Width := FWhitespaceBox.Width - FWhitespace.Left.TargetUnits - FWhitespace.Right.TargetUnits;
  FBarcodeBox.Height := FWhitespaceBox.Height - FWhitespace.Top.TargetUnits - FWhitespace.Bottom.TargetUnits;
end;

procedure TZintCustomRenderTarget.DrawBorder;
var
  DRP : TZintDrawRectParams;
begin
  if FBorder.Top.TargetUnits > 0 then
  begin
    DRP.X := FBorderBox.X;
    DRP.Y := FBorderBox.Y;
    DRP.Width := FBorderBox.Width;
    DRP.Height := FBorder.Top.TargetUnits;
    DrawRect(DRP);
  end;

  if FBorder.Bottom.TargetUnits > 0 then
  begin
    DRP.X := FBorderBox.X;
    DRP.Y := FBorderBox.Y + FBorderBox.Height - FBorder.Bottom.TargetUnits;
    DRP.Width := FBorderBox.Width;
    DRP.Height := FBorder.Bottom.TargetUnits;
    DrawRect(DRP);
  end;

  if FBorder.Left.TargetUnits > 0 then
  begin
    DRP.X := FBorderBox.X;
    DRP.Y := FBorderBox.Y;
    DRP.Width := FBorder.Left.TargetUnits;
    DRP.Height := FBorderBox.Height;
    DrawRect(DRP);
  end;

  if FBorder.Right.TargetUnits > 0 then
  begin
    DRP.X := FBorderBox.X + FBorderBox.Width - FBorder.Right.TargetUnits;
    DRP.Y := FBorderBox.Y;
    DRP.Width := FBorder.Right.TargetUnits;
    DRP.Height := FBorderBox.Height;
    DrawRect(DRP);
  end;
end;

procedure TZintCustomRenderTarget.DrawMaxiRings;
var
  DRP : TZintDrawRingParams;
  LineWidth : Single;
  OuterRadius : Single;
begin
  LineWidth := FBarcodeBox.Height / 40;
  OuterRadius := (11 * FModuleHeight * 0.75 - FModuleHeight * 0.25) / 2;

  DRP.X := FBarcodeBox.X + FBarcodeBox.Width / 2 - FModuleWidth / 2;
  DRP.Y := FBarcodeBox.Y + FBarcodeBox.Height / 2;

  DRP.OuterRadius := OuterRadius - 0 * LineWidth;
  DRP.InnerRadius := OuterRadius - 1 * LineWidth;
  DrawRing(DRP);

  DRP.OuterRadius := OuterRadius - 2 * LineWidth;
  DRP.InnerRadius := OuterRadius - 3 * LineWidth;
  DrawRing(DRP);

  DRP.OuterRadius := OuterRadius - 4 * LineWidth;
  DRP.InnerRadius := OuterRadius - 5 * LineWidth;
  DrawRing(DRP);
end;

procedure TZintCustomRenderTarget.DrawMaxiModules;
var
  row, col : Integer;
  LX, LY : Single;
  DHP : TZintDrawHexagonParams;
begin
  DHP.Width := FModuleWidth;
  DHP.Height := FModuleHeight;

  LY := FBarcodeBox.Y + FModuleHeight * 0.5;
  for row := 0 to FSymbol.rows - 1 do
  begin
    LX := FBarcodeBox.X + FModuleWidth * 0.5;
    for col := 0 to FSymbol.width - 1 do
    begin
      if module_is_set(FSymbol, row, col) <> 0 then
      begin
        DHP.Y := LY;

        if (row and 1) <> 0 then
          DHP.X := LX + FModuleWidth * 0.5
        else
          DHP.X := LX;

        DrawHexagon(DHP);
      end;
      LX := LX + FModuleWidth;
    end;
    LY := LY + FModuleHeight * 0.75;
  end;
end;

procedure TZintCustomRenderTarget.DrawModules;
var
  row, col : Integer;
  block_width : Integer;
  isspace : Boolean;
  LX,LY : Single;
  DRP : TZintDrawRectParams;
  BarHeight : Single;
  BarIndex : Integer;
begin
  LY := FBarcodeBox.Y;

  for row := 0 to FSymbol.rows - 1 do
  begin
    BarIndex := 0;
    LX := FBarcodeBox.X;
    col := 0;
    isspace := module_is_set(FSymbol, row, col) = 0;

    if FSymbol.row_height[row] = 0 then
      BarHeight := FLargeBarHeight
    else
      BarHeight := FSymbol.row_height[row] * FModuleWidth;

    if (row > 0) and ((FSymbol.output_options and (BARCODE_BIND or BARCODE_BIND)) <> 0) and
       (is_stackable(FSymbol.symbology) <> 0) then
    begin
      DRP.X := LX;
      DRP.Y := LY - (FSymbol.border_width * FModuleWidth) / 2;
      DRP.Width := FBarcodeBox.Width;
      DRP.Height := FSymbol.border_width * FModuleWidth;
      DrawRect(DRP);
    end;

    repeat
      block_width := 0;

      repeat
        Inc(block_width);
      until not (module_is_set(FSymbol, row, col + block_width) = module_is_set(FSymbol, row, col));

      if not isspace then
      begin
        DRP.X := LX;
        DRP.Y := LY;
        DRP.Width := block_width * FModuleWidth;
        DRP.Height := BarHeight;

        if FShowText and FHasText and (row = FSymbol.rows - 1) and (FEANUPCFlags <> []) then
          HandleSpecialBarsEANUPC(BarIndex, DRP);

        DrawRect(DRP);
        Inc(BarIndex)
      end;

      Inc(col, block_width);
      LX := LX + block_width * FModuleWidth;
      isspace := isspace xor true;
    until col >= FSymbol.width;

    LY := LY + BarHeight;
  end;
end;

procedure TZintCustomRenderTarget.DrawTexts;
var
  DTP : TZintDrawTextParams;
begin
  if FShowText and FHasText and (not FTextDone) then
  begin
    DTP.X := FTextBox.X;
    DTP.Y := FTextBox.Y;
    DTP.Width := FTextBox.Width;
    DTP.Height := FTextBox.Height;
    DTP.Text := FText;

    DrawText(DTP);
  end;
end;

procedure TZintCustomRenderTarget.RenderStart;
begin
end;

procedure TZintCustomRenderTarget.RenderStop;
begin
end;

procedure TZintCustomRenderTarget.DrawStart;
begin
end;

procedure TZintCustomRenderTarget.DrawStop;
begin
end;

procedure TZintCustomRenderTarget.HandleSpecialBarsEANUPC(ABarIndex : Integer; var ABar : TZintDrawRectParams);
var
  DTP : TZintDrawTextParams;
begin
  FTextDone := true;

  //guardbars are longer then the others
  if ((euEAN8 in FEANUPCFlags) and (ABarIndex in [0,1,10,11,20,21])) or
     ((euEAN8 in FEANUPCFlags) and (ABarIndex > 21)) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex in [0,1,14,15,28,29])) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex > 29)) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex in [0,1,2,3,14,15,26,27,28,29])) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex > 29)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex in [0,1,14,15,16])) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex > 16))then
    ABar.Height := ABar.Height + FWhitespace.Bottom.TargetUnits + FTextSpacing.Top.TargetUnits + FTextHeight / 2;

  //addon-bars need space above for the text
  if ((euEAN8 in FEANUPCFlags) and (ABarIndex > 21)) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex > 29)) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex > 29)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex > 16)) then
  begin
    ABar.Y := ABar.Y + FTextHeight + FTextSpacing.Top.TargetUnits + FTextSpacing.Bottom.TargetUnits;
    ABar.Height := ABar.Height - FTextHeight - FTextSpacing.Top.TargetUnits - FTextSpacing.Bottom.TargetUnits;
  end;

  //add leading digit
  if (ABarIndex = 0) and
     ((euEAN13 in FEANUPCFlags) or
      (euUPCA in FEANUPCFlags) or
      (euUPCE in FEANUPCFlags)) then
  begin
    DTP.X := ABar.X - FTextSpacing.Right.TargetUnits - FLeadingTextWidth;
    DTP.Y := FTextBox.Y;
    DTP.Width := FLeadingTextWidth;
    DTP.Height := FTextBox.Height;
    DTP.Text := Copy(FText, 1, 1);
    DrawText(DTP);
  end;

  //add trailing digit
  if ((euUPCA in FEANUPCFlags) and (ABarIndex = 29)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex = 16)) then
  begin
    DTP.X := ABar.X + ABar.Width + FTextSpacing.Left.TargetUnits;
    DTP.Y := FTextBox.Y;
    DTP.Width := FTrailingTextWidth;
    DTP.Height := FTextBox.Height;
    DTP.Text := Copy(FText, Length(FText), 1);
    DrawText(DTP);
  end;

  //draw the main text under the barcode
  if ((euEAN8 in FEANUPCFlags) and (ABarIndex in [10, 20])) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex in [14, 28])) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex in [14, 26])) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex = 14)) then
  begin
    DTP.X := FStartTextBar.X + FStartTextBar.Width + FTextSpacing.Left.TargetUnits;
    DTP.Y := FTextBox.Y;
    DTP.Height := FTextBox.Height;
    DTP.Width := ABar.X - FStartTextBar.X + FStartTextBar.Width - FTextSpacing.Left.TargetUnits - FTextSpacing.Right.TargetUnits;
    if euEAN8 in FEANUPCFlags then
    begin
      case ABarIndex of
        10: DTP.Text := Copy(FText, 1, 4);
        20: DTP.Text := Copy(FText, 5, 4);
      end;
    end
    else
    if (euEAN13 in FEANUPCFlags) then
    begin
      case ABarIndex of
        14: DTP.Text := Copy(FText, 2, 6);
        28: DTP.Text := Copy(FText, 8, 6);
      end;
    end
    else
    if (euUPCA in FEANUPCFlags) then
    begin
      case ABarIndex of
        14: DTP.Text := Copy(FText, 2, 5);
        26: DTP.Text := Copy(FText, 7, 5);
      end;
    end
    else
    if (euUPCE in FEANUPCFlags) then
    begin
      DTP.Text := Copy(FText, 2, 6);
    end;
    DrawText(DTP);
  end;

  if ((euEAN8 in FEANUPCFlags) and (ABarIndex = 28) and (euAddon2 in FEANUPCFlags)) or
     ((euEAN8 in FEANUPCFlags) and (ABarIndex = 36)) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex = 36) and (euAddon2 in FEANUPCFlags)) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex = 44)) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex = 36) and (euAddon2 in FEANUPCFlags)) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex = 44)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex = 23) and (euAddon2 in FEANUPCFlags)) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex = 32)) then
  begin
    DTP.X := FStartTextBar.X;
    DTP.Y := FBarcodeBox.Y + FTextSpacing.Top.TargetUnits;
    DTP.Height := FTextHeight;
    DTP.Width := ABar.X + ABar.Width - FStartTextBar.X;
    DTP.Text := FAddonText;
    DrawText(DTP);
  end;

  if ((euEAN8 in FEANUPCFlags) and (ABarIndex in [1, 11, 22])) or
     ((euEAN13 in FEANUPCFlags) and (ABarIndex in [1, 15, 30])) or
     ((euUPCA in FEANUPCFlags) and (ABarIndex in [3, 15, 30])) or
     ((euUPCE in FEANUPCFlags) and (ABarIndex in [1, 17])) then
    FStartTextBar := ABar;
end;

constructor TZintCustomRenderTarget.Create();
begin
  FTransparent:=False;
  FHexagonScale := 0.9;
  FMargin := TZintRenderBox.Create();
  FBorder := TZintRenderBox.Create();
  FPadding := TZintRenderBox.Create();
  FWhitespace := TZintRenderBox.Create();
  FTextSpacing := TZintRenderBox.Create();
  FShowText := true;
  FHAlign := haLeft;
  FVAlign := vaTop;
  FMinModuleWidth := 0;
  FRenderAdjustMode := ramScale;
end;

destructor TZintCustomRenderTarget.Destroy;
begin
  FreeAndNil(FMargin);
  FreeAndNil(FBorder);
  FreeAndNil(FPadding);
  FreeAndNil(FWhitespace);
  FreeAndNil(FTextSpacing);
  inherited;
end;

procedure TZintCustomRenderTarget.Render(ASymbol: TZintSymbol);
var
  CBP : TZintClearBackgroundParams;
begin
  FSymbol := ASymbol;
  FTextDone := false;

  RenderStart;
  AddSymbolOptions;
  FetchRowInfos;
  CalcText;
  CheckEANUPC;
  CalcTextEANUPC;
  CalcSize;
  AddBoxModulesToTargetUnits;
  CalcBoxes;
  CalcLargeBarHeight;

  DrawStart;

  if not FTransparent then
  begin
    CBP.X := FXDesired;
    CBP.Y := FYDesired;
    CBP.Width := FWidthDesired;
    CBP.Height := FHeightDesired;
    ClearBackground(CBP);
  end;

  DrawBorder;

  if FSymbol.symbology = BARCODE_MAXICODE then
  begin
    DrawMaxiRings;
    DrawMaxiModules;
  end
  else
  begin
    DrawModules;
    if FHasText then
      DrawTexts;
  end;

  DrawStop;
  RemoveBoxModulesFromTargetUnits;
  RemoveSymbolOptions;
  RenderStop;
end;

end.

