{-----------------------------------------------------------------------------
 Purpose: The interface to the zint.dll 
 Created: 03.12.2009 14:28:36

 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uZintBarcode;

interface

uses
  Windows,
  Classes,
  Controls,
  uZintInterface,
  SysUtils,
  Graphics;

type
  TZBRotation = (r0, r90, r180, r270);
  TZBType = (
    tBARCODE_CODE11,
    tBARCODE_C25MATRIX,
    tBARCODE_C25INTER,
    tBARCODE_C25IATA,
    tBARCODE_C25LOGIC,
    tBARCODE_C25IND,
    tBARCODE_CODE39,
    tBARCODE_EXCODE39,
    tBARCODE_EANX,
    tBARCODE_EAN128,
    tBARCODE_CODABAR,
    tBARCODE_CODE128,
    tBARCODE_DPLEIT,
    tBARCODE_DPIDENT,
    tBARCODE_CODE16K,
    tBARCODE_CODE49,
    tBARCODE_CODE93,
    tBARCODE_FLAT,
    tBARCODE_RSS14,
    tBARCODE_RSS_LTD,
    tBARCODE_RSS_EXP,
    tBARCODE_TELEPEN,
    tBARCODE_UPCA,
    tBARCODE_UPCE,
    tBARCODE_POSTNET,
    tBARCODE_MSI_PLESSEY,
    tBARCODE_FIM,
    tBARCODE_LOGMARS,
    tBARCODE_PHARMA,
    tBARCODE_PZN,
    tBARCODE_PHARMA_TWO,
    tBARCODE_PDF417,
    tBARCODE_PDF417TRUNC,
    tBARCODE_MAXICODE,
    tBARCODE_QRCODE,
    tBARCODE_CODE128B,
    tBARCODE_AUSPOST,
    tBARCODE_AUSREPLY,
    tBARCODE_AUSROUTE,
    tBARCODE_AUSREDIRECT,
    tBARCODE_ISBNX,
    tBARCODE_RM4SCC,
    tBARCODE_DATAMATRIX,
    tBARCODE_EAN14,
    tBARCODE_CODABLOCKF,
    tBARCODE_NVE18,
    tBARCODE_JAPANPOST,
    tBARCODE_KOREAPOST,
    tBARCODE_RSS14STACK,
    tBARCODE_RSS14STACK_OMNI,
    tBARCODE_RSS_EXPSTACK,
    tBARCODE_PLANET,
    tBARCODE_MICROPDF417,
    tBARCODE_ONECODE,
    tBARCODE_PLESSEY,
    tBARCODE_TELEPEN_NUM,
    tBARCODE_ITF14,
    tBARCODE_KIX,
    tBARCODE_AZTEC,
    tBARCODE_DAFT,
    tBARCODE_MICROQR,
    tBARCODE_HIBC_128,
    tBARCODE_HIBC_39,
    tBARCODE_HIBC_DM,
    tBARCODE_HIBC_QR,
    tBARCODE_HIBC_PDF,
    tBARCODE_HIBC_MICPDF,
    tBARCODE_HIBC_BLOCKF,
    tBARCODE_HIBC_AZTEC,
    tBARCODE_AZRUNE,
    tBARCODE_CODE32,
    tBARCODE_EANX_CC,
    tBARCODE_EAN128_CC,
    tBARCODE_RSS14_CC,
    tBARCODE_RSS_LTD_CC,
    tBARCODE_RSS_EXP_CC,
    tBARCODE_UPCA_CC,
    tBARCODE_UPCE_CC,
    tBARCODE_RSS14STACK_CC,
    tBARCODE_RSS14_OMNI_CC,
    tBARCODE_RSS_EXPSTACK_CC,
    tBARCODE_CHANNEL,
    tBARCODE_CODEONE,
    tBARCODE_GRIDMATRIX);

  TZintBarcode = class(TPersistent)
  private
    FSymbol : PZSymbol;
    FData : UTF8String;
    FRotation: TZBRotation;
    FOnChanged: TNotifyEvent;
    FStacked: Boolean;
    FScale : Single;

    procedure CheckForError(AReturnValue : Integer);
    function ErrorTextFromSymbol : String;
    procedure SetData(const Value: WideString);
    procedure Changed;
    procedure EncodeNow;

    procedure CreateSymbol;
    procedure FreeSymbol;

    procedure SetType(const Value: TZBType);
    function GetData: WideString;
    function GetType: TZBType;
    procedure SetScale(const Value: Single);
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    function GetBorderWidth: Integer;
    procedure SetBorderWidth(const Value: Integer);
    function GetOutputOptions: TZOutputOptions;
    procedure SetOutputOptions(const Value: TZOutputOptions);
    function GetColor(const Index: Integer): TColor;
    procedure SetColor(const Index: Integer; const Value: TColor);
    function GetOption(const Index: Integer): Integer;
    procedure SetOption(const Index, Value: Integer);
    procedure SetRotation(const Value: TZBRotation);
    function GetBarcodeSize: TPoint;
    function GetPrimary: String;
    procedure SetPrimary(const Value: String);
    function GetSHRT: Boolean;
    procedure SetSHRT(const Value: Boolean);
    procedure SetStacked(const Value: Boolean);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Assign(Source: TPersistent); override;

    procedure GetBarcode(const ABitmap : TBitmap);

    procedure Clear();

    property BarcodeSize : TPoint read GetBarcodeSize;

    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
  published
    property BarcodeType : TZBType read GetType write SetType;
    property Data : Widestring read GetData write SetData;
    property Scale : Single read FScale write SetScale stored true;
    property Height : Integer read GetHeight write SetHeight;
    property BorderWidth : Integer read GetBorderWidth write SetBorderWidth;
    property OutputOptions : TZOutputOptions read GetOutputOptions write SetOutputOptions;
    property FGColor : TColor index 0 read GetColor write SetColor;
    property BGColor : TColor index 1 read GetColor write SetColor;
    property Option1 : Integer index 1 read GetOption write SetOption;
    property Option2 : Integer index 2 read GetOption write SetOption;
    property Option3 : Integer index 3 read GetOption write SetOption;
    property Roatation : TZBRotation read FRotation write SetRotation;
    property Primary : String read GetPrimary write SetPrimary;
    property ShowHumanReadableText : Boolean read GetSHRT write SetSHRT;
    property Stacked : Boolean read FStacked write SetStacked;
  end;

  TCustomZintBarcodeComponent = class(TGraphicControl)
  private
    FCenter: Boolean;
    procedure SetCenter(const Value: Boolean);
    procedure SetBarode(const Value: TZintBarcode);
  protected
    FBarcode : TZintBarcode;
    FBitmap : TBitmap;

    procedure OnChanged(Sender : TObject);

    procedure Paint; override;

    property Barcode : TZintBarcode read FBarcode write SetBarode;
    property Center : Boolean read FCenter write SetCenter default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TZintBarcodeComponent = class(TCustomZintBarcodeComponent)
  published
    property Align;
    property Center;
    property Barcode;
  end;


  EZintError = type Exception;
  EZintErrorTooLong = type EZintError;
  EZintErrorInvalidData = type EZintError;
  EZintErrorInvalidCheck = type EZintError;
  EZintErrorInvalidOption = type EZintError;
  EZintErrorEncodingProblem = type EZintError;
  EZintErrorMemory = type EZintError;

implementation

{ TZintBarcode }

procedure TZintBarcode.Assign(Source: TPersistent);
var
  OldChanged : TNotifyEvent;
begin
  if Source is TZintBarcode then
  begin
    OldChanged := FOnChanged;
    FOnChanged := nil;

    Data := TZintBarcode(Source).Data;
    BarcodeType := TZintBarcode(Source).BarcodeType;
    Scale := TZintBarcode(Source).Scale;
    Height := TZintBarcode(Source).Height;
    BorderWidth := TZintBarcode(Source).BorderWidth;
    OutputOptions := TZintBarcode(Source).OutputOptions;
    FGColor := TZintBarcode(Source).FGColor;
    BGColor := TZintBarcode(Source).BGColor;
    Option1 := TZintBarcode(Source).Option1;
    Option2 := TZintBarcode(Source).Option2;
    Option3 := TZintBarcode(Source).Option3;
    Roatation := TZintBarcode(Source).Roatation;
    Primary := TZintBarcode(Source).Primary;
    ShowHumanReadableText:=TZintBarcode(Source).ShowHumanReadableText;
    Stacked := TZintBarcode(Source).Stacked;

    FOnChanged := OldChanged;
    Changed;
  end
  else
    inherited;
end;

procedure TZintBarcode.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);  
end;

procedure TZintBarcode.CheckForError(AReturnValue: Integer);
begin
  case TZErrorCode(AReturnValue) of
    ERROR_TOO_LONG: raise EZintErrorTooLong.Create(ErrorTextFromSymbol);
    ERROR_INVALID_DATA: raise EZintErrorInvalidData.Create(ErrorTextFromSymbol);
    ERROR_INVALID_CHECK: raise EZintErrorInvalidCheck.Create(ErrorTextFromSymbol);
    ERROR_INVALID_OPTION: raise EZintErrorInvalidOption.Create(ErrorTextFromSymbol);
    ERROR_ENCODING_PROBLEM: raise EZintErrorEncodingProblem.Create(ErrorTextFromSymbol);
    ERROR_MEMORY: raise EZintErrorMemory.Create(ErrorTextFromSymbol);
  end;
end;

procedure TZintBarcode.Clear;
{var
  temp : PZSymbol;}
begin
  {New(temp);

  CopyMemory(temp, FSymbol, SizeOf(TZSymbol));

  FreeSymbol;
  CreateSymbol;

  CopyMemory(FSymbol, temp, SizeOf(TZSymbol));

  FSymbol.bitmap := nil;
  FSymbol.bitmap_width := 0;
  FSymbol.bitmap_height := 0;
  FillMemory(@FSymbol.encoded_data, SizeOf(FSymbol.encoded_data), 0);
  FillMemory(@FSymbol.text, SizeOf(FSymbol.text), 0);
  FSymbol.rows := 0;
  FSymbol.width := 0;
  FillMemory(@FSymbol.row_height, SizeOf(FSymbol.row_height), 0);


  Dispose(temp);}

  if Assigned(FSymbol) then
    ZBarcode_Clear(FSymbol);
end;

constructor TZintBarcode.Create;
begin
  inherited;

  CreateSymbol;

  FStacked := false;

  FScale := 1;

  FSymbol.show_human_readable_text := 1;
  FSymbol.input_mode := Integer(UNICODE_MODE);
  FRotation := r0;
  Data := '123456789';
end;

procedure TZintBarcode.CreateSymbol;
begin
  FSymbol := nil;
  FSymbol := ZBarcode_Create;
  if not Assigned(FSymbol) then
    raise EZintError.Create('Can not create internal symbol structure');
end;

destructor TZintBarcode.Destroy;
begin
  FreeSymbol;
    
  inherited;
end;

procedure TZintBarcode.EncodeNow;
var
  rotation : Integer;
begin
  case FRotation of
    r0: rotation := 0;
    r90: rotation := 90;
    r180: rotation := 180;
    r270: rotation := 270;
    else
      rotation := 0;
  end;

  if not FStacked then
    Clear;

  CheckForError(ZBarcode_Encode_and_Buffer(FSymbol, PAnsiChar(FData), Length(FData), rotation));
end;

function TZintBarcode.ErrorTextFromSymbol: String;
begin
  Result := StrPas(PAnsiChar(@FSymbol.errtxt));
end;

procedure TZintBarcode.FreeSymbol;
begin
  if Assigned(FSymbol) then
  begin
    ZBarcode_Delete(FSymbol);
    FSymbol := nil;
  end;
end;

procedure TZintBarcode.GetBarcode(const ABitmap: TBitmap);
var
  bmp : TBitmap;
begin
  EncodeNow;

  bmp := TBitmap.Create;
  try
    ZBarcodeToBitmap(FSymbol, bmp);

    ABitmap.PixelFormat := bmp.PixelFormat;
    ABitmap.SetSize(Round(bmp.Width * FScale), Round(bmp.Height * FScale));

    StretchBlt(ABitmap.Canvas.Handle,
               0, 0, ABitmap.Width, ABitmap.Height,
               bmp.Canvas.Handle,
               0, 0, bmp.Width, bmp.Height,
               SRCCOPY);
  finally
    bmp.Free;
  end;
end;

function TZintBarcode.GetBarcodeSize: TPoint;
begin
  Result := Point(FSymbol.bitmap_width, FSymbol.bitmap_height);
end;

function TZintBarcode.GetBorderWidth: Integer;
begin
  Result := FSymbol.border_width;
end;

function TZintBarcode.GetColor(const Index: Integer): TColor;
var
  S : AnsiString;
begin
  case Index of
    0: S := FSymbol.fgcolour;
    1: S := FSymbol.bgcolour;
  end;

  Result := StrToInt('$' + S);
end;

function TZintBarcode.GetData: Widestring;
begin
  {$IFDEF UNICODE}
  Result := UTF8ToWideString(FData);
  {$ELSE}
  Result := UTF8Decode(FData);
  {$ENDIF}
end;

function TZintBarcode.GetHeight: Integer;
begin
  Result := FSymbol.height;
end;

function TZintBarcode.GetOption(const Index: Integer): Integer;
begin
  case Index of
    1: Result := FSymbol.option_1;
    2: Result := FSymbol.option_2;
    3: Result := FSymbol.option_3;
    else
      Result := 0;
  end;
end;

function TZintBarcode.GetOutputOptions: TZOutputOptions;
begin
  Result := TZOutputOptions(FSymbol.output_options);
end;

function TZintBarcode.GetPrimary: String;
begin
  Result := StrPas(PAnsiChar(@FSymbol.primary));
end;

function TZintBarcode.GetSHRT: Boolean;
begin
  Result := FSymbol.show_human_readable_text = 1;
end;

function TZintBarcode.GetType: TZBType;
begin
  case FSymbol.symbology of
    BARCODE_CODE11 : Result := tBARCODE_CODE11;
    BARCODE_C25MATRIX : Result := tBARCODE_C25MATRIX;
    BARCODE_C25INTER : Result := tBARCODE_C25INTER;
    BARCODE_C25IATA : Result := tBARCODE_C25IATA;
    BARCODE_C25LOGIC : Result := tBARCODE_C25LOGIC;
    BARCODE_C25IND : Result := tBARCODE_C25IND;
    BARCODE_CODE39 : Result := tBARCODE_CODE39;
    BARCODE_EXCODE39 : Result := tBARCODE_EXCODE39;
    BARCODE_EANX : Result := tBARCODE_EANX;
    BARCODE_EAN128 : Result := tBARCODE_EAN128;
    BARCODE_CODABAR : Result := tBARCODE_CODABAR;
    BARCODE_CODE128 : Result := tBARCODE_CODE128;
    BARCODE_DPLEIT : Result := tBARCODE_DPLEIT;
    BARCODE_DPIDENT : Result := tBARCODE_DPIDENT;
    BARCODE_CODE16K : Result := tBARCODE_CODE16K;
    BARCODE_CODE49 : Result := tBARCODE_CODE49;
    BARCODE_CODE93 : Result := tBARCODE_CODE93;
    BARCODE_FLAT : Result := tBARCODE_FLAT;
    BARCODE_RSS14 : Result := tBARCODE_RSS14;
    BARCODE_RSS_LTD : Result := tBARCODE_RSS_LTD;
    BARCODE_RSS_EXP : Result := tBARCODE_RSS_EXP;
    BARCODE_TELEPEN : Result := tBARCODE_TELEPEN;
    BARCODE_UPCA : Result := tBARCODE_UPCA;
    BARCODE_UPCE : Result := tBARCODE_UPCE;
    BARCODE_POSTNET : Result := tBARCODE_POSTNET;
    BARCODE_MSI_PLESSEY : Result := tBARCODE_MSI_PLESSEY;
    BARCODE_FIM : Result := tBARCODE_FIM;
    BARCODE_LOGMARS : Result := tBARCODE_LOGMARS;
    BARCODE_PHARMA : Result := tBARCODE_PHARMA;
    BARCODE_PZN : Result := tBARCODE_PZN;
    BARCODE_PHARMA_TWO : Result := tBARCODE_PHARMA_TWO;
    BARCODE_PDF417 : Result := tBARCODE_PDF417;
    BARCODE_PDF417TRUNC : Result := tBARCODE_PDF417TRUNC;
    BARCODE_MAXICODE : Result := tBARCODE_MAXICODE;
    BARCODE_QRCODE : Result := tBARCODE_QRCODE;
    BARCODE_CODE128B : Result := tBARCODE_CODE128B;
    BARCODE_AUSPOST : Result := tBARCODE_AUSPOST;
    BARCODE_AUSREPLY : Result := tBARCODE_AUSREPLY;
    BARCODE_AUSROUTE : Result := tBARCODE_AUSROUTE;
    BARCODE_AUSREDIRECT : Result := tBARCODE_AUSREDIRECT;
    BARCODE_ISBNX : Result := tBARCODE_ISBNX;
    BARCODE_RM4SCC : Result := tBARCODE_RM4SCC;
    BARCODE_DATAMATRIX : Result := tBARCODE_DATAMATRIX;
    BARCODE_EAN14 : Result := tBARCODE_EAN14;
    BARCODE_CODABLOCKF : Result := tBARCODE_CODABLOCKF;
    BARCODE_NVE18 : Result := tBARCODE_NVE18;
    BARCODE_JAPANPOST : Result := tBARCODE_JAPANPOST;
    BARCODE_KOREAPOST : Result := tBARCODE_KOREAPOST;
    BARCODE_RSS14STACK : Result := tBARCODE_RSS14STACK;
    BARCODE_RSS14STACK_OMNI : Result := tBARCODE_RSS14STACK_OMNI;
    BARCODE_RSS_EXPSTACK : Result := tBARCODE_RSS_EXPSTACK;
    BARCODE_PLANET : Result := tBARCODE_PLANET;
    BARCODE_MICROPDF417 : Result := tBARCODE_MICROPDF417;
    BARCODE_ONECODE : Result := tBARCODE_ONECODE;
    BARCODE_PLESSEY : Result := tBARCODE_PLESSEY;
    BARCODE_TELEPEN_NUM : Result := tBARCODE_TELEPEN_NUM;
    BARCODE_ITF14 : Result := tBARCODE_ITF14;
    BARCODE_KIX : Result := tBARCODE_KIX;
    BARCODE_AZTEC : Result := tBARCODE_AZTEC;
    BARCODE_DAFT : Result := tBARCODE_DAFT;
    BARCODE_MICROQR : Result := tBARCODE_MICROQR;
    BARCODE_HIBC_128 : Result := tBARCODE_HIBC_128;
    BARCODE_HIBC_39 : Result := tBARCODE_HIBC_39;
    BARCODE_HIBC_DM : Result := tBARCODE_HIBC_DM;
    BARCODE_HIBC_QR : Result := tBARCODE_HIBC_QR;
    BARCODE_HIBC_PDF : Result := tBARCODE_HIBC_PDF;
    BARCODE_HIBC_MICPDF : Result := tBARCODE_HIBC_MICPDF;
    BARCODE_HIBC_BLOCKF : Result := tBARCODE_HIBC_BLOCKF;
    BARCODE_HIBC_AZTEC : Result := tBARCODE_HIBC_AZTEC;
    BARCODE_AZRUNE : Result := tBARCODE_AZRUNE;
    BARCODE_CODE32 : Result := tBARCODE_CODE32;
    BARCODE_EANX_CC : Result := tBARCODE_EANX_CC;
    BARCODE_EAN128_CC : Result := tBARCODE_EAN128_CC;
    BARCODE_RSS14_CC : Result := tBARCODE_RSS14_CC;
    BARCODE_RSS_LTD_CC : Result := tBARCODE_RSS_LTD_CC;
    BARCODE_RSS_EXP_CC : Result := tBARCODE_RSS_EXP_CC;
    BARCODE_UPCA_CC : Result := tBARCODE_UPCA_CC;
    BARCODE_UPCE_CC : Result := tBARCODE_UPCE_CC;
    BARCODE_RSS14STACK_CC : Result := tBARCODE_RSS14STACK_CC;
    BARCODE_RSS14_OMNI_CC : Result := tBARCODE_RSS14_OMNI_CC;
    BARCODE_RSS_EXPSTACK_CC : Result := tBARCODE_RSS_EXPSTACK_CC;
    BARCODE_CHANNEL : Result := tBARCODE_CHANNEL;
    BARCODE_CODEONE : Result := tBARCODE_CODEONE;
    BARCODE_GRIDMATRIX : Result := tBARCODE_GRIDMATRIX;
  end;
end;

procedure TZintBarcode.SetBorderWidth(const Value: Integer);
begin
  FSymbol.border_width := Value;
  Changed;
end;

procedure TZintBarcode.SetColor(const Index: Integer; const Value: TColor);
var
  S : AnsiString;
begin

  S := Format('%.6x', [ColorToRGB(Value)]);

  case Index of
    0: StrPCopy(@FSymbol.fgcolour, S);
    1: StrPCopy(@FSymbol.bgcolour, S);
  end;

  Changed;
end;

procedure TZintBarcode.SetData(const Value: Widestring);
var
  OldData : UTF8String;
begin
  OldData := FData;
  FData := UTF8Encode(Value);
  
  try
    Changed;
  except
    FData := OldData;
    raise;
  end;
end;

procedure TZintBarcode.SetHeight(const Value: Integer);
begin
  FSymbol.height := Value;
  Changed;
end;

procedure TZintBarcode.SetOption(const Index, Value: Integer);
begin
  case Index of
    1 : FSymbol.option_1 := Value;
    2 : FSymbol.option_2 := Value;
    3 : FSymbol.option_3 := Value;
  end;
end;

procedure TZintBarcode.SetOutputOptions(const Value: TZOutputOptions);
begin
  FSymbol.output_options := Integer(Value);
end;

procedure TZintBarcode.SetPrimary(const Value: String);
begin
  StrPCopy(@FSymbol.primary, Value);
  Changed;
end;

procedure TZintBarcode.SetRotation(const Value: TZBRotation);
begin
  FRotation := Value;
  Changed;
end;

procedure TZintBarcode.SetScale(const Value: Single);
begin
  FScale := Value;
  Changed;
end;

procedure TZintBarcode.SetSHRT(const Value: Boolean);
begin
  if Value then
    FSymbol.show_human_readable_text := 1
  else
    FSymbol.show_human_readable_text := 0;

  Changed;
end;

procedure TZintBarcode.SetStacked(const Value: Boolean);
begin
  FStacked := Value;
  Changed;
end;

procedure TZintBarcode.SetType(const Value: TZBType);
var
  Oldsymbology : Integer;
begin
  Oldsymbology := FSymbol.symbology;

  case Value of
    tBARCODE_CODE11: FSymbol.symbology := BARCODE_CODE11;
    tBARCODE_C25MATRIX: FSymbol.symbology := BARCODE_C25MATRIX;
    tBARCODE_C25INTER: FSymbol.symbology := BARCODE_C25INTER;
    tBARCODE_C25IATA: FSymbol.symbology := BARCODE_C25IATA;
    tBARCODE_C25LOGIC: FSymbol.symbology := BARCODE_C25LOGIC;
    tBARCODE_C25IND: FSymbol.symbology := BARCODE_C25IND;
    tBARCODE_CODE39: FSymbol.symbology := BARCODE_CODE39;
    tBARCODE_EXCODE39: FSymbol.symbology := BARCODE_EXCODE39;
    tBARCODE_EANX: FSymbol.symbology := BARCODE_EANX;
    tBARCODE_EAN128: FSymbol.symbology := BARCODE_EAN128;
    tBARCODE_CODABAR: FSymbol.symbology := BARCODE_CODABAR;
    tBARCODE_CODE128: FSymbol.symbology := BARCODE_CODE128;
    tBARCODE_DPLEIT: FSymbol.symbology := BARCODE_DPLEIT;
    tBARCODE_DPIDENT: FSymbol.symbology := BARCODE_DPIDENT;
    tBARCODE_CODE16K: FSymbol.symbology := BARCODE_CODE16K;
    tBARCODE_CODE49: FSymbol.symbology := BARCODE_CODE49;
    tBARCODE_CODE93: FSymbol.symbology := BARCODE_CODE93;
    tBARCODE_FLAT: FSymbol.symbology := BARCODE_FLAT;
    tBARCODE_RSS14: FSymbol.symbology := BARCODE_RSS14;
    tBARCODE_RSS_LTD: FSymbol.symbology := BARCODE_RSS_LTD;
    tBARCODE_RSS_EXP: FSymbol.symbology := BARCODE_RSS_EXP;
    tBARCODE_TELEPEN: FSymbol.symbology := BARCODE_TELEPEN;
    tBARCODE_UPCA: FSymbol.symbology := BARCODE_UPCA;
    tBARCODE_UPCE: FSymbol.symbology := BARCODE_UPCE;
    tBARCODE_POSTNET: FSymbol.symbology := BARCODE_POSTNET;
    tBARCODE_MSI_PLESSEY: FSymbol.symbology := BARCODE_MSI_PLESSEY;
    tBARCODE_FIM: FSymbol.symbology := BARCODE_FIM;
    tBARCODE_LOGMARS: FSymbol.symbology := BARCODE_LOGMARS;
    tBARCODE_PHARMA: FSymbol.symbology := BARCODE_PHARMA;
    tBARCODE_PZN: FSymbol.symbology := BARCODE_PZN;
    tBARCODE_PHARMA_TWO: FSymbol.symbology := BARCODE_PHARMA_TWO;
    tBARCODE_PDF417: FSymbol.symbology := BARCODE_PDF417;
    tBARCODE_PDF417TRUNC: FSymbol.symbology := BARCODE_PDF417TRUNC;
    tBARCODE_MAXICODE: FSymbol.symbology := BARCODE_MAXICODE;
    tBARCODE_QRCODE: FSymbol.symbology := BARCODE_QRCODE;
    tBARCODE_CODE128B: FSymbol.symbology := BARCODE_CODE128B;
    tBARCODE_AUSPOST: FSymbol.symbology := BARCODE_AUSPOST;
    tBARCODE_AUSREPLY: FSymbol.symbology := BARCODE_AUSREPLY;
    tBARCODE_AUSROUTE: FSymbol.symbology := BARCODE_AUSROUTE;
    tBARCODE_AUSREDIRECT: FSymbol.symbology := BARCODE_AUSREDIRECT;
    tBARCODE_ISBNX: FSymbol.symbology := BARCODE_ISBNX;
    tBARCODE_RM4SCC: FSymbol.symbology := BARCODE_RM4SCC;
    tBARCODE_DATAMATRIX: FSymbol.symbology := BARCODE_DATAMATRIX;
    tBARCODE_EAN14: FSymbol.symbology := BARCODE_EAN14;
    tBARCODE_CODABLOCKF: FSymbol.symbology := BARCODE_CODABLOCKF;
    tBARCODE_NVE18: FSymbol.symbology := BARCODE_NVE18;
    tBARCODE_JAPANPOST: FSymbol.symbology := BARCODE_JAPANPOST;
    tBARCODE_KOREAPOST: FSymbol.symbology := BARCODE_KOREAPOST;
    tBARCODE_RSS14STACK: FSymbol.symbology := BARCODE_RSS14STACK;
    tBARCODE_RSS14STACK_OMNI: FSymbol.symbology := BARCODE_RSS14STACK_OMNI;
    tBARCODE_RSS_EXPSTACK: FSymbol.symbology := BARCODE_RSS_EXPSTACK;
    tBARCODE_PLANET: FSymbol.symbology := BARCODE_PLANET;
    tBARCODE_MICROPDF417: FSymbol.symbology := BARCODE_MICROPDF417;
    tBARCODE_ONECODE: FSymbol.symbology := BARCODE_ONECODE;
    tBARCODE_PLESSEY: FSymbol.symbology := BARCODE_PLESSEY;
    tBARCODE_TELEPEN_NUM: FSymbol.symbology := BARCODE_TELEPEN_NUM;
    tBARCODE_ITF14: FSymbol.symbology := BARCODE_ITF14;
    tBARCODE_KIX: FSymbol.symbology := BARCODE_KIX;
    tBARCODE_AZTEC: FSymbol.symbology := BARCODE_AZTEC;
    tBARCODE_DAFT: FSymbol.symbology := BARCODE_DAFT;
    tBARCODE_MICROQR: FSymbol.symbology := BARCODE_MICROQR;
    tBARCODE_HIBC_128: FSymbol.symbology := BARCODE_HIBC_128;
    tBARCODE_HIBC_39: FSymbol.symbology := BARCODE_HIBC_39;
    tBARCODE_HIBC_DM: FSymbol.symbology := BARCODE_HIBC_DM;
    tBARCODE_HIBC_QR: FSymbol.symbology := BARCODE_HIBC_QR;
    tBARCODE_HIBC_PDF: FSymbol.symbology := BARCODE_HIBC_PDF;
    tBARCODE_HIBC_MICPDF: FSymbol.symbology := BARCODE_HIBC_MICPDF;
    tBARCODE_HIBC_BLOCKF: FSymbol.symbology := BARCODE_HIBC_BLOCKF;
    tBARCODE_HIBC_AZTEC: FSymbol.symbology := BARCODE_HIBC_AZTEC;
    tBARCODE_AZRUNE: FSymbol.symbology := BARCODE_AZRUNE;
    tBARCODE_CODE32: FSymbol.symbology := BARCODE_CODE32;
    tBARCODE_EANX_CC: FSymbol.symbology := BARCODE_EANX_CC;
    tBARCODE_EAN128_CC: FSymbol.symbology := BARCODE_EAN128_CC;
    tBARCODE_RSS14_CC: FSymbol.symbology := BARCODE_RSS14_CC;
    tBARCODE_RSS_LTD_CC: FSymbol.symbology := BARCODE_RSS_LTD_CC;
    tBARCODE_RSS_EXP_CC: FSymbol.symbology := BARCODE_RSS_EXP_CC;
    tBARCODE_UPCA_CC: FSymbol.symbology := BARCODE_UPCA_CC;
    tBARCODE_UPCE_CC: FSymbol.symbology := BARCODE_UPCE_CC;
    tBARCODE_RSS14STACK_CC: FSymbol.symbology := BARCODE_RSS14STACK_CC;
    tBARCODE_RSS14_OMNI_CC: FSymbol.symbology := BARCODE_RSS14_OMNI_CC;
    tBARCODE_RSS_EXPSTACK_CC: FSymbol.symbology := BARCODE_RSS_EXPSTACK_CC;
    tBARCODE_CHANNEL: FSymbol.symbology := BARCODE_CHANNEL;
    tBARCODE_CODEONE: FSymbol.symbology := BARCODE_CODEONE;
    tBARCODE_GRIDMATRIX: FSymbol.symbology := BARCODE_GRIDMATRIX;
  end;

  try
    Changed;
  except
    FSymbol.symbology := Oldsymbology;
    raise;
  end;
end;

{ TCustomZintBarcodeComponent }

constructor TCustomZintBarcodeComponent.Create(AOwner: TComponent);
begin
  inherited;

  FBitmap := TBitmap.Create;

  FCenter := true;

  FBarcode := TZintBarcode.Create;
  FBarcode.OnChanged := OnChanged;
  FBarcode.Data := FBarcode.Data;
end;

destructor TCustomZintBarcodeComponent.Destroy;
begin
  FBarcode.Free;

  FBitmap.Free;

  inherited;
end;


procedure TCustomZintBarcodeComponent.OnChanged(Sender: TObject);
begin
  FBarcode.GetBarcode(FBitmap);
  Invalidate;
end;

procedure TCustomZintBarcodeComponent.Paint;
begin
  inherited;

  Canvas.Brush.Color := FBarcode.BGColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Canvas.ClipRect);

  if FCenter then
    Canvas.Draw((Width - FBitmap.Width) div 2, (Height - FBitmap.Height) div 2, FBitmap)
  else
    Canvas.Draw(0,0, FBitmap);
end;

procedure TCustomZintBarcodeComponent.SetCenter(const Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TCustomZintBarcodeComponent.SetBarode(const Value: TZintBarcode);
begin
  FBarcode.Assign(Value);
end;

end.
