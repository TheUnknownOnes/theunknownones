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
    tBARCODE_CODE11 = BARCODE_CODE11,
    tBARCODE_C25MATRIX = BARCODE_C25MATRIX,
    tBARCODE_C25INTER = BARCODE_C25INTER,
    tBARCODE_C25IATA = BARCODE_C25IATA,
    tBARCODE_C25LOGIC = BARCODE_C25LOGIC,
    tBARCODE_C25IND = BARCODE_C25IND,
    tBARCODE_CODE39 = BARCODE_CODE39,
    tBARCODE_EXCODE39 = BARCODE_EXCODE39,
    tBARCODE_EANX = BARCODE_EANX,
    tBARCODE_EAN128 = BARCODE_EAN128,
    tBARCODE_CODABAR = BARCODE_CODABAR,
    tBARCODE_CODE128 = BARCODE_CODE128,
    tBARCODE_DPLEIT = BARCODE_DPLEIT,
    tBARCODE_DPIDENT = BARCODE_DPIDENT,
    tBARCODE_CODE16K = BARCODE_CODE16K,
    tBARCODE_CODE49 = BARCODE_CODE49,
    tBARCODE_CODE93 = BARCODE_CODE93,
    tBARCODE_FLAT = BARCODE_FLAT,
    tBARCODE_RSS14 = BARCODE_RSS14,
    tBARCODE_RSS_LTD = BARCODE_RSS_LTD,
    tBARCODE_RSS_EXP = BARCODE_RSS_EXP,
    tBARCODE_TELEPEN = BARCODE_TELEPEN,
    tBARCODE_UPCA = BARCODE_UPCA,
    tBARCODE_UPCE = BARCODE_UPCE,
    tBARCODE_POSTNET = BARCODE_POSTNET,
    tBARCODE_MSI_PLESSEY = BARCODE_MSI_PLESSEY,
    tBARCODE_FIM = BARCODE_FIM,
    tBARCODE_LOGMARS = BARCODE_LOGMARS,
    tBARCODE_PHARMA = BARCODE_PHARMA,
    tBARCODE_PZN = BARCODE_PZN,
    tBARCODE_PHARMA_TWO = BARCODE_PHARMA_TWO,
    tBARCODE_PDF417 = BARCODE_PDF417,
    tBARCODE_PDF417TRUNC = BARCODE_PDF417TRUNC,
    tBARCODE_MAXICODE = BARCODE_MAXICODE,
    tBARCODE_QRCODE = BARCODE_QRCODE,
    tBARCODE_CODE128B = BARCODE_CODE128B,
    tBARCODE_AUSPOST = BARCODE_AUSPOST,
    tBARCODE_AUSREPLY = BARCODE_AUSREPLY,
    tBARCODE_AUSROUTE = BARCODE_AUSROUTE,
    tBARCODE_AUSREDIRECT = BARCODE_AUSREDIRECT,
    tBARCODE_ISBNX = BARCODE_ISBNX,
    tBARCODE_RM4SCC = BARCODE_RM4SCC,
    tBARCODE_DATAMATRIX = BARCODE_DATAMATRIX,
    tBARCODE_EAN14 = BARCODE_EAN14,
    tBARCODE_CODABLOCKF = BARCODE_CODABLOCKF,
    tBARCODE_NVE18 = BARCODE_NVE18,
    tBARCODE_JAPANPOST = BARCODE_JAPANPOST,
    tBARCODE_KOREAPOST = BARCODE_KOREAPOST,
    tBARCODE_RSS14STACK = BARCODE_RSS14STACK,
    tBARCODE_RSS14STACK_OMNI = BARCODE_RSS14STACK_OMNI,
    tBARCODE_RSS_EXPSTACK = BARCODE_RSS_EXPSTACK,
    tBARCODE_PLANET = BARCODE_PLANET,
    tBARCODE_MICROPDF417 = BARCODE_MICROPDF417,
    tBARCODE_ONECODE = BARCODE_ONECODE,
    tBARCODE_PLESSEY = BARCODE_PLESSEY,
    tBARCODE_TELEPEN_NUM = BARCODE_TELEPEN_NUM,
    tBARCODE_ITF14 = BARCODE_ITF14,
    tBARCODE_KIX = BARCODE_KIX,
    tBARCODE_AZTEC = BARCODE_AZTEC,
    tBARCODE_DAFT = BARCODE_DAFT,
    tBARCODE_MICROQR = BARCODE_MICROQR,
    tBARCODE_HIBC_128 = BARCODE_HIBC_128,
    tBARCODE_HIBC_39 = BARCODE_HIBC_39,
    tBARCODE_HIBC_DM = BARCODE_HIBC_DM,
    tBARCODE_HIBC_QR = BARCODE_HIBC_QR,
    tBARCODE_HIBC_PDF = BARCODE_HIBC_PDF,
    tBARCODE_HIBC_MICPDF = BARCODE_HIBC_MICPDF,
    tBARCODE_HIBC_BLOCKF = BARCODE_HIBC_BLOCKF,
    tBARCODE_HIBC_AZTEC = BARCODE_HIBC_AZTEC,
    tBARCODE_AZRUNE = BARCODE_AZRUNE,
    tBARCODE_CODE32 = BARCODE_CODE32,
    tBARCODE_EANX_CC = BARCODE_EANX_CC,
    tBARCODE_EAN128_CC = BARCODE_EAN128_CC,
    tBARCODE_RSS14_CC = BARCODE_RSS14_CC,
    tBARCODE_RSS_LTD_CC = BARCODE_RSS_LTD_CC,
    tBARCODE_RSS_EXP_CC = BARCODE_RSS_EXP_CC,
    tBARCODE_UPCA_CC = BARCODE_UPCA_CC,
    tBARCODE_UPCE_CC = BARCODE_UPCE_CC,
    tBARCODE_RSS14STACK_CC = BARCODE_RSS14STACK_CC,
    tBARCODE_RSS14_OMNI_CC = BARCODE_RSS14_OMNI_CC,
    tBARCODE_RSS_EXPSTACK_CC = BARCODE_RSS_EXPSTACK_CC,
    tBARCODE_CHANNEL = BARCODE_CHANNEL,
    tBARCODE_CODEONE = BARCODE_CODEONE,
    tBARCODE_GRIDMATRIX = BARCODE_GRIDMATRIX);

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
var
  temp : PZSymbol;
begin
  New(temp);

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


  Dispose(temp);
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

  CheckForError(ZBarcode_Encode_and_Buffer(FSymbol, PChar(FData), Length(FData), rotation));
end;

function TZintBarcode.ErrorTextFromSymbol: String;
begin
  Result := StrPas(@FSymbol.errtxt);
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
  S : String;
begin
  case Index of
    0: S := FSymbol.fgcolour;
    1: S := FSymbol.bgcolour;
  end;

  Result := StrToInt('$' + S);
end;

function TZintBarcode.GetData: Widestring;
begin
  Result := UTF8Decode(FData);
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
  Result := StrPas(@FSymbol.primary);
end;

function TZintBarcode.GetSHRT: Boolean;
begin
  Result := FSymbol.show_human_readable_text = 1;
end;

function TZintBarcode.GetType: TZBType;
begin
  Result := TZBType(FSymbol.symbology);
end;

procedure TZintBarcode.SetBorderWidth(const Value: Integer);
begin
  FSymbol.border_width := Value;
  Changed;
end;

procedure TZintBarcode.SetColor(const Index: Integer; const Value: TColor);
var
  S : String;
begin
  S := Format('%.6x', [Value]);

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

  FSymbol.symbology := Integer(Value);

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
