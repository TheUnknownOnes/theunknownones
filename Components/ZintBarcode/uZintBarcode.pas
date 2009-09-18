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

  TZintBarcode = class(TPersistent)
  private
    FSymbol : PZSymbol;
    FData : UTF8String;
    FRotation: TZBRotation;
    FOnEncoded: TNotifyEvent;

    procedure CheckForError(AReturnValue : Integer);
    function ErrorTextFromSymbol : String;
    procedure SetData(const Value: WideString);
    procedure Encode;

    procedure SetType(const Value: TZBarcodeType);
    function GetData: WideString;
    function GetType: TZBarcodeType;
    function GetScale: Single;
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
  public
    constructor Create();
    destructor Destroy(); override;

    procedure GetBarcode(const ABitmap : TBitmap);

    property BarcodeSize : TPoint read GetBarcodeSize;

    property OnEncoded : TNotifyEvent read FOnEncoded write FOnEncoded;
  published
    property Data : Widestring read GetData write SetData;
    property BarcodeType : TZBarcodeType read GetType write SetType;
    property Scale : Single read GetScale write SetScale;
    property Height : Integer read GetHeight write SetHeight;
    property BorderWidth : Integer read GetBorderWidth write SetBorderWidth;
    property OutputOptions : TZOutputOptions read GetOutputOptions write SetOutputOptions;
    property FGColor : TColor index 0 read GetColor write SetColor;
    property BGColor : TColor index 1 read GetColor write SetColor;
    property Option1 : Integer index 1 read GetOption write SetOption;
    property Option2 : Integer index 2 read GetOption write SetOption;
    property Option3 : Integer index 3 read GetOption write SetOption;
    property Roatation : TZBRotation read FRotation write SetRotation;
  end;

  TZintBarcodeControl = class(TGraphicControl)
  private
    FBarcode: TZintBarcode;

    procedure OnNewBarcode(Sender : TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Barcode : TZintBarcode read FBarcode;
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

constructor TZintBarcode.Create;
begin
  inherited;
  FSymbol := ZBarcode_Create;
  FSymbol.input_mode := Integer(UNICODE_MODE);

  FRotation := r0;
  FSymbol.outfile := '.BMP';
  Data := '123456789';

  if not Assigned(FSymbol) then
    raise Exception.Create('Can not create internal symbol structure');
end;

destructor TZintBarcode.Destroy;
begin
  if Assigned(FSymbol) then
    ZBarcode_Delete(FSymbol);
    
  inherited;
end;

procedure TZintBarcode.Encode();
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

  CheckForError(ZBarcode_Encode_and_Print_Rotated(FSymbol, PAnsiChar(FData), rotation));

  if Assigned(FOnEncoded) then
    FOnEncoded(Self);
end;

function TZintBarcode.ErrorTextFromSymbol: String;
begin
  Result := StrPas(@FSymbol.errtxt);
end;

procedure TZintBarcode.GetBarcode(const ABitmap: TBitmap);
begin
  ZBarcodeToBitmap(FSymbol, ABitmap);
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
  Result := UTF8Decode(StrPas(FSymbol.text));
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

function TZintBarcode.GetScale: Single;
begin
  Result := FSymbol.scale;
end;

function TZintBarcode.GetType: TZBarcodeType;
begin
  Result := TZBarcodeType(FSymbol.symbology);
end;

procedure TZintBarcode.SetBorderWidth(const Value: Integer);
begin
  FSymbol.border_width := Value;
  Encode;
end;

procedure TZintBarcode.SetColor(const Index: Integer; const Value: TColor);
var
  S : String;
begin
  S := IntToHex(GetRValue(Value), 2) + IntToHex(GetGValue(Value), 2) + IntToHex(GetBValue(Value), 2);

  case Index of
    0: StrPCopy(@FSymbol.fgcolour, S);
    1: StrPCopy(@FSymbol.bgcolour, S);
  end;

  Encode;
end;

procedure TZintBarcode.SetData(const Value: Widestring);
begin
  FData := UTF8Encode(Value);
  Encode;
end;

procedure TZintBarcode.SetHeight(const Value: Integer);
begin
  FSymbol.height := Value;
  Encode;
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

procedure TZintBarcode.SetRotation(const Value: TZBRotation);
begin
  FRotation := Value;
  Encode;
end;

procedure TZintBarcode.SetScale(const Value: Single);
begin
  FSymbol.scale := Value;
  Encode;
end;

procedure TZintBarcode.SetType(const Value: TZBarcodeType);
begin
  FSymbol.symbology := Integer(Value);
  Encode;
end;

{ TZintBarcodeControl }

constructor TZintBarcodeControl.Create(AOwner: TComponent);
begin
  inherited;

  FBarcode := TZintBarcode.Create;
  FBarcode.OnEncoded := OnNewBarcode;
end;

destructor TZintBarcodeControl.Destroy;
begin
  FBarcode.Free;
  inherited;
end;

procedure TZintBarcodeControl.OnNewBarcode(Sender: TObject);
begin
  Width := Barcode.BarcodeSize.X;
  Height := Barcode.BarcodeSize.Y;
end;

procedure TZintBarcodeControl.Paint;
var
  bmp : TBitmap;
begin
  inherited;

  bmp := TBitmap.Create;
  try
    FBarcode.GetBarcode(bmp);
    Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

end.
