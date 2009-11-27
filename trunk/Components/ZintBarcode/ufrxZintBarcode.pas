unit ufrxZintBarcode;

{$R 'frxZint.res'}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Menus, Controls, Variants,
  frxClass, frxDsgnIntf, uZintBarcode, uZintInterface, fs_iinterpreter;

type
  TfrxZintBarcodeDataFormat = (dfANSI, dfUTF8);

  TfrxZintBarcode = class(TfrxView)
  private
    FBarcode : TZintBarcode;
    FBitmap : TBitmap;
    FRotation : TZBRotation;
    FZoom : Single;
    FDataFormat: TfrxZintBarcodeDataFormat;

    FData : String;

    function GetZoom: Single;
    procedure SetZoom(const Value: Single);
    function GetData: String;
    procedure SetData(const Value: String);
    function GetBorderWidth: Integer;
    function GetColor(const Index: Integer): TColor;
    function GetOption(const Index: Integer): Integer;
    function GetOutputOptions: TZOutputOptions;
    function GetPrimary: String;
    function GetType: TZBType;
    procedure SetBorderWidth(const Value: Integer);
    procedure SetColor(const Index: Integer; const Value: TColor);
    procedure SetOption(const Index, Value: Integer);
    procedure SetOutputOptions(const Value: TZOutputOptions);
    procedure SetPrimary(const Value: String);
    procedure SetRotation(const Value: TZBRotation);
    procedure SetType(const Value: TZBType);
    function GetSHRT: Boolean;
    procedure SetSHRT(const Value: Boolean);
    procedure SetDataFormat(const Value: TfrxZintBarcodeDataFormat);
    function GetDataEncoded: Widestring;
  protected
    procedure BarcodeChanged(Sender: TObject);
    procedure SetHeight(Value: Extended); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDescription: String; override;
    procedure Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended); override;
  published
    property Zoom: Single read GetZoom write SetZoom;
    property BarcodeType : TZBType read GetType write SetType;  
    property DataFormat: TfrxZintBarcodeDataFormat read FDataFormat write SetDataFormat;
    property Data: String read GetData write SetData;
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
  end;

  TfrxZintBarcodeRTTI = class(TfsRTTIModule)
  public
    constructor Create(AScript : TfsScript); override;
  end;

implementation

uses
  Math;

{ TfrxZintBarcode }

procedure TfrxZintBarcode.BarcodeChanged(Sender: TObject);
begin
  FBarcode.OnChanged:=nil;
  try   
    FBarcode.Height:=Round(Self.Height / FZoom / 2);
    FBarcode.GetBarcode(FBitmap);

    Width:=Round(FBitmap.Width * FZoom);

  finally
    FBarcode.OnChanged:=Self.BarcodeChanged;
  end;
end;

constructor TfrxZintBarcode.Create(AOwner: TComponent);
begin
  inherited;
  
  FZoom:=1;

  FBitmap:=TBitmap.Create;

  FBarcode:=TZintBarcode.Create;
  BarcodeChanged(nil);
  FBarcode.OnChanged:=BarcodeChanged;
end;

destructor TfrxZintBarcode.Destroy;
begin
  FBarcode.Free;
  FBitmap.Free;
  inherited;
end;

procedure TfrxZintBarcode.Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX,
  OffsetY: Extended);
begin
  inherited;

  if IsDataField then
    FBarcode.Data:=DataSet.Value[DataField];

  Canvas.StretchDraw(Rect(FX,
                          FY,
                          FX+Round(FBitmap.Width * ScaleX * FZoom),
                          FY+Round(FBitmap.Height * ScaleY * FZoom)),
                     FBitmap);
end;

function TfrxZintBarcode.GetBorderWidth: Integer;
begin
  Result:=FBarcode.BorderWidth;
end;

function TfrxZintBarcode.GetColor(const Index: Integer): TColor;
begin
  case Index of
    0 : Result:=FBarcode.FGColor;
    1 : Result:=FBarcode.BGColor;
  end;
end;

function TfrxZintBarcode.GetData: String;
begin
  Result:=FData;
end;

function TfrxZintBarcode.GetDataEncoded: Widestring;
begin
  case FDataFormat of
    dfANSI: Result:=FData;
    dfUTF8: Result:=UTF8Decode(FData);
  end;
end;

class function TfrxZintBarcode.GetDescription: String;
begin
  Result:='Zint Barcode';
end;

function TfrxZintBarcode.GetOption(const Index: Integer): Integer;
begin
  case Index of
    1 : Result:=FBarcode.Option1;
    2 : Result:=FBarcode.Option2;
    3 : Result:=FBarcode.Option3;
  end;
end;

function TfrxZintBarcode.GetOutputOptions: TZOutputOptions;
begin
  Result:=FBarcode.OutputOptions;
end;

function TfrxZintBarcode.GetPrimary: String;
begin
  Result:=FBarcode.Primary;
end;

function TfrxZintBarcode.GetZoom: Single;
begin
  Result:=FZoom;
end;

function TfrxZintBarcode.GetSHRT: Boolean;
begin
  Result:=FBarcode.ShowHumanReadableText;
end;

function TfrxZintBarcode.GetType: TZBType;
begin
  Result:=FBarcode.BarcodeType;
end;

procedure TfrxZintBarcode.SetBorderWidth(const Value: Integer);
begin
  FBarcode.BorderWidth:=Value;
end;

procedure TfrxZintBarcode.SetColor(const Index: Integer; const Value: TColor);
begin
  case Index of
    0 : FBarcode.FGColor:=Value;
    1 : FBarcode.BGColor:=Value;
  end;
end;

procedure TfrxZintBarcode.SetData(const Value: String);
var
  ws : WideString;
begin
  FData:=Value;

  ws:=GetDataEncoded;
  if ws<>EmptyWideStr then
    FBarcode.Data:=ws;
end;

procedure TfrxZintBarcode.SetDataFormat(const Value: TfrxZintBarcodeDataFormat);
var
  ws : WideString;
begin
  FDataFormat := Value;

  ws:=GetDataEncoded;
  if ws<>EmptyWideStr then
    FBarcode.Data:=ws;
end;

procedure TfrxZintBarcode.SetHeight(Value: Extended);
var
  NewHeight : Integer;
begin      
  if Value<>Height then
  begin
    if Value<=0 then
      Value:=0.1;

    inherited;

    BarcodeChanged(nil);
  end;
end;

procedure TfrxZintBarcode.SetOption(const Index, Value: Integer);
begin
  case Index of
    1 : FBarcode.Option1 := Value;
    2 : FBarcode.Option2 := Value;
    3 : FBarcode.Option3 := Value;
  end;
end;

procedure TfrxZintBarcode.SetOutputOptions(const Value: TZOutputOptions);
begin
  FBarcode.OutputOptions:=Value;
end;

procedure TfrxZintBarcode.SetPrimary(const Value: String);
begin
  FBarcode.Primary:=Value;
end;

procedure TfrxZintBarcode.SetRotation(const Value: TZBRotation);
begin
  FRotation := Value;
end;

procedure TfrxZintBarcode.SetZoom(const Value: Single);
begin
  FZoom:=Value;
  BarcodeChanged(self);
end;

procedure TfrxZintBarcode.SetSHRT(const Value: Boolean);
begin
  FBarcode.ShowHumanReadableText:=Value;
end;

procedure TfrxZintBarcode.SetType(const Value: TZBType);
begin
  FBarcode.BarcodeType:=Value;
end;

var
  bmp : TBitmap;

{ TfrxZintBarcodeRTTI }

constructor TfrxZintBarcodeRTTI.Create(AScript: TfsScript);
begin
  inherited;

  AScript.AddClass(TfrxZintBarcode, 'TfrxView');
end;

initialization
  bmp:=TBitmap.Create;
  bmp.LoadFromResourceName(HInstance, 'ZINTTUOLOGO');

  frxObjects.RegisterObject1(TfrxZintBarcode, bmp);
  fsRTTIModules.Add(TfrxZintBarcodeRTTI)

finalization
  frxObjects.Unregister(TfrxZintBarcode);
  bmp.Free;

end.
