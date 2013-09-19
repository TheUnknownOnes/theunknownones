unit ufrxZintComponent;

interface

{$R frxZint.res}

uses
  Classes, SysUtils, frxClass, frxDsgnIntf, fs_iinterpreter,
  Graphics, zint, zint_render_canvas, zint_helper;

type
  TfrxZintComponent = class(TfrxView)
  protected
    FRenderTarget : TZintRenderTargetCanvas;
    FSymbol : TZintSymbol;
    FData: String;
    FPrimary: String;
    procedure SetRenderer(const Value: TZintRenderTargetCanvas); virtual;
    procedure SetSymbol(const Value: TZintSymbol); virtual;
    procedure SetData(const Value: String); virtual;
    procedure SetPrimary(const Value: String); virtual;
    procedure Encode; virtual;
    procedure OnSymbolChange(Sender : TObject); virtual;
  public
    constructor Create(AOnwer : TComponent); override;
    destructor Destroy; override;

    procedure Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended); override;
    class function GetDescription: String; override;
  published
    property Data : String read FData write SetData;
    property Primary : String read FPrimary write SetPrimary;
    property Renderer : TZintRenderTargetCanvas read FRenderTarget write SetRenderer;
    property Symbol : TZintSymbol read FSymbol write SetSymbol;

    property Frame;
  end;

  TfrxZintBarcodeRTTI = class(TfsRTTIModule)
  public
    constructor Create(AScript : TfsScript); override;
  end;

implementation

{ TfrxZintComponent }

constructor TfrxZintComponent.Create(AOnwer: TComponent);
begin
  inherited;
  FRenderTarget := TZintRenderTargetCanvas.Create(Self);
  FSymbol := TZintSymbol.Create(Self);
  FSymbol.OnChange := OnSymbolChange;
  Data := '123456';
  FPrimary := '';
end;

destructor TfrxZintComponent.Destroy;
begin
  FRenderTarget.Free;
  FSymbol.Free;

  inherited;
end;

procedure TfrxZintComponent.Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX,
  OffsetY: Extended);
begin
  inherited;
  BeginDraw(Canvas, ScaleX, ScaleY, OffsetX, OffsetY);
  try
    FRenderTarget.Canvas := Canvas;
    FRenderTarget.XDesired := FX;
    FRenderTarget.YDesired := FY;
    FRenderTarget.WidthDesired := FDX;
    FRenderTarget.HeightDesired := FDY;
    FRenderTarget.Render(FSymbol);
  except
    on e : Exception do
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := clRed;
      Canvas.TextOut(FX, FY, e.Message);
    end;
  end;
end;

procedure TfrxZintComponent.Encode;
begin
  FSymbol.Clear;
  FSymbol.primary := StrToArrayOfChar(FPrimary);
  FSymbol.Encode(FData);
end;

class function TfrxZintComponent.GetDescription: String;
begin
  Result := 'Zint Barcode';
end;

procedure TfrxZintComponent.OnSymbolChange(Sender: TObject);
begin
  Encode;
end;

procedure TfrxZintComponent.SetData(const Value: String);
begin
  FData := Value;
  Encode;
end;

procedure TfrxZintComponent.SetPrimary(const Value: String);
begin
  FPrimary := Value;
  Encode;
end;

procedure TfrxZintComponent.SetRenderer(const Value: TZintRenderTargetCanvas);
begin
  FRenderTarget.Assign(Value);
end;

procedure TfrxZintComponent.SetSymbol(const Value: TZintSymbol);
begin
  FSymbol.Assign(Value);
end;

var
  bmp : TBitmap;

{ TfrxZintBarcodeRTTI }

constructor TfrxZintBarcodeRTTI.Create(AScript: TfsScript);
begin
  inherited;

  AScript.AddClass(TfrxZintComponent, 'TfrxView');
end;

initialization
  bmp:=TBitmap.Create;
  bmp.LoadFromResourceName(HInstance, 'ZINTTUOLOGO');

  frxObjects.RegisterObject1(TfrxZintComponent, bmp);
  fsRTTIModules.Add(TfrxZintBarcodeRTTI)

finalization
  frxObjects.Unregister(TfrxZintComponent);
  bmp.Free;

end.
