unit uZintComponent;

interface

uses
  Classes, SysUtils, Controls, zint, zint_render_canvas;

type
  TZintComponent = class(TGraphicControl)
  protected
    FSymbol : TZintSymbol;
    FRenderTarget : TZintRenderTargetCanvas;
    FPrimary: String;
    FData: String;
    procedure Paint; override;
    procedure SetRenderTarget(const Value: TZintRenderTargetCanvas); virtual;
    procedure SetSymbol(const Value: TZintSymbol); virtual;
    procedure SetData(const Index: Integer; const Value: String); virtual;
    procedure OnChange(ASender : TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Symbol : TZintSymbol read FSymbol write SetSymbol;
    property Renderer : TZintRenderTargetCanvas read FRenderTarget write SetRenderTarget;

    property Data : String index 0 read FData write SetData;
    property Primary : String index 1 read FPrimary write SetData;

    property Align;
    property AlignWithMargins;
    property Constraints;
  end;

  procedure Register;

implementation

uses
  zint_helper, Graphics;

procedure Register;
begin
  RegisterComponents('Zint', [TZintComponent]);
end;

{ TZintComponent }

constructor TZintComponent.Create(AOwner: TComponent);
begin
  FData := '123456';
  FPrimary := '';
  FSymbol := TZintSymbol.Create(Self);
  FSymbol.input_mode := UNICODE_MODE;
  FSymbol.OnChange := OnChange;
  FRenderTarget := TZintRenderTargetCanvas.Create(Self);
  FRenderTarget.OnChange := OnChange;
  inherited;
end;

destructor TZintComponent.Destroy;
begin
  FSymbol.Free;
  FRenderTarget.Free;
  inherited;
end;

procedure TZintComponent.OnChange(ASender: TObject);
begin
  Invalidate;
end;

procedure TZintComponent.Paint;
begin
  try
    FSymbol.Clear;
    FSymbol.primary := StrToArrayOfChar(FPrimary);
    FSymbol.Encode(FData);
    FRenderTarget.Canvas := Canvas;
    FRenderTarget.XDesired := 0;
    FRenderTarget.YDesired := 0;
    FRenderTarget.WidthDesired := Width;
    FRenderTarget.HeightDesired := Height;
    FRenderTarget.Render(FSymbol);
  except
    on e : Exception do
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := clRed;
      Canvas.TextOut(0, 0, e.Message);
    end;
  end;
end;

procedure TZintComponent.SetData(const Index: Integer; const Value: String);
begin
  case Index of
    0 : FData := Value;
    1 : FPrimary := Value;
  end;
  OnChange(Self);
end;

procedure TZintComponent.SetRenderTarget(const Value: TZintRenderTargetCanvas);
begin
  FRenderTarget.Assign(Value);
end;

procedure TZintComponent.SetSymbol(const Value: TZintSymbol);
begin
  FSymbol.Assign(Value);
end;

end.
