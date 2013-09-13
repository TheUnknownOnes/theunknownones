unit zint_render_lmf;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

{$mode objfpc}{$H+}

interface

uses
  zint, zint_render_canvas, Graphics, zint_lmf;

type

  { TZintLMFRenderTarget }

  TZintLMFRenderTarget = class(TZintCanvasRenderTarget)
  protected
    FLMF : TlmfImage;
    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
    procedure ReInitCanvas;
    procedure DrawText(const AParams: TZintDrawTextParams); override;
  public
    constructor Create(ALMF: TlmfImage); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
  end;

implementation

uses
  Types, Classes;

{ TZintLMFRenderTarget }

procedure TZintLMFRenderTarget.Inflate(const ANewWidth, ANewHeight: Single);
begin
  if Assigned(FCanvas) then
    FCanvas.Free;

  FLMF.Width := Round(ANewWidth);
  FLMF.Height := Round(ANewHeight);

  ReInitCanvas;
end;

procedure TZintLMFRenderTarget.ReInitCanvas;
begin
  FCanvas := TlmfCanvas.Create(FLMF);
  FCanvas.Font.Assign(FFont);
end;

procedure TZintLMFRenderTarget.DrawText(const AParams: TZintDrawTextParams);
var
  r : TRect;
  ts : TTextStyle;
  w : Integer;
begin
  //because the lmf ignores the Alignment, we have to align it manually
  r.Left := Round(AParams.X);
  r.Top := Round(AParams.Y);
  r.Right := Round(AParams.X + AParams.Width);
  r.Bottom := Round(AParams.Y + AParams.Height);
  FCanvas.Brush.Style := bsClear;
  ts.Layout := tlCenter;
  ts.SystemFont := False;
  w := FCanvas.TextWidth(AParams.Text);
  FCanvas.TextRect(r, r.Left + Round((AParams.Width - w) / 2), r.Top, AParams.Text, ts);
end;

constructor TZintLMFRenderTarget.Create(ALMF: TlmfImage);
begin
  inherited Create(nil);
  FLMF := ALMF;
  FWidthDesired := FLMF.Width;
  FHeightDesired := FLMF.Height;
end;

procedure TZintLMFRenderTarget.Render(ASymbol: TZintSymbol);
begin
  ReInitCanvas;
  try
    inherited;
  finally
    FCanvas.Free;
  end;
end;

end.
