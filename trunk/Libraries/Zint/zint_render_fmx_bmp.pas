unit zint_render_fmx_bmp;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

interface

uses
  zint, zint_render_fmx_canvas, FMX.Types;

type
  TZintBMPRenderTarget = class(TZintCanvasRenderTarget)
  protected
    FBMP : TBitmap;

    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
  public
    constructor Create(ABMP: TBitmap); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
  end;

implementation

{ TZintBMPRenderTarget }


procedure TZintBMPRenderTarget.Inflate(const ANewWidth, ANewHeight : Single);
begin
  FBMP.Height:=Round(ANewHeight);
  FBMP.Width:=Round(ANewWidth);
end;

constructor TZintBMPRenderTarget.Create(ABMP: TBitmap);
begin
  inherited Create(nil);
  FBMP:=ABMP;
  FWidthDesired:=ABMP.Width;
  FHeightDesired:=ABMP.Height;
end;

procedure TZintBMPRenderTarget.Render(ASymbol: TZintSymbol);
begin
  FCanvas:=FBMP.Canvas;

  inherited;
end;

end.
