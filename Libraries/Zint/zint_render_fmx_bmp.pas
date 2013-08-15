unit zint_render_fmx_bmp;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)
}

interface

uses
  zint, zint_render_fmx_canvas, FMX.Types;

type
  TZintBMPRenderTarget = class(TZintCanvasRenderTarget)
  protected
    FBMP : TBitmap;
  public
    constructor Create(ABMP: TBitmap); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
  end;

implementation

{ TZintBMPRenderTarget }

constructor TZintBMPRenderTarget.Create(ABMP: TBitmap);
begin
  inherited Create(nil);
  FBMP:=ABMP;
  FWidthDesired:=ABMP.Width;
  FHeightDesired:=ABMP.Height;
end;

procedure TZintBMPRenderTarget.Render(ASymbol: TZintSymbol);
begin
  if FRenderAdjustMode=ramInflateImage then
  begin
    if ASymbol.rendered^.width+FLeft>FBMP.Width then
      FBMP.Width:=Round(ASymbol.rendered^.width+FLeft);
    if ASymbol.rendered^.height+FTop>FBMP.Height then
      FBMP.Height:=Round(ASymbol.rendered^.height+FTop);
  end;

  FCanvas:=FBMP.Canvas;

  inherited;
end;

end.
