unit zint_render_bmp;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  zint, zint_render_canvas, Graphics;

type

  { TZintBMPRenderTarget }

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
  FBMP := ABMP;
  FWidthDesired := FBMP.Width;
  FHeightDesired := FBMP.Height;
end;

procedure TZintBMPRenderTarget.Render(ASymbol: TZintSymbol);
begin
  if FRenderAdjustMode = ramInflateImage then
  begin
    if ASymbol.rendered^.width + FLeft > FBMP.Width then
      FBMP.Width := Round(ASymbol.rendered^.width + FLeft);
    if ASymbol.rendered^.height + FTop > FBMP.Height then
      FBMP.Height := Round(ASymbol.rendered^.height + FTop);
  end;

  FCanvas := FBMP.Canvas;
  inherited;
end;

end.

