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
    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
  public
    constructor Create(ABMP: TBitmap); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
  end;

implementation

{ TZintBMPRenderTarget }

procedure TZintBMPRenderTarget.Inflate(const ANewWidth, ANewHeight: Single);
begin
  FBMP.SetSize(Round(ANewWidth), Round(ANewHeight));
end;

constructor TZintBMPRenderTarget.Create(ABMP: TBitmap);
begin
  inherited Create(nil);
  FBMP := ABMP;
  FXDesired := 0;
  FYDesired := 0;
  FWidthDesired := FBMP.Width;
  FHeightDesired := FBMP.Height;
end;

procedure TZintBMPRenderTarget.Render(ASymbol: TZintSymbol);
begin
  FCanvas := FBMP.Canvas;
  inherited;
end;

end.

