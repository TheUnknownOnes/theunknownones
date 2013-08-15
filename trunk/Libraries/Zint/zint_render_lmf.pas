unit zint_render_lmf;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)
}

{$mode objfpc}{$H+}

interface

uses
  zint, zint_render_canvas, Graphics, zint_lmf;

type
  TZintLMFRenderTarget = class(TZintCanvasRenderTarget)
  protected
    FLMF : TlmfImage;
  public
    constructor Create(ALMF: TlmfImage); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
  end;

implementation

{ TZintLMFRenderTarget }

constructor TZintLMFRenderTarget.Create(ALMF: TlmfImage);
begin
  inherited Create(nil);
  FLMF := ALMF;
  FWidthDesired := FLMF.Width;
  FHeightDesired := FLMF.Height;
end;

procedure TZintLMFRenderTarget.Render(ASymbol: TZintSymbol);
begin
  if FRenderAdjustMode = ramInflateImage then
  begin
    if ASymbol.rendered^.width + FLeft > FLMF.Width then
      FLMF.Width := Round(ASymbol.rendered^.width + FLeft);
    if ASymbol.rendered^.height + FTop > FLMF.Height then
      FLMF.Height := Round(ASymbol.rendered^.height + FTop);
  end;

  FCanvas := TlmfCanvas.Create(FLMF);

  try
    inherited;
  finally
    FCanvas.Free;
  end;
end;

end.
