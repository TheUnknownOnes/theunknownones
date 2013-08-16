unit zint_render_wmf;

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
  TZintWMFRenderTarget = class(TZintCanvasRenderTarget)
  protected
    FWMF : TMetafile;
  public
    constructor Create(AWMF: TMetafile); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
  end;

implementation

{ TZintWMFRenderTarget }

constructor TZintWMFRenderTarget.Create(AWMF: TMetafile);
begin
  inherited Create(nil);
  FWMF:=AWMF;
  FWidthDesired:=FWMF.Width;
  FHeightDesired:=FWMF.Height;
end;

procedure TZintWMFRenderTarget.Render(ASymbol: TZintSymbol);
begin
  if FRenderAdjustMode=ramInflateImage then
  begin
    if ASymbol.rendered^.width+FLeft>FWMF.Width then
      FWMF.Width:=Round(ASymbol.rendered^.width+FLeft);
    if ASymbol.rendered^.height+FTop>FWMF.Height then
      FWMF.Height:=Round(ASymbol.rendered^.height+FTop);
  end;

  FCanvas:=TMetafileCanvas.Create(FWMF, 0);

  try
    inherited;
  finally
    FCanvas.Free;
  end;
end;

end.
