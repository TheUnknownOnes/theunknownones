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
  zint, zint_render_canvas, Graphics, SysUtils;

type
  TZintWMFRenderTarget = class(TZintCanvasRenderTarget)
  protected
    FWMF : TMetafile;
    function CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single; override;
    procedure Inflate(const ANewWidth, ANewHeight : Single); override;
    procedure ReInitCanvas;
  public
    constructor Create(AWMF: TMetafile); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
  end;

implementation

{ TZintWMFRenderTarget }

function TZintWMFRenderTarget.CalcTextWidth(
  const AParams: TZintCalcTextWidthParams): Single;
begin
  Result:=inherited CalcTextWidth(AParams);

  if Length(AParams.Text)=1 then
    Result:=Result * 2;
end;

constructor TZintWMFRenderTarget.Create(AWMF: TMetafile);
begin
  inherited Create(nil);
  FWMF:=AWMF;
  FWidthDesired:=FWMF.Width;
  FHeightDesired:=FWMF.Height;
end;

procedure TZintWMFRenderTarget.Inflate(const ANewWidth, ANewHeight: Single);
begin
  if Assigned(FCanvas) then
    FreeAndNil(FCanvas);

  FWMF.SetSize(Round(ANewWidth), Round(ANewHeight));

  ReInitCanvas;
end;

procedure TZintWMFRenderTarget.ReInitCanvas;
begin
  FCanvas:=TMetafileCanvas.Create(FWMF, 0);
  FCanvas.Font.Assign(FFont);
  inherited;
end;

procedure TZintWMFRenderTarget.Render(ASymbol: TZintSymbol);
begin
  ReInitCanvas;
  try
    inherited;
  finally
    FCanvas.Free;
  end;
end;

end.
