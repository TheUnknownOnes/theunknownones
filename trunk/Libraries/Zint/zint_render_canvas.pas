unit zint_render_canvas;

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
  zint, Graphics;

type

  { TZintCanvasRenderTarget }

  TZintCanvasRenderTarget = class(TZintCustomRenderTarget)
  protected
    FCanvas : TCanvas;
    FFGColor: TColor;
    FBGColor: TColor;
    FFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure ClearBackground(const AParams : TZintClearBackgroundParams); override;
    procedure DrawRect(const AParams : TZintDrawRectParams); override;
    procedure DrawHexagon(const AParams : TZintDrawHexagonParams); override;
    procedure DrawRing(const AParams : TZintDrawRingParams); override;
    procedure DrawText(const AParams: TZintDrawTextParams); override;
    function CalcTextHeight(const AParams : TZintCalcTextHeightParams) : Single; override;
    function CalcTextWidth(const AParams : TZintCalcTextWidthParams) : Single; override;
    procedure RenderStart; override;
  public
    constructor Create(ACanvas: TCanvas); reintroduce; virtual;
    destructor Destroy; override;
    property ForegroundColor : TColor read FFGColor write FFGColor;
    property BackgroundColor : TColor read FBGColor write FBGColor;
    property Font: TFont read FFont write SetFont;
  end;

implementation

uses
  Classes, Types;

{ TZintCanvasRenderTarget }

procedure TZintCanvasRenderTarget.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TZintCanvasRenderTarget.ClearBackground(
  const AParams: TZintClearBackgroundParams);
begin
  FCanvas.Brush.Color:=FBGColor;
  FCanvas.Brush.Style:=bsSolid;
  FCanvas.Pen.Style:=psSolid;
  FCanvas.Pen.Color:=FBGColor;

  FCanvas.Rectangle(Round(AParams.X),
                    Round(AParams.Y),
                    Round(AParams.X) + Round(AParams.Width),
                    Round(AParams.Y) + Round(AParams.Height));
end;

procedure TZintCanvasRenderTarget.DrawRect(const AParams: TZintDrawRectParams);
begin
  FCanvas.Pen.Style:=psSolid;
  FCanvas.Pen.Color:=FFGColor;
  FCanvas.Brush.Color:=FFGColor;
  FCanvas.Brush.Style:=bsSolid;
  FCanvas.Rectangle(Round(AParams.X),
                    Round(AParams.Y),
                    Round(AParams.X + AParams.Width),
                    Round(AParams.Y + AParams.Height));

end;

procedure TZintCanvasRenderTarget.DrawHexagon(const AParams: TZintDrawHexagonParams);
var
  hexagon_width, hexagon_height : Single;
  Points : array[0..5] of TPoint;
begin
  FCanvas.Pen.Style:=psClear;
  FCanvas.Brush.Style:=bsSolid;
  FCanvas.Brush.Color:=ffgcolor;

  hexagon_width:=AParams.Width*FHexagonScale;
  hexagon_height:=AParams.Height*FHexagonScale;

  Points[0] := Point(Round(AParams.X-(hexagon_width/2)), Round(AParams.Y - hexagon_height/4));
  Points[1] := Point(Round(AParams.X-(hexagon_width/2)), Round(AParams.Y + hexagon_height/4));
  Points[2] := Point(Round(AParams.X -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), Round(AParams.Y + hexagon_height / 2));
  Points[3] := Point(Round(AParams.X -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), Round(AParams.Y + hexagon_height / 4));
  Points[4] := Point(Round(AParams.X -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), Round(AParams.Y - hexagon_height / 4));
  Points[5] := Point(Round(AParams.X -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), Round(AParams.Y - hexagon_height / 2));

  {$IFDEF FPC}
  FCanvas.Polygon(@Points[0], Length(Points), true);
  {$ELSE}
  FCanvas.Polygon(Points);
  {$ENDIF}
end;

procedure TZintCanvasRenderTarget.DrawRing(const AParams: TZintDrawRingParams);
var
  LineWidth, HalfLineWidth : Integer;
begin
  LineWidth := Round(AParams.OuterRadius - AParams.InnerRadius);
  HalfLineWidth := Round((AParams.OuterRadius - AParams.InnerRadius) / 2);
  FCanvas.Brush.Style := bsClear;
  FCanvas.Pen.Width := LineWidth;
  FCanvas.Pen.Color := FFGColor;
  FCanvas.Pen.Style := psSolid;

  FCanvas.Ellipse(Round(AParams.x - AParams.OuterRadius + HalfLineWidth),
                  Round(AParams.y - AParams.OuterRadius + HalfLineWidth),
                  Round(AParams.x + AParams.OuterRadius - HalfLineWidth),
                  Round(AParams.y + AParams.OuterRadius - HalfLineWidth));
end;

procedure TZintCanvasRenderTarget.DrawText(const AParams: TZintDrawTextParams);
var
  r : TRect;
  txt : String;
  {$IFDEF FPC}
    ts : TTextStyle;
  {$ENDIF}
begin
  r.Left := Round(AParams.X);
  r.Top := Round(AParams.Y);
  r.Right := Round(AParams.X + AParams.Width);
  r.Bottom := Round(AParams.Y + AParams.Height);
  FCanvas.Brush.Style := bsClear;
  txt:=AParams.Text;
  {$IFDEF FPC}
    ts.Alignment := taCenter;
    ts.Layout := tlCenter;
    ts.SystemFont := False;
    FCanvas.TextRect(r, r.Left, r.Top, txt, ts);
  {$ELSE}
    FCanvas.TextRect(r, txt, [tfCenter, tfVerticalCenter]);
  {$ENDIF}
end;

function TZintCanvasRenderTarget.CalcTextHeight(const AParams : TZintCalcTextHeightParams): Single;
begin
  Result := FCanvas.TextHeight(AParams.Text);
end;

function TZintCanvasRenderTarget.CalcTextWidth(const AParams : TZintCalcTextWidthParams): Single;
begin
  Result := FCanvas.TextWidth(AParams.Text);
end;

procedure TZintCanvasRenderTarget.RenderStart;
begin
  FCanvas.Font.Assign(Font);
end;

constructor TZintCanvasRenderTarget.Create(ACanvas: TCanvas);
begin
  inherited Create();

  FFont:=TFont.Create;
  FFont.Color:=clBlack;
  FFGColor:=clBlack;
  FBGColor:=clWhite;

  if Assigned(ACanvas) then
  begin
    FWidth:=ACanvas.ClipRect.Right-ACanvas.ClipRect.Left;
    FHeight:=ACanvas.ClipRect.Right-ACanvas.ClipRect.Left;
  end;
  FCanvas := ACanvas;
end;

destructor TZintCanvasRenderTarget.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;


end.
