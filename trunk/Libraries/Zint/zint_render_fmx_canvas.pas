unit zint_render_fmx_canvas;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

interface

uses
  zint, FMX.Types, system.UITypes;

type

  { TZintCanvasRenderTarget }

  TZintCanvasRenderTarget = class(TZintCustomRenderTarget)
  protected
    FCanvas : TCanvas;
    FFGColor: TColor;
    FBGColor: TColor;
    FFont: TFont;
    function CalcLeft(AX: Single): single;
    function CalcTop (AY: Single): single;
    procedure SetFont(const Value: TFont);
  public
    constructor Create(ACanvas: TCanvas); reintroduce; virtual;
    destructor Destroy; override;
    procedure Render(ASymbol : TZintSymbol); override;
    property ForegroundColor : TColor read FFGColor write FFGColor;
    property BackgroundColor : TColor read FBGColor write FBGColor;
    property Font: TFont read FFont write SetFont;
  end;

implementation

uses
  Classes, Types;

{ TZintCanvasRenderTarget }

function TZintCanvasRenderTarget.CalcLeft(AX: Single): single;
begin
  Result:=(FLeft+AX*FMultiplikator);
end;

function TZintCanvasRenderTarget.CalcTop(AY: Single): single;
begin
  Result:=(FTop+AY*FMultiplikator);
end;

procedure TZintCanvasRenderTarget.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

constructor TZintCanvasRenderTarget.Create(ACanvas: TCanvas);
begin
  inherited Create();

  FFont:=TFont.Create;
  FFGColor:=FMX.Types.claBlack;
  FBGColor:=FMX.Types.claWhite;

  if Assigned(ACanvas) then
  begin
    FWidthDesired:=ACanvas.Width;
    FHeightDesired:=ACanvas.Height;
  end;
  FCanvas := ACanvas;
end;

destructor TZintCanvasRenderTarget.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TZintCanvasRenderTarget.Render(ASymbol: TZintSymbol);
var
  line : Pzint_render_line;
  hexagon : Pzint_render_hexagon;
  hexagon_width, hexagon_height: single;
  ring : Pzint_render_ring;
  s : Pzint_render_string;
  Points : TPolygon;
begin
  inherited;

  if Assigned(FCanvas) then
  begin
    FCanvas.BeginScene;
    //clear Background
    if not FTransparent then
    begin
      FCanvas.Fill.Color:=FBGColor;
      FCanvas.Fill.Kind:=TBrushKind.bkSolid;
      FCanvas.Stroke.Kind:=TBrushKind.bkNone;

      FCanvas.FillRect(RectF(CalcLeft(0),
                        CalcTop(0),
                        CalcLeft(ASymbol.rendered^.width),
                        CalcTop(ASymbol.rendered^.height)), 0, 0, [], 1);
    end;

    FCanvas.Stroke.Kind:=TBrushKind.bkSolid;
    FCanvas.Stroke.Color:=ffgcolor;

    FCanvas.Fill.Color:=FFGColor;
    FCanvas.Fill.Kind:=TBrushKind.bkSolid;
    line:=ASymbol.rendered^.lines;
    while Assigned(line) do
    begin
      FCanvas.FillRect(RectF(CalcLeft(line^.x),
                        CalcTop(line^.y),
                        CalcLeft(line^.x + line^.width),
                        CalcTop(line^.y + line^.length)), 0, 0, [], 1);

      FCanvas.DrawRect(RectF(CalcLeft(line^.x),
                        CalcTop(line^.y),
                        CalcLeft(line^.x + line^.width),
                        CalcTop(line^.y + line^.length)), 0, 0, [], 1);
      line:=line^.next;
    end;

    FCanvas.Fill.Kind:=TBrushKind.bkNone;
    ring:=ASymbol.rendered^.rings;
    while Assigned(ring) do
    begin
      FCanvas.StrokeThickness:=ring^.line_width*FMultiplikator;

      FCanvas.DrawEllipse(RectF(CalcLeft(ring^.x-ring^.radius),
                        CalcTop(ring^.y-ring^.radius),
                        CalcLeft(ring^.x + ring^.radius),
                        CalcTop(ring^.y + ring^.radius)),1);
      ring:=ring^.next;
    end;

    FCanvas.Fill.Color:=ffgcolor;
    FCanvas.Fill.Kind:=TBrushKind.bkSolid;
    hexagon:=ASymbol.rendered^.hexagons;
    while Assigned(hexagon) do
    begin
      hexagon_width:=hexagon^.width*FHexagonScale;
      hexagon_height:=hexagon^.height*FHexagonScale;

      SetLength(Points, 6);
      Points[0] := PointF(calcLeft(hexagon^.x-(hexagon_width/2)), CalcTop(hexagon^.y + hexagon_height/4));
      Points[1] := PointF(calcLeft(hexagon^.x-(hexagon_width/2)), CalcTop(hexagon^.y + hexagon_height*3/4));
      Points[2] := PointF(calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), CalcTop(hexagon^.y + hexagon_height));
      Points[3] := PointF(calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), CalcTop(hexagon^.y + hexagon_height * 3/4));
      Points[4] := PointF(calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), CalcTop(hexagon^.y + hexagon_height / 4));
      Points[5] := PointF(calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), CalcTop(hexagon^.y ));

      FCanvas.FillPolygon(Points, 1);

      hexagon:=hexagon^.next;
    end;

    FCanvas.Font.Assign(FFont);
    FCanvas.Fill.Kind:=TBrushKind.bkSolid;
    FCanvas.Fill.Color:=FFGColor;
    s:=ASymbol.rendered^.strings;
    while assigned(s) do
    begin
      FCanvas.Font.Size:=FMultiplikator*S^.fsize;
      if s^.width=0 then
      begin
        FCanvas.FillText(RectF(CalcLeft(s^.x) - FCanvas.TextWidth(s^.text) / 2,
                               CalcTop(s^.y),
                               CalcLeft(s^.x)+FCanvas.TextWidth(s^.text)/2,
                               CalcTop(s^.y)+FCanvas.TextHeight(s^.text)),
                         s^.text, false,1, [], TTextAlign.taCenter);
      end
      else
      begin
        FCanvas.FillText(RectF(CalcLeft(s^.x - s^.width / 2),
                               CalcTop(s^.y),
                               CalcLeft(s^.x+s^.width / 2),
                               CalcTop(s^.y+FCanvas.TextHeight(s^.text))),
                         s^.text, false,1, [], TTextAlign.taCenter);
      end;

      s:=s^.next;
    end;
    FCanvas.EndScene;
  end;
end;

end.
