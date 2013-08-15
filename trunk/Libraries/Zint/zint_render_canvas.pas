unit zint_render_canvas;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)
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
    function CalcLeft(AX: Single): Integer;
    function CalcTop (AY: Single): Integer;
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

function TZintCanvasRenderTarget.CalcLeft(AX: Single): Integer;
begin
  Result:=Round(FLeft+AX*FMultiplikator);
end;

function TZintCanvasRenderTarget.CalcTop(AY: Single): Integer;
begin
  Result:=Round(FTop+AY*FMultiplikator);
end;

procedure TZintCanvasRenderTarget.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
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
    FWidthDesired:=ACanvas.ClipRect.Right-ACanvas.ClipRect.Left;
    FHeightDesired:=ACanvas.ClipRect.Right-ACanvas.ClipRect.Left;
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
  Points : array[0..5] of TPoint;
begin
  inherited;

  if Assigned(FCanvas) then
  begin
    //clear Background
    if not FTransparent then
    begin
      FCanvas.Brush.Color:=FBGColor;
      FCanvas.Brush.Style:=bsSolid;
      FCanvas.Pen.Style:=psSolid;
      FCanvas.Pen.Color:=FBGColor;

      FCanvas.Rectangle(CalcLeft(0),
                        CalcTop(0),
                        CalcLeft(ASymbol.rendered^.width),
                        CalcTop(ASymbol.rendered^.height));
    end;

    FCanvas.Pen.Style:=psSolid;
    FCanvas.Pen.Color:=FFGColor;
    FCanvas.Brush.Color:=FFGColor;
    FCanvas.Brush.Style:=bsSolid;
    line:=ASymbol.rendered^.lines;
    while Assigned(line) do
    begin
      FCanvas.Rectangle(CalcLeft(line^.x),
                        CalcTop(line^.y),
                        CalcLeft(line^.x + line^.width),
                        CalcTop(line^.y + line^.length));
      line:=line^.next;
    end;

    FCanvas.Brush.Style:=bsClear;
    ring:=ASymbol.rendered^.rings;
    while Assigned(ring) do
    begin
      FCanvas.Pen.Width:=round(ring^.line_width*FMultiplikator);

      FCanvas.Ellipse(CalcLeft(ring^.x-ring^.radius),
                        CalcTop(ring^.y-ring^.radius),
                        CalcLeft(ring^.x + ring^.radius),
                        CalcTop(ring^.y + ring^.radius));
      ring:=ring^.next;
    end;

    FCanvas.Pen.Style:=psClear;
    FCanvas.Brush.Color:=ffgcolor;
    hexagon:=ASymbol.rendered^.hexagons;
    while Assigned(hexagon) do
    begin
      hexagon_width:=hexagon^.width*FHexagonScale;
      hexagon_height:=hexagon^.height*FHexagonScale;

      Points[0] := Point(calcLeft(hexagon^.x-(hexagon_width/2)), CalcTop(hexagon^.y + hexagon_height/4));
      Points[1] := Point(calcLeft(hexagon^.x-(hexagon_width/2)), CalcTop(hexagon^.y + hexagon_height*3/4));
      Points[2] := Point(calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), CalcTop(hexagon^.y + hexagon_height));
      Points[3] := Point(calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), CalcTop(hexagon^.y + hexagon_height * 3/4));
      Points[4] := Point(calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), CalcTop(hexagon^.y + hexagon_height / 4));
      Points[5] := Point(calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), CalcTop(hexagon^.y ));

      {$IFDEF FPC}
      FCanvas.Polygon(@Points[0], Length(Points), true);
      {$ELSE}
      FCanvas.Polygon(Points);
      {$ENDIF}

      hexagon:=hexagon^.next;
    end;

    FCanvas.Font.Assign(FFont);
    FCanvas.Brush.Style:=bsClear;

    s:=ASymbol.rendered^.strings;
    while assigned(s) do
    begin
      FCanvas.Font.Height:=Round(FMultiplikator*S^.fsize);
      if s^.width=0 then
      begin
        FCanvas.TextOut(CalcLeft(s^.x)-FCanvas.TextWidth(s^.text) div 2, CalcTop(s^.y), s^.text);
      end
      else
      begin
        FCanvas.TextRect(Rect(CalcLeft(s^.x), CalcTop(s^.y), CalcLeft(s^.x+s^.width), CalcTop(s^.y+FCanvas.Font.Height)), CalcLeft(s^.x), CalcTop(s^.y), s^.text);
      end;

      s:=s^.next;
    end;

  end;
end;

end.
