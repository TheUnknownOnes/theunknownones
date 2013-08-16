unit zint_render_tadrawer;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TADrawerSVG, zint, TAChartUtils, Graphics, TADrawUtils;

type

  { TZintTADrawerRenderTarget }

  TZintTADrawerRenderTarget = class(TZintCustomRenderTarget)
  protected
    FDrawer : IChartDrawer;
    FTextOut : ISimpleTextOut;
    FFGColor: TChartColor;
    FBGColor: TChartColor;
    FFont: TFont;
    function CalcLeft(AX: Single): Integer;
    function CalcTop (AY: Single): Integer;
    procedure SetFont(const Value: TFont);
  public
    constructor Create(ADrawer: IChartDrawer; ATextOut: ISimpleTextOut; AWidthDesired, AHeigthDesired: Single); reintroduce; virtual;
    destructor Destroy; override;
    procedure Render(ASymbol : TZintSymbol); override;
    property ForegroundColor : TColor read FFGColor write FFGColor;
    property BackgroundColor : TColor read FBGColor write FBGColor;
    property Font: TFont read FFont write SetFont;
  end;

implementation

{ TZintTADrawerRenderTarget }

function TZintTADrawerRenderTarget.CalcLeft(AX: Single): Integer;
begin
  Result:=Round(FLeft+AX*FMultiplikator);
end;

function TZintTADrawerRenderTarget.CalcTop(AY: Single): Integer;
begin
  Result:=Round(FTop+AY*FMultiplikator);
end;

procedure TZintTADrawerRenderTarget.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

constructor TZintTADrawerRenderTarget.Create(ADrawer: IChartDrawer; ATextOut: ISimpleTextOut; AWidthDesired, AHeigthDesired: Single);
begin
  inherited Create();

  FFont:=TFont.Create;
  FFont.Color:=clBlack;
  FFGColor:=clBlack;
  FBGColor:=clWhite;


  FWidthDesired:=AWidthDesired;
  FHeightDesired:=AHeigthDesired;

  FDrawer := ADrawer;
  FTextOut:= ATextOut;
end;

destructor TZintTADrawerRenderTarget.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TZintTADrawerRenderTarget.Render(ASymbol: TZintSymbol);
var
  line : Pzint_render_line;
  hexagon : Pzint_render_hexagon;
  hexagon_width, hexagon_height: single;
  ring : Pzint_render_ring;
  s : Pzint_render_string;
  Points : array[0..5] of TPoint;
  newPen : TPen;
begin
  inherited;

  if Assigned(FDrawer) then
  begin
    FDrawer.DrawingBegin(Rect(CalcLeft(0),
                        CalcTop(0),
                        CalcLeft(ASymbol.rendered^.width),
                        CalcTop(ASymbol.rendered^.height)));
    //clear Background
    if not FTransparent then
    begin
      FDrawer.SetBrushParams(bsSolid, FBGColor);
      FDrawer.SetPenParams(psSolid, FBGColor);

      FDrawer.Rectangle(CalcLeft(0),
                        CalcTop(0),
                        CalcLeft(ASymbol.rendered^.width),
                        CalcTop(ASymbol.rendered^.height));
    end;

    FDrawer.SetPenParams(psSolid, FFGColor);
    FDrawer.SetBrushParams(bsSolid, FFGColor);
    line:=ASymbol.rendered^.lines;
    while Assigned(line) do
    begin
      FDrawer.Rectangle(CalcLeft(line^.x),
                        CalcTop(line^.y),
                        CalcLeft(line^.x + line^.width),
                        CalcTop(line^.y + line^.length));
      line:=line^.next;
    end;

    FDrawer.SetBrushParams(bsClear, FBGColor);
    ring:=ASymbol.rendered^.rings;
    while Assigned(ring) do
    begin
      newPen:=TPen.Create;
      newPen.Width:=round(ring^.line_width*FMultiplikator);
        FDrawer.SetPenParams(psSolid, FFGColor);

      FDrawer.Pen:=newPen;

      FDrawer.Ellipse(CalcLeft(ring^.x-ring^.radius),
                        CalcTop(ring^.y-ring^.radius),
                        CalcLeft(ring^.x + ring^.radius),
                        CalcTop(ring^.y + ring^.radius));
      ring:=ring^.next;
    end;
    FDrawer.SetPenParams(psClear, FBGColor);
    FDrawer.SetBrushParams(bsSolid, ffgcolor);
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

      FDrawer.Polygon(Points, 0, Length(Points));

      hexagon:=hexagon^.next;
    end;

    FDrawer.Font:=FFont;
    FDrawer.SetBrushParams(bsClear, FBGColor);

    s:=ASymbol.rendered^.strings;
    while assigned(s) do
    begin
      FFont.Size:=Round(FMultiplikator*S^.fsize);
      FDrawer.Font:=FFont;
      FTextOut.SimpleTextOut(CalcLeft(s^.x)-FDrawer.TextExtent(s^.text).x div 2, CalcTop(s^.y), s^.text);
    end;

      s:=s^.next;
  end;
  FDrawer.DrawingEnd;
end;

end.

