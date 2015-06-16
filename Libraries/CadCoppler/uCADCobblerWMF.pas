unit uCADCobblerWMF;

interface

uses
  Classes, Graphics, uCADCobblerTextFile, Types;

type
  TCCPieceWMFConverter = class(TCustomCCPieceConverter)
  protected
    FWMF : TMetafile;
    FLineWidth : Double;
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Convert(const APiece : TCCPiece); override;

    property LineWidth : Double read FLineWidth write FLineWidth;

    property WMF : TMetafile read FWMF;
  end;

implementation

{ TCCPieceWMFDrawer }

procedure TCCPieceWMFConverter.Convert(const APiece: TCCPiece);
var
  MinCoord,
  MaxCoord : TCCCoord;
  Offset : TPoint;
  Canvas : TMetafileCanvas;
  e : TCCPieceElement;
  li : TCCPieceLIElement absolute e;
  bo : TCCPieceBOElement absolute e;
  st : TCCPieceSTElement absolute e;
const
  Multiplier = 10000;

  procedure DrawLI;
  var
    coord : TCCCoord;
    points : array of TPoint;
    idx : Integer;
  begin
    SetLength(points, li.Coords.Count);
    idx := 0;
    for coord in li.Coords do
    begin
      points[idx] := Point(Trunc(coord.X * Multiplier) + Offset.X,
                           Trunc(coord.Y * -1 * Multiplier) + Offset.Y);
      Inc(idx);
    end;

    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := Trunc(FLineWidth * Multiplier);
    Canvas.Polyline(points);
  end;

  procedure DrawBO;
  var
    coord : TCCCoord;
    points : array of TPoint;
    idx : Integer;
  begin
    SetLength(points, bo.Coords.Count);
    idx := 0;
    for coord in bo.Coords do
    begin
      points[idx] := Point(Trunc(coord.X * Multiplier) + Offset.X,
                           Trunc(coord.Y * -1 * Multiplier) + Offset.Y);
      Inc(idx);
    end;

    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := Trunc(FLineWidth * Multiplier);
    Canvas.Polyline(points);
  end;

  procedure DrawST;
  var
    coord : TCCCoord;
    radius : Double;
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := Trunc(FLineWidth * Multiplier);

    radius := st.Radius;
    if radius = 0 then
      radius := FLineWidth;

    for coord in st.Coords do
    begin
      Canvas.Ellipse(Trunc((coord.X - FLineWidth / 2) * Multiplier) + Offset.X,
                     Trunc((coord.Y - FLineWidth / 2) * -1 * Multiplier) + Offset.Y,
                     Trunc((coord.X + FLineWidth / 2) * Multiplier) + Offset.X,
                     Trunc((coord.Y + FLineWidth / 2) * -1 * Multiplier) + Offset.Y);
    end;


  end;

begin
  inherited;
  FWMF.Clear;

  SearchMinMax(MinCoord, MaxCoord);
  Offset := Point(Abs(Trunc((MinCoord.X - FLineWidth) * Multiplier)), Abs(Trunc((MinCoord.Y - FLineWidth) * Multiplier)));

  FWMF.SetSize(Trunc((MaxCoord.X - MinCoord.X + FLineWidth * 2) * Multiplier), Trunc((MaxCoord.Y - MinCoord.Y + FLineWidth * 2) * Multiplier));

  Canvas := TMetafileCanvas.Create(FWMF, 0);
  try
    for e in FPiece.Elements do
    begin
      if e is TCCPieceBOElement then
        DrawBO;
    end;

    for e in FPiece.Elements do
    begin
      if e is TCCPieceLIElement then
        DrawLI;

      if e is TCCPieceSTElement then
        DrawST;
    end;
  finally
    Canvas.Free;
  end;
end;

constructor TCCPieceWMFConverter.Create;
begin
  inherited;
  FWMF := TMetafile.Create;
  FLineWidth := 0.5;
end;

destructor TCCPieceWMFConverter.Destroy;
begin
  FWMF.Free;
  inherited;
end;

end.
