unit uCADCobblerSVG;

interface
uses
  Classes, Graphics, uCADCobblerTextFile, Types, SysUtils;

type
  TCCPieceSVGConverter = class(TCustomCCPieceConverter)
  protected
    FLineWidth : Double;
    FSVG : TStringList;
    FFormatSettings : TFormatSettings;
    procedure SetSVG(const Value: TStringList);
  public
    constructor Create(); override;
    destructor Destroy; override;

    procedure Convert(const APiece : TCCPiece); override;

    property LineWidth : Double read FLineWidth write FLineWidth;
    property SVG : TStringList read FSVG write SetSVG;
  end;

implementation

{ TCCPieceSVGConverter }

procedure TCCPieceSVGConverter.Convert(const APiece: TCCPiece);
var
  MinCoord, MaxCoord : TCCCoord;
  e : TCCPieceElement;
  bo : TCCPieceBOElement absolute e;
  li : TCCPieceLIElement absolute e;
  st : TCCPieceSTElement absolute e;
  coord : TCCCoord;
  Offset : TCCCoord;
  s : String;
  radius,
  strokeWidth : Double;
  fillColor : String;
begin
  inherited;

  FSVG.Clear;

  SearchMinMax(MinCoord, MaxCoord);
  Offset.X := Abs(MinCoord.X) + FLineWidth;
  Offset.Y := Abs(MinCoord.Y) + FLineWidth;

  FSVG.Append(Format('<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="%f" height="%f">',
                     [MaxCoord.X - MinCoord.X + FLineWidth * 2,
                      MaxCoord.Y - MinCoord.Y + FLineWidth * 2],
                     FFormatSettings));
  FSVG.Append(Format('<g id="%s"><title>%:1s</title><desc>%s</desc>', [APiece.Name + '/' + APiece.Size, APiece.Comment], FFormatSettings));

  for e in FPiece.Elements do
  begin
    if e is TCCPieceBOElement then
    begin
      s := Format('<polygon fill="none" stroke="black" stroke-width="%f" points="', [FLineWidth], FFormatSettings);
      for coord in bo.Coords do
        s := s + Format('%f,%f ', [coord.X + Offset.X,
                                   coord.Y * -1 + Offset.Y],
                                  FFormatSettings);
      s := s + '" />';
      FSVG.Append(s);
    end;

    if e is TCCPieceLIElement then
    begin
      s := Format('<polygon fill="none" stroke="black" stroke-width="%f" points="', [FLineWidth], FFormatSettings);
      for coord in li.Coords do
        s := s + Format('%f,%f ', [coord.X + Offset.X,
                                   coord.Y * -1 + Offset.Y],
                                  FFormatSettings);
      s := s + '" />';
      FSVG.Append(s);
    end;

    if e is TCCPieceSTElement then
    begin
      radius := st.Radius;
      if radius = 0 then
      begin
        radius := FLineWidth;
        strokeWidth := 0;
        fillColor := 'black';
      end
      else
      begin
        fillColor := 'none';
        strokeWidth := FLineWidth;
      end;

      for coord in st.Coords do
      begin
        s := Format('<circle fill="%s" stroke="black" stroke-width="%f" cx="%f" cy="%f" r="%f" />',
                    [fillColor,
                     strokeWidth,
                     coord.X + Offset.X,
                     coord.Y * -1 + Offset.Y,
                     radius],
                    FFormatSettings);
        FSVG.Append(s);
      end;
    end;
  end;

  FSVG.Append('</g></svg>');
end;

constructor TCCPieceSVGConverter.Create;
begin
  inherited;
  FSVG := TStringList.Create;

  FFormatSettings.DecimalSeparator:='.';
  FFormatSettings.ThousandSeparator:=#0;

  FLineWidth := 0.5;
end;

destructor TCCPieceSVGConverter.Destroy;
begin
  FSVG.Free;
  inherited;
end;

procedure TCCPieceSVGConverter.SetSVG(const Value: TStringList);
begin
  FSVG.Assign(Value);
end;

end.
