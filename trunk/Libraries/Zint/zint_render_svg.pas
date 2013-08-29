unit zint_render_svg;

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
  zint, Classes;

type
  TZintSVGColor = String;
  TZintSVGFont = String;

  TZintSVGDocOption = (doSingleFile, //A brand new SVG Document is created including header etc. -- existing StringList content is deleted
                       doInsertIntoExisting, //Barcode will be inserted into existing SVG document before the closing </svg> tag
                       doAppendFlat  //Barcode information will be written in SVG Format without and opening/closing tags
                       );
  { TZintSVGRenderTarget }

  TZintSVGRenderTarget = class(TZintCustomRenderTarget)
  protected
    FSVGFile : TStringList;
    FDocOption : TZintSVGDocOption;
    FIncludeDocInfo : Boolean;
    FFGColor: TZintSVGColor;
    FBGColor: TZintSVGColor;
    FFont: TZintSVGFont;
    function CalcLeft(AX: Single): Single;
    function CalcTop (AY: Single): Single;
    function CalcWidth(AX: Single): Single;
    function CalcHeight(AY: Single): Single;
  public
    constructor Create(ASVGFile: TStringList); reintroduce; virtual;
    procedure Render(ASymbol : TZintSymbol); override;
    property ForegroundColor : TZintSVGColor read FFGColor write FFGColor;
    property DocOption : TZintSVGDocOption read FDocOption write FDocOption;
    property BackgroundColor : TZintSVGColor read FBGColor write FBGColor;
    property Font: TZintSVGFont read FFont write FFont;
  end;

implementation

uses
  Types, SysUtils, zint_helper;


{ TZintSVGRenderTarget }

function TZintSVGRenderTarget.CalcHeight(AY: Single): Single;
begin
  Result:=AY*FMultiplikator;
end;

function TZintSVGRenderTarget.CalcLeft(AX: Single): Single;
begin
  Result:=FLeft+AX*FMultiplikator;
end;

function TZintSVGRenderTarget.CalcTop(AY: Single): Single;
begin
  Result:=FTop+AY*FMultiplikator;
end;

function TZintSVGRenderTarget.CalcWidth(AX: Single): Single;
begin
  Result:=AX*FMultiplikator;
end;

constructor TZintSVGRenderTarget.Create(ASVGFile: TStringList);
begin
  inherited Create;
  FSVGFile:=ASVGFile;
  FDocOption:=doSingleFile;
  FFGColor:='black';
  FBGColor:='white';
  FFont:='sans-serif';
end;

procedure TZintSVGRenderTarget.Render(ASymbol: TZintSymbol);
var
  ResultStr, tmpStr : string;
  line : Pzint_render_line;
  hexagon : Pzint_render_hexagon;
  hexagon_width, hexagon_height: single;
  ring : Pzint_render_ring;
  s : Pzint_render_string;
  p : Integer;
  fs : TFormatSettings;
  DocWidth, DocHeight: Single;
  i : integer;
begin
  inherited;
  if Assigned(FSVGFile) then
  begin
    fs.DecimalSeparator:='.';
    fs.ThousandSeparator:=#0;

    ResultStr:='<g><title>' + ArrayOfByteToString(ASymbol.text) + '</title><desc>Barcode generated using Zint</desc>';

    //clear Background
    if not FTransparent then
    begin
      ResultStr:=ResultStr+
          Format('<rect x="%f" y="%f" width="%f" height="%f" style="fill:%s;stroke-width:0"/>',
                      [CalcLeft(0),
                       CalcTop(0),
                       CalcWidth(ASymbol.rendered^.width),
                       CalcHeight(ASymbol.rendered^.height),
                       FBGColor],fs);
    end;


    line:=ASymbol.rendered^.lines;
    while Assigned(line) do
    begin
      ResultStr:=ResultStr+
        Format('<rect x="%f" y="%f" width="%f" height="%f" style="fill:%s;stroke-width:0"/>',
                      [CalcLeft(line^.x),
                        CalcTop(line^.y),
                        CalcWidth(line^.width),
                        CalcHeight(line^.length),
                       FFGColor],fs);
      line:=line^.next;
    end;

    ring:=ASymbol.rendered^.rings;
    while Assigned(ring) do
    begin
       ResultStr:=ResultStr+
        Format('<circle cx="%f" cy="%f" r="%f" stroke="%s" stroke-width="%f" fill="none"/>',
                      [CalcLeft(ring^.x),
                       CalcTop(ring^.y),
                       CalcWidth(ring^.radius),
                       FFGColor,
                       CalcWidth(ring^.line_width)],fs);
      ring:=ring^.next;
    end;

    hexagon:=ASymbol.rendered^.hexagons;
    while Assigned(hexagon) do
    begin
      hexagon_width:=hexagon^.width*FHexagonScale;
      hexagon_height:=hexagon^.height*FHexagonScale;

      ResultStr:=ResultStr+
        Format('<polygon fill="%s" points="%f,%f %f,%f %f,%f %f,%f %f,%f %f,%f" />',
                 [FFGColor,
                  calcLeft(hexagon^.x-(hexagon_width/2)), CalcTop(hexagon^.y + hexagon_height/4),
                  calcLeft(hexagon^.x-(hexagon_width/2)), CalcTop(hexagon^.y + hexagon_height*3/4),
                  calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), CalcTop(hexagon^.y + hexagon_height),
                  calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), CalcTop(hexagon^.y + hexagon_height * 3/4),
                  calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 2), CalcTop(hexagon^.y + hexagon_height / 4),
                  calcLeft(hexagon^.x -(hexagon_width/2) + sqrt(3) * hexagon_height / 4), CalcTop(hexagon^.y )], fs);

      hexagon:=hexagon^.next;
    end;

    s:=ASymbol.rendered^.strings;
    while assigned(s) do
    begin
      ResultStr:=ResultStr+
        Format('<text x="%f" y="%f" fill="%s" font-family="%s" font-size="%f" style="text-anchor:middle">%s</text>',
               [CalcLeft(s^.x), CalcTop(s^.y+s^.fsize/2), FFGColor, FFont, CalcHeight(s^.fsize), s^.text], fs);
      s:=s^.next;
    end;

    ResultStr:=ResultStr+'</g>';

    case FDocOption of
      doSingleFile: begin
                      DocWidth:=FWidthDesired;
                      DocHeight:=FHeightDesired;
                      if FRenderAdjustMode=ramInflateImage then
                      begin
                        if ASymbol.rendered^.width+FLeft>FWidthDesired then
                          DocWidth:=ASymbol.rendered^.width+FLeft;
                        if ASymbol.rendered^.height+FTop>FHeightDesired then
                          DocHeight:=ASymbol.rendered^.height+FTop;
                      end;

                      FSVGFile.Text:=
                        format('<svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="%f" height="%f">',[DocWidth, DocHeight], fs)+
                        ResultStr+
                        '</svg>';
                    end;
      doInsertIntoExisting: begin
                              tmpStr:=FSVGFile.Text;
                              p:=Pos('</svg>', LowerCase(tmpStr));
                              Insert(ResultStr, tmpStr, p);
                              FSVGFile.Text:=tmpStr;
                            end;
      doAppendFlat: FSVGFile.Append(ResultStr);
    end;
  end;
end;

end.
