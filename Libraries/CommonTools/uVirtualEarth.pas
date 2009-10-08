unit uVirtualEarth;

//Thanks to http://geekyprojects.blogspot.com/2006/11/virtual-earth-maps-on-go.html

interface

uses
  SysUtils, Math;

type
  TveTileSpecs = packed record
    Longitude,
    Latitude : Extended;
    Zoom : Integer;
    PositionX,
    PositionY,
    TileX,
    TileY : Integer;
    FileName,
    URL : String;
  end;

  TveMapType = (mtMap = Ord('r'),
                mtSatellit = Ord('a'),
                mtHybrid = Ord('h'));


function veGetTileSpecs(ALongitude, ALatitude : Extended; AZoom : Integer; AMapType : TveMapType = mtMap) : TveTileSpecs; overload;
function veGetTileSpecs(ATileX, ATileY : Integer; AZoom : Integer; AMapType : TveMapType = mtMap) : TveTileSpecs; overload;

function veHourMinSecToDeg(AHour, AMinute : Integer; ASecond : Extended) : Extended;
procedure veDegToHourMinSec(ADegree : Extended; out AHour, AMinute : Integer; out ASecond : Extended);

implementation

const
  veEarthRadius = 6378137;
  veEarthCircum = veEarthRadius * 2 * Pi;
  veEarthHalfCircum = veEarthCircum / 2;
  veURL = 'http://%1s%1d.ortho.tiles.virtualearth.net/tiles/%s?g=45';

function veHourMinSecToDeg(AHour, AMinute : Integer; ASecond : Extended) : Extended;
begin
  Result := AHour + (AMinute / 60) + (ASecond / 3600);
end;

procedure veDegToHourMinSec(ADegree : Extended; out AHour, AMinute : Integer; out ASecond : Extended);
var
  Rest : Extended;
begin
  Rest := ADegree;

  AHour := Trunc(Rest);
  Rest := (Rest - AHour) * 60;
  AMinute := Trunc(Rest);
  Rest := (Rest - AMinute) * 60;
  ASecond := Rest;
end;

function veLongitudeToXAtZoom(ALongitude : Extended; AZoom : Integer) : Integer;
var
  Arc : Extended;
  MetersX : Extended;
begin
  Arc := veEarthCircum / ((1 shl AZoom) * 256);
  MetersX := veEarthRadius * DegToRad(ALongitude);

  Result := Round((MetersX + veEarthHalfCircum) / Arc);
end;

function veLatitudeToYAtZoom(ALatitude : Extended; AZoom : Integer) : Integer;
var
  Arc,
  SinLat,
  MetersY : Extended;
begin
  Arc := veEarthCircum / ((1 shl AZoom) * 256);
  SinLat := Sin(DegToRad(ALatitude));
  MetersY := veEarthRadius / 2 * Ln((1 + SinLat) / (1 - SinLat));

  Result := Round((veEarthHalfCircum - MetersY) / Arc);
end;

function veTileToQuadKey(ATileX, ATileY, AZoom : Integer) : String;
var
  Mask,
  Cell,
  i : Integer;
begin
  Result := '';

  for i := AZoom downto 1 do
  begin
    Mask := 1 shl (i - 1);
    Cell := 0;

    if (ATileX and Mask) <> 0 then
      Inc(Cell);

    if (ATileY and Mask) <> 0 then
      Inc(Cell, 2);

    Result := Result + IntToStr(Cell);
  end;
end;

function veGetTileSpecs(ATileX, ATileY : Integer; AZoom : Integer; AMapType : TveMapType = mtMap) : TveTileSpecs; overload;
var
  Server : Integer;
  FileExt : String;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  Result.Zoom := AZoom;
  Result.TileX := ATileX;
  Result.TileY := ATileY;

  Server := ((Result.TileX and 1) + ((Result.TileY and 1) shl 1)) mod 4;

  case AMapType of
    mtMap : FileExt := 'png';
    mtSatellit,
    mtHybrid : FileExt := 'jpeg';
  end;

  Result.FileName := Format(Chr(Byte(AMapType)) +'%s.%s', [veTileToQuadKey(Result.TileX, Result.TileY, AZoom), FileExt]);
  Result.URL := Format(veURL, [Chr(Byte(AMapType)), Server, Result.FileName]);
end;

function veGetTileSpecs(ALongitude, ALatitude : Extended; AZoom : Integer; AMapType : TveMapType = mtMap) : TveTileSpecs; overload;
begin
  Result.TileX := veLongitudeToXAtZoom(ALongitude, AZoom) div 256;
  Result.TileY := veLatitudeToYAtZoom(ALatitude, AZoom) div 256;

  Result := veGetTileSpecs(Result.TileX, Result.TileY, AZoom, AMapType);

  Result.Longitude := ALongitude;
  Result.Latitude := ALatitude;

  Result.PositionX := veLongitudeToXAtZoom(ALongitude, AZoom) mod 256;
  Result.PositionY := veLatitudeToYAtZoom(ALatitude, AZoom) mod 256;
end;

end.
